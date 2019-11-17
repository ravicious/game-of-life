module Grid exposing (Grid, addLiveCell, init, isCellAlive, isCellDead, killCell, tick)

import IntDict exposing (IntDict)
import Set exposing (Set)


{-| IntDict keys are x coordinates, set values are y coordinates.
-}
type alias Grid =
    IntDict (Set Int)


type alias Point =
    ( Int, Int )


init : Grid
init =
    IntDict.empty


addLiveCell : Point -> Grid -> Grid
addLiveCell ( x, y ) grid =
    let
        update =
            Maybe.map (Set.insert y)
                >> Maybe.withDefault (Set.singleton y)
                >> Just
    in
    IntDict.update x update grid


killCell : Point -> Grid -> Grid
killCell ( x, y ) grid =
    let
        update =
            Maybe.map (Set.remove y)
    in
    IntDict.update x update grid


isCellAlive : Point -> Grid -> Bool
isCellAlive ( x, y ) grid =
    IntDict.get x grid
        |> Maybe.map (Set.member y)
        |> Maybe.withDefault False


isCellDead : Point -> Grid -> Bool
isCellDead point grid =
    not <| isCellAlive point grid


tick : Grid -> Grid
tick grid =
    let
        liveCells =
            getLiveCells grid

        neighbourCountsForLiveCells =
            countNeighboursForCells grid liveCells

        deadNeighboursOfLiveCells =
            getDeadNeighboursOfAliveCells liveCells

        neighbourCountsForDeadCells =
            countNeighboursForCells grid deadNeighboursOfLiveCells

        processAliveCells grid_ =
            List.foldl
                (\( point, neighbourCount ) gridAcc ->
                    if neighbourCount == 2 || neighbourCount == 3 then
                        -- Keep cell alive
                        gridAcc

                    else
                        killCell point gridAcc
                )
                grid_
                neighbourCountsForLiveCells

        processDeadCells grid_ =
            List.foldl
                (\( point, neighbourCount ) gridAcc ->
                    if neighbourCount == 3 then
                        -- Make cell alive.
                        addLiveCell point gridAcc

                    else
                        -- Keep it dead.
                        gridAcc
                )
                grid_
                neighbourCountsForDeadCells
    in
    grid
        |> processAliveCells
        |> processDeadCells


countNeighboursForCells : Grid -> List Point -> List ( Point, Int )
countNeighboursForCells grid points =
    List.map (\point -> ( point, countNeighboursForCell point grid )) points


neighbourPointDeltas =
    [ ( -1, 1 )
    , ( 0, 1 )
    , ( 1, 1 )
    , ( -1, 0 )
    , ( 1, 0 )
    , ( -1, -1 )
    , ( 0, -1 )
    , ( 1, -1 )
    ]


countNeighboursForCell : Point -> Grid -> Int
countNeighboursForCell ( x, y ) grid =
    List.foldl
        (\( dx, dy ) count ->
            let
                neighbourPoint =
                    ( x + dx, y + dy )
            in
            if isCellAlive neighbourPoint grid then
                count + 1

            else
                count
        )
        0
        neighbourPointDeltas


getLiveCells : Grid -> List Point
getLiveCells grid =
    IntDict.toList grid
        |> List.foldl
            (\( x, ys ) acc ->
                Set.foldl (\y acc_ -> ( x, y ) :: acc_) acc ys
            )
            []


getDeadNeighboursOfAliveCells : List Point -> List Point
getDeadNeighboursOfAliveCells alivePoints =
    let
        alivePointsSet =
            Set.fromList alivePoints
    in
    Set.toList <|
        -- Fold over alive points and check neighbours for each alive point.
        List.foldl
            (\( x, y ) deadPoints ->
                -- Fold over neighbour, checking if a neighbour is alive or dead.
                List.foldl
                    (\( dx, dy ) deadPoints_ ->
                        let
                            neighbourPoint =
                                ( x + dx, y + dy )
                        in
                        if Set.member neighbourPoint alivePointsSet then
                            deadPoints_

                        else
                            Set.insert neighbourPoint deadPoints_
                    )
                    deadPoints
                    neighbourPointDeltas
            )
            Set.empty
            alivePoints
