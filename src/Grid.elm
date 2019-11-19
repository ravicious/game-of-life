module Grid exposing (Grid, addLiveCell, init, isCellAlive, isCellDead, killCell, tick)

import Set exposing (Set)


{-| IntDict keys are x coordinates, set values are y coordinates.
-}
type alias Grid =
    Set Point


type alias Point =
    ( Int, Int )


type alias Points =
    Set Point


init : Grid
init =
    Set.empty


addLiveCell : Point -> Grid -> Grid
addLiveCell =
    Set.insert


killCell : Point -> Grid -> Grid
killCell =
    Set.remove


isCellAlive : Point -> Grid -> Bool
isCellAlive =
    Set.member


isCellDead : Point -> Grid -> Bool
isCellDead point grid =
    not <| isCellAlive point grid


tick : Grid -> Grid
tick grid =
    let
        liveCells =
            grid

        neighbourCountsForLiveCells =
            countNeighboursForCells grid liveCells

        deadNeighboursOfLiveCells =
            getDeadNeighboursOfAliveCells liveCells

        neighbourCountsForDeadCells =
            countNeighboursForCells grid deadNeighboursOfLiveCells

        processAliveCells grid_ =
            Set.foldl
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
            Set.foldl
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


countNeighboursForCells : Grid -> Points -> Set ( Point, Int )
countNeighboursForCells grid points =
    Set.map (\point -> ( point, countNeighboursForCell point grid )) points


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


getDeadNeighboursOfAliveCells : Points -> Points
getDeadNeighboursOfAliveCells alivePoints =
    -- Fold over alive points and check neighbours for each alive point.
    Set.foldl
        (\( x, y ) deadPoints ->
            -- Fold over neighbour, checking if a neighbour is alive or dead.
            List.foldl
                (\( dx, dy ) deadPoints_ ->
                    let
                        neighbourPoint =
                            ( x + dx, y + dy )
                    in
                    if Set.member neighbourPoint alivePoints then
                        deadPoints_

                    else
                        Set.insert neighbourPoint deadPoints_
                )
                deadPoints
                neighbourPointDeltas
        )
        Set.empty
        alivePoints
