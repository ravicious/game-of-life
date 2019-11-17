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
        neighbourCountsForLiveCells =
            countNeighboursForLiveCells grid
    in
    List.foldl
        (\( point, neighbourCount ) gridAcc ->
            if neighbourCount == 2 || neighbourCount == 3 then
                -- Keep cell alive
                gridAcc

            else
                killCell point gridAcc
        )
        grid
        neighbourCountsForLiveCells


countNeighboursForLiveCells : Grid -> List ( Point, Int )
countNeighboursForLiveCells grid =
    liveCells grid
        |> List.map (\point -> ( point, countNeighboursForCell point grid ))


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


liveCells : Grid -> List Point
liveCells grid =
    IntDict.toList grid
        |> List.foldl
            (\( x, ys ) acc ->
                Set.foldl (\y acc_ -> ( x, y ) :: acc_) acc ys
            )
            []
