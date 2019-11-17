module Grid exposing (Grid, addLiveCell, init, isCellAlive, isCellDead, tick)

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


isCellAlive : Point -> Grid -> Bool
isCellAlive ( x, y ) grid =
    IntDict.get x grid
        |> Maybe.map (Set.member y)
        |> Maybe.withDefault False


isCellDead : Point -> Grid -> Bool
isCellDead point grid =
    not <| isCellAlive point grid


tick : Grid -> Grid
tick _ =
    init
