module GridTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid
import Test exposing (..)


arbitraryPointFuzzer =
    Fuzz.map2 Tuple.pair int int


neighbourPointFuzzer =
    Fuzz.map2 Tuple.pair (Fuzz.intRange -1 -1) (Fuzz.intRange -1 -1)


gridTest : Test
gridTest =
    describe "Grid"
        [ fuzz arbitraryPointFuzzer "adds a live cell and verifies that it's alive" <|
            \point ->
                Grid.init
                    |> Grid.addLiveCell point
                    |> Grid.isCellAlive point
                    |> Expect.true "Expected cell to be alive"
        , fuzz (list arbitraryPointFuzzer) "adds multiple live cells and verifies that they're all alive" <|
            \points ->
                let
                    expected =
                        List.map (Tuple.pair True) points

                    grid =
                        List.foldl Grid.addLiveCell Grid.init points

                    actual =
                        List.map (\point -> Tuple.pair (Grid.isCellAlive point grid) point) points
                in
                Expect.equal expected actual
        ]


rules : Test
rules =
    describe "Game of Life rules"
        [ fuzz (list neighbourPointFuzzer) "A live cell dies if has less than two live neighbours" <|
            \neighbourPoints ->
                let
                    grid =
                        Grid.init |> Grid.addLiveCell ( 0, 0 )
                in
                neighbourPoints
                    |> List.take 1
                    |> List.foldl Grid.addLiveCell grid
                    |> Grid.tick
                    |> Grid.isCellDead ( 0, 0 )
                    |> Expect.true "Expected cell (0, 0) to be dead"
        ]
