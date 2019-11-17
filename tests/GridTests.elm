module GridTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid
import Random
import Random.List
import Test exposing (..)


arbitraryPointFuzzer =
    Fuzz.tuple ( int, int )


neighbourPointFuzzer =
    Fuzz.tuple ( Fuzz.intRange -1 -1, Fuzz.intRange -1 -1 )


allNeighbourPoints =
    [ ( -1, 1 )
    , ( 0, 1 )
    , ( 1, 1 )
    , ( -1, 0 )
    , ( 1, 0 )
    , ( -1, -1 )
    , ( 0, -1 )
    , ( 1, -1 )
    ]


neighbourPointsRangeFuzzer min max =
    Fuzz.map2
        (\neighbourCount seedInt ->
            let
                seed =
                    Random.initialSeed seedInt

                generator =
                    Random.List.shuffle allNeighbourPoints

                ( shuffledList, _ ) =
                    Random.step generator seed
            in
            shuffledList |> List.take neighbourCount
        )
        (Fuzz.intRange min max)
        Fuzz.int


twoOrThreeNeighbourPointsFuzzer =
    neighbourPointsRangeFuzzer 2 3


moreThanThreeNeighbourPointsFuzzer =
    neighbourPointsRangeFuzzer 4 8


threeNeighbourPointsFuzzer =
    neighbourPointsRangeFuzzer 3 3


gridTest : Test
gridTest =
    describe "Grid"
        [ fuzz (list arbitraryPointFuzzer) "adds multiple live cells and verifies that they're all alive" <|
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
        , fuzz (list arbitraryPointFuzzer) "adds multiple live cells, kills them and verifies that they're all dead" <|
            \points ->
                let
                    expected =
                        List.map (Tuple.pair True) points

                    gridWithAliveCells =
                        List.foldl Grid.addLiveCell Grid.init points

                    gridWithDeadCells =
                        List.foldl Grid.killCell gridWithAliveCells points

                    actual =
                        List.map (\point -> Tuple.pair (Grid.isCellDead point gridWithDeadCells) point) points
                in
                Expect.equal expected actual
        ]


rules : Test
rules =
    describe "Game of Life rules"
        [ fuzz (list neighbourPointFuzzer) "A live cell dies if has less than two neighbours" <|
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
        , fuzz twoOrThreeNeighbourPointsFuzzer "A live cell survives if it has two or three neighbours" <|
            \neighbourPoints ->
                let
                    grid =
                        Grid.init |> Grid.addLiveCell ( 0, 0 )
                in
                neighbourPoints
                    |> List.foldl Grid.addLiveCell grid
                    |> Grid.tick
                    |> Grid.isCellAlive ( 0, 0 )
                    |> Expect.true "Expected cell (0, 0) to be alive"
        , fuzz moreThanThreeNeighbourPointsFuzzer "A live cell dies if it has more than three neighbours" <|
            \neighbourPoints ->
                let
                    grid =
                        Grid.init |> Grid.addLiveCell ( 0, 0 )
                in
                neighbourPoints
                    |> List.foldl Grid.addLiveCell grid
                    |> Grid.tick
                    |> Grid.isCellDead ( 0, 0 )
                    |> Expect.true "Expected cell (0, 0) to be dead"
        , fuzz threeNeighbourPointsFuzzer "A dead cell becomes alive if it has exactly three neighbours" <|
            \neighbourPoints ->
                let
                    grid =
                        Grid.init
                in
                neighbourPoints
                    |> List.foldl Grid.addLiveCell grid
                    |> Grid.tick
                    |> Grid.isCellAlive ( 0, 0 )
                    |> Expect.true "Expected cell (0, 0) to become alive"
        ]
