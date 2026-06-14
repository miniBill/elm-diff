module TestsToString exposing (toStringTests)

import Diff
import Diff.ToString
import Expect
import Test exposing (Test, describe, test)


toStringTests : Test
toStringTests =
    describe "Diff.ToString"
        [ test "includes first unchanged line before a change" <|
            \() ->
                let
                    fst =
                        List.range 1 9
                            |> List.map String.fromInt
                            |> String.join "\n"

                    snd =
                        List.range 1 9
                            |> List.filter (\x -> x /= 6)
                            |> List.map String.fromInt
                            |> String.join "\n"

                    result =
                        Diff.diffLinesWith
                            (Diff.defaultOptions
                                |> Diff.ignoreLeadingWhitespace
                            )
                            fst
                            snd
                            |> Diff.ToString.diffToString { context = 10000, color = False }
                in
                Expect.all
                    [ String.startsWith " 1\n" >> Expect.equal True
                    , String.contains "-6\n" >> Expect.equal True
                    ]
                    result
        ]
