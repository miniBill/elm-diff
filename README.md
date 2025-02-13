# elm-diff
A diff implementation for Elm.

This is a fork of [jinjor/elm-diff](https://github.com/jinjor/elm-diff), adding the possibility of considering two items similar.

## The algorithm

This library implements [Wu's O(NP) algorithm](http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf). It shares the idea with [Myers's O(ND) algorithm](http://www.xmailserver.org/diff2.pdf), but is much faster in some cases.

## LICENSE

BSD-3-Clause

## Using `Diff.diffLinesWith` with `elm-test`

You can use this function to pretty print differences when comparing `String`s in `elm-test`.

```elm
expectEqualMultiline : String -> String -> Expect.Expectation
expectEqualMultiline exp actual =
    if exp == actual then
        Expect.pass

    else
        let
            header : String
            header =
                Ansi.Color.fontColor Ansi.Color.blue "Diff from expected to actual:"
        in
        Expect.fail
            (header
                ++ "\n"
                ++ (Diff.diffLinesWith
                        (Diff.defaultOptions
                            |> Diff.ignoreLeadingWhitespace
                        )
                        exp
                        actual
                        |> Diff.ToString.diffToString { context = 4, color = True }
                   )
            )
```
