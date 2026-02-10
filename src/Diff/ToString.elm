module Diff.ToString exposing (diffToString)

{-| Convert a diff to a string representation.

@docs diffToString

-}

import Ansi.Color
import Diff exposing (Change(..))
import List.Extra


{-| Converts a diff between strings into a pretty-printed, optionally colored, formatted diff.

`context` is the number of lines of context to show in the diff.

-}
diffToString : { context : Int, color : Bool } -> List (Change (List (Change Never Char)) String) -> String
diffToString options lines =
    let
        groups : List ( Change (List (Change Never Char)) String, List (Change (List (Change Never Char)) String) )
        groups =
            gatherGroups lines

        groupCount : Int
        groupCount =
            List.length groups
    in
    groups
        |> List.indexedMap
            (\i ( head, tail ) ->
                case head of
                    NoChange _ ->
                        if i == 0 then
                            List.reverse (List.take options.context (List.reverse tail))

                        else if i == groupCount - 1 then
                            head
                                :: List.take (options.context - 1) tail

                        else if List.length tail > 2 * options.context then
                            head
                                :: List.take (options.context - 1) tail
                                ++ NoChange ""
                                :: NoChange "---"
                                :: NoChange ""
                                :: List.reverse (List.take options.context (List.reverse tail))

                        else
                            head :: tail

                    _ ->
                        head :: tail
            )
        |> List.concat
        |> List.map (changeToString options)
        |> String.join "\n"


changeToString : { a | color : Bool } -> Change (List (Change Never Char)) String -> String
changeToString options change =
    case change of
        NoChange before ->
            " " ++ before

        Similar _ _ d ->
            lineChangeToString options d

        Added line ->
            if options.color then
                brightGreen ("+" ++ line)

            else
                "+" ++ line

        Removed line ->
            if options.color then
                brightRed ("-" ++ line)

            else
                "-" ++ line


lineChangeToString : { a | color : Bool } -> List (Change Never Char) -> String
lineChangeToString options lines =
    let
        ( befores, afters ) =
            lines
                |> List.foldr
                    (\change ( beforeAcc, afterAcc ) ->
                        case change of
                            Similar _ _ ever ->
                                never ever

                            NoChange c ->
                                let
                                    s : String
                                    s =
                                        String.fromChar c
                                in
                                if options.color then
                                    ( brightRed s :: beforeAcc
                                    , brightGreen s :: afterAcc
                                    )

                                else
                                    ( s :: beforeAcc
                                    , s :: afterAcc
                                    )

                            Added a ->
                                let
                                    s : String
                                    s =
                                        if options.color then
                                            String.fromChar a
                                                |> Ansi.Color.fontColor Ansi.Color.black
                                                |> Ansi.Color.backgroundColor Ansi.Color.green

                                        else
                                            String.fromChar a
                                in
                                ( beforeAcc
                                , s :: afterAcc
                                )

                            Removed r ->
                                let
                                    s : String
                                    s =
                                        if options.color then
                                            String.fromChar r
                                                |> Ansi.Color.fontColor Ansi.Color.white
                                                |> Ansi.Color.backgroundColor Ansi.Color.red

                                        else
                                            String.fromChar r
                                in
                                ( s :: beforeAcc
                                , afterAcc
                                )
                    )
                    ( [], [] )
    in
    if options.color then
        brightRed ("-" ++ String.concat befores)
            ++ "\n"
            ++ brightGreen ("+" ++ String.concat afters)

    else
        ("-" ++ String.concat befores)
            ++ "\n"
            ++ ("+" ++ String.concat afters)


brightGreen : String -> String
brightGreen s =
    s
        |> Ansi.Color.fontColor Ansi.Color.black
        |> Ansi.Color.backgroundColor Ansi.Color.brightGreen


brightRed : String -> String
brightRed s =
    s
        |> Ansi.Color.fontColor Ansi.Color.white
        |> Ansi.Color.backgroundColor Ansi.Color.brightRed


gatherGroups : List (Change similar a) -> List ( Change similar a, List (Change similar a) )
gatherGroups list =
    List.Extra.groupWhile (\l r -> isChange l == isChange r) list


isChange : Change similar a -> Bool
isChange c =
    case c of
        NoChange _ ->
            False

        _ ->
            True
