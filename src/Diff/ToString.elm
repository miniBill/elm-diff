module Diff.ToString exposing
    ( Theme, Colors
    , diffToString
    , dracula
    )

{-| Convert a diff to a string representation.

@docs Theme, Colors
@docs diffToString


## Themes

@docs dracula

-}

import Ansi.Color
import Diff exposing (Change(..))
import List.Extra


{-| Colors to use to highlight the diff.

If the two inputs are:

        removed
        unchanged changedBefore

and

        unchanged changedAfter
        added

Then `unchanged` will have the `unchangedBefore` color in the `-` lines and `unchangedAfter` color in the `+` lines, and everything else will have the color with the same name.

-}
type alias Theme =
    { removed : Colors
    , unchangedBefore : Colors
    , unchangedAfter : Colors
    , changedBefore : Colors
    , changedAfter : Colors
    , added : Colors
    }


{-| A foreground/background color pair.

Use `Nothing` to avoid changing the color.

-}
type alias Colors =
    { foreground : Maybe Ansi.Color.Color
    , background : Maybe Ansi.Color.Color
    }


{-| A theme inspired by the [Dracula](https://draculatheme.com) color scheme as used by [`delta`](https://github.com/dandavison/delta).
-}
dracula : Theme
dracula =
    let
        color : Int -> Int -> Int -> Ansi.Color.Color
        color red green blue =
            Ansi.Color.CustomTrueColor
                { red = red
                , green = green
                , blue = blue
                }

        offWhite : Ansi.Color.Color
        offWhite =
            color 0xDE 0xDE 0xDE

        darkGreen : Ansi.Color.Color
        darkGreen =
            color 0x0C 0x26 0x05

        brightGreen : Ansi.Color.Color
        brightGreen =
            color 0x27 0x5D 0x17

        darkRed : Ansi.Color.Color
        darkRed =
            color 0x38 0x04 0x02

        brightRed : Ansi.Color.Color
        brightRed =
            color 0x84 0x1F 0x19

        white : Ansi.Color.Color
        white =
            color 0xFF 0xFF 0xFF

        removed : Colors
        removed =
            { background = Just darkRed
            , foreground = Just offWhite
            }

        added : Colors
        added =
            { background = Just darkGreen
            , foreground = Just white
            }
    in
    { removed = removed
    , unchangedBefore = removed
    , changedBefore = { foreground = Just offWhite, background = Just brightRed }
    , changedAfter = { foreground = Just white, background = Just brightGreen }
    , unchangedAfter = added
    , added = added
    }


{-| Converts a diff between strings into a pretty-printed, optionally colored, formatted diff.

`context` is the number of lines of context to show in the diff.

-}
diffToString : { context : Int, colors : Maybe Theme } -> List (Change (List (Change Never Char)) String) -> String
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


changeToString : { a | colors : Maybe Theme } -> Change (List (Change Never Char)) String -> String
changeToString options change =
    case change of
        NoChange before ->
            " " ++ before

        Similar _ _ d ->
            lineChangeToString options d

        Added line ->
            colorWith .added options ("+" ++ line)

        Removed line ->
            colorWith .removed options ("-" ++ line)


colorWith : (Theme -> Colors) -> { a | colors : Maybe Theme } -> String -> String
colorWith toColor options input =
    case options.colors of
        Nothing ->
            input

        Just theme ->
            let
                { foreground, background } =
                    toColor theme
            in
            case ( foreground, background ) of
                ( Just f, Just b ) ->
                    Ansi.Color.fontColor f (Ansi.Color.backgroundColor b input)

                ( Nothing, Just b ) ->
                    Ansi.Color.backgroundColor b input

                ( Just f, Nothing ) ->
                    Ansi.Color.fontColor f input

                ( Nothing, Nothing ) ->
                    input


lineChangeToString : { a | colors : Maybe Theme } -> List (Change Never Char) -> String
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
                                ( colorWith .unchangedBefore options s :: beforeAcc
                                , colorWith .unchangedAfter options s :: afterAcc
                                )

                            Added a ->
                                let
                                    s : String
                                    s =
                                        String.fromChar a
                                in
                                ( beforeAcc
                                , colorWith .changedAfter options s :: afterAcc
                                )

                            Removed r ->
                                let
                                    s : String
                                    s =
                                        String.fromChar r
                                in
                                ( colorWith .changedBefore options s :: beforeAcc
                                , afterAcc
                                )
                    )
                    ( [], [] )
    in
    colorWith .unchangedBefore options ("-" ++ String.concat befores)
        ++ "\n"
        ++ colorWith .unchangedAfter options ("+" ++ String.concat afters)


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
