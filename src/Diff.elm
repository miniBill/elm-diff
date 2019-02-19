module Diff exposing (Change(..), diff, diffLines)

{-| Compares two list and returns how they have changed.
Each function internally uses Wu's [O(NP) algorithm](http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf).


# Types

@docs Change


# Diffing

@docs diff, diffLines

-}

import Array exposing (Array)


{-| This describes how each line has changed and also contains its value.
-}
type Change a
    = Added a
    | Removed a
    | NoChange a


type StepResult
    = Continue (Array (List ( Int, Int )))
    | Found (List ( Int, Int ))


type BugReport
    = CannotGetA Int
    | CannotGetB Int
    | UnexpectedPath ( Int, Int ) (List ( Int, Int ))


{-| Compares two text.

Giving the following text

    a =
        """aaa
    bbb
    ddd"""

    b =
        """zzz
    aaa
    ccc
    ddd"""

results in

    [ Added "zzz"
    , NoChange "aaa"
    , Removed "bbb"
    , Added "ccc"
    , NoChange "ddd"
    ]

.

-}
diffLines : String -> String -> List (Change String)
diffLines a b =
    diff (String.lines a) (String.lines b)


{-| Compares general lists.

    diff [1, 3] [2, 3] == [Removed 1, Added 2, NoChange 3] -- True

-}
diff : List a -> List a -> List (Change a)
diff a b =
    case testDiff a b of
        Ok changes ->
            changes

        Err _ ->
            []


{-| Test the algolithm itself.
If it returns Err, it should be a bug.
-}
testDiff : List a -> List a -> Result BugReport (List (Change a))
testDiff a b =
    let
        arrA =
            Array.fromList a

        arrB =
            Array.fromList b

        m =
            Array.length arrA

        n =
            Array.length arrB

        -- Elm's Array doesn't allow null element,
        -- so we'll use shifted index to access source.
        getA =
            \x -> Array.get (x - 1) arrA

        getB =
            \y -> Array.get (y - 1) arrB

        path =
            -- Is there any case ond is needed?
            -- ond getA getB m n
            onp getA getB m n
    in
    makeChanges getA getB path


makeChanges :
    (Int -> Maybe a)
    -> (Int -> Maybe a)
    -> List ( Int, Int )
    -> Result BugReport (List (Change a))
makeChanges getA getB path =
    case path of
        [] ->
            Ok []

        latest :: tail ->
            makeChangesHelp [] getA getB latest tail


makeChangesHelp :
    List (Change a)
    -> (Int -> Maybe a)
    -> (Int -> Maybe a)
    -> ( Int, Int )
    -> List ( Int, Int )
    -> Result BugReport (List (Change a))
makeChangesHelp changes getA getB ( x, y ) path =
    case path of
        [] ->
            Ok changes

        ( prevX, prevY ) :: tail ->
            let
                change =
                    if x - 1 == prevX && y - 1 == prevY then
                        case getA x of
                            Just a ->
                                Ok (NoChange a)

                            Nothing ->
                                Err (CannotGetA x)

                    else if x == prevX then
                        case getB y of
                            Just b ->
                                Ok (Added b)

                            Nothing ->
                                Err (CannotGetB y)

                    else if y == prevY then
                        case getA x of
                            Just a ->
                                Ok (Removed a)

                            Nothing ->
                                Err (CannotGetA x)

                    else
                        Err (UnexpectedPath ( x, y ) path)
            in
            case change of
                Ok c ->
                    makeChangesHelp (c :: changes) getA getB ( prevX, prevY ) tail

                Err e ->
                    Err e


-- Myers's O(ND) algorithm (http://www.xmailserver.org/diff2.pdf)


ond : (Int -> Maybe a) -> (Int -> Maybe a) -> Int -> Int -> List ( Int, Int )
ond getA getB m n =
    let
        v =
            Array.initialize (m + n + 1) (always [])
    in
    ondLoopDK (snake getA getB) m 0 0 v


ondLoopDK :
    (Int -> Int -> List ( Int, Int ) -> ( List ( Int, Int ), Bool ))
    -> Int
    -> Int
    -> Int
    -> Array (List ( Int, Int ))
    -> List ( Int, Int )
ondLoopDK snake_ offset d k v =
    if k > d then
        ondLoopDK snake_ offset (d + 1) (-d - 1) v

    else
        case step snake_ offset k v of
            Found path ->
                path

            Continue v_ ->
                ondLoopDK snake_ offset d (k + 2) v_



-- Wu's O(NP) algorithm (http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf)


onp : (Int -> Maybe a) -> (Int -> Maybe a) -> Int -> Int -> List ( Int, Int )
onp getA getB m n =
    let
        v =
            Array.initialize (m + n + 1) (always [])

        delta =
            n - m
    in
    onpLoopP (snake getA getB) delta m 0 v


onpLoopP :
    (Int -> Int -> List ( Int, Int ) -> ( List ( Int, Int ), Bool ))
    -> Int
    -> Int
    -> Int
    -> Array (List ( Int, Int ))
    -> List ( Int, Int )
onpLoopP snake_ delta offset p v =
    let
        ks =
            if delta > 0 then
                List.reverse (List.range (delta + 1) (delta + p))
                    ++ List.range -p delta

            else
                List.reverse (List.range (delta + 1) p)
                    ++ List.range (-p + delta) delta
    in
    case onpLoopK snake_ offset ks v of
        Found path ->
            path

        Continue v_ ->
            onpLoopP snake_ delta offset (p + 1) v_


onpLoopK :
    (Int -> Int -> List ( Int, Int ) -> ( List ( Int, Int ), Bool ))
    -> Int
    -> List Int
    -> Array (List ( Int, Int ))
    -> StepResult
onpLoopK snake_ offset ks v =
    case ks of
        [] ->
            Continue v

        k :: ks_ ->
            case step snake_ offset k v of
                Found path ->
                    Found path

                Continue v_ ->
                    onpLoopK snake_ offset ks_ v_


step :
    (Int -> Int -> List ( Int, Int ) -> ( List ( Int, Int ), Bool ))
    -> Int
    -> Int
    -> Array (List ( Int, Int ))
    -> StepResult
step snake_ offset k v =
    let
        fromLeft =
            Maybe.withDefault [] (Array.get (k - 1 + offset) v)

        fromTop =
            Maybe.withDefault [] (Array.get (k + 1 + offset) v)

        ( path, ( x, y ) ) =
            case ( fromLeft, fromTop ) of
                ( [], [] ) ->
                    ( [], ( 0, 0 ) )

                ( [], ( topX, topY ) :: _ ) ->
                    ( fromTop, ( topX + 1, topY ) )

                ( ( leftX, leftY ) :: _, [] ) ->
                    ( fromLeft, ( leftX, leftY + 1 ) )

                ( ( leftX, leftY ) :: _, ( topX, topY ) :: _ ) ->
                    -- this implies "remove" comes always earlier than "add"
                    if leftY + 1 >= topY then
                        ( fromLeft, ( leftX, leftY + 1 ) )

                    else
                        ( fromTop, ( topX + 1, topY ) )

        ( newPath, goal ) =
            snake_ (x + 1) (y + 1) (( x, y ) :: path)
    in
    if goal then
        Found newPath

    else
        Continue (Array.set (k + offset) newPath v)


snake :
    (Int -> Maybe a)
    -> (Int -> Maybe a)
    -> Int
    -> Int
    -> List ( Int, Int )
    -> ( List ( Int, Int ), Bool )
snake getA getB nextX nextY path =
    case ( getA nextX, getB nextY ) of
        ( Just a, Just b ) ->
            if a == b then
                snake
                    getA
                    getB
                    (nextX + 1)
                    (nextY + 1)
                    (( nextX, nextY ) :: path)

            else
                ( path, False )

        -- reached bottom-right corner
        ( Nothing, Nothing ) ->
            ( path, True )

        _ ->
            ( path, False )
