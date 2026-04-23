module Diff.Histogram exposing (Change(..), diff)

import FastDict as Dict exposing (Dict)
import FastSet as Set



-- This file is an Elm translation of https://www.raygard.net/2025/01/28/how-histogram-diff-works/


type Change comparable
    = Added (List comparable)
    | Removed (List comparable)
    | Changed (List comparable) (List comparable)


diff : List comparable -> List comparable -> List (Change comparable)
diff a b =
    let
        aDict : Dict Int comparable
        aDict =
            a
                |> List.indexedMap Tuple.pair
                |> Dict.fromList

        bDict : Dict Int comparable
        bDict =
            b
                |> List.indexedMap Tuple.pair
                |> Dict.fromList

        regions : List Region
        regions =
            diffHelp
                [ { a =
                        { from = 0
                        , to = Dict.size aDict - 1
                        }
                  , b =
                        { from = 0
                        , to = Dict.size bDict - 1
                        }
                  }
                ]
                []
                aDict
                bDict

        renderRegion : Region -> Maybe (Change comparable)
        renderRegion region =
            let
                extractRange : Range -> List comparable -> List comparable
                extractRange range list =
                    list
                        |> List.drop range.from
                        |> List.take (range.to - range.from + 1)

                aList : List comparable
                aList =
                    extractRange region.a a

                bList : List comparable
                bList =
                    extractRange region.b b
            in
            if List.isEmpty aList then
                if List.isEmpty bList then
                    Nothing

                else
                    Just (Added bList)

            else if List.isEmpty bList then
                Just (Removed aList)

            else
                Just (Changed aList bList)
    in
    List.filterMap renderRegion regions


type alias Range =
    { from : Int
    , to : Int
    }


type alias Region =
    { a : Range
    , b : Range
    }


diffHelp :
    List Region
    -> List Region
    -> Dict Int comparable
    -> Dict Int comparable
    -> List Region
diffHelp regionStack diffList a b =
    case regionStack of
        [] ->
            diffList

        currentRegion :: regionStackTail ->
            let
                bestMatch : Region
                bestMatch =
                    findBestMatchingRegionIn a b currentRegion
            in
            if bestMatch.a.to - bestMatch.a.from < 0 then
                diffHelp regionStackTail (currentRegion :: diffList) a b

            else
                let
                    afterMatch : Region
                    afterMatch =
                        { a =
                            { from = bestMatch.a.to + 1
                            , to = currentRegion.a.to
                            }
                        , b =
                            { from = bestMatch.b.to + 1
                            , to = currentRegion.b.to
                            }
                        }

                    withAfter : List Region
                    withAfter =
                        if isRegionEmpty afterMatch then
                            regionStackTail

                        else
                            afterMatch :: regionStackTail

                    beforeMatch : Region
                    beforeMatch =
                        { a =
                            { from = currentRegion.a.from
                            , to = bestMatch.a.from - 1
                            }
                        , b =
                            { from = currentRegion.b.from
                            , to = bestMatch.b.from - 1
                            }
                        }

                    withBefore : List Region
                    withBefore =
                        if isRegionEmpty beforeMatch then
                            withAfter

                        else
                            beforeMatch :: withAfter
                in
                diffHelp withBefore diffList a b


isRegionEmpty : Region -> Bool
isRegionEmpty { a, b } =
    isRangeEmpty a && isRangeEmpty b


isRangeEmpty : Range -> Bool
isRangeEmpty { from, to } =
    to < from


toHistogram : Range -> Dict Int comparable -> Dict comparable ( Int, List Int )
toHistogram range dict =
    Dict.restructure Dict.empty
        (\{ key, value, left, right } ->
            if key < range.from then
                right ()

            else if key > range.to then
                left ()

            else
                unionWith Set.union (left ()) (right ())
                    |> Dict.update value
                        (\v ->
                            v
                                |> Maybe.withDefault Set.empty
                                |> Set.insert key
                                |> Just
                        )
        )
        dict
        |> Dict.map (\_ v -> ( Set.size v, Set.toList v ))


findBestMatchingRegionIn :
    Dict Int comparable
    -> Dict Int comparable
    -> Region
    -> Region
findBestMatchingRegionIn a b currentRegion =
    let
        aHistogram : Dict comparable ( Int, List Int )
        aHistogram =
            toHistogram currentRegion.a a

        loop : Region -> Int -> Int -> Region
        loop bestMatch lowCount i =
            if i > currentRegion.b.to then
                bestMatch

            else
                case Dict.get i b of
                    Nothing ->
                        -- Reached the end of b. Should not happen with the check above
                        bestMatch

                    Just bLine ->
                        case Dict.get bLine aHistogram of
                            Nothing ->
                                loop bestMatch lowCount (i + 1)

                            Just ( matchingCount, matching ) ->
                                if matchingCount > lowCount then
                                    loop bestMatch lowCount (i + 1)

                                else
                                    let
                                        inner : List Int -> Region -> Int -> Int -> ( Region, Int, Int )
                                        inner queue innerBestMatch innerLowCount innerNextI =
                                            case queue of
                                                [] ->
                                                    ( innerBestMatch, innerLowCount, innerNextI )

                                                j :: tail ->
                                                    let
                                                        currentMatch : Region
                                                        currentMatch =
                                                            { a = { from = j, to = j }
                                                            , b = { from = i, to = i }
                                                            }
                                                                |> widenMatch a b

                                                        regionLowCount : Int
                                                        regionLowCount =
                                                            toHistogram currentMatch.a a
                                                                |> calculateRegionLowCount innerLowCount

                                                        width : Region -> Int
                                                        width r =
                                                            r.a.to - r.a.from

                                                        ( nextInnerBestMatch, nextInnerLowCount ) =
                                                            if
                                                                isRegionEmpty innerBestMatch
                                                                    || (width currentMatch > width innerBestMatch)
                                                                    || (regionLowCount < innerLowCount)
                                                            then
                                                                ( currentMatch, regionLowCount )

                                                            else
                                                                ( innerBestMatch, innerLowCount )

                                                        nextInnerNextI : Int
                                                        nextInnerNextI =
                                                            currentMatch.b.to + 1
                                                    in
                                                    inner tail nextInnerBestMatch nextInnerLowCount nextInnerNextI

                                        ( nextBestMatch, nextLowCount, nextI ) =
                                            inner matching bestMatch lowCount (i + 1)
                                    in
                                    loop nextBestMatch nextLowCount nextI
    in
    loop
        { a = emptyRange, b = emptyRange }
        -- Arbitrary limit, copied from the source
        65
        currentRegion.b.from


emptyRange : Range
emptyRange =
    { from = 0
    , to = -1
    }


calculateRegionLowCount : Int -> Dict comparable ( Int, List Int ) -> Int
calculateRegionLowCount seed d =
    Dict.foldl (\_ ( s, _ ) acc -> min acc s) seed d


widenMatch :
    Dict Int comparable
    -> Dict Int comparable
    -> Region
    -> Region
widenMatch a b region =
    region |> widenUp a b |> widenDown a b


widenUp : Dict Int v -> Dict Int v -> Region -> Region
widenUp a b region =
    case Dict.get (region.a.from - 1) a of
        Nothing ->
            region

        Just aLine ->
            case Dict.get (region.b.from - 1) b of
                Nothing ->
                    region

                Just bLine ->
                    if aLine == bLine then
                        widenUp
                            a
                            b
                            { a = { from = region.a.from - 1, to = region.a.to }
                            , b = { from = region.b.from - 1, to = region.b.to }
                            }

                    else
                        region


widenDown : Dict Int v -> Dict Int v -> Region -> Region
widenDown a b region =
    case Dict.get (region.a.to + 1) a of
        Nothing ->
            region

        Just aLine ->
            case Dict.get (region.b.to + 1) b of
                Nothing ->
                    region

                Just bLine ->
                    if aLine == bLine then
                        widenDown
                            a
                            b
                            { a = { from = region.a.from, to = region.a.to + 1 }
                            , b = { from = region.b.from, to = region.b.to + 1 }
                            }

                    else
                        region


unionWith :
    (v -> v -> v)
    -> Dict comparable v
    -> Dict comparable v
    -> Dict comparable v
unionWith valueABMerge aDict bDict =
    if (aDict |> Dict.size) > (bDict |> Dict.size) then
        Dict.foldl
            (\key b soFar ->
                case Dict.get key soFar of
                    Nothing ->
                        Dict.insert key b soFar

                    Just a ->
                        Dict.insert key (valueABMerge a b) soFar
            )
            aDict
            bDict

    else
        Dict.foldl
            (\key a soFar ->
                case Dict.get key soFar of
                    Nothing ->
                        Dict.insert key a soFar

                    Just b ->
                        Dict.insert key (valueABMerge a b) soFar
            )
            bDict
            aDict
