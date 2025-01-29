module Tests exposing (basic, perf)

import Diff exposing (Change(..), diff, diffLines)
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


basic : Test
basic =
    describe "Basic"
        [ test "basic 1" (\_ -> Expect.equal [] (diff [] []))
        , test "basic 2" (\_ -> Expect.equal [ Removed 1 ] (diff [ 1 ] []))
        , test "basic 3" (\_ -> Expect.equal [ Added 1 ] (diff [] [ 1 ]))
        , test "basic 4" (\_ -> Expect.equal [ NoChange 1 ] (diff [ 1 ] [ 1 ]))
        , test "basic 5" (\_ -> Expect.equal [ NoChange 1, Removed 2 ] (diff [ 1, 2 ] [ 1 ]))
        , test "basic 6" (\_ -> Expect.equal [ Removed 1, NoChange 2 ] (diff [ 1, 2 ] [ 2 ]))
        , test "basic 7" (\_ -> Expect.equal [ NoChange 1, Added 2 ] (diff [ 1 ] [ 1, 2 ]))
        , test "basic 8" (\_ -> Expect.equal [ Added 1, NoChange 2 ] (diff [ 2 ] [ 1, 2 ]))
        , test "basic 9" (\_ -> Expect.equal [ NoChange 1, NoChange 2 ] (diff [ 1, 2 ] [ 1, 2 ]))
        , test "basic 10" (\_ -> Expect.equal [ Removed 1, Removed 2 ] (diff [ 1, 2 ] []))
        , test "basic 11" (\_ -> Expect.equal [ Added 1, Added 2 ] (diff [] [ 1, 2 ]))
        , test "basic 12" (\_ -> Expect.equal [ Removed 1, Added 2 ] (diff [ 1 ] [ 2 ]))
        , test "basic 13" (\_ -> Expect.equal [ Removed 1, Added 2, NoChange 3 ] (diff [ 1, 3 ] [ 2, 3 ]))
        , test "basic 14" (\_ -> Expect.equal [ Removed 1, Removed 2, Added 3, Added 4 ] (diff [ 1, 2 ] [ 3, 4 ]))
        ]


runManyTimes : Int -> String -> String -> (() -> Expectation)
runManyTimes times a_ b_ =
    let
        total : Int
        total =
            List.foldl (\_ n -> n + List.length (diffLines a_ b_)) 0 (List.range 1 times)
    in
    \_ -> total |> Expect.greaterThan 0


perf : Test
perf =
    describe "Perf"
        [ test "exactly same" (runManyTimes 100 a a)
        , test "add line to first" (runManyTimes 100 a b)
        , test "add line to last" (runManyTimes 100 a c)
        , test "drop first line" (runManyTimes 100 a d)
        , test "remove line at middle" (runManyTimes 100 a e)
        , test "add line at middle" (runManyTimes 100 a f)

        -- O(ND): 0.63s ( O(ND) = (280*2)*(280*2) )
        -- O(NP): 0.32s ( O(NP) = (280*2)*((280*2-0)/2) )
        , test "modify all" (runManyTimes 10 a g)

        -- O(ND): 0.13s ( O(ND) = 280*280 )
        -- O(NP): 0.0s ( O(NP) = 280*((280-280)/2) )
        , test "add all" (runManyTimes 10 "" a)

        -- O(ND): 0.13s ( O(ND) = 280*280 )
        -- O(NP): 0.0s ( O(NP) = 280*((280-280)/2) )
        , test "remove all" (runManyTimes 10 a "")
        ]


b : String
b =
    "first\n" ++ a


c : String
c =
    a ++ "\nlast"


d : String
d =
    mapLines (List.drop 1) a


e : String
e =
    mapLines (List.take 100) a ++ mapLines (List.drop 101) a


f : String
f =
    mapLines (List.take 101) a ++ mapLines (List.drop 100) a


g : String
g =
    mapEachLine (\l -> "_" ++ l) a


mapLines : (List String -> List String) -> String -> String
mapLines f_ s =
    String.join "\n" (f_ (String.lines s))


mapEachLine : (String -> String) -> String -> String
mapEachLine f_ s =
    mapLines (List.map f_) s


a : String
a =
    """
{ a =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, b =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, c =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, d = 0
, e =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, f =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, g =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, h =
    { a = 1
    , b = 1
    , c = 1
    , d = 1
    , e = "1"
    , f = "1"
    , g = "1"
    , h = "1"
    , i = "1"
    , j = "1"
    , k = "1"
    }
, i = 0
, j =
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
, k =
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
, l =
    "ccccccccccccccccccccccccccccccccccc"
, o =
    "dddddddddddddddddddddddddddddddddddddddddddddddddddddd"
, p =
    Just
      ( Just
          ( Just
              ( Just
                  ( Just
                      ( Just ( Just ( Just ( Just 1 ) ) ) )
                  )
              )
          )
      )
, q =
    Just
      ( Just
          ( Just
              ( Just
                  ( Just
                      ( Just ( Just ( Just ( Just 2 ) ) ) )
                  )
              )
          )
      )
, r =
    Just
      ( Just
          ( Just
              ( Just
                  ( Just
                      ( Just ( Just ( Just ( Just 3 ) ) ) )
                  )
              )
          )
      )
, s =
    Just
      ( Just
          ( Just
              ( Just
                  ( Just
                      ( Just ( Just ( Just ( Just 4 ) ) ) )
                  )
              )
          )
      )
, t = "Ok, Google"
, u = 123456789
, v = 123.456
, w =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, x =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, y =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
, z =
    [ 0
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    , 1
    , 2
    , 3
    , 4
    , 5
    , 6
    , 7
    , 8
    , 9
    ]
}
"""
