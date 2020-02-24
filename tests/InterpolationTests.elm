module InterpolationTests exposing (suite)

import Array
import Color exposing (Color)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, floatRange, intRange, list)
import Interpolation exposing (Interpolator)
import Test exposing (..)


unit : Fuzzer Float
unit =
    floatRange 0 1


guaranteedTolerance =
    Absolute 0.0000000001


tuple2 : Fuzzer a -> Fuzzer b -> Fuzzer ( a, b )
tuple2 a b =
    Fuzz.tuple ( a, b )


tuple3 : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer ( a, b, c )
tuple3 a b c =
    Fuzz.tuple3 ( a, b, c )


suite : Test
suite =
    describe "Interpolation"
        [ describe "float"
            [ test "interpolates between two floats" <|
                \() ->
                    Interpolation.float 10 42
                        |> Expect.all
                            ([ 10, 13.2, 16.4, 19.6, 22.8, 26, 29.2, 32.4, 35.6, 38.8, 42 ]
                                |> List.indexedMap (\i exp interp -> interp (toFloat i / 10) |> Expect.within (Absolute 1.0e-10) exp)
                            )
            , fuzz2 (floatRange 0 10) (floatRange 0 10) "gives exact ends for t=0 and t=1" <|
                \min delta ->
                    Interpolation.float min (delta + min)
                        |> Expect.all
                            [ \interp -> interp 0 |> Expect.within (Absolute 1.0e-10) min
                            , \interp -> interp 1 |> Expect.within (Absolute 1.0e-10) (delta + min)
                            ]
            ]
        , describe "int"
            [ test "interpolates between two floats" <|
                \() ->
                    Interpolation.int 10 42
                        |> Expect.all
                            ([ 10, 13, 16, 20, 23, 26, 29, 32, 36, 39, 42 ]
                                |> List.indexedMap (\i exp interp -> interp (toFloat i / 10) |> Expect.equal exp)
                            )
            , fuzz2 (intRange 0 10) (intRange 0 10) "gives exact ends for t=0 and t=1" <|
                \min delta ->
                    Interpolation.int min (delta + min)
                        |> Expect.all
                            [ \interp -> interp 0 |> Expect.equal min
                            , \interp -> interp 1 |> Expect.equal (delta + min)
                            ]
            ]
        , describe "step"
            [ test "returns the expected values" <|
                \() ->
                    Interpolation.step 'a' [ 'b', 'c', 'd', 'e' ]
                        |> Expect.all
                            [ \interp -> interp -1 |> Expect.equal 'a'
                            , \interp -> interp 0 |> Expect.equal 'a'
                            , \interp -> interp 0.19 |> Expect.equal 'a'
                            , \interp -> interp 0.21 |> Expect.equal 'b'
                            , \interp -> interp 1 |> Expect.equal 'e'
                            ]
            , fuzz (floatRange 0 1) "works like round" <|
                \t ->
                    Interpolation.step 0 [ 1 ] t |> Expect.equal (round t)
            ]
        , describe "rgb"
            [ test "interpolates in sRGB color space" <|
                \() ->
                    interpolateColorTest Interpolation.rgb
                        { red = 107, green = 104, blue = 144 }
            ]
        , describe "rgbWithGamma" <|
            [ test "returns the expected values" <|
                \() ->
                    interpolateColorTest (Interpolation.rgbWithGamma 3)
                        { red = 153, green = 121, blue = 167 }
            ]
        , describe "hsl" <|
            [ test "interpolates in hsl" <|
                \() ->
                    interpolateColorTest Interpolation.hsl
                        { red = 56, green = 61, blue = 195 }
            , test "uses the shortest path when interpolating" <|
                \() ->
                    Interpolation.hsl (Color.hsl (10 / 360) 0.5 0.5) (Color.hsl (350 / 360) 0.5 0.5)
                        |> equalsSamples equalsColor
                            [ Color.rgb255 191 85 64
                            , Color.rgb255 191 76 64
                            , Color.rgb255 191 68 64
                            , Color.rgb255 191 64 68
                            , Color.rgb255 191 64 77
                            , Color.rgb255 191 64 85
                            ]
            ]
        , describe "hslLong" <|
            [ test "interpolates in hsl" <|
                \() ->
                    interpolateColorTest Interpolation.hslLong
                        { red = 56, green = 195, blue = 162 }
            , test "does not use the shortest path when interpolating" <|
                \() ->
                    Interpolation.hslLong (Color.hsl (10 / 360) 0.5 0.5) (Color.hsl (350 / 360) 0.5 0.5)
                        |> equalsSamples equalsColor
                            [ Color.rgb255 191 85 64
                            , Color.rgb255 153 191 64
                            , Color.rgb255 64 191 119
                            , Color.rgb255 64 119 191
                            , Color.rgb255 153 64 191
                            , Color.rgb255 191 64 85
                            ]
            ]
        , describe "piecewise"
            [ fuzz3 float (list float) (floatRange 0 1) "never exceeds the range" <|
                \head tail t ->
                    Interpolation.piecewise Interpolation.float head tail t
                        |> Expect.all
                            [ Expect.atMost (List.foldl max head tail)
                            , Expect.atLeast (List.foldl min head tail)
                            ]
            ]
        , describe "inParallel"
            [ fuzz (list float) "does not change order" <|
                \inp ->
                    inp
                        |> List.map always
                        |> Interpolation.inParallel
                        |> (\i ->
                                i 0.5
                           )
                        |> Expect.equal inp
            ]
        , describe "list"
            (let
                setOpacity rec op =
                    { rec | opacity = op }

                interpolateRecords before after =
                    Interpolation.map2 (\op pos -> { after | opacity = op, position = pos })
                        (Interpolation.float before.opacity after.opacity)
                        (Interpolation.int before.position after.position)

                options =
                    { add = \rec -> Interpolation.map (setOpacity rec) (Interpolation.float 0 rec.opacity)
                    , remove = \rec -> Interpolation.map (setOpacity rec) (Interpolation.float rec.opacity 0)
                    , change = interpolateRecords
                    , id = .id
                    , combine = Interpolation.combineParallel
                    }

                mkRecs =
                    List.indexedMap (\idx id -> { id = id, opacity = 1, position = idx })
             in
             [ test "it interpolates between lists" <|
                \() ->
                    Interpolation.list options (mkRecs [ "a", "b" ]) (mkRecs [ "b", "c" ])
                        |> equalsSamples Expect.equalLists
                            [ [ { id = "a", position = 0, opacity = 1 }
                              , { id = "b", position = 1, opacity = 1 }
                              , { id = "c", position = 1, opacity = 0 }
                              ]
                            , [ { id = "a", position = 0, opacity = 0.5 }
                              , { id = "b", position = 1, opacity = 1 }
                              , { id = "c", position = 1, opacity = 0.5 }
                              ]
                            , mkRecs [ "b", "c" ]
                            ]
             , test "in interpolates additions" <|
                \() ->
                    Interpolation.list options (mkRecs [ "a" ]) (mkRecs [ "b", "c" ])
                        |> equalsSamples Expect.equalLists
                            [ [ { id = "a", position = 0, opacity = 1 }
                              , { id = "b", position = 0, opacity = 0 }
                              , { id = "c", position = 1, opacity = 0 }
                              ]
                            , [ { id = "a", position = 0, opacity = 0.5 }
                              , { id = "b", position = 0, opacity = 0.5 }
                              , { id = "c", position = 1, opacity = 0.5 }
                              ]
                            , mkRecs [ "b", "c" ]
                            ]
             ]
            )
        , describe "samples"
            [ test "returns n uniformly spaced samples" <|
                \() ->
                    Interpolation.float 0 1
                        |> Interpolation.samples 5
                        |> Expect.equalLists [ 0 / 4, 1 / 4, 2 / 4, 3 / 4, 4 / 4 ]
            ]
        , describe "Color lab"
            [ fuzz (tuple2 (tuple3 (floatRange 0 100) (floatRange -160 160) (floatRange -160 160)) unit)
                "can represent Lab colors (fromLab)"
              <|
                \( ( l, a, b ), alpha ) ->
                    Interpolation.fromLab { l = l, a = a, b = b, alpha = alpha }
                        |> Interpolation.toLab
                        |> Expect.all
                            [ .l >> Expect.within (Absolute 0.001) l
                            , .a >> Expect.within (Absolute 0.001) a
                            , .b >> Expect.within (Absolute 0.001) b
                            , .alpha >> Expect.within guaranteedTolerance alpha
                            ]
            , test "hcl exposes the hue, chroma, luminance values" <|
                \() ->
                    Color.rgb255 170 187 204
                        |> Interpolation.toHcl
                        |> expectHclEqual { hue = 252.37145234745182, chroma = 11.223567114593477, luminance = 74.96879980931759, alpha = 1 }
            , test "hcl converts to proper RGB" <|
                \() ->
                    Interpolation.fromHcl { hue = 120, chroma = 30, luminance = 50, alpha = 0.4 }
                        |> Color.toRgba
                        |> expectRgbEqual { red = 105 / 255, green = 126 / 255, blue = 73 / 255, alpha = 0.4 }
            , fuzz (tuple2 (tuple3 unit unit unit) unit)
                "can represent Hcl colors (fromHcl)"
              <|
                \( ( r, g, b ), alpha ) ->
                    Color.rgba r g b alpha
                        |> Interpolation.toHcl
                        |> Interpolation.fromHcl
                        |> Color.toRgba
                        |> expectRgbEqual { red = r, green = g, blue = b, alpha = alpha }
            ]
        ]


equalsSamples : (a -> a -> Expectation) -> List a -> Interpolator a -> Expectation
equalsSamples eq expectedSamples interp =
    Expect.all (List.map2 (\a b c -> eq a b) expectedSamples (Interpolation.samples (List.length expectedSamples) interp))
        ()


interpolateColorTest : (Color -> Color -> Interpolator Color) -> { red : Int, green : Int, blue : Int } -> Expectation
interpolateColorTest interp { red, green, blue } =
    interp (Color.rgb255 70 130 180) (Color.rgb255 255 0 0) 0.2
        |> equalsColor (Color.rgb255 red green blue)


equalsColor : Color -> Color -> Expectation
equalsColor exp actu =
    Expect.equal (normalizeInRGB exp) (normalizeInRGB actu)


normalizeInRGB : Color -> { red : Int, green : Int, blue : Int, alpha : Float }
normalizeInRGB color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    { red = to255 red, green = to255 green, blue = to255 blue, alpha = toFloat (to255 alpha) / 255 }


to255 : Float -> Int
to255 v =
    round (v * 255)


areHclCoordsEqual expected actual =
    (isNaN actual && isNaN expected) || (expected - 1.0e-6 <= actual && actual <= expected + 1.0e-6)


expectHclEqual expected actual =
    Expect.true (Debug.toString expected ++ "\n    ╷\n    │ expectHclEqual\n    ╵\n" ++ Debug.toString actual) <|
        areHclCoordsEqual actual.hue expected.hue
            && areHclCoordsEqual actual.chroma expected.chroma
            && areHclCoordsEqual actual.luminance expected.luminance
            && expected.alpha
            == actual.alpha



-- sRGB is 8-bit


areRgbCoordsEqual expected actual =
    round (expected * 255) == round (actual * 255)


expectRgbEqual expected actual =
    Expect.true (Debug.toString expected ++ "\n    ╷\n    │ expectRgbEqual\n    ╵\n" ++ Debug.toString actual) <|
        areRgbCoordsEqual actual.red expected.red
            && areRgbCoordsEqual actual.green expected.green
            && areRgbCoordsEqual actual.blue expected.blue
            && expected.alpha
            == actual.alpha
