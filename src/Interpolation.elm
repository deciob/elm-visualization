module Interpolation exposing
    ( Interpolator
    , float, int, step, rgb, rgbWithGamma, hsl, hslLong, lab, hcl
    , map, map2, map3, map4, map5, piecewise, tuple
    , inParallel, list, ListCombiner(..), combineParallel
    , fromLab, toLab, fromHcl, toHcl
    , samples
    )

{-| This module provides a variety of interpolation methods for blending between two values.
While primitives for numbers, colors and lists are provided, the library focuses on composition
so that you can build interpolators for your own custom datatypes.

@docs Interpolator


### Primitive interpolators

@docs float, int, step, rgb, rgbWithGamma, hsl, hslLong, lab, hcl


### Composition

@docs map, map2, map3, map4, map5, piecewise, tuple


### Lists

@docs inParallel, list, ListCombiner, combineParallel


### Color Spaces

@docs fromLab, toLab, fromHcl, toHcl


## Helpers

@docs samples

-}

import Array
import Color exposing (Color, toRgba)
import Color.Lab as Lab
import Dict exposing (Dict)


{-| An interpolator is merely a function that takes a float parameter `t` roughly in the range [0..1].
0 would represent the "before" value, 1 the after value and values in between are the values in between.

Note: Sometimes the range of the interpolator can go slightly above or below zero - this is useful for some
animation techniques. If this is not suitable for your data type, remember to clamp the values as necessary.

-}
type alias Interpolator a =
    Float -> a


{-| Transform values from another interpolator.

Note: This function is provided as a convenience, since thinking in `mapN` is pretty natural for Elm developers (and
works well in pipelines). However, keep in mind that this function is literally an alias for `<<`.

-}
map : (a -> b) -> Interpolator a -> Interpolator b
map =
    (<<)


{-| Combine two interpolators, combining them with the given function.

    type alias Coords =
        ( Float, Float )

    interpolateCoords : Coords -> Coords -> Interpolator Coords
    interpolateCoords ( x1, y1 ) ( x2, y2 ) =
        Interpolation.map2
            Tuple.pair
            (Interpolation.float x1 x2)
            (Interpolation.float y1 y2)

-}
map2 : (a -> b -> c) -> Interpolator a -> Interpolator b -> Interpolator c
map2 fn a b =
    \t -> fn (a t) (b t)


{-| -}
map3 : (a -> b -> c -> d) -> Interpolator a -> Interpolator b -> Interpolator c -> Interpolator d
map3 fn a b c =
    \t -> fn (a t) (b t) (c t)


{-| -}
map4 : (a -> b -> c -> d -> e) -> Interpolator a -> Interpolator b -> Interpolator c -> Interpolator d -> Interpolator e
map4 fn a b c d =
    \t -> fn (a t) (b t) (c t) (d t)


{-| -}
map5 : (a -> b -> c -> d -> e -> f) -> Interpolator a -> Interpolator b -> Interpolator c -> Interpolator d -> Interpolator e -> Interpolator f
map5 fn a b c d e =
    \t -> fn (a t) (b t) (c t) (d t) (e t)


{-| Returns a piecewise interpolator, composing interpolators for each adjacent pair of values.

For example:

    myInterpolator : Interpolator Int
    myInterpolator =
         Interpolation.piecewise Interpolation.int 6 [ 10, -2 ]

    myInterpolator 0 --> 6
    myInterpolator 0.25 --> 8
    myInterpolator 0.5 --> 10
    myInterpolator 0.75 --> 4
    myInterpolator 1 --> -2

-}
piecewise : (a -> a -> Interpolator a) -> a -> List a -> Interpolator a
piecewise makeInterpolator head tail =
    let
        n =
            List.length tail

        interpolators =
            List.foldl (\item ( prev, accu ) -> ( item, makeInterpolator prev item :: accu )) ( head, [] ) tail
                |> Tuple.second
                |> List.reverse
                |> Array.fromList
    in
    \t ->
        let
            tn =
                t * toFloat n

            i =
                clamp 0 (n - 1) (floor tn)
        in
        Array.get i interpolators
            |> Maybe.map (\fn -> fn (tn - toFloat i))
            |> Maybe.withDefault head


{-| Composes interpolators around a tuple. This is a convenience function for the common case of 2 element tuples.

You can for example define an interpolator for a position:

    interpolatePosition : ( Float, Float ) -> ( Float, Float ) -> Interpolator ( Float, Float )
    interpolatePosition =
        Interpolation.tuple Interpolation.float Interpolation.float

-}
tuple : (a -> a -> Interpolator a) -> (b -> b -> Interpolator b) -> ( a, b ) -> ( a, b ) -> Interpolator ( a, b )
tuple ia ib ( fromA, fromB ) ( toA, toB ) =
    map2 Tuple.pair (ia fromA toA) (ib fromB toB)



-- Does this even make any sense?


andThen : (a -> Interpolator b) -> Interpolator a -> Interpolator b
andThen fn interpolator =
    \param ->
        fn (interpolator param) param



-- Basic interpolators


{-| Interpolates between the two provided float values.

    myInterpolator : Interpolator Float
    myInterpolator = Interpolation.float 5 17

    myInterpolator 0.2 -- 7.4
    myInterpolator 0.5 -- 11

-}
float : Float -> Float -> Interpolator Float
float a to =
    let
        b =
            to - a
    in
    \t -> a + b * t


{-| Interpolates between ints.
-}
int : Int -> Int -> Interpolator Int
int from to =
    round << float (toFloat from) (toFloat to)


{-| Interpolate between arbitrary values by just showing them in sequence.

The list is provided is passed as head and tail seperately, to avoid needing to handle the empty list case.

     type StageOfGrief
         = Denial
         | Anger
         | Bargaining
         | Depression
         | Acceptance

     griefInterpolator : Interpolator StageOfGrief
     griefInterpolator =
         Interpolation.step Denial
             [ Anger
             , Bargaining
             , Depression
             , Acceptance
             ]

     griefInterpolator 0 --> Denial
     griefInterpolator 0.5 --> Bargaining
     griefInterpolator 1.1 --> Acceptance

-}
step : a -> List a -> Interpolator a
step head tail =
    let
        n =
            List.length tail

        items =
            Array.fromList (head :: tail)

        nt =
            toFloat n + 1
    in
    \t ->
        Array.get (clamp 0 n (floor (t * nt))) items
            |> Maybe.withDefault head



-- Color


{-| Interpolates between two Color values using the sRGB color space.
-}
rgb : Color -> Color -> Interpolator Color
rgb =
    rgbWithGamma 1.0


{-| Interpolates between two Color values using the sRGB color space using [gamma correction](https://web.archive.org/web/20160112115812/http://www.4p8.com/eric.brasseur/gamma.html).
-}
rgbWithGamma : Float -> Color -> Color -> Interpolator Color
rgbWithGamma gamma from to =
    let
        start =
            Color.toRgba from

        end =
            Color.toRgba to

        c =
            gammaCorrected gamma
    in
    map4 Color.rgba (c start.red end.red) (c start.green end.green) (c start.blue end.blue) (float start.alpha end.alpha)


{-| Interpolates between two Color values using the HSL color space. It will always take the shortest path between the target hues.
-}
hsl : Color -> Color -> Interpolator Color
hsl =
    hslImpl hue


{-| Like `Interpolation.hsl`, but does not use the shortest path between hues.
-}
hslLong : Color -> Color -> Interpolator Color
hslLong =
    hslImpl float


hslImpl : (Float -> Float -> Interpolator Float) -> Color -> Color -> Interpolator Color
hslImpl hueInt from to =
    let
        start =
            Color.toHsla from

        end =
            Color.toHsla to
    in
    map4 Color.hsla (hueInt start.hue end.hue) (float start.saturation end.saturation) (float start.lightness end.lightness) (float start.alpha end.alpha)


{-| Interpolates between two Color values using the [CIELAB color space](https://en.wikipedia.org/wiki/CIELAB_color_space) color space.
-}
lab : Color -> Color -> Interpolator Color
lab from to =
    let
        start =
            toLab from

        end =
            toLab to
    in
    map4 fromLabPositional (float start.l end.l) (float start.a end.a) (float start.b end.b) (float start.alpha end.alpha)


{-| Interpolates between two Color values using the [CIE Lch(ab)](https://en.wikipedia.org/wiki/HCL_color_space) color space.
-}
hcl : Color -> Color -> Interpolator Color
hcl from to =
    let
        start =
            toHcl from
                |> Debug.log "start -- this seems OK"

        end =
            toHcl to
                |> Debug.log "end -- this seems OK"
    in
    map4 fromHclPositional
        (hue (start.hue / 360) (end.hue / 360))
        (float start.chroma end.chroma)
        (float start.luminance end.luminance)
        (float start.alpha end.alpha)


hue : Float -> Float -> Interpolator Float
hue from to =
    let
        d =
            to
                - from
                |> Debug.log "d"
    in
    float from
        (if d > 0.5 || d < -0.5 then
            from + (d - 1 * toFloat (round d)) |> Debug.log "to-1"

         else
            to |> Debug.log "to"
        )


gammaCorrected : Float -> Float -> Float -> Interpolator Float
gammaCorrected gamma from to =
    if gamma == 1 then
        float from to

    else
        let
            a =
                from ^ gamma

            b =
                to ^ gamma - a

            y =
                1 / gamma
        in
        \t -> (a + t * b) ^ y



-- Sampling


{-| Returns a list of uniformly spaced samples from the specified interpolator. The first sample is always at t = 0, and the last sample is always at t = 1. This can be useful in generating a fixed number of samples from a given interpolator.

Can be quite handy when debugging interpolators or as a way to create a quantize scale.

-}
samples : Int -> Interpolator a -> List a
samples n interpolator =
    List.map (\i -> interpolator (toFloat i / (toFloat n - 1))) (List.range 0 (n - 1))



-- modifications


delay : Float -> Interpolator a -> Interpolator a
delay time inter =
    let
        t =
            clamp 0 0.99999 time
    in
    \p ->
        if p <= t then
            inter 0

        else
            inter ((p - t) / (1 - t))



-- OK, this gets complex


{-| This will run all of the interpolators provided in parallel.

Can be handy for constructing complex interpolations in conjuction with `List.map2`:

    before : List Float
    before =
         [ 3, 4, 7, 8 ]

    after : List Float
    after =
         [ 6, 4, 1, 9 ]

    myInterpolator0 : Interpolator (List Float)
    myInterpolator0 =
         List.map2 Interpolation.float before after
             |> Interpolation.inParallel

    myInterpolator0 0 --> [ 3, 4, 7, 8 ]
    myInterpolator0 0.5 --> [ 4.5, 4, 4, 8.5]
    myInterpolator0 1 --> [ 6, 4, 1, 9 ]

-}
inParallel : List (Interpolator a) -> Interpolator (List a)
inParallel =
    List.foldr (map2 (::)) (always [])


{-| This is an interpolator for lists. It is quite complex and should be used if these conditions hold:

1.  You need to interpolate additions, removals and changes.
2.  Each item in the list has some notion of identity, for example an `.id` member, which is `comparable`.
3.  You have a way to deal with positions in the list being somewhat muddy during the transition (e.g. if an item is being created at the same position a different item is being removed, while adjacent items are switching position, then the exact order of items will be arbitrary during the interpolation).

The first argument is a configuration record. It has the following keys:

  - `id : a -> comparable` is a function that retrieves some sort of identifier for each item in the list. It is used to figure out if an item is added, removed, or modified.
  - `add : a -> Interpolator a` will be invoked for each item being added to the list.
  - `remove : a -> Interpolator a` will be invoked for each item disappearing from the list. Note that the item won't actually be removed from the list until `t = 1`, so you will most likely want to make the item disappear visually.
  - `change : a -> a -> Interpolator a` is called for an item where the `id` matches for both lists, but which are not equal.
  - `combine : ListCombiner` configures a strategy that orchestrates all the interpolations created. At the moment only 'combineParallel' is supported, but staggered transitions will be supported in the future.

-}
list :
    { add : a -> Interpolator a
    , remove : a -> Interpolator a
    , change : a -> a -> Interpolator a
    , id : a -> comparable
    , combine : ListCombiner
    }
    -> List a
    -> List a
    -> Interpolator (List a)
list config from to =
    let
        fromIds =
            Dict.fromList (List.indexedMap (\idx a -> ( config.id a, ( idx, a ) )) from)

        toIds =
            Dict.fromList (List.indexedMap (\idx a -> ( config.id a, ( idx, a ) )) to)

        removals : Dict comparable ( Int, a )
        removals =
            Dict.diff fromIds toIds

        additions =
            Dict.diff toIds fromIds
                |> Dict.values
                |> Dict.fromList

        fromMaxIndex =
            List.length from - 1

        onTop =
            Dict.toList additions
                |> List.filter (\( idx, _ ) -> idx > fromMaxIndex)
                |> List.map (\( idx, a ) -> config.add a)

        folder : comparable -> ( Int, a ) -> List (Interpolator a) -> List (Interpolator a)
        folder id ( idx, a ) result =
            let
                add =
                    Dict.get idx additions
                        |> Maybe.map (\x -> config.add x)
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            result
                ++ [ if Dict.member id removals then
                        config.remove a

                     else
                        case Dict.get id toIds of
                            Just ( idxB, b ) ->
                                if b == a then
                                    always b

                                else
                                    config.change a b

                            Nothing ->
                                cantHappen ()
                   ]
                ++ add

        cantHappen a =
            cantHappen a

        resultingInterpolator =
            Dict.foldl folder [] fromIds
                ++ onTop
                |> inParallel
    in
    \t ->
        let
            result =
                resultingInterpolator t
        in
        if t >= 1 then
            List.filter (\item -> not (Dict.member (config.id item) removals)) result

        else
            result


{-| -}
type ListCombiner
    = CombineParallel


{-| Runs all the list interpolations in parallel.
-}
combineParallel : ListCombiner
combineParallel =
    CombineParallel



-- COLOR EXTRAS


{-| Extract the L\*a\*b\* and alpha components in the [CIELAB color space](https://en.wikipedia.org/wiki/CIELAB_color_space).

TODO: remove when done: d3.lab(l, a, b[, opacity])

-}
toLab : Color -> { l : Float, a : Float, b : Float, alpha : Float }
toLab color =
    let
        { red, green, blue, alpha } =
            toRgba color

        result =
            Lab.to red green blue
    in
    { l = result.l, a = result.a, b = result.b, alpha = alpha }


{-| Specify a color using the [CIELAB color space](https://en.wikipedia.org/wiki/CIELAB_color_space).
The value of l is typically in the range [0, 100], while a and b are typically in [-160, +160].
-}
fromLab : { l : Float, a : Float, b : Float, alpha : Float } -> Color
fromLab { l, a, b, alpha } =
    let
        result =
            Lab.from l a b
    in
    Color.rgba result.red result.green result.blue alpha


fromLabPositional : Float -> Float -> Float -> Float -> Color
fromLabPositional l a b alpha =
    fromLab { l = l, a = a, b = b, alpha = alpha }


{-| Extract the hue, chroma, luminance and alpha components in the [CIE Lch(ab)](https://en.wikipedia.org/wiki/HCL_color_space) color space.

TODO: remove when done: d3.hcl(h, c, l[, opacity])

-}
toHcl : Color -> { hue : Float, chroma : Float, luminance : Float, alpha : Float }
toHcl color =
    let
        { l, a, b, alpha } =
            toLab color

        nan =
            0 / 0
    in
    if a == 0 && b == 0 then
        { hue = nan
        , chroma =
            if 0 < l && l < 100 then
                nan

            else
                0
        , luminance = l
        , alpha = alpha
        }

    else
        let
            h =
                atan2 b a * 180 / pi
        in
        { hue =
            if h < 0 then
                h + 360

            else
                h
        , chroma = sqrt (a ^ 2 + b ^ 2)
        , luminance = l
        , alpha = alpha
        }


{-| Constructs a color in the [CIE Lch(ab)](https://en.wikipedia.org/wiki/HCL_color_space) color space.
This is especially useful for generating and manipulating colors, as this is a perceptually uniform color space.
The value of l is typically in the range [0, 100], c is typically in [0, 230], and h is typically in [0, 360).
-}
fromHcl : { hue : Float, chroma : Float, luminance : Float, alpha : Float } -> Color
fromHcl p =
    if isNaN p.hue then
        fromLab { l = p.luminance, a = 0, b = 0, alpha = p.alpha }

    else
        let
            h =
                p.hue
                    * pi
                    / 180
                    |> Debug.log "h"

            _ =
                Debug.log "p" p

            _ =
                Debug.log "ssssssssss" { l = p.luminance, a = cos h * p.chroma, b = sin h * p.chroma, alpha = p.alpha }

            _ =
                Debug.log "fromLab ssssssss" (fromLab { l = p.luminance, a = cos h * p.chroma, b = sin h * p.chroma, alpha = p.alpha })
        in
        fromLab { l = p.luminance, a = cos h * p.chroma, b = sin h * p.chroma, alpha = p.alpha }


fromHclPositional : Float -> Float -> Float -> Float -> Color
fromHclPositional h c l alpha =
    fromHcl { hue = h, chroma = c, luminance = l, alpha = alpha }
