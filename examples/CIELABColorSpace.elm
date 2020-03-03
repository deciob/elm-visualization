module CIELABColorSpace exposing (main)

{-| This module shows how to build some simple CIELAB colour space palettes.
-}

import Color exposing (Color, rgb255)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Interpolation exposing (Interpolator, hcl, lab)
import Scale exposing (QuantizeScale)


css : String
css =
    """
body {
    font-family: Sans-Serif;
}

body, html {
    width: 100%;
    position: relative;
}

.wrapper {
    margin: 25px;
    width: 90%;
    position: relative;
}

.palette {
    display: flex;
    width: 100%;
    margin: 20px;
}

.palette div {
    height: 80px;
    flex: 1;
}
"""


seedValues : List Float
seedValues =
    let
        count =
            20

        s =
            Scale.linear ( 0, 1 ) ( 0, toFloat count )
                |> Scale.convert
    in
    count
        |> List.range 0
        |> List.map toFloat
        |> List.map s


palette1 : Html msg
palette1 =
    div [ class "palette" ]
        (seedValues
            |> List.map (hcl (rgb255 250 250 110) (rgb255 42 72 88))
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color) ] []
                )
        )


palette2 : Html msg
palette2 =
    div [ class "palette" ]
        (seedValues
            |> List.map (hcl (rgb255 85 170 255) (rgb255 0 170 127))
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color) ] []
                )
        )


palette3 : Html msg
palette3 =
    div [ class "palette" ]
        (seedValues
            |> List.map (hcl (rgb255 255 170 0) (rgb255 170 0 0))
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color) ] []
                )
        )


palette4 : Html msg
palette4 =
    div [ class "palette" ]
        (seedValues
            |> List.map (hcl (rgb255 0 85 255) (rgb255 170 0 0))
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color) ] []
                )
        )


palette5 : Html msg
palette5 =
    div [ class "palette" ]
        (seedValues
            |> List.map (lab (rgb255 250 250 110) (rgb255 42 72 88))
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color) ] []
                )
        )


palette6 : Html msg
palette6 =
    div [ class "palette" ]
        (seedValues
            |> List.map (lab (rgb255 85 170 255) (rgb255 0 170 127))
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color) ] []
                )
        )


palette7 : Html msg
palette7 =
    div [ class "palette" ]
        (seedValues
            |> List.map (lab (rgb255 255 170 0) (rgb255 170 0 0))
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color) ] []
                )
        )


palette8 : Html msg
palette8 =
    div [ class "palette" ]
        (seedValues
            |> List.map (lab (rgb255 0 85 255) (rgb255 170 0 0))
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color) ] []
                )
        )


main : Html msg
main =
    Html.div []
        [ Html.node "style" [] [ Html.text css ]
        , Html.div
            [ class "wrapper" ]
            [ div [] [ Html.text "hcl" ]
            , palette1
            , palette2
            , palette3
            , palette4
            , div [] [ Html.text "lab" ]
            , palette5
            , palette6
            , palette7
            , palette8
            ]
        ]
