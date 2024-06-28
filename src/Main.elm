module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, br)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Parser exposing (..)

import Debug exposing (log)

type alias Model =
  { inputString : String
  , stdIn : String
  , result : Term
  , stdOut : String
  , errMsg : String
  }

initialModel : Model
initialModel =
  { inputString = ""
  , stdIn = ""
  , result = I
  , stdOut = ""
  , errMsg = ""
  }

type Term
  = S
  | K
  | I
  | Apply Term Term

type Msg
  = Input String
  | InputStdIn String
  | Pressed

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input str ->
      { model
      | inputString = str
      , errMsg = ""
      }

    InputStdIn str ->
      { model
      | stdIn = str
      , errMsg = ""
      }

    Pressed ->
      let
        parseRes =
          model.inputString
            |> String.toLower
            |> String.words
            |> String.concat
            |> applyParser
            |> (\res ->
                case res of
                  Success hd "" -> Just hd
                  _ -> Nothing
              )

        evalRes =
          parseRes
            |> Maybe.map
                (\res -> evaluate res model.stdIn |> termToString)

        (stdOut, errMsg) =
          case evalRes of
            Just res -> (res, "")
            Nothing -> ("Standard output is empty", "Parse Error")
      in
        { model |
          result = Maybe.withDefault initialModel.result parseRes
        , stdOut = stdOut
        , errMsg = errMsg
        }

evaluate res _ =
  reduction res

reduction : Term -> Term
reduction res =
  case res of
    Apply t1 t2 ->
      case (reduction t1, reduction t2) of
        (I, x) -> x
        (Apply K x, y) -> x
        (Apply (Apply S x) y, z) -> Apply (Apply x z) (Apply y z) |> reduction
        (f, x) -> Apply f x
    _ -> res

sParser : Parser Term
sParser =
  charMatch 's'
    |> map (always <| S)

kParser : Parser Term
kParser =
  charMatch 'k'
    |> map (always <| K)

iParser : Parser Term
iParser =
  charMatch 'i'
    |> map (always <| I)

parenOpenParser : Parser ()
parenOpenParser =
  charMatch '('

parenCloseParser : Parser ()
parenCloseParser =
  charMatch ')'

termParser : Parser Term
termParser =
  choice
    [ sParser
    , kParser
    , iParser
    , lazy (\() -> termParenParser)
    ]

applyParser : Parser Term
applyParser =
  oneOrMore termParser
    |> map listToApply

termParenParser : Parser Term
termParenParser =
  concat3
    parenOpenParser applyParser parenCloseParser
    (\_ exp _ -> exp)

listToApply : List Term -> Term
listToApply list =
  case List.reverse list of
    tl::[] -> tl
    tl::hd -> Apply (listToApply <| List.reverse hd) tl
    _ -> I


view : Model -> Html Msg
view model =
  let
    lambdaStr = termToString model.result
  in
    div []
      [ input
        [ type_ "textbox"
        , onInput Input
        ][]
      , input
        [ type_ "textbox"
        , onInput InputStdIn
        ][]
      , button [ onClick Pressed ][ text "Parse it !" ]
      , br[][]
      , text <| "Reduction Result : " ++ model.stdOut

      --debug
      , br[][]
      , text <| "Parse     Result : " ++  lambdaStr
      ]


main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }

termToString : Term -> String
termToString term =
  case term of
    S -> "S"
    K -> "K"
    I -> "I"
    Apply t1 t2 ->
      let
        t1s = termToString t1
        t2s = termToString t2
        parenStr s =
          if String.length s == 1
          then s
          else
            "(" ++ s ++ ")"

      in
        "Apply " ++ parenStr t1s ++ " " ++ parenStr t2s