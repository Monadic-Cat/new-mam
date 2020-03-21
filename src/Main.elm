port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, datalist, div, input, option, table, text, textarea, tr, td, span, br)
import Html.Attributes exposing (class, id, list, name, value)
import Html.Events exposing (onClick, onInput)
import OrderedDict exposing (OrderedDict)
import Json.Encode as JsonEncode
import Json.Decode as JsonDecode
import Json.Decode.Pipeline as Pipe

port cache : JsonEncode.Value -> Cmd msg

main =
    Browser.element { init = \x -> ( init x, Cmd.none )
                    , subscriptions = \c -> Sub.none
                    , update = withCache update
                    , view = view masksPlaybooks
                    }


type alias CharacterState =
    { player : String
    , name : String
    , heroName : String
    , playbook : String
    , powers : String
    , labels : OrderedDict String Int
    , conditions : OrderedDict String Bool
    , potential : Int
    , markedPotential : Int
    }

encodeOrderedDict : (k -> String) -> (v -> JsonEncode.Value) -> OrderedDict k v -> JsonEncode.Value
encodeOrderedDict keyFunc valFunc dict =
    JsonEncode.object
        [ ("order", JsonEncode.list (\k -> JsonEncode.string <| keyFunc k) dict.order)
        , ("dict", JsonEncode.dict keyFunc valFunc dict.dict)
        ]

-- Note that JSON cannot have non-String keys.
orderedDictDecoder : (JsonDecode.Decoder v) -> JsonDecode.Decoder (OrderedDict String v)
orderedDictDecoder valDecoder =
    JsonDecode.succeed OrderedDict
        |> Pipe.required "order" (JsonDecode.list JsonDecode.string)
        |> Pipe.required "dict" (JsonDecode.dict valDecoder)

encodeCharacterState : CharacterState -> JsonEncode.Value
encodeCharacterState state =
    JsonEncode.object
        [ ("player",          JsonEncode.string state.player)
        , ("name",            JsonEncode.string state.name)
        , ("heroName",        JsonEncode.string state.heroName)
        , ("playbook",        JsonEncode.string state.playbook)
        , ("powers",          JsonEncode.string state.powers)
        , ("labels",          encodeOrderedDict identity JsonEncode.int state.labels)
        , ("conditions",      encodeOrderedDict identity JsonEncode.bool state.conditions)
        , ("potential",       JsonEncode.int state.potential)
        , ("markedPotential", JsonEncode.int state.markedPotential)
        ]

characterStateDecoder : JsonDecode.Decoder CharacterState
characterStateDecoder =
    JsonDecode.succeed CharacterState
        |> Pipe.optional "player"          JsonDecode.string ""
        |> Pipe.optional "name"            JsonDecode.string ""
        |> Pipe.optional "heroName"        JsonDecode.string ""
        |> Pipe.optional "playbook"        JsonDecode.string ""
        |> Pipe.optional "powers"          JsonDecode.string ""
        |> Pipe.optional "labels"          (orderedDictDecoder JsonDecode.int) defaultLabelState
        |> Pipe.optional "conditions"      (orderedDictDecoder JsonDecode.bool) defaultConditionsState
        |> Pipe.optional "potential"       JsonDecode.int 0
        |> Pipe.optional "markedPotential" JsonDecode.int 0

init : JsonDecode.Value -> CharacterState
init state =
    case JsonDecode.decodeValue characterStateDecoder state of
        Ok char -> char
        Err err ->
            emptyCharacterState
    -- { emptyCharacterState | potential = x }

defaultLabelState : OrderedDict String Int
defaultLabelState =
    (OrderedDict.empty
    |> OrderedDict.insert "Danger" 0
    |> OrderedDict.insert "Freak" 0
    |> OrderedDict.insert "Savior" 0
    |> OrderedDict.insert "Superior" 0
    |> OrderedDict.insert "Mundane" 0
    )
defaultConditionsState : OrderedDict String Bool
defaultConditionsState =
    (OrderedDict.empty
    |> OrderedDict.insert "Afraid" False
    |> OrderedDict.insert "Angry" False
    |> OrderedDict.insert "Guilty" False
    |> OrderedDict.insert "Hopeless" False
    |> OrderedDict.insert "Insecure" False
    )
    
emptyCharacterState : CharacterState
emptyCharacterState =
    CharacterState ""
        ""
        ""
        ""
        ""
        defaultLabelState
        defaultConditionsState
        0
        0


type Msg
    = ChangePlayer String
    | ChangeName String
    | ChangeHeroName String
    | ChangePlaybook String
    | ChangePowers String
    | SetLabel String Int
    | ToggleCondition String
    | IncrementPotential
    | DecrementPotential
    | TogglePotentialMark Int


withoutCache : (Msg -> CharacterState -> CharacterState) -> Msg -> CharacterState -> (CharacterState, Cmd Msg)
withoutCache func msg prevState =
    (func msg prevState, Cmd.none)

withCache : (Msg -> CharacterState -> CharacterState) -> Msg -> CharacterState -> (CharacterState, Cmd Msg)
withCache func msg prevState =
    let state = func msg prevState
    in
        (state, cache <| encodeCharacterState state)

update : Msg -> CharacterState -> CharacterState
update msg model =
    case msg of
        ChangePlayer str ->
            { model | player = str }

        ChangeName str ->
            { model | name = str }

        ChangeHeroName str ->
            { model | heroName = str }

        ChangePlaybook str ->
            { model | playbook = str }

        ChangePowers str ->
            { model | powers = str }

        SetLabel label value ->
            { model | labels = model.labels |> OrderedDict.insert label value }

        ToggleCondition condition ->
            { model
                | conditions =
                    model.conditions |> OrderedDict.update condition (\mv -> Maybe.map (\v -> not v) mv)
            }

        IncrementPotential ->
            { model | potential = model.potential + 1 }

        DecrementPotential ->
            if model.potential > 0 then
                { model | potential = model.potential - 1 }
            else
                model

        TogglePotentialMark position ->
            -- Note that `position` is normally 0 indexed,
            -- what we're doing here works only if it's 1 indexed.
            if (position + 1) * masksPotentialPerAdvance > model.markedPotential then
                { model | markedPotential = model.markedPotential + masksPotentialPerAdvance }
            else
                { model | markedPotential = model.markedPotential - masksPotentialPerAdvance }



-- Thank fuck for currying.
view : List String -> CharacterState -> Html Msg
view playbooks model =
    div []
        [ div [ class "prelude" ]
              [ nameRow "Player Name: " model.player ChangePlayer
              , nameRow "Character Name: " model.name ChangeName
              , nameRow "Hero Name: " model.heroName ChangeHeroName
              , div [ class "prelude-field-box" ]
                  [ span [ class "prelude-field-title" ] [ text "Playbook: " ]
                  , input
                        [ value model.playbook
                        , name "playbook"
                        , list "playbooks"
                        , onInput ChangePlaybook
                        , class "prelude-field"
                        ]
                        []
                  , datalist [ id "playbooks" ]
                      (List.map (\x -> option [] [ text x ]) playbooks)
                  ]
              , div [ class "prelude-field-box" ]
                  [ span [ class "prelude-field-title" ] [ text "Powers/Abilities: " ]
                  , textarea [ value model.powers, onInput ChangePowers, class "prelude-field" ] []
                  ]
              ]
        , labelTable model.labels
        , conditionRow model.conditions
        , potentialRow masksPotentialPerAdvance model.potential model.markedPotential
        ]


nameRow : String -> String -> (String -> Msg) -> Html Msg
nameRow fieldName fieldValue updateFunc =
    div [ class "prelude-field-box" ]
        [ span [ class "prelude-field-title" ] [ text fieldName ]
        , input [ value fieldValue, onInput updateFunc, class "prelude-field" ] []
        ]



-- This is used as a parameter for our view,
-- though perhaps it may be more suited to the model.
-- Hm.
masksPlaybooks : List String
masksPlaybooks =
    [ "The Beacon"
    , "The Bull"
    , "The Delinquent"
    , "The Doomed"
    , "The Janus"
    , "The Legacy"
    , "The Nova"
    , "The Outsider"
    , "The Protege"
    , "The Transformed"
    , "The Scion"
    , "The Nomad"
    , "The Harbinger"
    , "The Innocent"
    , "The Joined"
    , "The Newborn"
    , "The Reformed"
    , "The Star"
    , "The Brain"
    , "The Soldier"

    -- Homebrew Playbooks:
    , "The Persona"
    , "The Host"
    ]


masksPotentialPerAdvance : Int
masksPotentialPerAdvance =
    5


labelTable : OrderedDict String Int -> Html Msg
labelTable dict =
    div [] <|
        [ table []
              (mapOrdDict labelRow dict 0)
        ]

labelRow : String -> Int -> Html Msg
labelRow label value =
    let cellFunc n =
            td [ onClick <| SetLabel label n
               , class "label-cell"
               , if n == value then
                     class "marked-label-cell"
                 else
                     class "unmarked-label-cell"
               ] [ text <| String.fromInt n ]
    in
        div []
            [ tr [ class "label-row" ]
                ([ span [ class "label-title" ] <| [ text label ]
                 , br [ class "label-title-break" ] []
                 ] ++ (List.map cellFunc (List.range -2 3)))
            ]

mapOrdDict : (comparable -> v -> x) -> OrderedDict comparable v -> v -> List x
mapOrdDict func dict d =
    let
        mapFunc key =
            let
                value =
                    Maybe.withDefault d (Dict.get key dict.dict)
            in
            func key value
    in
    List.map mapFunc dict.order


conditionRow conditions =
    let conditionFunc name marked =
            div [ class "condition-cell"
                , if marked then
                      class "marked-condition-cell"
                  else
                      class "unmarked-condition-cell"
                , onClick <| ToggleCondition name
                ]
                [ text name ]
    in
        div [ class "condition-row" ]
            (mapOrdDict conditionFunc conditions False)


potentialRow : Int -> Int -> Int -> Html Msg
potentialRow perAdvance potential markedPotential =
    let
        cellFunc position amount =
            div
                [ class "potential-cell"
                , if position < markedCells then
                    class "marked-potential-cell"

                  else
                    class "unmarked-potential-cell"
                , onClick <| TogglePotentialMark position
                ]
                [ text <| String.fromInt amount ]

        markedCells =
            markedPotential // perAdvance
    in
    div []
        (button [ onClick IncrementPotential ] [ text "+" ]
            :: button [ onClick DecrementPotential ] [ text "-" ]
            :: List.indexedMap cellFunc (groupPotential perAdvance potential)
        )



-- Note poor behavior over negative numbers,
-- but this app prohibits negative potential,
-- so it's fine.
groupPotential : Int -> Int -> List Int
groupPotential groupSize potential =
    let
        groups =
            potential // groupSize

        leftOver =
            remainderBy groupSize potential
    in
    List.map (\x -> groupSize) (List.range 1 groups) ++ [ leftOver ]
