port module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import OrderedDict exposing (OrderedDict)
import Json.Encode as JsonEncode
import Json.Decode as JsonDecode
import Json.Decode.Pipeline as Pipe
import Array exposing (Array)
import Http
import Character exposing (CharacterState)

port cache : JsonEncode.Value -> Cmd msg
port sheetToClipboard : JsonEncode.Value -> Cmd msg

main =
    Browser.element { init = \x -> ( init x, Cmd.none )
                    , subscriptions = \c -> Sub.none
                    , update = withAppCache update
                    , view = view
                    }

-- Settings for the user to configure directly
type alias UserSettings =
    { syncing : Syncing }

type Syncing
    = SyncEnabled SyncData
    | SyncDisabled MaybeSync

type alias MaybeSync =
    { token : Maybe JsonWebToken
    , backend : Maybe String
    }

-- Wrapper to make misusing a JWT a bit harder.
-- This will also make it a bit harder to use a
-- String that doesn't belong on auth methods.
type JsonWebToken = JsonWebToken String

type alias SyncData =
    -- This field should not actually be shown,
    -- and clearing it is tantamount to disabling syncing.
    -- It may be refreshed, however.
    { token : JsonWebToken
    -- This should be a guaranteed valid URL
    -- The first value it holds will be statically guaranteed,
    -- by virtue of being written in at compile time by me,
    -- but ultimately this should be a customizable field.
    , backend : String
    }

type alias AppState =
    { characters : Array CharacterState
    , selectedCharacter : Int
    , settings : UserSettings
    }
getCharacterState : Int -> AppState -> Maybe CharacterState
getCharacterState index state = Array.get index state.characters

init : JsonDecode.Value -> AppState
init state = case JsonDecode.decodeValue appStateDecoder state of
                    Ok app -> app
                    Err err -> emptyAppState

emptyUserSettings : UserSettings
emptyUserSettings =
    UserSettings (SyncDisabled (MaybeSync Nothing Nothing))

emptyAppState : AppState
emptyAppState =
    AppState Array.empty 0 emptyUserSettings

type Msg
    -- All messages that deal with the current character specifcally.
    = CharMsg Character.Msg
    | SwitchChar String
    | ExportChar
    -- | GotAuth (Result Http.Error String)
    -- | CreateCharacter
    -- | ReceivedSync


withoutCache : (Character.Msg -> CharacterState -> CharacterState) -> Character.Msg -> CharacterState -> (CharacterState, Cmd Character.Msg)
withoutCache func msg prevState =
    (func msg prevState, Cmd.none)

withCache : (Character.Msg -> CharacterState -> CharacterState) -> Character.Msg -> CharacterState -> (CharacterState, Cmd Character.Msg)
withCache func msg prevState =
    let state = func msg prevState
    in
        (state, cache <| encodeCharacterState state)

withAppCache : (Msg -> AppState -> (AppState, Cmd Msg)) -> Msg -> AppState -> (AppState, Cmd Msg)
withAppCache func msg prevState =
    let (state, cmd) = func msg prevState
    in
        (state, Cmd.batch [cache <| encodeAppState state, cmd])

viewWithCharacterState : (CharacterState -> Html Character.Msg) -> AppState -> Html Character.Msg
viewWithCharacterState charView model = case getCharacterState model.selectedCharacter model of
                                            Just char -> charView char
                                            Nothing -> charView Character.emptyCharacterState

view : AppState -> Html Msg
view model = Html.div [] [ Html.select [Html.Events.onInput SwitchChar] <| Array.toList <| Array.indexedMap
                               (\i c -> Html.option
                                    [Html.Attributes.value <| String.fromInt i]
                                    [Html.text c.name])
                               model.characters
                         , Html.map CharMsg <| viewWithCharacterState (Character.view masksPlaybooks) model
                         , Html.button [Html.Events.onClick ExportChar] [Html.text "Copy to Clipboard"]
                         ]

update : Msg -> AppState -> (AppState, Cmd Msg)
update msg model =
    case msg of
        CharMsg m ->
            case getCharacterState model.selectedCharacter model of
                Just char -> let new_char = Character.update m char
                                 new_chars = Array.set model.selectedCharacter new_char model.characters
                                 state = { model | characters = new_chars }
                             in (state, Cmd.none)
                -- If there is no character where selected,
                -- create them until there is. Then, handle the actual message.
                Nothing -> update msg
                           { model | characters = Array.push Character.emptyCharacterState model.characters }
        SwitchChar s -> case String.toInt s of
                                    Just n -> ({ model | selectedCharacter = n }, Cmd.none)
                                    Nothing -> (model, Cmd.none)
        ExportChar -> (model, sheetToClipboard <| encodeCharacterState <|
                           (getCharacterState model.selectedCharacter model
                           |> Maybe.withDefault Character.emptyCharacterState))
-- GotAuth r ->
        --     case r of
        --         Ok token -> model
        --         Err _ -> model

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


-- All of the JSON encoders and decoders for this app are here at the top level,
-- because they're really global to the app- the field names are arbitrary and
-- not universal to all things that use the data types involved.
-- If I ever want to make sweeping changes to this, it'd be a real pain to try and
-- chase down all the pieces throughout a large number of files.
-- Admittedly, the Elm Language Server + Emacs LSP mode would make this less of a pain,
-- but I'd rather not deal with that.

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
encodeAppState : AppState -> JsonEncode.Value
encodeAppState state =
    JsonEncode.object
        [ ("characters", JsonEncode.array encodeCharacterState state.characters)
        , ("selectedCharacter", JsonEncode.int state.selectedCharacter)
        ]

characterStateDecoder : JsonDecode.Decoder CharacterState
characterStateDecoder =
    JsonDecode.succeed CharacterState
        |> Pipe.optional "player"          JsonDecode.string ""
        |> Pipe.optional "name"            JsonDecode.string ""
        |> Pipe.optional "heroName"        JsonDecode.string ""
        |> Pipe.optional "playbook"        JsonDecode.string ""
        |> Pipe.optional "powers"          JsonDecode.string ""
        |> Pipe.optional "labels"          (orderedDictDecoder JsonDecode.int) Character.defaultLabelState
        |> Pipe.optional "conditions"      (orderedDictDecoder JsonDecode.bool) Character.defaultConditionsState
        |> Pipe.optional "potential"       JsonDecode.int 0
        |> Pipe.optional "markedPotential" JsonDecode.int 0

-- userSettingsDecoder : JsonDecode.Decoder UserSettings
-- userSettingsDecoder =
--     JsonDecode.succeed 
        

appStateDecoder : JsonDecode.Decoder AppState
appStateDecoder =
    JsonDecode.succeed AppState
        |> Pipe.optional "characters" (JsonDecode.array characterStateDecoder) Array.empty
        |> Pipe.optional "selectedCharacter" JsonDecode.int 0
        |> Pipe.hardcoded emptyUserSettings
