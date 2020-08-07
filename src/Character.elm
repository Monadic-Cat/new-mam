module Character exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, button, datalist, div, input, option, table, text, textarea, tr, td, span, br)
import Html.Attributes exposing (class, id, list, name, value)
import Html.Events exposing (onClick, onInput)
import OrderedDict exposing (OrderedDict)
import Json.Encode as JsonEncode
import Json.Decode as JsonDecode
import Json.Decode.Pipeline as Pipe

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



masksPotentialPerAdvance : Int
masksPotentialPerAdvance =
    5
