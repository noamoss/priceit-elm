module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe exposing (withDefault)


-- model


type alias Model =
    { items : List Item
    , name : String
    , id : Maybe Int
    , itemType : String
    , parts : List Part
    }


type alias Item =
    { id : Int
    , name : String
    , itemType : String
    , hours : Int
    }


type alias Part =
    { id : Int
    , name : String
    , hours : Int
    }



-- Initial model


initModel : Model
initModel =
    { items = []
    , name = ""
    , id = Nothing
    , itemType = ""
    , parts = []
    }



--  Update


type Msg
    = Edit Item
    | Hours Item Int
    | Reset Item
    | Input String
    | Save
    | Cancel
    | DeletePart Part
    | SelectItemType String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input name ->
            Debug.log "Input updated Model"
                { model | name = name }

        Cancel ->
            { model | name = "", id = Nothing }

        Save ->
            if (String.isEmpty model.name) then
                model
            else
                save model

        Hours item estimated ->
            addHours model item estimated

        Reset item ->
            reset model item

        Edit item ->
            { model | name = item.name, id = Just item.id, itemType = item.itemType }

        SelectItemType selection ->
            { model | itemType = selection }

        _ ->
            model


save : Model -> Model
save model =
    case model.id of
        Just id ->
            edit model id

        Nothing ->
            add model


add : Model -> Model
add model =
    let
        item =
            Item (List.length model.items) model.name model.itemType 0

        newItems =
            item :: model.items
    in
        { model
            | items = newItems
            , name = ""
        }


edit : Model -> Int -> Model
edit model id =
    let
        newItems =
            List.map
                (\item ->
                    if item.id == id then
                        { item
                            | name = model.name
                            , itemType = model.itemType
                        }
                    else
                        item
                )
                model.items

        newParts =
            List.map
                (\part ->
                    if part.id == id then
                        { part | name = model.name }
                    else
                        part
                )
                model.parts
    in
        { model
            | items = newItems
            , parts = newParts
            , name = ""
            , id = Nothing
        }


addHours : Model -> Item -> Int -> Model
addHours model item_to_add hours =
    let
        newItems =
            List.map
                (\item ->
                    if (item.id == item_to_add.id) && (item.hours + hours >= 0) then
                        { item
                            | hours = item.hours + hours
                        }
                    else
                        item
                )
                model.items

        part =
            Part (List.length model.parts) item_to_add.name hours
    in
        { model | items = newItems, parts = part :: model.parts }


reset : Model -> Item -> Model
reset model item_to_reset =
    let
        newItems =
            List.map
                (\item ->
                    if item.id == item_to_reset.id then
                        { item
                            | hours = 0
                        }
                    else
                        item
                )
                model.items
    in
        { model | items = newItems }



-- view


view : Model -> Html Msg
view model =
    div [ class "board" ]
        [ h1 [] [ text "Hours Keeper" ]
        , itemSection model
        , itemForm model
        , p [] [ text (toString model) ]
        ]


itemSection : Model -> Html Msg
itemSection model =
    div []
        [ itemListHeader
        , itemList model
        , hoursTotal model
        ]


itemListHeader : Html Msg
itemListHeader =
    header []
        [ div [] [ text "Item" ]
        , div [] [ text "Type" ]
        , div [] [ text "Hours Estimated" ]
        ]


itemList : Model -> Html Msg
itemList model =
    ul []
        (List.map item model.items)


item : Item -> Html Msg
item item =
    li []
        [ i
            [ class "edit"
            , onClick (Edit item)
            ]
            []
        , div []
            [ text item.name ]
        , div []
            [ text item.itemType ]
        , button
            [ type_ "button"
            , onClick (Reset item)
            ]
            [ text "Reset" ]
        , button
            [ type_ "button"
            , onClick (Hours item 1)
            ]
            [ text "+1" ]
        , button
            [ type_ "button"
            , onClick (Hours item -1)
            ]
            [ text "-1" ]
        , div []
            [ text (toString item.hours) ]
        ]


hoursTotal : Model -> Html Msg
hoursTotal model =
    let
        total =
            List.map .hours model.items
                |> List.sum
    in
        footer []
            [ div [] [ text "Total:" ]
            , div [] [ text (toString total) ]
            ]


itemForm : Model -> Html Msg
itemForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "text"
            , placeholder "Save/Edit Item..."
            , onInput Input
            , value model.name
            ]
            []
        , viewItemTypes
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick Cancel ] [ text "Cancel" ]
        ]


viewItemTypes : Html Msg
viewItemTypes =
    select [ onItemTypesChange SelectItemType ] itemTypesOptions


onItemTypesChange : (String -> msg) -> Html.Attribute msg
onItemTypesChange selection =
    on "change" (Json.Decode.map selection Html.Events.targetValue)


itemTypesOptions : List (Html Msg)
itemTypesOptions =
    [ option [ disabled True, selected True ] [ text "Select Item Type" ]
    , option [ value "Graphic" ] [ text "Graphic" ]
    , option [ value "Content Type" ] [ text "Content Type" ]
    , option [ value "Action" ] [ text "Action" ]
    , option [ value "Other" ] [ text "Other" ]
    ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
