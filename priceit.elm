module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- model


type alias Model =
    { items : List Item
    , name : String
    , id : Maybe Int
    , parts : List Part
    }


type alias Item =
    { id : Int
    , name : String
    , hours : Int
    }


type ItemType
    = Graphic
    | ContentType
    | Action


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
    , parts = []
    }



--  Update


type Msg
    = Edit Item
    | Hours Item Int
    | Reset Item
    | Input String
    | SelectItemType Item
    | Save
    | Cancel
    | DeletePart Part


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
            { model | name = item.name, id = Just item.id }

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
            Item (List.length model.items) model.name 0

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
                        { item | name = model.name }
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
        , itemTypeSelector
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick Cancel ] [ text "Cancel" ]
        ]


itemTypeSelector : Html Msg
itemTypeSelector =
    select
        [ id "elementType"
        ]
        [ option [ disabled True, selected True ] [ text "Select Item Type" ]
        , option [] [ text "Graphic" ]
        , option [] [ text "Content Type" ]
        , option [] [ text "Action" ]
        , option [] [ text "Other" ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
