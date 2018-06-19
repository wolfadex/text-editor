module Main exposing (main)


import Css
-- import Debug exposing (log)
import Dom
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Keyboard
import Keyboard.Extra as Keys
import Mouse exposing (Position)
import Rope exposing (Rope(..))
import Task


---- MODEL ----


type alias Model =
    { content : Rope
    , cursor : Position
    }


init : ( Model, Cmd Msg )
init =
    ( { content = Empty
      , cursor = Position 0 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | FocusContent
    | FocusContentResult (Result Dom.Error ())
    | CursorMove Position
    | TextInsert String
    | TextBackSpace
    | TextDelete
    | TextRemoveHighlighted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CursorMove amount ->
            ( { model | cursor = moveCursor model.cursor amount (Rope.getLength model.content) }
            , Cmd.none
            )
        TextInsert text ->
            let
                newContent = Rope.insert text model.cursor.x model.content
            in
                ( { model
                  | content = newContent
                  , cursor = moveCursor model.cursor (Position (String.length text) 0) (Rope.getLength newContent)
                  }
                , Cmd.none
                )
        TextBackSpace ->
            let
                cursorX = model.cursor.x
                newContent = Rope.remove (cursorX - 1) cursorX model.content
            in
                ( { model
                  | content = newContent
                  , cursor = moveCursor model.cursor (Position -1 0) (Rope.getLength newContent)
                  }
                , Cmd.none
                )
        TextDelete ->
            let
                cursorX = model.cursor.x
                newContent = Rope.remove cursorX (cursorX + 1) model.content
            in
                ( { model
                  | content = newContent
                  }
                , Cmd.none
                )
        -- TextRemoveHighlighted ->
        --
        FocusContent ->
            ( model, Task.attempt FocusContentResult (Dom.focus "editor-content") )
        FocusContentResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    ( model, Cmd.none )
                Ok () ->
                    ( model, Cmd.none )
        NoOp ->
            ( model, Cmd.none )
        _ ->
            ( model, Cmd.none )


moveCursor : Position -> Position -> Int -> Position
moveCursor { x, y } amount maxX =
    let
        newX = min (x + amount.x) maxX
        newY = y + amount.y
    in
        Position (max 0 newX) newY


---- VIEW ----


view : Model -> Html Msg
view { content, cursor } =
    Html.div [ Attribute.css [ Css.displayFlex
                             , Css.flexDirection Css.column
                             , Css.position Css.absolute
                             , Css.left (Css.px 0)
                             , Css.right (Css.px 0)
                             , Css.top (Css.px 0)
                             , Css.bottom (Css.px 0)
                             ]
             ]
             [ contentArea content cursor
             , Html.input [ Attribute.id "editor-content"
                          , Event.onInput TextInsert
                          , Attribute.value ""
                          , Attribute.css [ Css.opacity (Css.num 0)
                                          , Css.height (Css.rem 0)
                                          , Css.border (Css.rem 0)
                                          , Css.padding (Css.rem 0)
                                          ]
                          ]
                          []
             , Html.text ((toString cursor.x) ++ ", " ++ (toString cursor.y))
             ]


contentArea : Rope -> Position -> Html Msg
contentArea content cursor =
    Html.div [ Event.onClick FocusContent
             , Attribute.css [ Css.minHeight (Css.rem 5)
                             , Css.backgroundColor (Css.rgb 200 200 200)
                             , Css.flex (Css.num 1)
                             ]
             ]
             [ Html.text <| Rope.toString content
             , cursorNode
             ]


cursorNode : Html Msg
cursorNode =
    Html.div [ Attribute.css [ Css.height (Css.rem 1)
                             , Css.width (Css.rem 0.8)
                             , Css.borderLeftStyle Css.solid
                             , Css.borderLeftColor (Css.rgb 0 0 0)
                             , Css.borderLeftWidth (Css.rem 0.1)
                             , Css.marginLeft (Css.rem 1) -- TODO: temp only
                             ]
             , Attribute.class "cursor"
             ]
             []


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        -- , subscriptions = always Sub.none
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Keyboard.downs handleKeyDown
              ]


-- handleKeyDown : KeyCode -> Sub Msg
handleKeyDown code =
    case (Keys.fromCode code) of
        Keys.BackSpace ->
            TextBackSpace
        Keys.Delete ->
            TextDelete
        Keys.ArrowRight ->
            CursorMove (Position 1 0)
        Keys.ArrowLeft ->
            CursorMove (Position -1 0)
        Keys.ArrowUp ->
            CursorMove (Position 0 -1)
        Keys.ArrowDown ->
            CursorMove (Position 0 1)
        _ ->
            NoOp
