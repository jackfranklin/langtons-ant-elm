module App exposing (..)

import Html exposing (Html)
import AllDict exposing (AllDict)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, width, height, x, y, viewBox, stroke)
import Time exposing (Time)


type alias Coord =
    ( Int, Int )


type Colour
    = White
    | Black


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Ant =
    { position : Coord
    , direction : Direction
    }


type alias Cell =
    { position : Coord
    , colour : Colour
    }


type alias Board =
    AllDict Coord Cell String


type alias Model =
    { board : Board
    , ant : Ant
    }


initialCells : Board
initialCells =
    AllDict.fromList toString []


initialAnt : Ant
initialAnt =
    Ant ( 0, 0 ) Left


init : ( Model, Cmd Msg )
init =
    ( Model initialCells initialAnt, Cmd.none )


type Msg
    = Tick Time


getCoordInFront : Ant -> Coord
getCoordInFront { direction, position } =
    case direction of
        Up ->
            Tuple.mapSecond (\x -> x + 1) position

        Down ->
            Tuple.mapSecond (\x -> x - 1) position

        Left ->
            Tuple.mapFirst (\x -> x - 1) position

        Right ->
            Tuple.mapFirst (\x -> x + 1) position


getNextDirection : Ant -> Cell -> Direction
getNextDirection { direction } { colour } =
    case ( colour, direction ) of
        ( White, Up ) ->
            Right

        ( White, Right ) ->
            Down

        ( White, Down ) ->
            Left

        ( White, Left ) ->
            Up

        ( Black, Up ) ->
            Left

        ( Black, Right ) ->
            Up

        ( Black, Down ) ->
            Right

        ( Black, Left ) ->
            Down


getCell : Board -> Coord -> Cell
getCell board coord =
    AllDict.get coord board |> Maybe.withDefault (Cell coord White)


flipColour : Colour -> Colour
flipColour colour =
    case colour of
        Black ->
            White

        White ->
            Black


tick : Model -> Model
tick { ant, board } =
    let
        currentCell =
            getCell board ant.position

        newCell =
            { currentCell | colour = flipColour currentCell.colour }

        newBoard =
            AllDict.insert ant.position newCell board

        newAnt1 =
            { ant | direction = getNextDirection ant currentCell }

        newAnt2 =
            { newAnt1 | position = getCoordInFront newAnt1 }
    in
        Model newBoard newAnt2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( tick model, Cmd.none )


colourToSvgFill : Colour -> String
colourToSvgFill colour =
    case colour of
        White ->
            "white"

        Black ->
            "black"


renderItem : Coord -> String -> Svg Msg
renderItem ( xPos, yPos ) colour =
    rect
        [ stroke "black"
        , fill colour
        , x (toString (xPos * 10 - 5))
        , y (toString (yPos * 10 - 5))
        , width "10"
        , height "10"
        ]
        []


renderCell : Cell -> Svg Msg
renderCell { position, colour } =
    renderItem position (colourToSvgFill colour)


renderCells : Board -> List (Svg Msg)
renderCells board =
    AllDict.toList board
        |> List.map Tuple.second
        |> List.map renderCell


renderAnt : Ant -> Svg Msg
renderAnt { position, direction } =
    renderItem position (colourForAnt direction)


colourForAnt : Direction -> String
colourForAnt direction =
    case direction of
        Right ->
            "red"

        Left ->
            "blue"

        Down ->
            "purple"

        Up ->
            "pink"


view : Model -> Html Msg
view { board, ant } =
    svg [ width "600", height "600", viewBox "-500 -500 1000 1000" ]
        (renderCells board ++ [ renderAnt ant ])


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Time.millisecond) Tick
