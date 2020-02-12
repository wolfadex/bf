module Main exposing (main)

import Array exposing (Array)
import Browser
import Element exposing (Element)
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Process
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { commands : Array Command
    , state : Array Int
    , output : String
    , statePointer : Int
    , commandPointer : Int
    , speed : Int
    , input : Maybe Char
    }


type Msg
    = Run
    | Reset
    | SetCommands String
    | SetSpeed String
    | SetInput String


type Command
    = IncrementPointer
    | DecrementPointer
    | IncrementValue
    | DecrementValue
    | OuputValue
    | AwaitInput
    | WhileBegin
    | WhileEnd


init : () -> ( Model, Cmd Msg )
init _ =
    ( reset
    , Cmd.none
    )


reset : Model
reset =
    { commands = Array.empty
    , state = Array.repeat 30 0
    , output = ""
    , statePointer = 0
    , commandPointer = 0
    , speed = 500
    , input = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCommands commands ->
            ( { model
                | commands =
                    commands
                        |> String.toList
                        |> List.filterMap
                            (\char ->
                                case char of
                                    '>' ->
                                        Just IncrementPointer

                                    '<' ->
                                        Just DecrementPointer

                                    '+' ->
                                        Just IncrementValue

                                    '-' ->
                                        Just DecrementValue

                                    '.' ->
                                        Just OuputValue

                                    ',' ->
                                        Just AwaitInput

                                    '[' ->
                                        Just WhileBegin

                                    ']' ->
                                        Just WhileEnd

                                    _ ->
                                        Nothing
                            )
                        |> Array.fromList
              }
            , Cmd.none
            )

        SetInput str ->
            case ( model.input, String.uncons str ) of
                ( Nothing, Just ( newInput, _ ) ) ->
                    case Array.get model.statePointer model.state of
                        Nothing ->
                            ( { model | commandPointer = model.commandPointer + 1 }, nextCommand model.speed )

                        Just i ->
                            ( { model
                                | input = Just newInput
                                , commandPointer = model.commandPointer + 1
                                , state = Array.set i (Char.toCode newInput) model.state
                              }
                            , nextCommand model.speed
                            )

                _ ->
                    ( model, Cmd.none )

        Run ->
            case Array.get model.commandPointer model.commands of
                Just OuputValue ->
                    case Array.get model.statePointer model.state of
                        Nothing ->
                            ( { model | commandPointer = model.commandPointer + 1 }, nextCommand model.speed )

                        Just i ->
                            ( { model
                                | output = model.output ++ (i |> Char.fromCode |> String.fromChar)
                                , commandPointer = model.commandPointer + 1
                              }
                            , nextCommand model.speed
                            )

                Just IncrementPointer ->
                    ( { model | commandPointer = model.commandPointer + 1, statePointer = model.statePointer + 1 }, nextCommand model.speed )

                Just DecrementPointer ->
                    ( { model | commandPointer = model.commandPointer + 1, statePointer = model.statePointer - 1 }, nextCommand model.speed )

                Just IncrementValue ->
                    case Array.get model.statePointer model.state of
                        Nothing ->
                            ( { model | commandPointer = model.commandPointer + 1 }, nextCommand model.speed )

                        Just i ->
                            ( { model
                                | state = Array.set model.statePointer (i + 1) model.state
                                , commandPointer = model.commandPointer + 1
                              }
                            , nextCommand model.speed
                            )

                Just DecrementValue ->
                    case Array.get model.statePointer model.state of
                        Nothing ->
                            ( { model | commandPointer = model.commandPointer + 1 }, nextCommand model.speed )

                        Just i ->
                            ( { model
                                | state = Array.set model.statePointer (i - 1) model.state
                                , commandPointer = model.commandPointer + 1
                              }
                            , nextCommand model.speed
                            )

                Just AwaitInput ->
                    ( { model | input = Nothing }, Cmd.none )

                Just WhileBegin ->
                    case Array.get model.statePointer model.state of
                        Nothing ->
                            ( { model | commandPointer = model.commandPointer + 1 }, nextCommand model.speed )

                        Just 0 ->
                            ( { model
                                | commandPointer = indexOfEndWhile model.commandPointer 0 model.commands + 1
                              }
                            , nextCommand model.speed
                            )

                        Just _ ->
                            ( { model
                                | commandPointer = model.commandPointer + 1
                              }
                            , nextCommand model.speed
                            )

                Just WhileEnd ->
                    case Array.get model.statePointer model.state of
                        Nothing ->
                            ( { model | commandPointer = model.commandPointer + 1 }, nextCommand model.speed )

                        Just 0 ->
                            ( { model
                                | commandPointer = model.commandPointer + 1
                              }
                            , nextCommand model.speed
                            )

                        Just _ ->
                            ( { model
                                | commandPointer = indexOfBeginWhile (model.commandPointer - 1) 0 model.commands + 1
                              }
                            , nextCommand model.speed
                            )

                Nothing ->
                    ( model, Cmd.none )

        Reset ->
            ( reset, Cmd.none )

        SetSpeed newSpeedStr ->
            case String.toInt newSpeedStr of
                Nothing ->
                    ( model, Cmd.none )

                Just newSpeed ->
                    if newSpeed <= 0 then
                        ( { model | speed = 1 }, Cmd.none )

                    else
                        ( { model | speed = newSpeed }, Cmd.none )


indexOfEndWhile : Int -> Int -> Array Command -> Int
indexOfEndWhile currentIndex extraMatchesToAccountFor commands =
    case Array.get currentIndex commands of
        Nothing ->
            -1

        Just command ->
            case command of
                WhileBegin ->
                    indexOfEndWhile (currentIndex + 1) (extraMatchesToAccountFor + 1) commands

                WhileEnd ->
                    if extraMatchesToAccountFor == 0 then
                        currentIndex

                    else
                        indexOfEndWhile (currentIndex + 1) (extraMatchesToAccountFor - 1) commands

                _ ->
                    indexOfEndWhile (currentIndex + 1) extraMatchesToAccountFor commands


indexOfBeginWhile : Int -> Int -> Array Command -> Int
indexOfBeginWhile currentIndex extraMatchesToAccountFor commands =
    case Array.get currentIndex commands of
        Nothing ->
            -1

        Just command ->
            case command of
                WhileEnd ->
                    indexOfBeginWhile (currentIndex - 1) (extraMatchesToAccountFor + 1) commands

                WhileBegin ->
                    if extraMatchesToAccountFor == 0 then
                        currentIndex

                    else
                        indexOfBeginWhile (currentIndex - 1) (extraMatchesToAccountFor - 1) commands

                _ ->
                    indexOfBeginWhile (currentIndex - 1) extraMatchesToAccountFor commands


nextCommand : Int -> Cmd Msg
nextCommand speed =
    Process.sleep (toFloat speed)
        |> Task.perform (\_ -> Run)


view : Model -> Html Msg
view { commands, state, output, commandPointer, statePointer, speed, input } =
    Element.layout
        [ Element.padding 16 ]
        (Element.column
            [ Element.spacing 16 ]
            [ button
                { onPress = Just Reset
                , label = Element.text "Reset"
                }
            , Input.text
                []
                { onChange = SetCommands
                , text = Array.map commandToString commands |> Array.toList |> String.join ""
                , placeholder = Nothing
                , label = Input.labelAbove [] (Element.text "Commands")
                }
            , Input.text
                []
                { onChange = SetSpeed
                , text = String.fromInt speed
                , placeholder = Nothing
                , label = Input.labelAbove [] (Element.text "Speed (larger = slower)")
                }
            , button
                { onPress =
                    if Array.length commands > 0 then
                        Just Run

                    else
                        Nothing
                , label = Element.text "Run"
                }
            , Element.text "Output"
            , Element.text output
            , Element.text "Run Input"
            , Element.row
                []
                (commands
                    |> Array.toList
                    |> List.map
                        (\command ->
                            Element.el
                                [ Element.width (Element.px 16), Element.height (Element.px 16) ]
                                (command |> commandToString |> Element.text)
                        )
                )
            , Element.row
                []
                (Array.repeat (Array.length commands) " "
                    |> Array.set commandPointer "^"
                    |> Array.toList
                    |> List.map
                        (\p ->
                            Element.el
                                [ Element.width (Element.px 16), Element.height (Element.px 16) ]
                                (Element.text p)
                        )
                )
            , case input of
                Nothing ->
                    Input.text
                        []
                        { onChange = SetInput
                        , text = ""
                        , placeholder = Nothing
                        , label = Input.labelAbove [] (Element.text "Enter a character please")
                        }

                Just c ->
                    Element.text ("Input: " ++ String.fromChar c)
            , Element.text "Run State"
            , Element.row
                []
                (state
                    |> Array.toList
                    |> List.map
                        (\command ->
                            Element.el
                                [ Element.width (Element.px 32), Element.height (Element.px 16) ]
                                (command |> String.fromInt |> Element.text)
                        )
                )
            , Element.row
                []
                (Array.repeat (Array.length state) " "
                    |> Array.set statePointer "^"
                    |> Array.toList
                    |> List.map
                        (\p ->
                            Element.el
                                [ Element.width (Element.px 32), Element.height (Element.px 16) ]
                                (Element.text p)
                        )
                )

            -- , Element.text "State"
            -- , Element.wrappedRow
            --     []
            --     (state
            --         |> Array.toList
            --         |> List.indexedMap viewCell
            --     )
            ]
        )


button : { onPress : Maybe Msg, label : Element Msg } -> Element Msg
button =
    Input.button
        [ Border.solid
        , Border.width 1
        , Element.paddingXY 16 8
        ]


commandToString : Command -> String
commandToString command =
    case command of
        IncrementPointer ->
            ">"

        DecrementPointer ->
            "<"

        IncrementValue ->
            "+"

        DecrementValue ->
            "-"

        OuputValue ->
            "."

        AwaitInput ->
            ","

        WhileBegin ->
            "["

        WhileEnd ->
            "]"
