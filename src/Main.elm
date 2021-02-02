module Main exposing (..)

{-|


# Due Diligence


## The feel-based decision maker

-}

import Browser
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import List.Extra
import Random


type SecondDeckCard
    = SkipCard -- Don't buy it, move on, move on to the next one.
    | Plus2Card -- Buy extra 2k
    | Plus4Card -- Buy extra 4k
      -- If you're not sure, if there's a bit of ambiguity there, don't force it.
      -- Don't coerce - reverse. Reverse the tickers!
      -- If looking at PENN, look at NNEP, ok, that's the ticker to buy.
      -- If NNEP is not a ticker, chop off letters until you arrive at a ticker.
      -- Chop off the end, now you're at NEP.
      -- K.I.S.S.
    | ReverseCard
      -- When in doubt, shake it about! Shake magic 8-ball about.
      -- If you don't get what you want and are still unsure, shake it about.
      -- You just keep on going until you get the answer you want.
    | WildCard
      -- If you're unsure of the fate, go back to the blank slate.
      -- Go back to the drawing board, forget the ticker you were working with.
      -- Go start from scratch.
      -- Don't start screening for a ticker.
      -- You don't need to go around and do research, it's such a waste of time.
      -- Refer to trusty bananagrams.
      -- Gotta put them in the order you picked them out in, otherwise you'll buy a ticker that's not right for you.
      -- Put them together, that's it. that's the company you're buying.
      -- See how simple that is!?
      -- KISS
    | BlankCard


type FirstDeckCard
    = TwoCard
    | FourCard
    | SixCard
    | EightCard


firstDeck : Random.Generator FirstDeckCard
firstDeck =
    Random.uniform TwoCard [ FourCard, SixCard, EightCard ]


secondDeck : Random.Generator SecondDeckCard
secondDeck =
    Random.uniform SkipCard [ Plus2Card, Plus4Card, ReverseCard, WildCard, BlankCard ]


eightBall : Random.Generator String
eightBall =
    Random.uniform "It is certain."
        [ "It is decidedly so."
        , "Without a doubt."
        , "Yes – definitely."
        , "You may rely on it."
        , "As I see it, yes."
        , "Most likely."
        , "Outlook good."
        , "Yes."
        , "Signs point to yes."
        , "Reply hazy, try again."
        , "Ask again later."
        , "Better not tell you now."
        , "Cannot predict now."
        , "Concentrate and ask again."
        , "Don't count on it."
        , "My reply is no."
        , "My sources say no."
        , "Outlook not so good."
        , "Very doubtful."
        ]


{-| Letter distribution:

    2: J, K, Q, X, Z
    3: B, C, F, H, M, P, V, W, Y
    4: G
    5: L
    6: D, S, U
    8: N
    9: T, R
    11: O
    12: I
    13: A
    18: E

-}
letters : List Char
letters =
    List.concat
        (List.repeat 2 [ 'J', 'K', 'Q', 'X', 'Z' ]
            ++ List.repeat 3 [ 'B', 'C', 'F', 'H', 'M', 'P', 'V', 'W', 'Y' ]
            ++ List.repeat 4 [ 'G' ]
            ++ List.repeat 5 [ 'L' ]
            ++ List.repeat 6 [ 'D', 'S', 'U' ]
            ++ List.repeat 8 [ 'N' ]
            ++ List.repeat 9 [ 'T', 'R' ]
            ++ List.repeat 11 [ 'O' ]
            ++ List.repeat 12 [ 'I' ]
            ++ List.repeat 13 [ 'A' ]
            ++ List.repeat 18 [ 'E' ]
        )



-- MODEL


type alias Model =
    { ticker : String
    , eightBallMessage : String
    , step : Step
    }


init : Model
init =
    Model "PENN" "" TickerStep


type Step
    = TickerStep
    | FirstDeckStep FirstDeckCard
    | SecondDeckStep FirstDeckCard SecondDeckCard



-- UPDATE


type Msg
    = TickerChanged String
    | AddLetterClicked
    | LetterAdded Char
    | DrawFirstCardClicked
    | GotFirstDeckCard FirstDeckCard
    | DrawSecondCardClicked
    | GotSecondDeckCard SecondDeckCard
    | StartOverClicked
    | StartOverWithTickerClicked String
    | LetterChopped
    | ShakeClicked
    | Shaked String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.step ) of
        ( TickerChanged ticker, TickerStep ) ->
            ( { model | ticker = ticker }, Cmd.none )

        ( AddLetterClicked, SecondDeckStep _ _ ) ->
            ( model
            , case availableLetters model of
                first :: others ->
                    Random.generate LetterAdded (Random.uniform first others)

                [] ->
                    Cmd.none
            )

        ( LetterAdded letter, SecondDeckStep _ _ ) ->
            ( { model | ticker = model.ticker ++ String.fromChar letter }, Cmd.none )

        ( DrawFirstCardClicked, TickerStep ) ->
            ( model, Random.generate GotFirstDeckCard firstDeck )

        ( GotFirstDeckCard card, TickerStep ) ->
            ( { model | step = FirstDeckStep card }, Cmd.none )

        ( DrawSecondCardClicked, FirstDeckStep _ ) ->
            ( model, Random.generate GotSecondDeckCard secondDeck )

        ( GotSecondDeckCard card, FirstDeckStep firstDeckCard ) ->
            ( { model
                | step = SecondDeckStep firstDeckCard card
                , ticker =
                    if card == ReverseCard then
                        String.reverse model.ticker

                    else if card == BlankCard then
                        ""

                    else
                        model.ticker
              }
            , Cmd.none
            )

        ( StartOverClicked, SecondDeckStep _ _ ) ->
            ( { init | ticker = model.ticker }, Cmd.none )

        ( StartOverWithTickerClicked ticker, SecondDeckStep _ _ ) ->
            ( { init | ticker = ticker }, Cmd.none )

        ( LetterChopped, SecondDeckStep _ _ ) ->
            ( { model | ticker = String.dropLeft 1 model.ticker }
            , Cmd.none
            )

        ( ShakeClicked, SecondDeckStep _ _ ) ->
            ( model, Random.generate Shaked eightBall )

        ( Shaked message, SecondDeckStep _ _ ) ->
            ( { model | eightBallMessage = message }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


availableLetters : Model -> List Char
availableLetters model =
    removeAll (String.toList model.ticker) letters


removeAll : List a -> List a -> List a
removeAll toRemove list =
    case ( toRemove, list ) of
        ( [], _ ) ->
            list

        ( _, [] ) ->
            list

        ( y :: ys, _ ) ->
            removeAll ys (List.Extra.remove y list)



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color (Element.rgb255 0 0 0)
        , Element.Font.family [ Element.Font.monospace ]
        ]
        (Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 20
            , Element.Font.color (Element.rgb255 248 63 20)
            , Element.spacing 20
            ]
            [ titleView
            , stepView model
            ]
        )


titleView : Element msg
titleView =
    Element.column
        [ Element.spacing 20 ]
        [ Element.el
            [ Element.Font.size 30
            , Element.Font.color (Element.rgb255 0 193 56)
            ]
            (Element.text "Due Diligence")
        , Element.el
            [ Element.Font.size 20
            , Element.Font.color (Element.rgb255 190 190 190)
            ]
            (Element.text "The feel based decision maker.")
        ]


stepView : Model -> Element Msg
stepView model =
    case model.step of
        TickerStep ->
            tickerStepView model

        FirstDeckStep card ->
            firstCardStepView model card

        SecondDeckStep firstCard secondCard ->
            secondCardStepView model firstCard secondCard



-- TICKER STEP VIEW


tickerStepView : Model -> Element Msg
tickerStepView model =
    Element.column [ Element.spacing 20, Element.width Element.fill ]
        [ tickerInputView model.ticker
        , Element.row [ Element.spacing 20 ]
            [ Element.Input.button buttonStyle
                { onPress = Just DrawFirstCardClicked
                , label = Element.text "draw first card"
                }
            ]
        ]


lettersView : Model -> Element msg
lettersView model =
    Element.paragraph []
        [ Element.text (String.join " " (List.map String.fromChar (availableLetters model))) ]


tickerInputView : String -> Element Msg
tickerInputView ticker =
    Element.Input.text
        inputStyle
        { onChange = TickerChanged
        , text = ticker
        , placeholder = Nothing
        , label =
            Element.Input.labelLeft
                [ Element.Font.size 20
                , Element.Font.color (Element.rgb255 190 190 190)
                ]
                (Element.text "ticker:")
        }



-- FIRST CARD STEP


firstCardStepView : Model -> FirstDeckCard -> Element Msg
firstCardStepView model card =
    Element.column [ Element.spacing 20, Element.width Element.fill ]
        [ tickerView model.ticker
        , Element.row [ Element.spacing 20 ]
            [ Element.Input.button buttonStyle
                { onPress = Just DrawSecondCardClicked
                , label = Element.text "draw second card"
                }
            ]
        , firstDeckCardView card
        , firstDeckCardExplanationView card
        ]


tickerView : String -> Element msg
tickerView ticker =
    Element.row []
        [ Element.el
            [ Element.Font.size 20
            , Element.Font.color (Element.rgb255 190 190 190)
            ]
            (Element.text "ticker:")
        , Element.el inputStyle (Element.text ticker)
        ]


firstDeckCardView : FirstDeckCard -> Element msg
firstDeckCardView card =
    case card of
        TwoCard ->
            firstCardView "2"

        FourCard ->
            firstCardView "4"

        SixCard ->
            firstCardView "6"

        EightCard ->
            firstCardView "8"


firstCardView : String -> Element msg
firstCardView text =
    Element.el
        [ Element.Border.width 5
        , Element.Border.rounded 0
        , Element.Border.color (Element.rgb255 250 250 90)
        , Element.Background.color (Element.rgb255 0 0 0)
        , Element.Font.color (Element.rgb255 250 250 90)
        , Element.height (Element.px 300)
        , Element.width (Element.px 200)
        , Element.Font.size 200
        ]
        (Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (Element.text text)
        )


firstDeckCardExplanationView : FirstDeckCard -> Element msg
firstDeckCardExplanationView card =
    Element.column []
        [ Element.el
            [ Element.Font.color (Element.rgb255 190 190 190) ]
            (Element.text "First card: ")
        , case card of
            TwoCard ->
                Element.paragraph [] [ Element.text "Buy 2k" ]

            FourCard ->
                Element.paragraph [] [ Element.text "Buy 4k" ]

            SixCard ->
                Element.paragraph [] [ Element.text "Buy 6k" ]

            EightCard ->
                Element.paragraph [] [ Element.text "Buy 8k" ]
        ]



-- SECOND CARD STEP


secondCardStepView : Model -> FirstDeckCard -> SecondDeckCard -> Element Msg
secondCardStepView model firstCard secondCard =
    Element.column [ Element.spacing 20, Element.width Element.fill ]
        [ tickerView model.ticker
        , secondDeckCardActionView model secondCard
        , Element.row [ Element.spacing 20 ]
            [ firstDeckCardView firstCard
            , secondDeckCardView secondCard
            ]
        , firstDeckCardExplanationView firstCard
        , secondDeckCardExplanationView model secondCard
        , if secondCard == WildCard && not (String.isEmpty model.eightBallMessage) then
            Element.column []
                [ Element.el
                    [ Element.Font.color (Element.rgb255 190 190 190) ]
                    (Element.text "8-ball answer: ")
                , eightBallView model.eightBallMessage
                ]

          else
            Element.none
        ]


secondDeckCardActionView : Model -> SecondDeckCard -> Element Msg
secondDeckCardActionView model secondCard =
    case secondCard of
        SkipCard ->
            Element.Input.button buttonStyle
                { onPress = Just StartOverClicked
                , label = Element.text "start over"
                }

        Plus2Card ->
            Element.none

        Plus4Card ->
            Element.none

        ReverseCard ->
            if String.length model.ticker < 2 then
                Element.none

            else
                Element.Input.button buttonStyle
                    { onPress = Just LetterChopped
                    , label = Element.text "chop off letter"
                    }

        WildCard ->
            Element.Input.button buttonStyle
                { onPress = Just ShakeClicked
                , label = Element.text "shake it about"
                }

        BlankCard ->
            Element.column [ Element.spacing 20 ]
                [ Element.row [ Element.spacing 20 ]
                    [ Element.Input.button buttonStyle
                        { onPress = Just AddLetterClicked
                        , label = Element.text "add letter to ticker"
                        }
                    , if not (String.isEmpty model.ticker) then
                        Element.Input.button buttonStyle
                            { onPress = Just (StartOverWithTickerClicked model.ticker)
                            , label = Element.text ("start over with " ++ model.ticker)
                            }

                      else
                        Element.none
                    ]
                , lettersView model
                ]


secondDeckCardView : SecondDeckCard -> Element msg
secondDeckCardView secondCard =
    case secondCard of
        SkipCard ->
            secondCardView "🚫"

        Plus2Card ->
            secondCardView "+2"

        Plus4Card ->
            secondCardView "+4"

        ReverseCard ->
            secondCardView "🔄"

        WildCard ->
            secondCardView "WILD"

        BlankCard ->
            secondCardView "_"


secondDeckCardExplanationView : Model -> SecondDeckCard -> Element msg
secondDeckCardExplanationView model secondCard =
    Element.column []
        [ Element.el
            [ Element.Font.color (Element.rgb255 190 190 190) ]
            (Element.text "Second card: ")
        , case secondCard of
            SkipCard ->
                Element.paragraph []
                    [ Element.text "Don't buy it, move on, move on to the next one." ]

            Plus2Card ->
                Element.paragraph []
                    [ Element.text "Buy extra 2k" ]

            Plus4Card ->
                Element.paragraph []
                    [ Element.text "Buy extra 4k" ]

            ReverseCard ->
                Element.paragraph []
                    [ Element.text "Don't coerce - reverse. Reverse the tickers! "
                    , Element.text ("You're looking at " ++ model.ticker ++ ", ok, that's the ticker to buy. ")
                    , if String.length model.ticker > 1 then
                        Element.text ("If " ++ model.ticker ++ " is not a ticker, chop off letters until you arrive at a ticker. ")

                      else
                        Element.none
                    , Element.text "K.I.S.S."
                    ]

            WildCard ->
                Element.paragraph []
                    [ Element.text "When in doubt, shake it about! Shake magic 8-ball about. "
                    , Element.text "If you don't get what you want and are still unsure, shake it about. "
                    , Element.text "You just keep on going until you get the answer you want."
                    ]

            BlankCard ->
                Element.paragraph
                    []
                    [ Element.text "If you're unsure of the fate, go back to the blank slate. "
                    , Element.text "Go back to the drawing board, forget the ticker you were working with. "
                    , Element.text "Go start from scratch. "
                    , Element.text "Don't start screening for a ticker. "
                    , Element.text "You don't need to go around and do research, it's such a waste of time. "
                    , Element.text "Refer to trusty bananagrams. "
                    , Element.text "Gotta put them in the order you picked them out in, otherwise you'll buy a ticker that's not right for you. "
                    , Element.text "Put them together, that's it. that's the company you're buying. "
                    , Element.text "See how simple that is!? "
                    , Element.text "KISS"
                    ]
        ]


secondCardView : String -> Element msg
secondCardView text =
    Element.el
        [ Element.Border.width 5
        , Element.Border.rounded 0
        , Element.Border.color (Element.rgb255 250 90 250)
        , Element.Background.color (Element.rgb255 0 0 0)
        , Element.Font.color (Element.rgb255 250 90 250)
        , Element.height (Element.px 300)
        , Element.width (Element.px 200)
        , Element.Font.size 200
        ]
        (Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (Element.text text)
        )


eightBallView : String -> Element msg
eightBallView text =
    Element.el
        [ Element.Border.width 5
        , Element.Border.rounded 250
        , Element.Border.color (Element.rgb255 50 250 50)
        , Element.Background.color (Element.rgb255 0 0 0)
        , Element.Font.color (Element.rgb255 50 250 50)
        , Element.height (Element.px 500)
        , Element.width (Element.px 500)
        , Element.Font.size 50
        , Element.padding 20
        ]
        (Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (Element.paragraph
                [ Element.centerX
                , Element.centerY
                ]
                [ Element.text text ]
            )
        )



-- STYLES


inputStyle : List (Element.Attribute msg)
inputStyle =
    [ Element.Border.width 5
    , Element.Border.rounded 0
    , Element.Border.color (Element.rgb255 0 91 173)
    , Element.Background.color (Element.rgb255 0 0 0)
    , Element.Font.color (Element.rgb255 0 188 250)
    , Element.Font.size 20
    , Element.padding 10
    ]


buttonStyle : List (Element.Attribute msg)
buttonStyle =
    [ Element.Border.width 5
    , Element.Border.rounded 0
    , Element.Border.color (Element.rgb255 55 55 55)
    , Element.Background.color (Element.rgb255 215 215 215)
    , Element.Font.color (Element.rgb255 0 0 0)
    , Element.Font.size 20
    , Element.padding 10
    ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
