import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import WebSocket
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Debug exposing (..)

webSocketAddress : String
webSocketAddress = "ws://localhost:3000"

main =
  Html.program
    { 
      init = init, 
      view = view, 
      update = update, 
      subscriptions = subscriptions
    }


-- MODEL

type alias Message = {
  username : String,
  message : String
}

type alias Model = { 
  currentUserName: String,
  currentMessage: String,
  users : List String,
  messages : List Message
}

init : (Model, Cmd Msg)
init =
  (Model "" "" [] [], Cmd.none)


-- UPDATE

type Msg = 
  UpdateUserName String | 
  UpdateMessage String |
  NewUser | 
  SendMessage |
  ReceiveMessage String

update : Msg -> Model -> (Model, Cmd Msg)
update msg {currentUserName, currentMessage, users, messages} =
  case msg of
    UpdateUserName newUserName -> (Model newUserName currentMessage users messages, Cmd.none)

    UpdateMessage newMessage -> (Model currentUserName newMessage users messages, Cmd.none)

    NewUser -> (Model currentUserName currentMessage (currentUserName :: users) messages, WebSocket.send webSocketAddress (createUserMessageJson "joined the chat" currentUserName))

    SendMessage -> (Model currentUserName "" users messages, WebSocket.send webSocketAddress (createUserMessageJson currentMessage currentUserName))

    ReceiveMessage userMessage -> (Model currentUserName currentMessage users (jsonToMessage userMessage :: messages), Cmd.none)

createUserMessage : String -> String -> Message
createUserMessage message currentUserName =
  (Message currentUserName message)

createUserMessageJson : String -> String -> String
createUserMessageJson message currentUserName =
  messageToJson (Message currentUserName message)

messageDecoder : Decoder Message
messageDecoder = map2 Message (field "name" Json.Decode.string) (field "message" Json.Decode.string)

messageEncoder : Message -> Json.Encode.Value
messageEncoder message =
    Json.Encode.object [ ("name", Json.Encode.string message.username), ("message", Json.Encode.string message.message) ]

jsonToMessage : String -> Message 
jsonToMessage messageJson =
  case decodeString messageDecoder messageJson of
    Ok message -> message

    Err err -> 
      Debug.log "Failed to decode message"
      (Message "" "")

messageToJson : Message -> String
messageToJson message =
  encode 4 (messageEncoder message)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen webSocketAddress ReceiveMessage


-- VIEW

view : Model -> Html Msg
view model =
  case model.users of
    [] ->
      div []
        [ 
          input [ type_ "text", placeholder "User Name", onInput UpdateUserName, Html.Attributes.value model.currentUserName ] [],
          button [ onClick NewUser ] [ text "Login" ]
        ]
    _  ->
      div []
        [ 
          input [ type_ "text", placeholder "Message", onInput UpdateMessage, Html.Attributes.value model.currentMessage ] [],
          button [ onClick SendMessage ] [ text "Send Message" ],
          div [] (List.map viewMessage (List.reverse model.messages))
        ]

viewMessage : Message -> Html msg
viewMessage msg =
  div [] [ text (msg.username ++ ": " ++ msg.message) ]