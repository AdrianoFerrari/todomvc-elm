module Todo where

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import List
import Maybe
import Signal
import String

{---- State ----}
type alias State =
  { todos: List Todo
  , field: String
  , uid: Int
  }

type alias Todo =
  { title: String
  , completed: Bool
  , id: Int
  }

{---- Evolution ----}
type Delta
    = NoOp
    | UpdateField String
    | TodoAdd Todo
    | TodoToggle Int Bool
    | TodoDelete Int

step : Delta -> State -> State
step delta state =
  case delta of
    NoOp -> state

    UpdateField str ->
      { state | field <- str }

    TodoAdd todo ->
      { state | todos <- state.todos ++ [todo]
              , field <- ""
              , uid <- state.uid + 1
              }

    TodoToggle id isCompleted ->
      let updateTodo t = if t.id == id then {t | completed <- isCompleted} else t
      in
        { state | todos <- List.map updateTodo state.todos }    

    TodoDelete id ->
      { state | todos <- List.filter (\t -> t.id /= id) state.todos }

{---- View ----}

view : State -> Html
view state =
  div
  []
  [ input [ placeholder "A New Todo"
          , value state.field
          , on "input" targetValue (Signal.send updates << UpdateField)
          , onKeyPress (\key -> if key == 13 then (Signal.send updates (TodoAdd { title = state.field, completed = False, id = state.uid + 1})) else (Signal.send updates NoOp))
          , autofocus True
          ]
          []
  , ul [] (List.map todoItemView state.todos)
  ]

todoItemView : Todo -> Html
todoItemView todo =
  li
  []
  [ input [ type' "checkbox"
          , checked todo.completed
          , onClick (Signal.send updates (TodoToggle todo.id (not todo.completed))) 
          ]
          []
  , text todo.title
  , button [onClick (Signal.send updates (TodoDelete todo.id))] [text "x"]
  ]

{---- Signals ----}
updates : Signal.Channel Delta
updates = Signal.channel NoOp

state : Signal State
state =
  Signal.foldp step initialState (Signal.subscribe updates)

{---- Portals ----}
main : Signal Html
main = Signal.map view state

initialState : State
initialState =
  Maybe.withDefault (State [] "" 0) getStorage

port getStorage : Maybe State

port setStorage : Signal State
port setStorage = state