module Todo where

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import List
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
    | TodoDelete Int

step : Delta -> State -> State
step delta state =
  case delta of
    NoOp -> state

    UpdateField str ->
      { state | field <- str }

    TodoAdd todo ->
      { state | todos <- state.todos ++ [todo] }

    TodoDelete id ->
      { state | todos <- List.filter (\t -> t.id /= id) state.todos }

{---- View ----}

view : State -> Html
view state =
  div
  []
  [ input [on "input" targetValue (Signal.send updates << UpdateField)] []
  , button [onClick (Signal.send updates (TodoAdd { title = state.field, completed = False, id = state.uid + 1}))] [text "Add"]
  , ul [] (List.map todoItemView state.todos)
  ]

todoItemView : Todo -> Html
todoItemView todo =
  li
  []
  [text todo.title]

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
initialState = State [Todo "test1" False 0, Todo "test2" True 1] "default" 1
