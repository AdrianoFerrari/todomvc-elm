module Todo where

import Html
import List
import Signal
import String

{---- State ----}
type alias State =
  { todos: List Todo
  , uid: Int
  }

type alias Todo =
  { title: String
  , completed: Bool
  , id: Int
  }

{---- Evolution ----}
type Delta  = NoOp
            | TodoAdd Todo
            | TodoDelete Int

step : Delta -> State -> State
step delta state =
  case delta of
    NoOp -> state

    TodoAdd todo ->
      { state | todos <- state.todos ++ [todo] }

    TodoDelete id ->
      { state | todos <- List.filter (\t -> not (t.id == id)) state.todos }

{---- View ----}

view : State -> Html.Html
view state =
  Html.ul
  []
  [Html.text "testing"]

{---- Signals ----}
updates : Signal.Channel Delta
updates = Signal.channel NoOp

state : Signal State
state =
  Signal.foldp step initialState (Signal.subscribe updates)

{---- Portals In ----}
initialState : State
initialState = State [Todo "test1" False 0, Todo "test2" True 1] 1
