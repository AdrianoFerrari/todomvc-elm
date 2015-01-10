module Todo where

import Html
import List
import Signal
import String

{---- State ----}
type Visibility = All | Active | Completed

type alias State =
  { todos: List Todo
  , visibility: Visibility
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
            | SetFilter Visibility
            {-| TodoToggle Int-}

update : State -> Delta -> State
update state delta =
  case delta of
    TodoAdd todo ->
      { state | todos <- state.todos ++ [todo] }

    TodoDelete id ->
      { state | todos <- List.filter (\t -> not (t.id == id)) state.todos }

    {-TodoToggle id ->
      { state | todos <- List.map (\t -> if t.id then (t.completed <- not t.complete)) todos}
        -}

    SetFilter vis ->
      { state | visibility <- vis }

    NoOp -> state

{---- View ----}

{---- Signals ----}
