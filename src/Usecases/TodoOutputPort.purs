module Usecases.TodoOutputPort where

import Prelude

import Domains.Error (Error)
import Domains.Todo (Todos)
import Effect.Aff (Aff)
import Run (AFF, Run, interpret, lift, liftAff, on, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type TodoOutputPortType = {
  display :: Todos -> Aff Unit,
  displayError :: Error -> Aff Unit
}

data TodoOutputPort a
  = Display Todos a
  | DisplayError Error a

-- The following is almost boilerplate
derive instance todoOutputPortF :: Functor TodoOutputPort
type TODO_OUTPUT_PORT r = (todoOutputPort :: TodoOutputPort | r)
_todoOutputPort = Proxy :: Proxy "todoOutputPort"

display :: forall r. Todos -> Run (TODO_OUTPUT_PORT + r) Unit
display todos = lift _todoOutputPort (Display todos unit)

displayError :: forall r. Error -> Run (TODO_OUTPUT_PORT + r) Unit
displayError error = lift _todoOutputPort (DisplayError error unit)

runOutputPort :: forall r. TodoOutputPortType -> Run (TODO_OUTPUT_PORT + AFF + r) ~> Run (AFF + r)
runOutputPort t run = interpret (on _todoOutputPort (todoOutputHandler t) send) run

todoOutputHandler :: forall r. TodoOutputPortType -> TodoOutputPort ~> Run (AFF + r)
todoOutputHandler t r = case r of
  Display todos a -> do
    liftAff $ t.display todos
    pure a
  DisplayError e a -> do
    liftAff $ t.displayError e
    pure a