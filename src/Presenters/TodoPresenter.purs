module Presenters.TodoPresenter where

import Prelude

import Data.String (joinWith)
import Domains.Error (Error(..))
import Domains.Todo (Todo(..), TodoTitle(..), Todos)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Usecases.TodoOutputPort (TodoOutputPortType)

createOutputPort :: TodoOutputPortType
createOutputPort = {
  display: display,
  displayError: displayError
}

display :: Todos -> Aff Unit
display todos = do
  affLog $ "[Completed Todo Title]\n" <> joinWith "\n" (todos <#> (\(Todo {title: TodoTitle t}) -> t))

displayError :: Error -> Aff Unit
displayError (Error e) = do
  affLog e

affLog :: String -> Aff Unit
affLog = liftEffect <<< log