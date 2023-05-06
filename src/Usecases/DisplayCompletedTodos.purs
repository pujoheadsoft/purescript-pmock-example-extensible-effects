module Usecases.DisplayCompletedTodos where

import Prelude

import Data.Either (Either(..))
import Domains.Todo (Logics)
import Domains.User (UserId)
import Run (AFF, Run)
import Type.Row (type (+))
import Usecases.TodoOutputPort (TODO_OUTPUT_PORT, display, displayError)
import Usecases.TodoPort (TODO_PORT, findTodos)

execute 
  :: UserId
  -> Logics
  -> Run (TODO_PORT + TODO_OUTPUT_PORT + AFF + ()) Unit
execute id logics = do
  result <- findTodos id
  case result of
    Right todos -> display $ logics.completed todos
    Left e -> displayError e
