module Main where

import Prelude

import Domains.Todo (logics)
import Domains.User (UserId(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Gateways.TodoGateway (createTodoPort)
import Presenters.TodoPresenter (createOutputPort)
import Run (runBaseAff)
import Usecases.DisplayCompletedTodos (execute)
import Usecases.TodoOutputPort (runOutputPort)
import Usecases.TodoPort (runPort)

main :: Effect Unit
main =
  launchAff_ do
    let
      todoLogics = logics
    execute (UserId 1) todoLogics
      # runPort createTodoPort
      # runOutputPort createOutputPort
      # runBaseAff
