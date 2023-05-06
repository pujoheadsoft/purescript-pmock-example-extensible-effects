module Test.Usecases.DisplayCompletedTodosSpec where

import Prelude

import Data.Either (Either(..))
import Domains.Error (Error(..))
import Domains.Todo (TodoStatus(..), TodoTitle(..), Todos, todo)
import Domains.User (UserId(..))
import Effect.Aff (Aff)
import Run (runBaseAff)
import Test.PMock (Param, any, fun, mock, mockFun, verify, verifyCount, (:>))
import Test.Spec (Spec, describe, it)
import Unsafe.Coerce (unsafeCoerce)
import Usecases.DisplayCompletedTodos (execute)
import Usecases.TodoOutputPort (runOutputPort)
import Usecases.TodoPort (runPort)

spec :: Spec Unit
spec = do
  describe "DisplayCompletedTodos Test" do
    it "指定されたユーザーIDに紐づくTodoのうち完了したTodoをすべて表示する" do
      let
        userId = UserId 1
        todo1 = todo (TodoTitle "Todo1") Completed
        todo2 = todo (TodoTitle "Todo2") InCompleted
        todos = [todo1, todo2]
        completedTodos = [todo1]

        findTodosFun = mockFun $ userId :> (pure $ Right todos :: Aff (Either Error Todos))
        completedTodosFun = mockFun $ todos :> completedTodos

        displayMock = mock $ completedTodos :> (pure unit :: Aff Unit)

        logics = { completed: completedTodosFun }
        todoPort = { findTodos: findTodosFun }
        todoOutputPort = { 
          display: fun displayMock,
          displayError: unsafeCoerce
        }

      _ <- execute (UserId 1) logics
        # runPort todoPort
        # runOutputPort todoOutputPort
        # runBaseAff
      
      verify displayMock completedTodos

    it "Todoの取得でエラーが発生した場合、エラーメッセージを表示する" do
      let
        userId = UserId 1

        findTodosFun = mockFun $ userId :> (pure $ Left $ Error "todo find error" :: Aff (Either Error Todos))

        displayMock = mock $ (any :: Param Todos) :> (pure unit :: Aff Unit)
        displayErrorMock = mock $ Error "todo find error" :> (pure unit :: Aff Unit)

        logics = { completed: unsafeCoerce }
        todoPort = { findTodos: findTodosFun }
        todoOutputPort = {
          display: fun displayMock,
          displayError: fun displayErrorMock
        }

      _ <- execute (UserId 1) logics
        # runPort todoPort
        # runOutputPort todoOutputPort
        # runBaseAff

      verify displayErrorMock $ Error "todo find error"
      verifyCount displayMock 0 (any :: Param Todos)
