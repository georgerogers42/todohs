{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Foundation
import Control.Applicative
import Database.Persist.Join hiding (runJoin)
import Database.Persist.Join.Sql
import Data.Maybe
import qualified Data.Text as T

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  categories <- runDB $ runJoin $ 
                    ((selectOneMany (TaskCategory<-.) taskCategory)
                       {somIncludeNoMatch = True})
  defaultLayout $ do
    h2id <- lift newIdent
    setTitle "todohs homepage"
    $(widgetFile "homepage")

postRootR :: Handler RepHtml
postRootR = do 
  (task,category) <- runInputPost $
                        (,)  <$> ireq textField "task"
                             <*> ireq textField "category"
  Just (catId, _cat) <- runDB $ selectFirst [CategoryName ==. category] []
  _ <- runDB $  insert $ Task task False catId
  redirect RedirectPermanent $ RootR 

postMkCategoryR :: Handler RepHtml
postMkCategoryR = do
  cat <- runInputPost $ ireq textField "name"
  _ <- runDB $ insert $ Category cat
  redirect RedirectPermanent $ RootR

postCategoryR :: CategoryId -> Handler RepHtml
postCategoryR i = do
  runDB $ deleteWhere [TaskCategory ==. i]
  runDB $ delete i
  redirect RedirectPermanent $ RootR

postDelTodoR :: TaskId -> Handler RepHtml
postDelTodoR i = do
  runDB $ delete i
  redirect RedirectPermanent $ RootR

postTodoR :: TaskId -> Handler RepHtml
postTodoR i = do
  d <- runInputPost $ iopt textField "done"
  let done = isJust d
  runDB $ update i [TaskDone =. done]
  redirect RedirectPermanent $ RootR
join :: [T.Text] -> T.Text
join = T.intercalate " "
