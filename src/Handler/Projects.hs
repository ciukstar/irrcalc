{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Projects
  ( getProjectsR, postProjectsR
  , getProjectR, postProjectR
  , getProjectNewR, getProjectEditR, postProjectDeleR
  ) where

import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, where_, val
    , (^.), (==.)
    )
import Database.Persist
    ( Entity (Entity), entityVal, insert_, replace, delete) 

import Foundation
    ( Handler, Form, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR (ProjectR, ProjectNewR, ProjectsR, ProjectEditR, ProjectDeleR)
    , AppMessage
      ( MsgProjects, MsgStartDate, MsgEndDate, MsgDescription, MsgName
      , MsgProject, MsgSave, MsgCancel, MsgAlreadyExists, MsgRecordAdded
      , MsgNoDataYet, MsgPleaseAddIfYouWish, MsgDele, MsgDeleteAreYouSure
      , MsgConfirmPlease, MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted
      )
    )
    
import Material3 (md3textareaWidget, md3widget)

import Model
    ( ProjectId
    , Project (Project, projectName, projectDescr, projectStart, projectEnd)
    , EntityField (ProjectName, ProjectId), msgSuccess, msgError
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    , SomeMessage (SomeMessage), addMessageI, redirect, whamlet
    )
import Yesod.Form.Fields (dayField, textareaField, textField)
import Yesod.Form.Functions (mopt, mreq, generateFormPost, checkM, runFormPost)
import Yesod.Form.Types
    ( Field
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postProjectDeleR :: ProjectId -> Handler Html
postProjectDeleR pid = do
    ((fr,_),_) <- runFormPost formProjectDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete pid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR ProjectsR
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ ProjectR pid


postProjectR :: ProjectId -> Handler Html
postProjectR pid = do
    
    project <- runDB $ selectOne $ do
        x <- from $ table @Project
        where_ $ x ^. ProjectId ==. val pid
        return x
        
    ((fr,fw),et) <- runFormPost $ formProject project
    case fr of
      FormSuccess r -> do
          runDB $ replace pid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ ProjectR pid
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgProject
              idOverlay <- newIdent
              $(widgetFile "data/projects/edit")


getProjectEditR :: ProjectId -> Handler Html
getProjectEditR pid = do
    
    project <- runDB $ selectOne $ do
        x <- from $ table @Project
        where_ $ x ^. ProjectId ==. val pid
        return x
        
    (fw,et) <- generateFormPost $ formProject project

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgProject
        idOverlay <- newIdent
        $(widgetFile "data/projects/edit")


postProjectsR :: Handler Html
postProjectsR = do
    ((fr,fw),et) <- runFormPost $ formProject Nothing      
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR ProjectsR
      
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgProject
              idOverlay <- newIdent
              $(widgetFile "data/projects/new")


getProjectNewR :: Handler Html
getProjectNewR = do
    (fw,et) <- generateFormPost $ formProject Nothing      

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgProject
        idOverlay <- newIdent
        $(widgetFile "data/projects/new")
        

formProject :: Maybe (Entity Project) -> Form Project
formProject project extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (projectName . entityVal <$> project)

    (startR,startV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgStartDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (projectStart . entityVal <$> project)

    (endR,endV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgEndDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (projectEnd . entityVal <$> project)

    (descrR,descrV) <- mopt textareaField  FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (projectDescr . entityVal <$> project)

    let r = Project <$> nameR <*> startR <*> endR <*> descrR

    return (r,$(widgetFile "data/projects/form"))
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Project
              where_ $ x ^. ProjectName ==. val name
              return x
              
          return $ case x of
            Nothing -> Right name
            Just (Entity pid' _) -> case project of
              Nothing -> Left MsgAlreadyExists
              Just (Entity pid'' _) | pid' == pid'' -> Right name
                                    | otherwise -> Left MsgAlreadyExists


getProjectR :: ProjectId -> Handler Html
getProjectR pid = do

    project <- runDB $ selectOne $ do
        x <- from $ table @Project
        where_ $ x ^. ProjectId ==. val pid
        return x

    (fw0,et0) <- generateFormPost formProjectDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgProjects
        idOverlay <- newIdent
        idDialogDelete <- newIdent        
        $(widgetFile "data/projects/project") 


formProjectDelete :: Form ()
formProjectDelete extra = return (pure (),[whamlet|^{extra}|])


getProjectsR :: Handler Html
getProjectsR = do

    projects <- runDB $ select $ do
        x <- from $ table @Project
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgProjects
        idOverlay <- newIdent
        $(widgetFile "data/projects/projects")
