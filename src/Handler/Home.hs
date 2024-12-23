{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Handler.Home
  ( getHomeR, postHomeR
  ) where

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc
    , (^.)
    )
import Database.Persist (entityVal, entityKey)

import Foundation
    ( Handler, Form, widgetSnackbar, widgetTopbar
    , Route (ReportFixedParamsR, HomeR)
    , AppMessage
      ( MsgAppName, MsgProject, MsgLoadParameters, MsgReport
      )
    )
    
import Material3 (md3selectWidget)

import Model
    ( ProjectId, Project(projectName)
    , ReportId, Report (reportName)
    , EntityField (ProjectId, ReportId)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, getMessageRender, newIdent, whamlet
    , MonadHandler (liftHandler), SomeMessage (SomeMessage), redirect
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Fields (selectField, optionsPairs)
import Yesod.Form.Types
    ( FieldSettings (FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postHomeR :: Handler Html
postHomeR = do

    ((fr,fw),et) <- runFormPost formOne
    case fr of
      FormSuccess (pid,rid) -> redirect $ ReportFixedParamsR pid rid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAppName 

              idOverlay <- newIdent
              idMain <- newIdent

              $(widgetFile "homepage")


getHomeR :: Handler Html
getHomeR = do

    (fw,et) <- generateFormPost formOne
        
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppName 

        idOverlay <- newIdent
        idMain <- newIdent

        $(widgetFile "homepage")


formOne :: Form (ProjectId, ReportId)
formOne extra = do
    
    projects <- liftHandler $ ((\e -> ((projectName . entityVal) e, entityKey e)) <$>) <$> runDB ( select $ do
        x <- from $ table @Project
        orderBy [asc (x ^. ProjectId)]
        return x )
    
    (projectR, projectV) <- mreq (selectField (optionsPairs projects)) FieldSettings
        { fsLabel = SomeMessage MsgProject
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
    
    reports <- liftHandler $ ((\e -> ((reportName . entityVal) e, entityKey e)) <$>) <$> runDB ( select $ do
        x <- from $ table @Report
        orderBy [asc (x ^. ReportId)]
        return x )
    
    (reportR, reportV) <- mreq (selectField (optionsPairs reports)) FieldSettings
        { fsLabel = SomeMessage MsgReport
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
        
    return ( (,) <$> projectR <*> reportR
           , [whamlet|
                     ^{extra}
                     ^{md3selectWidget projectV}
                     ^{md3selectWidget reportV}
                     |]
           )
