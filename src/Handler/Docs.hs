{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Docs
  ( getDocsR
  ) where

import Foundation
    ( Handler, widgetSnackbar, widgetTopbar
    , Route (HomeR)
    , AppMessage
      ( MsgAppName, MsgDocumentation, MsgIssueTracking, MsgSourceCode
      , MsgOverview, MsgBasicEntities, MsgUsername, MsgSuperuser, MsgPassword
      , MsgUserRoles, MsgDataAdministrator
      , MsgDoc000, MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc004, MsgDoc007
      )
    )

import Settings (widgetFile)

import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)

import Yesod
    ( getMessageRender, getUrlRender
    )
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages
    )
import Yesod.Core.Widget (setTitleI)


getDocsR :: Handler Html
getDocsR = do
    r <- getUrlRender
    m <- getMessageRender
    msgs <- getMessages
    let t = preEscapedToHtml . m
    defaultLayout $ do
        setTitleI MsgDocumentation
        idOverlay <- newIdent
        $(widgetFile "docs/docs")
