{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Periods
  ( getPeriodsR, postPeriodsR
  , getPeriodR, postPeriodR
  , getPeriodNewR, getPeriodEditR, postPeriodDeleR
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
    , DataR (PeriodR, PeriodNewR, PeriodsR, PeriodEditR, PeriodDeleR)
    , AppMessage
      ( MsgPeriods, MsgStartDate, MsgEndDate, MsgName, MsgPeriod, MsgSave
      , MsgCancel, MsgAlreadyExists, MsgRecordAdded, MsgNoDataYet
      , MsgPleaseAddIfYouWish, MsgDele, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted
      )
    )
    
import Material3 (md3widget)

import Model
    ( PeriodId
    , Period (Period, periodName, periodStart, periodEnd)
    , EntityField (PeriodName, PeriodId), msgSuccess, msgError
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    , SomeMessage (SomeMessage), addMessageI, redirect, whamlet
    )
import Yesod.Form.Fields (dayField, textField)
import Yesod.Form.Functions (mreq, generateFormPost, checkM, runFormPost)
import Yesod.Form.Types
    ( Field
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postPeriodDeleR :: PeriodId -> Handler Html
postPeriodDeleR pid = do
    ((fr,_),_) <- runFormPost formPeriodDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete pid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR PeriodsR
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ PeriodR pid


postPeriodR :: PeriodId -> Handler Html
postPeriodR pid = do
    
    period <- runDB $ selectOne $ do
        x <- from $ table @Period
        where_ $ x ^. PeriodId ==. val pid
        return x
        
    ((fr,fw),et) <- runFormPost $ formPeriod period
    case fr of
      FormSuccess r -> do
          runDB $ replace pid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ PeriodR pid
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPeriod
              idOverlay <- newIdent
              $(widgetFile "data/periods/edit")


getPeriodEditR :: PeriodId -> Handler Html
getPeriodEditR pid = do
    
    period <- runDB $ selectOne $ do
        x <- from $ table @Period
        where_ $ x ^. PeriodId ==. val pid
        return x
        
    (fw,et) <- generateFormPost $ formPeriod period

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPeriod
        idOverlay <- newIdent
        $(widgetFile "data/periods/edit")


postPeriodsR :: Handler Html
postPeriodsR = do
    ((fr,fw),et) <- runFormPost $ formPeriod Nothing      
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR PeriodsR
      
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPeriod
              idOverlay <- newIdent
              $(widgetFile "data/periods/new")


getPeriodNewR :: Handler Html
getPeriodNewR = do
    (fw,et) <- generateFormPost $ formPeriod Nothing      

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPeriod
        idOverlay <- newIdent
        $(widgetFile "data/periods/new")
        

formPeriod :: Maybe (Entity Period) -> Form Period
formPeriod period extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (periodName . entityVal <$> period)

    (startR,startV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgStartDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (periodStart . entityVal <$> period)

    (endR,endV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgEndDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (periodEnd . entityVal <$> period)

    let r = Period <$> nameR <*> startR <*> endR

    return (r,$(widgetFile "data/periods/form"))
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Period
              where_ $ x ^. PeriodName ==. val name
              return x
              
          return $ case x of
            Nothing -> Right name
            Just (Entity pid' _) -> case period of
              Nothing -> Left MsgAlreadyExists
              Just (Entity pid'' _) | pid' == pid'' -> Right name
                                    | otherwise -> Left MsgAlreadyExists


getPeriodR :: PeriodId -> Handler Html
getPeriodR pid = do

    period <- runDB $ selectOne $ do
        x <- from $ table @Period
        where_ $ x ^. PeriodId ==. val pid
        return x

    (fw0,et0) <- generateFormPost formPeriodDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPeriods
        idOverlay <- newIdent
        idDialogDelete <- newIdent        
        $(widgetFile "data/periods/period") 


formPeriodDelete :: Form ()
formPeriodDelete extra = return (pure (),[whamlet|^{extra}|])


getPeriodsR :: Handler Html
getPeriodsR = do

    periods <- runDB $ select $ do
        x <- from $ table @Period
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPeriods
        idOverlay <- newIdent
        $(widgetFile "data/periods/periods")
