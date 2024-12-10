{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Rules
    ( getRulesR, postRulesR
    , getRuleR, postRuleR
    , getRuleNewR, getRuleEditR, postRuleDeleR
    ) where

import Data.Fixed (Centi)
import Data.Text (Text)
import Data.Time.Calendar (Day)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val
    , (^.), (==.), orderBy, asc
    )
import Database.Persist
    ( Entity (Entity), entityVal, delete, insert_, replace
    ) 

import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR)
    , DataR
      ( RuleR, RuleNewR, RuleDeleR, RuleEditR, RulesR, RulesR
      , ReportR, ReportsR
      )
    , AppMessage
      ( MsgReportParameters, MsgInflation, MsgGenerateReport, MsgPeriodicity
      , MsgConstructionArea, MsgArticle, MsgStart, MsgEnd, MsgEveryJanuary
      , MsgGeneralPlanDocuments, MsgDuration, MsgLandPlotPurposeChange
      , MsgRules, MsgSequence, MsgRules, MsgPleaseAddIfYouWish, MsgNoDataYet
      , MsgAlreadyExists, MsgName, MsgDescription, MsgDele, MsgCancel
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgRule, MsgSave, MsgRecordAdded
      , MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted, MsgDetails, MsgFlow
      , MsgInflow, MsgOutflow, MsgReport
      )
    )

import Material3 (md3widget, md3selectWidget, md3textareaWidget)

import Model
       ( CashFlowType (Outflow, Inflow)
       , ProjectId, floatToCenti, msgSuccess, msgError
       , ReportId
       , RuleId, Rule (Rule, ruleArticle, ruleDescr, ruleFlow)
       , EntityField (RuleId, RuleArticle, RuleReport)
       )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, whamlet
    , SomeMessage (SomeMessage), getMessages, addMessageI, redirect
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt, checkM, runFormPost)
import Yesod.Form.Fields
    ( doubleField, dayField, selectField, optionsPairs, intField, textField
    , textareaField
    )
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , FormResult (FormSuccess), Field
    )
import Yesod.Persist.Core (YesodPersist(runDB))
 

postRuleDeleR :: ReportId -> RuleId -> Handler Html
postRuleDeleR oid rid = do
    ((fr,_),_) <- runFormPost formRuleDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete rid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ RulesR oid
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ RuleR oid rid


postRuleR :: ReportId -> RuleId -> Handler Html
postRuleR oid rid = do
    
    project <- runDB $ selectOne $ do
        x <- from $ table @Rule
        where_ $ x ^. RuleId ==. val rid
        return x
        
    ((fr,fw),et) <- runFormPost $ formRule oid project
    case fr of
      FormSuccess r -> do
          runDB $ replace rid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ RuleR oid rid
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgRule
              idOverlay <- newIdent
              $(widgetFile "data/reports/rules/edit")


getRuleEditR :: ReportId -> RuleId -> Handler Html
getRuleEditR oid rid = do
    
    project <- runDB $ selectOne $ do
        x <- from $ table @Rule
        where_ $ x ^. RuleId ==. val rid
        return x
        
    (fw,et) <- generateFormPost $ formRule oid project

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRule
        idOverlay <- newIdent
        $(widgetFile "data/reports/rules/edit")


postRulesR :: ReportId -> Handler Html
postRulesR oid = do
    ((fr,fw),et) <- runFormPost $ formRule oid Nothing      
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ RulesR oid
      
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgRule
              idOverlay <- newIdent
              $(widgetFile "data/reports/rules/new")


getRuleNewR :: ReportId -> Handler Html
getRuleNewR oid = do
    (fw,et) <- generateFormPost $ formRule oid Nothing      

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRule
        idOverlay <- newIdent
        $(widgetFile "data/reports/rules/new") 
        

formRule :: ReportId -> Maybe (Entity Rule) -> Form Rule
formRule rid rule extra = do

    (articleR,articleV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgArticle
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (ruleArticle . entityVal <$> rule)

    let flows = optionsPairs [(MsgInflow,Outflow),(MsgOutflow,Inflow)]

    (flowR,flowV) <- mreq (selectField flows) FieldSettings
        { fsLabel = SomeMessage MsgFlow
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (ruleFlow . entityVal <$> rule)

    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (ruleDescr . entityVal <$> rule)

    let r = Rule rid <$> articleR <*> flowR <*> pure Nothing <*> descrR

    return (r,$(widgetFile "data/reports/rules/form"))   
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Rule
              where_ $ x ^. RuleArticle ==. val name
              return x
              
          return $ case x of
            Nothing -> Right name
            Just (Entity pid' _) -> case rule of
              Nothing -> Left MsgAlreadyExists
              Just (Entity pid'' _) | pid' == pid'' -> Right name
                                    | otherwise -> Left MsgAlreadyExists


getRuleR :: ReportId -> RuleId -> Handler Html
getRuleR oid rid = do

    rule <- runDB $ selectOne $ do
        x <- from $ table @Rule
        where_ $ x ^. RuleId ==. val rid
        return x

    (fw0,et0) <- generateFormPost formRuleDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRules
        idOverlay <- newIdent
        idDialogDelete <- newIdent        
        $(widgetFile "data/reports/rules/rule") 


formRuleDelete :: Form ()
formRuleDelete extra = return (pure (),[whamlet|^{extra}|])


getRulesR :: ReportId -> Handler Html
getRulesR oid = do

    rules <- runDB $ select $ do
        x <- from $ table @Rule
        where_ $ x ^. RuleReport ==. val oid
        orderBy [asc (x ^. RuleId)]
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRules
        idOverlay <- newIdent
        $(widgetFile "data/reports/rules/rules")
