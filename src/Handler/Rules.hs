{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Rules
    ( getRulesR, postRulesR
    , getRuleR, postRuleR
    , getRuleNewR, getRuleEditR, postRuleDeleR
    , getRuleSequencesR, postRuleSequencesR
    , getRuleSequenceR
    , getRuleSequenceNewR
    ) where

import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val
    , (^.), (==.)
    , orderBy, asc, Value (unValue)
    )
import Database.Persist
    ( Entity (Entity), entityVal, insert, delete, insert_, replace, insertMany_
    ) 

import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR)
    , DataR
      ( RuleR, RuleNewR, RuleDeleR, RuleEditR, RulesR, RulesR
      , ReportR, ReportsR, RuleSequencesR, RuleSequenceR, RuleSequenceNewR
      )
    , AppMessage
      ( MsgReportParameters, MsgInflation, MsgGenerateReport, MsgPeriodicity
      , MsgArticle, MsgStart, MsgEnd, MsgEveryJanuary, MsgGeneralPlanDocuments
      , MsgDuration, MsgLandPlotPurposeChange
      , MsgRules, MsgSequence, MsgRules, MsgPleaseAddIfYouWish, MsgNoDataYet
      , MsgAlreadyExists, MsgName, MsgDescription, MsgDele, MsgCancel
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgRule, MsgSave, MsgRecordAdded
      , MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted, MsgDetails, MsgFlow
      , MsgInflow, MsgOutflow, MsgReport, MsgIndex, MsgSequences, MsgAfter
      , MsgBefore, MsgType, MsgOffset, MsgLength
      )
    )

import Material3 (md3widget, md3selectWidget, md3textareaWidget)

import Model
       ( msgSuccess, msgError
       , CashFlowType (Outflow, Inflow)
       , ReportId
       , RuleId, Rule (Rule, ruleArticle, ruleDescr, ruleFlow, ruleIndex)
       , SequenceId, Sequence (Sequence, sequenceRelation, sequenceName)
       , RuleType (RuleTypeAfter, RuleTypeBefore), ParamName (ParamOffset, ParamLength)
       , Param (Param)
       , EntityField
         ( RuleId, RuleArticle, RuleReport, RuleIndex, SequenceRule, SequenceId
         , ParamName, ParamVal
         )
       )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, whamlet
    , SomeMessage (SomeMessage), MonadHandler (liftHandler), getMessages
    , addMessageI, redirect
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt, checkM, runFormPost)
import Yesod.Form.Fields
    ( selectField, optionsPairs, intField, textField, textareaField
    )
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , Field, FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


getRuleSequenceR :: ReportId -> RuleId -> SequenceId -> Handler Html
getRuleSequenceR oid rid sid = undefined 


postRuleSequencesR :: ReportId -> RuleId -> Handler Html
postRuleSequencesR oid rid = do
    ((fr,fw),et) <- runFormPost $ formSequence rid Nothing

    case fr of
      FormSuccess (r,params) -> do
          sid <- runDB $ insert r
          runDB $ insertMany_ (uncurry (Param sid) <$> params)
          redirect $ DataR $ RuleSequencesR oid rid
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSequences
              idOverlay <- newIdent
              $(widgetFile "data/reports/rules/sequences/new")


getRuleSequenceNewR :: ReportId -> RuleId -> Handler Html
getRuleSequenceNewR oid rid = do
    (fw,et) <- generateFormPost $ formSequence rid Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSequences
        idOverlay <- newIdent
        $(widgetFile "data/reports/rules/sequences/new")


formSequence :: RuleId -> Maybe (Entity Sequence) -> Form (Sequence,[(ParamName, Int)])
formSequence rid sequence extra = do

    rules <- liftHandler $ runDB $ select $ do
        x <- from $ table @Rule
        orderBy [asc (x ^. RuleIndex)]
        return x

    let typeOpts = optionsPairs [(MsgAfter, RuleTypeAfter), (MsgBefore, RuleTypeBefore)]

    (typeR,typeV) <- mreq (selectField typeOpts) FieldSettings
        { fsLabel = SomeMessage MsgType
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sequenceName . entityVal <$> sequence)

    let relationOpts = optionsPairs $ (\(Entity oid (Rule _ _ name _ _)) -> (name,oid)) <$> rules

    (relationR,relationV) <- mopt (selectField relationOpts) FieldSettings
        { fsLabel = SomeMessage MsgRule
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sequenceRelation . entityVal <$> sequence)

    offset <- liftHandler $ (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Param
        where_ $ x ^. ParamName ==. val ParamOffset
        return $ x ^. ParamVal )

    (offsetR,offsetV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgOffset
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } offset

    paramDur <- liftHandler $ (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Param
        where_ $ x ^. ParamName ==. val ParamLength
        return $ x ^. ParamVal )

    (durationR,durationV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgDuration
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } paramDur
    
    let r = (,) <$> (Sequence rid <$> relationR <*> typeR)
            <*> sequenceA [ (,) ParamOffset <$> offsetR
                          , (,) ParamOffset <$> durationR
                          ]
    let w = [whamlet|
                    ^{extra}
                    ^{md3selectWidget typeV}
                    ^{md3selectWidget relationV}
                    ^{md3widget offsetV}
                    ^{md3widget durationV}
                    |]
    return (r,w)    


getRuleSequencesR :: ReportId -> RuleId -> Handler Html
getRuleSequencesR oid rid = do

    sequences <- runDB $ select $ do
        x <- from $ table @Sequence
        where_ $ x ^. SequenceRule ==. val rid
        orderBy [asc (x ^. SequenceId)]
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSequences
        idOverlay <- newIdent
        $(widgetFile "data/reports/rules/sequences/sequences")
 

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

    (idxR,idxV) <- mreq uniqueIndexField FieldSettings
        { fsLabel = SomeMessage MsgIndex
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (ruleIndex . entityVal <$> rule)

    (articleR,articleV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgArticle
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (ruleArticle . entityVal <$> rule)

    let flows = optionsPairs [(MsgOutflow,Outflow),(MsgInflow,Inflow)]

    (flowR,flowV) <- mreq (selectField flows) FieldSettings
        { fsLabel = SomeMessage MsgFlow
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (ruleFlow . entityVal <$> rule)

    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (ruleDescr . entityVal <$> rule)

    let r = Rule rid <$> idxR <*> articleR <*> flowR <*> descrR

    return (r,$(widgetFile "data/reports/rules/form"))   
  where
      uniqueIndexField :: Field Handler Int
      uniqueIndexField = checkM uniqueIndex intField

      uniqueIndex :: Int -> Handler (Either AppMessage Int)
      uniqueIndex idx = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Rule
              where_ $ x ^. RuleReport ==. val rid
              where_ $ x ^. RuleIndex ==. val idx
              return x
              
          return $ case x of
            Nothing -> Right idx
            Just (Entity pid' _) -> case rule of
              Nothing -> Left MsgAlreadyExists
              Just (Entity pid'' _) | pid' == pid'' -> Right idx
                                    | otherwise -> Left MsgAlreadyExists
                                    
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
