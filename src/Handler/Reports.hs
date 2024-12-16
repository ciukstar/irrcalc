{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Reports
  ( getReportFixedParamsR, postReportFixedRunR
  , getReportsR, postReportsR
  , getReportR, postReportR
  , getReportNewR, getReportEditR, postReportDeleR
  ) where

import Control.Monad (forM)

import Data.List ((!?), unsnoc, zipWith4, zip4) 
import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val
    , (^.), (==.)
    )
import Database.Persist
    ( Entity (Entity), entityVal, delete, insert_, replace
    ) 

import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (HomeR, ReportFixedRunR, DataR, ReportFixedParamsR)
    , DataR
      ( ReportR, ReportNewR, ReportDeleR, ReportEditR, ReportsR, RulesR
      )
    , AppMessage
      ( MsgReportParameters, MsgGenerateReport, MsgArticle, MsgNoDataYet
      , MsgRules, MsgSequence, MsgReports, MsgPleaseAddIfYouWish
      , MsgAlreadyExists, MsgName, MsgDescription, MsgDele, MsgCancel
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgReport, MsgSave, MsgRule
      , MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted, MsgBefore
      , MsgDetails, MsgNumberSign, MsgInflow, MsgOutflow, MsgType, MsgAfter
      , MsgRelativeSequence, MsgReportsFixedName, MsgRecordAdded, MsgOffset
      , MsgLength
      )
    )

import Material3 (md3widget, md3selectWidget, md3textareaWidget)

import Model
       ( ProjectId, msgSuccess, msgError
       , ReportId, Report (Report, reportName, reportDescr)
       , EntityField (ReportId, ReportName), CashFlowType (Outflow, Inflow)
       )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, whamlet
    , SomeMessage (SomeMessage), getMessages, addMessageI, redirect
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt, checkM, runFormPost)
import Yesod.Form.Fields
    ( doubleField, selectField, optionsPairs, intField, textField
    , textareaField
    )
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , FormResult (FormSuccess), Field
    )
import Yesod.Persist.Core (YesodPersist(runDB))


data Sequence = After  Int Int Int
              | Before Int Int Int
    deriving (Show, Read)

type Rules = [ ( Int           -- Index
               , Text          -- Article
               , CashFlowType
               , Double        -- Value
               , [Sequence]
               )
             ]

rules :: Rules
rules = [ (1,"Смена назначения ЗУ",  Outflow, 4.0, [After 0 0 2])
        , (2,"Документы на ГенПлан", Outflow, 5.0, [After 1 2 1, After 2 3 1]) -- 
        , (3,"Стройка 1", Outflow, 6.0, [After 2 3 2])
        , (4,"Рабочая документация на очереди",Outflow, 9.0, [Before 3 1 4])
        , (5,"Стройка 2", Outflow, 7.0, [After 4 0 3])
        , (6,"Рабочая документация на очереди",Outflow, 10.0, [Before 5 1 4])
        , (7,"Стройка 3", Outflow, 8.0, [After 6 0 7]) 
        , (8,"Рабочая документация на очереди",Outflow, 11.0, [Before 7 1 4])
        ]



offset :: Int -> [Sequence] -> Sequence -> Int
offset i xs (After n _ _) = case rules !? (n - 1) of
    Nothing -> 0
      
    Just (j,_,_,_,_) | j == i -> case unsnoc xs of
      Nothing -> 0
      Just (xs'',e@(After _ o'' l'')) -> o'' + l'' + offset j xs'' e
      Just (xs'',e@(Before _ o'' l'')) -> o'' + l'' + offset j xs'' e
      
    Just (j,_,_,_,xs') -> case unsnoc xs' of
      Nothing -> 0
      Just (xs'',e@(After _ o'' l'')) -> o'' + l'' + offset j xs'' e
      Just (xs'',e@(Before _ o'' _)) -> (-o'') + offset j xs'' e
      
offset i xs (Before n _ _) = case rules !? (n - 1) of
    Nothing -> 0
      
    Just (j,_,_,_,_) | j == i -> case unsnoc xs of
      Nothing -> 0
      Just (xs'',e@(After _ o'' _l'')) -> o'' + offset j xs'' e
      Just (xs'',e@(Before _ o'' l'')) -> o'' + l'' + offset j xs'' e
      
    Just (j,_,_,_,xs') -> case unsnoc xs' of
      Nothing -> 0
      Just (xs'',e@(After _ o'' _l'')) -> o'' + offset j xs'' e
      Just (xs'',e@(Before _ o'' l'')) -> o'' + l'' + offset j xs'' e


range :: Int -> [Int]
range n = [1..n]


postReportFixedRunR :: ProjectId -> Handler Html
postReportFixedRunR pid = do
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgReportsFixedName
        idOverlay <- newIdent
        $(widgetFile "reports/fixed/fixed")


getReportFixedParamsR :: ProjectId -> Handler Html
getReportFixedParamsR pid = do
    (fw,et) <- generateFormPost formParamsFixed
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgReportParameters
        idOverlay <- newIdent
        $(widgetFile "reports/fixed/params") 


data Periodicity = PeriodicityEveryJanuary
    deriving (Show, Read, Eq)


formParamsFixed :: Form [((Double,CashFlowType),[(Text,Int,Int,Int)])]
formParamsFixed extra = do

    amounts <- forM rules $ \(_,article,_,amount,_) -> mreq doubleField FieldSettings
        { fsLabel = SomeMessage article
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just amount)

    let flowTypes = optionsPairs [(MsgOutflow, Outflow), (MsgInflow,Inflow)]

    flows <- forM rules $ \(_,_,typ,_,_) -> mreq (selectField flowTypes) FieldSettings
        { fsLabel = SomeMessage MsgType
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just typ)

    let sequenceOptions = optionsPairs [(MsgAfter, "After"),(MsgBefore, "Before")]

    sequenses <- forM rules $ \(_,_,_,_,ss) -> forM ss $ \s -> mreq (selectField sequenceOptions) FieldSettings
        { fsLabel = SomeMessage MsgSequence
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just $ case s of After {} -> "After"; Before {} -> "Before")

    relatives <- forM rules $ \(_,_,_,_,ss) -> forM ss $ \s -> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgRule
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just $ case s of After n _ _ -> n; Before n _ _ -> n)

    offsets <- forM rules $ \(_,_,_,_,ss) -> forM ss $ \s -> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgOffset
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just $ case s of After _ o _ -> o; Before _ o _ -> o)

    lengths <- forM rules $ \(_,_,_,_,ss) -> forM ss $ \s -> mreq intField FieldSettings
        { fsLabel = SomeMessage MsgLength
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just $ case s of After _ _ l -> l; Before _ _ l -> l)

    let ss = traverse (traverse fst) sequenses
    let rs = traverse (traverse fst) relatives
    let os = traverse (traverse fst) offsets
    let ls = traverse (traverse fst) lengths

    let params = zipWith4 zip4 <$> ss <*> rs <*> os <*> ls
    
    let r = zip <$> ( zip <$> traverse fst amounts <*> traverse fst flows
                    )
            <*> params

    let idxs = zip [1 :: Int ..]
            ( zip (snd <$> amounts)
              ( zip (snd <$> flows)
                (zipWith4 zip4
                  ((snd <$>) <$> sequenses)
                  ((snd <$>) <$> relatives)
                  ((snd <$>) <$> offsets)
                  ((snd <$>) <$> lengths))
              )
            )
            
    let w = [whamlet|
                    ^{extra}
                    $forall (i,(a,(f,ss))) <- idxs
                      <details>
                        <summary>_{MsgRule} _{MsgNumberSign}#{i}
                        <table>
                          <tbody>
                            <tr>
                              <td>^{md3widget a}
                              <td>^{md3selectWidget f}
                        <fieldset>
                          <legend>_{MsgRelativeSequence}
                          $forall (s,p1,p2,p3) <- ss
                            <table>
                              <tbody>
                                <tr>
                                  <td>^{md3selectWidget s}
                                  <td>^{md3widget p1}
                                  <td>^{md3widget p2}
                                  <td>^{md3widget p3}
                          
                    |]
    return (r,w)
 

postReportDeleR :: ReportId -> Handler Html
postReportDeleR pid = do
    ((fr,_),_) <- runFormPost formReportDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete pid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR ReportsR
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ ReportR pid


postReportR :: ReportId -> Handler Html
postReportR pid = do
    
    project <- runDB $ selectOne $ do
        x <- from $ table @Report
        where_ $ x ^. ReportId ==. val pid
        return x
        
    ((fr,fw),et) <- runFormPost $ formReport project
    case fr of
      FormSuccess r -> do
          runDB $ replace pid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ ReportR pid
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgReport
              idOverlay <- newIdent
              $(widgetFile "data/reports/edit")


getReportEditR :: ReportId -> Handler Html
getReportEditR pid = do
    
    project <- runDB $ selectOne $ do
        x <- from $ table @Report
        where_ $ x ^. ReportId ==. val pid
        return x
        
    (fw,et) <- generateFormPost $ formReport project

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgReport
        idOverlay <- newIdent
        $(widgetFile "data/reports/edit")


postReportsR :: Handler Html
postReportsR = do
    ((fr,fw),et) <- runFormPost $ formReport Nothing      
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR ReportsR
      
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgReport
              idOverlay <- newIdent
              $(widgetFile "data/reports/new")


getReportNewR :: Handler Html
getReportNewR = do
    (fw,et) <- generateFormPost $ formReport Nothing      

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgReport
        idOverlay <- newIdent
        $(widgetFile "data/reports/new") 
        

formReport :: Maybe (Entity Report) -> Form Report
formReport project extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (reportName . entityVal <$> project)

    (descrR,descrV) <- mopt textareaField  FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (reportDescr . entityVal <$> project)

    let r = Report <$> nameR <*> descrR

    return (r,$(widgetFile "data/reports/form"))   
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Report
              where_ $ x ^. ReportName ==. val name
              return x
              
          return $ case x of
            Nothing -> Right name
            Just (Entity pid' _) -> case project of
              Nothing -> Left MsgAlreadyExists
              Just (Entity pid'' _) | pid' == pid'' -> Right name
                                    | otherwise -> Left MsgAlreadyExists


getReportR :: ReportId -> Handler Html
getReportR pid = do

    project <- runDB $ selectOne $ do
        x <- from $ table @Report
        where_ $ x ^. ReportId ==. val pid
        return x

    (fw0,et0) <- generateFormPost formReportDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgReports
        idOverlay <- newIdent
        idDialogDelete <- newIdent        
        $(widgetFile "data/reports/report") 


formReportDelete :: Form ()
formReportDelete extra = return (pure (),[whamlet|^{extra}|])


getReportsR :: Handler Html
getReportsR = do

    reports <- runDB $ select $ do
        x <- from $ table @Report
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgReports
        idOverlay <- newIdent
        $(widgetFile "data/reports/reports")
