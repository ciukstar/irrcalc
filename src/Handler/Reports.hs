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

import Data.List ((!?)) 
import Data.Fixed (Centi)
import Data.Text (Text)
import Data.Time.Calendar (Day)

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
      ( MsgReportParameters, MsgInflation, MsgGenerateReport, MsgPeriodicity
      , MsgConstructionArea, MsgArticle, MsgStart, MsgEnd, MsgEveryJanuary
      , MsgGeneralPlanDocuments, MsgDuration, MsgLandPlotPurposeChange
      , MsgRules, MsgSequence, MsgReports, MsgPleaseAddIfYouWish, MsgNoDataYet
      , MsgAlreadyExists, MsgName, MsgDescription, MsgDele, MsgCancel
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgReport, MsgSave, MsgRecordAdded
      , MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted, MsgReportsFixedName
      , MsgDetails
      )
    )

import Material3 (md3widget, md3selectWidget, md3textareaWidget)

import Model
       ( ProjectId, floatToCenti, msgSuccess, msgError
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
    ( doubleField, dayField, selectField, optionsPairs, intField, textField
    , textareaField
    )
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , FormResult (FormSuccess), Field
    )
import Yesod.Persist.Core (YesodPersist(runDB))

import System.Cron (yearly, CronSchedule)


type Inflation  = (Text, Double, CronSchedule)
inflation :: Inflation
inflation = ("Inflation",0.1,yearly)

data Sequence = After Int | Before Int | SameAs Int | Repeat Int

newtype Off = Off Int

newtype Length = Length Int

type Rules = [ ( Text          -- ^ Article
               , CashFlowType
               , Double        -- ^ Value
               , [Sequence]
               , [Off]           -- ^ Offset
               , [Length]
               )
             ]

rules :: Rules
rules = [ (" 1.Смена назначения ЗУ",  Outflow, 4166667, [After 0], [Off 0], [Length 12])
        
        -- , (" 2.Документы на ГенПлан", Outflow, 5000000, [After 0], [Off 14, Off 18], [Length 1])
        , (" 2.Документы на ГенПлан", Outflow, 5000000, [After 1], [Off 2, Off 6], [Length 1])
        
        , (" 3.Стройка", Outflow, 55902000, [After 2,After 3,Repeat 6], [Off 10], [Length 5,Length 12])
        , (" 4.Рабочая документация на очереди",Outflow, 1815000, [Before 3,Repeat 7], [Off 1],[Length 4])
        , (" 5.Дорога внутри парка",Outflow,1995290,[SameAs 3], [Off 0],[Length 5, Length 12])
        , (" 6.Лес билеты",Outflow,5552755,[After 0],[Off 1], [Length 50])
        , (" 7.Лес срубка",Outflow,807673,[After 0],[Off 1], [Length 50])
        , (" 8.ФОТ Аутсорс Команды",Outflow,1331000,[SameAs 3], [Off 0],[Length 5, Length 12])
        , (" 9.Маркетинг",Outflow,1331000,[SameAs 8], [Off 0],[Length 5, Length 12])
        , ("10.Коммуникации",Outflow,5926011,[SameAs 3], [Off 0],[Length 5, Length 12])
        , ("11.Непредвиденные расходы",Outflow,11650590,[SameAs 3], [Off 0],[Length 5, Length 12])
        , ("12.Продажа",Inflow,113135000,[After 3,Repeat 7],[Off 0],[Length 6])
        , ("13.Комиссия брокера",Outflow,3394050,[SameAs 12,Repeat 7],[Off 0],[Length 6])
        , ("14.Налог на доход",Outflow,76026720,[After 0], [Off 3],[Length 1])
        ]


range :: Int -> [Int]
range n = [1..n]


after :: Int -> Int
after j = let x = rules !? j
          in case x of
               Just (_,_,_,sequences,offs,lengths) -> n + m + k
                 where
                   n = length sequences
                   m = length offs
                   k = length lengths
               Nothing -> 0


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

type Constr = (Centi,Maybe Day, Int, Maybe Periodicity, Maybe Day) 

data ParamsFixed = ParamsFixed
    { paramsFixedInflation :: Constr
    , paramsFixedConstructionArea :: Constr
    , paramsFixedGeneralPlanDocs :: Constr
    , paramsFixedPurposeChange :: Constr
    }


formParamsFixed :: Form ParamsFixed
formParamsFixed extra = do
    
    let ps = optionsPairs [(MsgEveryJanuary, PeriodicityEveryJanuary)]
    
    (inflationR, inflationV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgInflation
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just 10)
    
    (inflationSartR, inflationStartV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgStart
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
    
    (inflationLengthR, inflationLengthV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgDuration
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just 1)
    
    (inflationPeriodicityR, inflationPeriodicityV) <- mopt (selectField ps) FieldSettings
        { fsLabel = SomeMessage MsgPeriodicity
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (pure $ Just PeriodicityEveryJanuary)
    
    (inflationEndR, inflationEndV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgEnd
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
    
    (constructionAreaR, constructionAreaV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgConstructionArea
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just 50)
    
    (gplanDocsR, gplanDocsV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgGeneralPlanDocuments
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just 5000000)
    
    (gplanDocsSartR, gplanDocsStartV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgStart
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
    
    (gplanDocsLengthR, gplanDocsLengthV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgDuration
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just 1)
    
    (gplanDocsPeriodicityR, gplanDocsPeriodicityV) <- mopt (selectField ps) FieldSettings
        { fsLabel = SomeMessage MsgPeriodicity
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
    
    (gplanDocsEndR, gplanDocsEndV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgEnd
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
    
    (purposeChangeR, purposeChangeV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgLandPlotPurposeChange
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just 4166667)
    
    (purposeChangeSartR, purposeChangeStartV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgStart
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
    
    (purposeChangeLengthR, purposeChangeLengthV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgDuration
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just 12)
    
    (purposeChangePeriodicityR, purposeChangePeriodicityV) <- mopt (selectField ps) FieldSettings
        { fsLabel = SomeMessage MsgPeriodicity
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
    
    (purposeChangeEndR, purposeChangeEndV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgEnd
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
        
    let r = ParamsFixed
            <$> ( (,,,,)
                  <$> (floatToCenti <$> inflationR)
                  <*> inflationSartR
                  <*> inflationLengthR
                  <*> inflationPeriodicityR
                  <*> inflationEndR
                )
            <*> ( (,,,,)
                  <$> (floatToCenti <$> constructionAreaR)
                  <*> pure Nothing
                  <*> pure 1
                  <*> pure Nothing
                  <*> pure Nothing
                )
            <*> ( (,,,,)
                  <$> (floatToCenti <$> gplanDocsR)
                  <*> gplanDocsSartR
                  <*> gplanDocsLengthR
                  <*> gplanDocsPeriodicityR
                  <*> gplanDocsEndR
                )
            <*> ( (,,,,)
                  <$> (floatToCenti <$> purposeChangeR)
                  <*> purposeChangeSartR
                  <*> purposeChangeLengthR
                  <*> purposeChangePeriodicityR
                  <*> purposeChangeEndR
                )
            
    let w = [whamlet|
                    ^{extra}
                    <table>
                      <thead>
                        <tr>
                          <th>_{MsgArticle}
                          <th>_{MsgStart}
                          <th>_{MsgDuration}
                          <th>_{MsgPeriodicity}
                          <th>_{MsgEnd}
                      <tbody>
                        $forall t <- []
                        <tr>
                          <td>^{md3widget inflationV}
                          <td>^{md3widget inflationStartV}
                          <td>^{md3widget inflationLengthV}
                          <td>^{md3selectWidget inflationPeriodicityV}
                          <td>^{md3widget inflationEndV}
                        <tr>
                          <td>^{md3widget constructionAreaV}
                          <td>
                          <td>
                          <td>
                          <td>
                        <tr>
                          <td>^{md3widget gplanDocsV}
                          <td>^{md3widget gplanDocsStartV}
                          <td>^{md3widget gplanDocsLengthV}
                          <td>^{md3selectWidget gplanDocsPeriodicityV}
                          <td>^{md3widget gplanDocsEndV}
                        <tr>
                          <td>^{md3widget purposeChangeV}
                          <td>^{md3widget purposeChangeStartV}
                          <td>^{md3widget purposeChangeLengthV}
                          <td>^{md3selectWidget purposeChangePeriodicityV}
                          <td>^{md3widget purposeChangeEndV}
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
