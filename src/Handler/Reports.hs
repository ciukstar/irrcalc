{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Handler.Reports
  ( getReportsR
  , getReportFixedParamsR, postReportFixedRunR
  ) where

import Data.Fixed (Centi)

import Foundation
    ( Handler, Form, widgetTopbar
    , Route (HomeR, ReportFixedRunR)
    , AppMessage
      ( MsgReportParameters, MsgInflation, MsgGenerateReport, MsgPeriodicity
      , MsgConstructionArea, MsgArticle, MsgStart, MsgEnd, MsgEveryJanuary
      , MsgGeneralPlanDocuments, MsgDuration, MsgLandPlotPurposeChange
      , MsgRules, MsgSequence
      )
    )

import Material3 (md3widget, md3selectWidget)

import Model (ProjectId, floatToCenti)

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, whamlet
    , SomeMessage (SomeMessage)
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt)
import Yesod.Form.Fields (doubleField, dayField, selectField, optionsPairs, intField)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    )
import Data.Time.Calendar (Day)


postReportFixedRunR :: ProjectId -> Handler Html
postReportFixedRunR pid = undefined 


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


getReportsR :: Handler Html
getReportsR = undefined
