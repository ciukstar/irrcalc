{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Reports
  ( getReportFixedParamsR, postReportFixedRunR
  ) where

import Data.Fixed (Centi)

import Foundation
    ( Handler, Form, widgetTopbar
    , Route (HomeR, ReportFixedRunR)
    , AppMessage
      ( MsgReportParameters, MsgInflation, MsgGenerateReport
      , MsgConstructionArea, MsgArticle, MsgStart, MsgPeriodicity, MsgEnd
      )
    )

import Material3 (md3widget)

import Model (ProjectId, floatToCenti)

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, whamlet
    , SomeMessage (SomeMessage)
    )
import Yesod.Form.Functions (generateFormPost, mreq)
import Yesod.Form.Fields (doubleField)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    )


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


data ParamsFixed = ParamsFixed
    { paramsFixedInflation :: Centi
    , paramsFixedConstructionArea :: Centi
    }


formParamsFixed :: Form ParamsFixed
formParamsFixed extra = do
    
    (inflationR, inflationV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgInflation
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just 10)
    
    (constructionAreaR, constructionAreaV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgConstructionArea
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just 50)
        
    let r = ParamsFixed <$> (floatToCenti <$> inflationR)
            <*> (floatToCenti <$> constructionAreaR)
            
    let w = [whamlet|
                    ^{extra}
                    <table>
                      <thead>
                        <tr>
                          <th>_{MsgArticle}
                          <th>_{MsgStart}
                          <th>_{MsgPeriodicity}
                          <th>_{MsgEnd}
                      <tbody>
                        <tr>
                          <td>^{md3widget inflationV}
                          <td>
                          <td>
                          <td>
                        <tr>
                          <td>^{md3widget constructionAreaV}
                          <td>
                          <td>
                          <td>
                    |]
    return (r,w)
