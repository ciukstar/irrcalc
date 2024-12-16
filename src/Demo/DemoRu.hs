{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoRu (fillDemoRu) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS
import Data.Time.Calendar (addGregorianMonthsClip, pattern YearMonthDay)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( User
      ( User, userEmail, userPassword, userSuper, userAdmin, userName
      )
    , UserPhoto
      ( UserPhoto, userPhotoUser, userPhotoMime, userPhotoAttribution
      , userPhotoPhoto
      )
    , Project (Project, projectName, projectStart, projectEnd, projectDescr)
    , Report (Report, reportName, reportDescr)
    , Rule (Rule, ruleReport, ruleIndex, ruleArticle, ruleFlow, ruleDescr)
    , Sequence (Sequence, sequenceRule, sequenceName)
    , Param (Param, paramSequence, paramName, paramValue)
    , CashFlowType (Outflow), RuleType (RuleTypeAfter)
    , ParamName (ParamRuleIndex, ParamOffset, ParamDuration)
    )
    
import Settings (AppSettings)
    
import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoRu :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoRu _appSettings = do

    now <- liftIO getCurrentTime

    let today = utctDay now

    let minute = 60 :: Int
    let hour = 60 * minute
    let _day = 24 * hour

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]
    

    pass1 <- liftIO $ saltPass "bulanovalm"
    let user1 = User { userEmail = "bulanovalm@mail.ru"
                     , userPassword = Just pass1
                     , userName = Just "Буланова Любовь Михайловна"
                     , userSuper = False
                     , userAdmin = True
                     }
    uid1 <- insert user1

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "petrovia"
    let user2 = User { userEmail = "petrovia@mail.ru"
                     , userPassword = Just pass2
                     , userName = Just "Петров Иван Александрович"
                     , userSuper = False
                     , userAdmin = False
                     }
    uid2 <- insert user2

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "smirnovav"
    let user3 = User { userEmail = "smirnovav@mail.ru"
                     , userPassword = Just pass3
                     , userName = Just "Смирнов Андрей Васильевич"
                     , userSuper = False
                     , userAdmin = False
                     }
    uid3 <- insert user3

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "sergeevaav"
    let user4 = User { userEmail = "sergeevaav@mail.ru"
                     , userPassword = Just pass4
                     , userName = Just "Сергеева Александра Владимировна"
                     , userSuper = False
                     , userAdmin = False
                     }
    uid4 <- insert user4

    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid4
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    let project1 = Project { projectName = "Проект 1"
                           , projectStart = YearMonthDay 2024 10 1
                           , projectEnd = YearMonthDay 2035 3 1
                           , projectDescr = Just "Первый проект"
                           }
    pid1 <- insert project1

    let project2 = Project { projectName = "Проект 2"
                           , projectStart = addGregorianMonthsClip (-10) today
                           , projectEnd = addGregorianMonthsClip 9 today
                           , projectDescr = Just "Второй проект"
                           }
    pid2 <- insert project2

    let report1 = Report { reportName = "Расчет (Смена ЗУ фикс)"
                         , reportDescr = Nothing
                         }
    rpt1 <- insert report1

    let rule11 = Rule { ruleReport = rpt1
                      , ruleIndex = 1
                      , ruleArticle = "Покупка объекта"
                      , ruleFlow = Outflow
                      , ruleDescr = Nothing
                      }

    rid11 <- insert rule11

    let sequence111 = Sequence { sequenceRule = rid11
                               , sequenceName = RuleTypeAfter
                               }

    sid111 <- insert sequence111

    insert_ Param { paramSequence = sid111
                  , paramName = ParamRuleIndex
                  , paramValue = 0
                  }

    insert_ Param { paramSequence = sid111
                  , paramName = ParamOffset
                  , paramValue = 0
                  }

    insert_ Param { paramSequence = sid111
                  , paramName = ParamDuration
                  , paramValue = 1
                  }

    return ()
