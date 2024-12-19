{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS
import Data.Time.Calendar
    ( addGregorianMonthsClip, pattern YearMonthDay
    )
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
    , Period (Period, periodName, periodStart, periodEnd)
    , Report (Report, reportName, reportDescr)
    , Rule (Rule, ruleIndex, ruleArticle, ruleFlow, ruleDescr, ruleReport, ruleAmount)
    , CashFlowType (Outflow)
    , Sequence (Sequence, sequenceRule, sequenceName)
    , RuleType (RuleTypeAfter, RuleTypeBefore)
    , Param (Param, paramName, paramValue, paramSequence)
    , ParamName (ParamRuleIndex, ParamOffset, ParamDuration)
    )
    
import Settings (AppSettings)

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoEn :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoEn _appSettings = do

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

    pass1 <- liftIO $ saltPass "marylopez"
    let user1 = User { userEmail = "marylopez@xmail.edu"
                     , userPassword = Just pass1
                     , userName = Just "Mary Lopez"
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

    pass2 <- liftIO $ saltPass "jjohnson"
    let user2 = User { userEmail = "jjohnson@xmail.edu"
                     , userPassword = Just pass2
                     , userName = Just "John Johnson"
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

    pass3 <- liftIO $ saltPass "jmaulsby"
    let user3 = User { userEmail = "jmaulsby@xmail.edu"
                     , userPassword = Just pass3
                     , userName = Just "Julian Maulsby"
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

    pass4 <- liftIO $ saltPass "vschoen"
    let user4 = User { userEmail = "vschoen@xmail.edu"
                     , userPassword = Just pass4
                     , userName = Just "Valentina Schoen"
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

    let project1 = Project { projectName = "Project 1"
                           , projectStart = YearMonthDay 2024 10 1
                           , projectEnd = YearMonthDay 2035 3 1
                           , projectDescr = Just "The First project"
                           }
    pid1 <- insert project1

    let project2 = Project { projectName = "Project 2"
                           , projectStart = addGregorianMonthsClip (-10) today
                           , projectEnd = addGregorianMonthsClip 9 today
                           , projectDescr = Just "The Second project"
                           }
    pid2 <- insert project2


    let period1 = Period { periodName = "October 2024"
                         , periodStart = YearMonthDay 2024 10 1
                         , periodEnd = YearMonthDay 2024 10 31
                         }
    iid1 <- insert period1

    let report1 = Report { reportName = "Calculation (Change of Land Plot Fixed)"
                         , reportDescr = Nothing
                         }
    rpt1 <- insert report1

    let rule11 = Rule { ruleReport = rpt1
                      , ruleIndex = 1
                      , ruleArticle = "Object purchase"
                      , ruleFlow = Outflow
                      , ruleAmount = 75000000.00
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

    let rule12 = Rule { ruleReport = rpt1
                      , ruleIndex = 2
                      , ruleArticle = "Purpose change of the land plot"
                      , ruleFlow = Outflow
                      , ruleAmount = 807673.00
                      , ruleDescr = Nothing
                      }

    rid12 <- insert rule12

    let sequence112 = Sequence { sequenceRule = rid12
                               , sequenceName = RuleTypeAfter
                               }

    sid112 <- insert sequence112

    insert_ Param { paramSequence = sid112
                  , paramName = ParamRuleIndex
                  , paramValue = 0
                  }

    insert_ Param { paramSequence = sid112
                  , paramName = ParamOffset
                  , paramValue = 0
                  }

    insert_ Param { paramSequence = sid112
                  , paramName = ParamDuration
                  , paramValue = 12
                  }

    let rule13 = Rule { ruleReport = rpt1
                      , ruleIndex = 3
                      , ruleArticle = "Documents for the General Plan"
                      , ruleFlow = Outflow
                      , ruleAmount = 5500000.00
                      , ruleDescr = Nothing
                      }

    rid13 <- insert rule13

    let sequence131 = Sequence { sequenceRule = rid13
                               , sequenceName = RuleTypeAfter
                               }

    sid131 <- insert sequence131

    insert_ Param { paramSequence = sid131
                  , paramName = ParamRuleIndex
                  , paramValue = 2
                  }

    insert_ Param { paramSequence = sid131
                  , paramName = ParamOffset
                  , paramValue = 1
                  }

    insert_ Param { paramSequence = sid131
                  , paramName = ParamDuration
                  , paramValue = 1
                  }

    let sequence132 = Sequence { sequenceRule = rid13
                               , sequenceName = RuleTypeAfter
                               }

    sid132 <- insert sequence132

    insert_ Param { paramSequence = sid132
                  , paramName = ParamRuleIndex
                  , paramValue = 3
                  }

    insert_ Param { paramSequence = sid132
                  , paramName = ParamOffset
                  , paramValue = 3
                  }

    insert_ Param { paramSequence = sid132
                  , paramName = ParamDuration
                  , paramValue = 1
                  }

    let rule14 = Rule { ruleReport = rpt1
                      , ruleIndex = 4
                      , ruleArticle = "Working documentation for the stage"
                      , ruleFlow = Outflow
                      , ruleAmount = 1815000.00
                      , ruleDescr = Nothing
                      }

    rid14 <- insert rule14

    let sequence141 = Sequence { sequenceRule = rid14
                               , sequenceName = RuleTypeBefore
                               }

    sid141 <- insert sequence141

    insert_ Param { paramSequence = sid141
                  , paramName = ParamRuleIndex
                  , paramValue = 5
                  }

    insert_ Param { paramSequence = sid141
                  , paramName = ParamOffset
                  , paramValue = 1
                  }

    insert_ Param { paramSequence = sid141
                  , paramName = ParamDuration
                  , paramValue = 4
                  }

    let rule15 = Rule { ruleReport = rpt1
                      , ruleIndex = 5
                      , ruleArticle = "Construction of the 1st stage, 6000 m2"
                      , ruleFlow = Outflow
                      , ruleAmount = 55902000.00
                      , ruleDescr = Nothing
                      }

    rid15 <- insert rule15

    let sequence151 = Sequence { sequenceRule = rid15
                               , sequenceName = RuleTypeAfter
                               }

    sid151 <- insert sequence151

    insert_ Param { paramSequence = sid151
                  , paramName = ParamRuleIndex
                  , paramValue = 3
                  }

    insert_ Param { paramSequence = sid151
                  , paramName = ParamOffset
                  , paramValue = 9
                  }

    insert_ Param { paramSequence = sid151
                  , paramName = ParamDuration
                  , paramValue = 5
                  }

    let rule16 = Rule { ruleReport = rpt1
                      , ruleIndex = 6
                      , ruleArticle = "Working documentation for the stage"
                      , ruleFlow = Outflow
                      , ruleAmount = 1815000.00
                      , ruleDescr = Nothing
                      }

    rid16 <- insert rule16

    let sequence161 = Sequence { sequenceRule = rid16
                               , sequenceName = RuleTypeBefore
                               }

    sid161 <- insert sequence161

    insert_ Param { paramSequence = sid161
                  , paramName = ParamRuleIndex
                  , paramValue = 7
                  }

    insert_ Param { paramSequence = sid161
                  , paramName = ParamOffset
                  , paramValue = 1
                  }

    insert_ Param { paramSequence = sid161
                  , paramName = ParamDuration
                  , paramValue = 4
                  }

    let rule17 = Rule { ruleReport = rpt1
                      , ruleIndex = 7
                      , ruleArticle = "Construction of the 2nd stage, 10000 m2"
                      , ruleFlow = Outflow
                      , ruleAmount = 38820833.00
                      , ruleDescr = Nothing
                      }

    rid17 <- insert rule17

    let sequence171 = Sequence { sequenceRule = rid17
                               , sequenceName = RuleTypeAfter
                               }

    sid171 <- insert sequence171

    insert_ Param { paramSequence = sid171
                  , paramName = ParamRuleIndex
                  , paramValue = 5
                  }

    insert_ Param { paramSequence = sid171
                  , paramName = ParamOffset
                  , paramValue = 0
                  }

    insert_ Param { paramSequence = sid171
                  , paramName = ParamDuration
                  , paramValue = 12
                  }

    let rule18 = Rule { ruleReport = rpt1
                      , ruleIndex = 8
                      , ruleArticle = "Road inside the park"
                      , ruleFlow = Outflow
                      , ruleAmount = 1995290.00
                      , ruleDescr = Nothing
                      }

    rid18 <- insert rule18

    let sequence181 = Sequence { sequenceRule = rid18
                               , sequenceName = RuleTypeAfter
                               }

    sid181 <- insert sequence181

    insert_ Param { paramSequence = sid181
                  , paramName = ParamRuleIndex
                  , paramValue = 3
                  }

    insert_ Param { paramSequence = sid181
                  , paramName = ParamOffset
                  , paramValue = 9
                  }

    insert_ Param { paramSequence = sid181
                  , paramName = ParamDuration
                  , paramValue = 17
                  }
    
    return ()
