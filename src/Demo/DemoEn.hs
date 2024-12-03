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
    
    return ()
