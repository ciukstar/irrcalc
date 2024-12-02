{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS
import Data.Time.Clock (getCurrentTime)

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
    )
    
import Settings (AppSettings)

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoEn :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoEn _appSettings = do

    _now <- liftIO getCurrentTime

    
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

    pass5 <- liftIO $ saltPass "jaturnbow"
    let user5 = User { userEmail = "jaturnbow@xmail.edu"
                     , userPassword = Just pass5
                     , userName = Just "Jill A. Turnbow"
                     , userSuper = False
                     , userAdmin = False
                     }
    uid5 <- insert user5

    liftIO (BS.readFile "demo/user_5.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid5
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass6 <- liftIO $ saltPass "cswatkins"
    let user6 = User { userEmail = "cswatkins@xmail.edu"
                     , userPassword = Just pass6
                     , userName = Just "Charles S. Watkins"
                     , userSuper = False
                     , userAdmin = False
                     }
    uid6 <- insert user6

    liftIO (BS.readFile "demo/user_6.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid6
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass7 <- liftIO $ saltPass "dlmeyer"
    let user7 = User { userEmail = "dlmeyer@xmail.edu"
                     , userPassword = Just pass7
                     , userName = Just "Donald L. Meyer"
                     , userSuper = False
                     , userAdmin = False
                     }
    uid7 <- insert user7

    liftIO (BS.readFile "demo/user_7.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid7
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass8 <- liftIO $ saltPass "jjbullock"
    let user8 = User { userEmail = "jjbullock@xmail.edu"
                     , userPassword = Just pass8
                     , userName = Just "Jean J. Bullock"
                     , userSuper = False
                     , userAdmin = False
                     }
    uid8 <- insert user8

    liftIO (BS.readFile "demo/user_8.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid8
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }
    
    return ()
