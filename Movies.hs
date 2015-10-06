{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Movies (mkMoviesDB) where

import           Control.Monad
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] 
          [persistLowerCase|
           Movies
             name String
             year Int
             rating Int
             deriving Show
           |]

movies = [ Movies "Rise of the Planet of the Apes" 2011 77
         , Movies "Dawn of the Planet of the Apes" 2014 91
         , Movies "Alien" 1979 97
         , Movies "Aliens" 1986 98
         , Movies "Mad Max" 1979 95
         , Movies "Mad Max 2: The Road Warrior" 1981 100 ]

mkMoviesDB :: IO ()
mkMoviesDB = runSqlite "/tmp/movies.db" $ do
    runMigration migrateAll

    mapM insert movies
    liftIO $ print 10
