{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DataKinds                  #-}

module Note
  ( Note(..)
  , migrateAll
  ) where

import           Database.Persist.TH            ( mkMigrate
                                                , mkPersist
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                )

import           Data.Text                      ( Text )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Note json
  text Text
  author Text
  date Text
  deriving Show
|]

