{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Note where

import Data.Text (Text)

import Database.Persist.TH (
    mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
 )

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Note json
    title Text
    text Text
    author Text
    date Int
    deriving Show
|]
