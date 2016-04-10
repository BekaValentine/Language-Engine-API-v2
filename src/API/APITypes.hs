{-# OPTIONS -Wall #-}

{-# LANGUAGE DataKinds #-}







-- | This module defines some common types used in the API.

module API.APITypes where

import Servant

import Data.Int (Int32)







type AppID = Int32

type TokenID = Int32

type CaptureID = Capture "id" Int32

type ConversationID = Int32

type FileID = Int32

type PackageID = Int32

type UserID = Int32