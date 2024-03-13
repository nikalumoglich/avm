{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller.AwsUtils
    ( getPresignedURL
    , uploadFile
    , sendEmail
    ) where

import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import qualified Amazonka.SES as SES
import qualified Amazonka.SES.SendRawEmail as SES.SendRawEmail
import qualified Amazonka.SES.Types as SESTypes

import qualified System.IO as IO
import qualified Data.Time
import qualified Data.UUID.V4 as V4
import qualified Data.UUID as UUID
import qualified Network.Wai.Parse
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T


getPresignedURL :: S3.BucketName -> S3.ObjectKey -> IO String
getPresignedURL bucket key = do
    logger <- AWS.newLogger AWS.Debug IO.stdout
    discoveredEnv <- AWS.newEnv AWS.discover
    let env = discoveredEnv { AWS.logger = logger, AWS.region = AWS.SaoPaulo }

    ts <- Data.Time.getCurrentTime
    presignedUrl <- AWS.runResourceT $ AWS.presignURL env ts 60 (S3.newGetObject bucket key)
    return (BSU.toString presignedUrl)

uploadFile bucket (_, fileInfo) = do
    logger <- AWS.newLogger AWS.Debug IO.stdout
    discoveredEnv <- AWS.newEnv AWS.discover
    let env = discoveredEnv { AWS.logger = logger, AWS.region = AWS.SaoPaulo }

    key <- V4.nextRandom
    let s3Key = S3.ObjectKey (UUID.toText key)
    let fileContents = Network.Wai.Parse.fileContent fileInfo

    _ <- AWS.runResourceT $ AWS.send env (S3.newPutObject bucket s3Key (AWS.Hashed (AWS.toHashed fileContents)))

    return (UUID.toText key)



sendEmail destination title message = do
    logger <- AWS.newLogger AWS.Debug IO.stdout
    discoveredEnv <- AWS.newEnv AWS.discover
    let env = discoveredEnv { AWS.logger = logger, AWS.region = AWS.NorthVirginia }

    let rm = SES.newRawMessage ("From: tiozao@tiozao.co\nTo: nikal.umoglich@gmail.com\nSubject: " <> BSU.fromString title <> "\n\n" <> BSU.fromString message)

    let re = SES.SendRawEmail' {
        SES.SendRawEmail.destinations = Just [ T.pack destination ],
        SES.SendRawEmail.configurationSetName = Nothing,
        SES.SendRawEmail.fromArn = Nothing,
        SES.SendRawEmail.returnPathArn = Nothing,
        SES.SendRawEmail.source = Just "tiozao@tiozao.co",
        SES.SendRawEmail.sourceArn = Nothing,
        SES.SendRawEmail.tags = Nothing,
        SES.SendRawEmail.rawMessage = rm }

    AWS.runResourceT $ AWS.send env re