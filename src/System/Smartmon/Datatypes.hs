{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

{- | Generating this file was aided by the package `json-autotype`. -}

module System.Smartmon.Datatypes
where

import           Control.Monad                   (join, mzero)
import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  Value (..), object, pairs,
                                                  (.:), (.:?), (.=))
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson.Types                (Object, Parser)
import           Data.Text                       (Text)
import qualified GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
(.:??) :: FromJSON a => Object -> Text -> Parser (Maybe a)
o .:?? val = fmap join (o .:? val)


data LocalTime = LocalTime {
    localTimeTimeT :: Double,
    localTimeAsctime :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON LocalTime where
  parseJSON (Object v) = LocalTime <$> v .:   "time_t" <*> v .:   "asctime"
  parseJSON _          = mzero


instance ToJSON LocalTime where
  toJSON     (LocalTime {..}) = object ["time_t" .= localTimeTimeT, "asctime" .= localTimeAsctime]
  toEncoding (LocalTime {..}) = pairs  ("time_t" .= localTimeTimeT<>"asctime" .= localTimeAsctime)


data DeviceType = DeviceType {
    deviceTypeName :: Text,
    deviceTypeScsiValue :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON DeviceType where
  parseJSON (Object v) = DeviceType <$> v .:   "name" <*> v .:   "scsi_value"
  parseJSON _          = mzero


instance ToJSON DeviceType where
  toJSON     (DeviceType {..}) = object ["name" .= deviceTypeName, "scsi_value" .= deviceTypeScsiValue]
  toEncoding (DeviceType {..}) = pairs  ("name" .= deviceTypeName<>"scsi_value" .= deviceTypeScsiValue)


data SataVersion = SataVersion {
    sataVersionString :: Text,
    sataVersionValue :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON SataVersion where
  parseJSON (Object v) = SataVersion <$> v .:   "string" <*> v .:   "value"
  parseJSON _          = mzero


instance ToJSON SataVersion where
  toJSON     (SataVersion {..}) = object ["string" .= sataVersionString, "value" .= sataVersionValue]
  toEncoding (SataVersion {..}) = pairs  ("string" .= sataVersionString<>"value" .= sataVersionValue)


data SmartStatus = SmartStatus {
    smartStatusPassed :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON SmartStatus where
  parseJSON (Object v) = SmartStatus <$> v .:   "passed"
  parseJSON _          = mzero


instance ToJSON SmartStatus where
  toJSON     (SmartStatus {..}) = object ["passed" .= smartStatusPassed]
  toEncoding (SmartStatus {..}) = pairs  ("passed" .= smartStatusPassed)


data Current = Current {
    currentUnitsPerSecond :: Double,
    currentBitsPerUnit :: Double,
    currentString :: Text,
    currentSataValue :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Current where
  parseJSON (Object v) = Current <$> v .:   "units_per_second" <*> v .:   "bits_per_unit" <*> v .:   "string" <*> v .:   "sata_value"
  parseJSON _          = mzero


instance ToJSON Current where
  toJSON     (Current {..}) = object ["units_per_second" .= currentUnitsPerSecond, "bits_per_unit" .= currentBitsPerUnit, "string" .= currentString, "sata_value" .= currentSataValue]
  toEncoding (Current {..}) = pairs  ("units_per_second" .= currentUnitsPerSecond<>"bits_per_unit" .= currentBitsPerUnit<>"string" .= currentString<>"sata_value" .= currentSataValue)


data InterfaceSpeed = InterfaceSpeed {
    interfaceSpeedMax :: Current,
    interfaceSpeedCurrent :: (Maybe (Current:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON InterfaceSpeed where
  parseJSON (Object v) = InterfaceSpeed <$> v .:   "max" <*> v .:?? "current"
  parseJSON _          = mzero


instance ToJSON InterfaceSpeed where
  toJSON     (InterfaceSpeed {..}) = object ["max" .= interfaceSpeedMax, "current" .= interfaceSpeedCurrent]
  toEncoding (InterfaceSpeed {..}) = pairs  ("max" .= interfaceSpeedMax<>"current" .= interfaceSpeedCurrent)


data Status = Status {
    statusString :: Text,
    statusPassed :: (Maybe (Bool:|:[(Maybe Value)])),
    statusValue :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Status where
  parseJSON (Object v) = Status <$> v .:   "string" <*> v .:?? "passed" <*> v .:   "value"
  parseJSON _          = mzero


instance ToJSON Status where
  toJSON     (Status {..}) = object ["string" .= statusString, "passed" .= statusPassed, "value" .= statusValue]
  toEncoding (Status {..}) = pairs  ("string" .= statusString<>"passed" .= statusPassed<>"value" .= statusValue)


data OfflineDataCollection = OfflineDataCollection {
    offlineDataCollectionStatus :: Status,
    offlineDataCollectionCompletionSeconds :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON OfflineDataCollection where
  parseJSON (Object v) = OfflineDataCollection <$> v .:   "status" <*> v .:   "completion_seconds"
  parseJSON _          = mzero


instance ToJSON OfflineDataCollection where
  toJSON     (OfflineDataCollection {..}) = object ["status" .= offlineDataCollectionStatus, "completion_seconds" .= offlineDataCollectionCompletionSeconds]
  toEncoding (OfflineDataCollection {..}) = pairs  ("status" .= offlineDataCollectionStatus<>"completion_seconds" .= offlineDataCollectionCompletionSeconds)


data PollingMinutes = PollingMinutes {
    pollingMinutesExtended :: Double,
    pollingMinutesShort :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON PollingMinutes where
  parseJSON (Object v) = PollingMinutes <$> v .:   "extended" <*> v .:   "short"
  parseJSON _          = mzero


instance ToJSON PollingMinutes where
  toJSON     (PollingMinutes {..}) = object ["extended" .= pollingMinutesExtended, "short" .= pollingMinutesShort]
  toEncoding (PollingMinutes {..}) = pairs  ("extended" .= pollingMinutesExtended<>"short" .= pollingMinutesShort)


data SelfTest = SelfTest {
    selfTestStatus :: Status,
    selfTestPollingMinutes :: PollingMinutes
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON SelfTest where
  parseJSON (Object v) = SelfTest <$> v .:   "status" <*> v .:   "polling_minutes"
  parseJSON _          = mzero


instance ToJSON SelfTest where
  toJSON     (SelfTest {..}) = object ["status" .= selfTestStatus, "polling_minutes" .= selfTestPollingMinutes]
  toEncoding (SelfTest {..}) = pairs  ("status" .= selfTestStatus<>"polling_minutes" .= selfTestPollingMinutes)


data Capabilities = Capabilities {
    capabilitiesAttributeAutosaveEnabled :: Bool,
    capabilitiesSelfTestsSupported :: Bool,
    capabilitiesExecOfflineImmediateSupported :: Bool,
    capabilitiesConveyanceSelfTestSupported :: Bool,
    capabilitiesOfflineIsAbortedUponNewCmd :: Bool,
    capabilitiesErrorLoggingSupported :: Bool,
    capabilitiesValues :: [Double],
    capabilitiesGpLoggingSupported :: Bool,
    capabilitiesOfflineSurfaceScanSupported :: Bool,
    capabilitiesSelectiveSelfTestSupported :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Capabilities where
  parseJSON (Object v) = Capabilities <$> v .:   "attribute_autosave_enabled" <*> v .:   "self_tests_supported" <*> v .:   "exec_offline_immediate_supported" <*> v .:   "conveyance_self_test_supported" <*> v .:   "offline_is_aborted_upon_new_cmd" <*> v .:   "error_logging_supported" <*> v .:   "values" <*> v .:   "gp_logging_supported" <*> v .:   "offline_surface_scan_supported" <*> v .:   "selective_self_test_supported"
  parseJSON _          = mzero


instance ToJSON Capabilities where
  toJSON     (Capabilities {..}) = object ["attribute_autosave_enabled" .= capabilitiesAttributeAutosaveEnabled, "self_tests_supported" .= capabilitiesSelfTestsSupported, "exec_offline_immediate_supported" .= capabilitiesExecOfflineImmediateSupported, "conveyance_self_test_supported" .= capabilitiesConveyanceSelfTestSupported, "offline_is_aborted_upon_new_cmd" .= capabilitiesOfflineIsAbortedUponNewCmd, "error_logging_supported" .= capabilitiesErrorLoggingSupported, "values" .= capabilitiesValues, "gp_logging_supported" .= capabilitiesGpLoggingSupported, "offline_surface_scan_supported" .= capabilitiesOfflineSurfaceScanSupported, "selective_self_test_supported" .= capabilitiesSelectiveSelfTestSupported]
  toEncoding (Capabilities {..}) = pairs  ("attribute_autosave_enabled" .= capabilitiesAttributeAutosaveEnabled<>"self_tests_supported" .= capabilitiesSelfTestsSupported<>"exec_offline_immediate_supported" .= capabilitiesExecOfflineImmediateSupported<>"conveyance_self_test_supported" .= capabilitiesConveyanceSelfTestSupported<>"offline_is_aborted_upon_new_cmd" .= capabilitiesOfflineIsAbortedUponNewCmd<>"error_logging_supported" .= capabilitiesErrorLoggingSupported<>"values" .= capabilitiesValues<>"gp_logging_supported" .= capabilitiesGpLoggingSupported<>"offline_surface_scan_supported" .= capabilitiesOfflineSurfaceScanSupported<>"selective_self_test_supported" .= capabilitiesSelectiveSelfTestSupported)


data AtaSmartData = AtaSmartData {
    ataSmartDataOfflineDataCollection :: OfflineDataCollection,
    ataSmartDataSelfTest :: SelfTest,
    ataSmartDataCapabilities :: Capabilities
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON AtaSmartData where
  parseJSON (Object v) = AtaSmartData <$> v .:   "offline_data_collection" <*> v .:   "self_test" <*> v .:   "capabilities"
  parseJSON _          = mzero


instance ToJSON AtaSmartData where
  toJSON     (AtaSmartData {..}) = object ["offline_data_collection" .= ataSmartDataOfflineDataCollection, "self_test" .= ataSmartDataSelfTest, "capabilities" .= ataSmartDataCapabilities]
  toEncoding (AtaSmartData {..}) = pairs  ("offline_data_collection" .= ataSmartDataOfflineDataCollection<>"self_test" .= ataSmartDataSelfTest<>"capabilities" .= ataSmartDataCapabilities)


data Device = Device {
    deviceProtocol :: Text,
    deviceName :: Text,
    deviceType :: Text,
    deviceInfoName :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Device where
  parseJSON (Object v) = Device <$> v .:   "protocol" <*> v .:   "name" <*> v .:   "type" <*> v .:   "info_name"
  parseJSON _          = mzero


instance ToJSON Device where
  toJSON     (Device {..}) = object ["protocol" .= deviceProtocol, "name" .= deviceName, "type" .= deviceType, "info_name" .= deviceInfoName]
  toEncoding (Device {..}) = pairs  ("protocol" .= deviceProtocol<>"name" .= deviceName<>"type" .= deviceType<>"info_name" .= deviceInfoName)


data PowerOnTime = PowerOnTime {
    powerOnTimeHours :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON PowerOnTime where
  parseJSON (Object v) = PowerOnTime <$> v .:   "hours"
  parseJSON _          = mzero


instance ToJSON PowerOnTime where
  toJSON     (PowerOnTime {..}) = object ["hours" .= powerOnTimeHours]
  toEncoding (PowerOnTime {..}) = pairs  ("hours" .= powerOnTimeHours)


data Standard = Standard {
    standardCount :: Double,
    standardRevision :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Standard where
  parseJSON (Object v) = Standard <$> v .:   "count" <*> v .:   "revision"
  parseJSON _          = mzero


instance ToJSON Standard where
  toJSON     (Standard {..}) = object ["count" .= standardCount, "revision" .= standardRevision]
  toEncoding (Standard {..}) = pairs  ("count" .= standardCount<>"revision" .= standardRevision)


data AtaSmartSelfTestLog = AtaSmartSelfTestLog {
    ataSmartSelfTestLogStandard :: Standard
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON AtaSmartSelfTestLog where
  parseJSON (Object v) = AtaSmartSelfTestLog <$> v .:   "standard"
  parseJSON _          = mzero


instance ToJSON AtaSmartSelfTestLog where
  toJSON     (AtaSmartSelfTestLog {..}) = object ["standard" .= ataSmartSelfTestLogStandard]
  toEncoding (AtaSmartSelfTestLog {..}) = pairs  ("standard" .= ataSmartSelfTestLogStandard)


data AtaSctCapabilities = AtaSctCapabilities {
    ataSctCapabilitiesErrorRecoveryControlSupported :: Bool,
    ataSctCapabilitiesValue :: Double,
    ataSctCapabilitiesFeatureControlSupported :: Bool,
    ataSctCapabilitiesDataTableSupported :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON AtaSctCapabilities where
  parseJSON (Object v) = AtaSctCapabilities <$> v .:   "error_recovery_control_supported" <*> v .:   "value" <*> v .:   "feature_control_supported" <*> v .:   "data_table_supported"
  parseJSON _          = mzero


instance ToJSON AtaSctCapabilities where
  toJSON     (AtaSctCapabilities {..}) = object ["error_recovery_control_supported" .= ataSctCapabilitiesErrorRecoveryControlSupported, "value" .= ataSctCapabilitiesValue, "feature_control_supported" .= ataSctCapabilitiesFeatureControlSupported, "data_table_supported" .= ataSctCapabilitiesDataTableSupported]
  toEncoding (AtaSctCapabilities {..}) = pairs  ("error_recovery_control_supported" .= ataSctCapabilitiesErrorRecoveryControlSupported<>"value" .= ataSctCapabilitiesValue<>"feature_control_supported" .= ataSctCapabilitiesFeatureControlSupported<>"data_table_supported" .= ataSctCapabilitiesDataTableSupported)


data Flags = Flags {
    flagsPrefailure :: (Maybe (Bool:|:[(Maybe Value)])),
    flagsString :: (Maybe (Text:|:[(Maybe Value)])),
    flagsErrorRate :: (Maybe (Bool:|:[(Maybe Value)])),
    flagsPerformance :: (Maybe (Bool:|:[(Maybe Value)])),
    flagsEventCount :: (Maybe (Bool:|:[(Maybe Value)])),
    flagsValue :: Double,
    flagsAutoKeep :: (Maybe (Bool:|:[(Maybe Value)])),
    flagsUpdatedOnline :: (Maybe (Bool:|:[(Maybe Value)])),
    flagsRemainderScanEnabled :: (Maybe (Bool:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Flags where
  parseJSON (Object v) = Flags <$> v .:?? "prefailure" <*> v .:?? "string" <*> v .:?? "error_rate" <*> v .:?? "performance" <*> v .:?? "event_count" <*> v .:   "value" <*> v .:?? "auto_keep" <*> v .:?? "updated_online" <*> v .:?? "remainder_scan_enabled"
  parseJSON _          = mzero


instance ToJSON Flags where
  toJSON     (Flags {..}) = object ["prefailure" .= flagsPrefailure, "string" .= flagsString, "error_rate" .= flagsErrorRate, "performance" .= flagsPerformance, "event_count" .= flagsEventCount, "value" .= flagsValue, "auto_keep" .= flagsAutoKeep, "updated_online" .= flagsUpdatedOnline, "remainder_scan_enabled" .= flagsRemainderScanEnabled]
  toEncoding (Flags {..}) = pairs  ("prefailure" .= flagsPrefailure<>"string" .= flagsString<>"error_rate" .= flagsErrorRate<>"performance" .= flagsPerformance<>"event_count" .= flagsEventCount<>"value" .= flagsValue<>"auto_keep" .= flagsAutoKeep<>"updated_online" .= flagsUpdatedOnline<>"remainder_scan_enabled" .= flagsRemainderScanEnabled)


data TableElt = TableElt {
    tableEltRaw :: (Maybe (SataVersion:|:[(Maybe Value)])),
    tableEltStatus :: (Maybe (Status:|:[(Maybe Value)])),
    tableEltFlags :: (Maybe (Flags:|:[(Maybe Value)])),
    tableEltLbaMin :: (Maybe (Double:|:[(Maybe Value)])),
    tableEltWorst :: (Maybe (Double:|:[(Maybe Value)])),
    tableEltValue :: (Maybe (Double:|:[(Maybe Value)])),
    tableEltLbaMax :: (Maybe (Double:|:[(Maybe Value)])),
    tableEltName :: (Maybe (Text:|:[(Maybe Value)])),
    tableEltId :: (Maybe (Double:|:[(Maybe Value)])),
    tableEltWhenFailed :: (Maybe (Text:|:[(Maybe Value)])),
    tableEltThresh :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TableElt where
  parseJSON (Object v) = TableElt <$> v .:?? "raw" <*> v .:?? "status" <*> v .:?? "flags" <*> v .:?? "lba_min" <*> v .:?? "worst" <*> v .:?? "value" <*> v .:?? "lba_max" <*> v .:?? "name" <*> v .:?? "id" <*> v .:?? "when_failed" <*> v .:?? "thresh"
  parseJSON _          = mzero


instance ToJSON TableElt where
  toJSON     (TableElt {..}) = object ["raw" .= tableEltRaw, "status" .= tableEltStatus, "flags" .= tableEltFlags, "lba_min" .= tableEltLbaMin, "worst" .= tableEltWorst, "value" .= tableEltValue, "lba_max" .= tableEltLbaMax, "name" .= tableEltName, "id" .= tableEltId, "when_failed" .= tableEltWhenFailed, "thresh" .= tableEltThresh]
  toEncoding (TableElt {..}) = pairs  ("raw" .= tableEltRaw<>"status" .= tableEltStatus<>"flags" .= tableEltFlags<>"lba_min" .= tableEltLbaMin<>"worst" .= tableEltWorst<>"value" .= tableEltValue<>"lba_max" .= tableEltLbaMax<>"name" .= tableEltName<>"id" .= tableEltId<>"when_failed" .= tableEltWhenFailed<>"thresh" .= tableEltThresh)


data AtaSmartSelectiveSelfTestLog = AtaSmartSelectiveSelfTestLog {
    ataSmartSelectiveSelfTestLogFlags :: Flags,
    ataSmartSelectiveSelfTestLogPowerUpScanResumeMinutes :: Double,
    ataSmartSelectiveSelfTestLogRevision :: Double,
    ataSmartSelectiveSelfTestLogTable :: [TableElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON AtaSmartSelectiveSelfTestLog where
  parseJSON (Object v) = AtaSmartSelectiveSelfTestLog <$> v .:   "flags" <*> v .:   "power_up_scan_resume_minutes" <*> v .:   "revision" <*> v .:   "table"
  parseJSON _          = mzero


instance ToJSON AtaSmartSelectiveSelfTestLog where
  toJSON     (AtaSmartSelectiveSelfTestLog {..}) = object ["flags" .= ataSmartSelectiveSelfTestLogFlags, "power_up_scan_resume_minutes" .= ataSmartSelectiveSelfTestLogPowerUpScanResumeMinutes, "revision" .= ataSmartSelectiveSelfTestLogRevision, "table" .= ataSmartSelectiveSelfTestLogTable]
  toEncoding (AtaSmartSelectiveSelfTestLog {..}) = pairs  ("flags" .= ataSmartSelectiveSelfTestLogFlags<>"power_up_scan_resume_minutes" .= ataSmartSelectiveSelfTestLogPowerUpScanResumeMinutes<>"revision" .= ataSmartSelectiveSelfTestLogRevision<>"table" .= ataSmartSelectiveSelfTestLogTable)


data AtaSmartErrorLog = AtaSmartErrorLog {
    ataSmartErrorLogSummary :: Standard
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON AtaSmartErrorLog where
  parseJSON (Object v) = AtaSmartErrorLog <$> v .:   "summary"
  parseJSON _          = mzero


instance ToJSON AtaSmartErrorLog where
  toJSON     (AtaSmartErrorLog {..}) = object ["summary" .= ataSmartErrorLogSummary]
  toEncoding (AtaSmartErrorLog {..}) = pairs  ("summary" .= ataSmartErrorLogSummary)


data AtaVersion = AtaVersion {
    ataVersionMajorValue :: Double,
    ataVersionString :: Text,
    ataVersionMinorValue :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON AtaVersion where
  parseJSON (Object v) = AtaVersion <$> v .:   "major_value" <*> v .:   "string" <*> v .:   "minor_value"
  parseJSON _          = mzero


instance ToJSON AtaVersion where
  toJSON     (AtaVersion {..}) = object ["major_value" .= ataVersionMajorValue, "string" .= ataVersionString, "minor_value" .= ataVersionMinorValue]
  toEncoding (AtaVersion {..}) = pairs  ("major_value" .= ataVersionMajorValue<>"string" .= ataVersionString<>"minor_value" .= ataVersionMinorValue)


data Temperature = Temperature {
    temperatureCurrent :: Double,
    temperatureDriveTrip :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Temperature where
  parseJSON (Object v) = Temperature <$> v .:   "current" <*> v .:?? "drive_trip"
  parseJSON _          = mzero


instance ToJSON Temperature where
  toJSON     (Temperature {..}) = object ["current" .= temperatureCurrent, "drive_trip" .= temperatureDriveTrip]
  toEncoding (Temperature {..}) = pairs  ("current" .= temperatureCurrent<>"drive_trip" .= temperatureDriveTrip)


data UserCapacity = UserCapacity {
    userCapacityBlocks :: Double,
    userCapacityBytes :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON UserCapacity where
  parseJSON (Object v) = UserCapacity <$> v .:   "blocks" <*> v .:   "bytes"
  parseJSON _          = mzero


instance ToJSON UserCapacity where
  toJSON     (UserCapacity {..}) = object ["blocks" .= userCapacityBlocks, "bytes" .= userCapacityBytes]
  toEncoding (UserCapacity {..}) = pairs  ("blocks" .= userCapacityBlocks<>"bytes" .= userCapacityBytes)


data AtaSmartAttributes = AtaSmartAttributes {
    ataSmartAttributesRevision :: Double,
    ataSmartAttributesTable :: [TableElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON AtaSmartAttributes where
  parseJSON (Object v) = AtaSmartAttributes <$> v .:   "revision" <*> v .:   "table"
  parseJSON _          = mzero


instance ToJSON AtaSmartAttributes where
  toJSON     (AtaSmartAttributes {..}) = object ["revision" .= ataSmartAttributesRevision, "table" .= ataSmartAttributesTable]
  toEncoding (AtaSmartAttributes {..}) = pairs  ("revision" .= ataSmartAttributesRevision<>"table" .= ataSmartAttributesTable)


data MessagesElt = MessagesElt {
    messagesEltString :: Text,
    messagesEltSeverity :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON MessagesElt where
  parseJSON (Object v) = MessagesElt <$> v .:   "string" <*> v .:   "severity"
  parseJSON _          = mzero


instance ToJSON MessagesElt where
  toJSON     (MessagesElt {..}) = object ["string" .= messagesEltString, "severity" .= messagesEltSeverity]
  toEncoding (MessagesElt {..}) = pairs  ("string" .= messagesEltString<>"severity" .= messagesEltSeverity)


data Smartctl = Smartctl {
    smartctlBuildInfo :: Text,
    smartctlSvnRevision :: Text,
    smartctlVersion :: [Double],
    smartctlArgv :: [Text],
    smartctlMessages :: (Maybe ([MessagesElt])),
    smartctlExitStatus :: Double,
    smartctlPlatformInfo :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Smartctl where
  parseJSON (Object v) = Smartctl <$> v .:   "build_info" <*> v .:   "svn_revision" <*> v .:   "version" <*> v .:   "argv" <*> v .:?? "messages" <*> v .:   "exit_status" <*> v .:   "platform_info"
  parseJSON _          = mzero


instance ToJSON Smartctl where
  toJSON     (Smartctl {..}) = object ["build_info" .= smartctlBuildInfo, "svn_revision" .= smartctlSvnRevision, "version" .= smartctlVersion, "argv" .= smartctlArgv, "messages" .= smartctlMessages, "exit_status" .= smartctlExitStatus, "platform_info" .= smartctlPlatformInfo]
  toEncoding (Smartctl {..}) = pairs  ("build_info" .= smartctlBuildInfo<>"svn_revision" .= smartctlSvnRevision<>"version" .= smartctlVersion<>"argv" .= smartctlArgv<>"messages" .= smartctlMessages<>"exit_status" .= smartctlExitStatus<>"platform_info" .= smartctlPlatformInfo)


data Wwn = Wwn {
    wwnOui :: Double,
    wwnId :: Double,
    wwnNaa :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Wwn where
  parseJSON (Object v) = Wwn <$> v .:   "oui" <*> v .:   "id" <*> v .:   "naa"
  parseJSON _          = mzero


instance ToJSON Wwn where
  toJSON     (Wwn {..}) = object ["oui" .= wwnOui, "id" .= wwnId, "naa" .= wwnNaa]
  toEncoding (Wwn {..}) = pairs  ("oui" .= wwnOui<>"id" .= wwnId<>"naa" .= wwnNaa)


data Smart = Smart {
    topLevelSerialNumber :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelPowerCycleCount :: (Maybe (Double:|:[(Maybe Value)])),
    topLevelVendor :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelFirmwareVersion :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelLocalTime :: (Maybe (LocalTime:|:[(Maybe Value)])),
    topLevelModelName :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelLogicalBlockSize :: (Maybe (Double:|:[(Maybe Value)])),
    topLevelDeviceType :: (Maybe (DeviceType:|:[(Maybe Value)])),
    topLevelSataVersion :: (Maybe (SataVersion:|:[(Maybe Value)])),
    topLevelSmartStatus :: (Maybe (SmartStatus:|:[(Maybe Value)])),
    topLevelInterfaceSpeed :: (Maybe (InterfaceSpeed:|:[(Maybe Value)])),
    topLevelAtaSmartData :: (Maybe (AtaSmartData:|:[(Maybe Value)])),
    topLevelDevice :: (Maybe (Device:|:[(Maybe Value)])),
    topLevelPowerOnTime :: (Maybe (PowerOnTime:|:[(Maybe Value)])),
    topLevelAtaSmartSelfTestLog :: (Maybe (AtaSmartSelfTestLog:|:[(Maybe Value)])),
    topLevelAtaSctCapabilities :: (Maybe (AtaSctCapabilities:|:[(Maybe Value)])),
    topLevelAtaSmartSelectiveSelfTestLog :: (Maybe (AtaSmartSelectiveSelfTestLog:|:[(Maybe Value)])),
    topLevelAtaSmartErrorLog :: (Maybe (AtaSmartErrorLog:|:[(Maybe Value)])),
    topLevelAtaVersion :: (Maybe (AtaVersion:|:[(Maybe Value)])),
    topLevelRotationRate :: (Maybe (Double:|:[(Maybe Value)])),
    topLevelPhysicalBlockSize :: (Maybe (Double:|:[(Maybe Value)])),
    topLevelJsonFormatVersion :: [Double],
    topLevelTemperature :: (Maybe (Temperature:|:[(Maybe Value)])),
    topLevelScsiVersion :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelUserCapacity :: (Maybe (UserCapacity:|:[(Maybe Value)])),
    topLevelAtaSmartAttributes :: (Maybe (AtaSmartAttributes:|:[(Maybe Value)])),
    topLevelSmartctl :: Smartctl,
    topLevelInSmartctlDatabase :: (Maybe (Bool:|:[(Maybe Value)])),
    topLevelProduct :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelRevision :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelModelFamily :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelWwn :: (Maybe (Wwn:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Smart where
  parseJSON (Object v) = Smart <$> v .:?? "serial_number" <*> v .:?? "power_cycle_count" <*> v .:?? "vendor" <*> v .:?? "firmware_version" <*> v .:?? "local_time" <*> v .:?? "model_name" <*> v .:?? "logical_block_size" <*> v .:?? "device_type" <*> v .:?? "sata_version" <*> v .:?? "smart_status" <*> v .:?? "interface_speed" <*> v .:?? "ata_smart_data" <*> v .:?? "device" <*> v .:?? "power_on_time" <*> v .:?? "ata_smart_self_test_log" <*> v .:?? "ata_sct_capabilities" <*> v .:?? "ata_smart_selective_self_test_log" <*> v .:?? "ata_smart_error_log" <*> v .:?? "ata_version" <*> v .:?? "rotation_rate" <*> v .:?? "physical_block_size" <*> v .:   "json_format_version" <*> v .:?? "temperature" <*> v .:?? "scsi_version" <*> v .:?? "user_capacity" <*> v .:?? "ata_smart_attributes" <*> v .:   "smartctl" <*> v .:?? "in_smartctl_database" <*> v .:?? "product" <*> v .:?? "revision" <*> v .:?? "model_family" <*> v .:?? "wwn"
  parseJSON _          = mzero


instance ToJSON Smart where
  toJSON     (Smart {..}) = object ["serial_number" .= topLevelSerialNumber, "power_cycle_count" .= topLevelPowerCycleCount, "vendor" .= topLevelVendor, "firmware_version" .= topLevelFirmwareVersion, "local_time" .= topLevelLocalTime, "model_name" .= topLevelModelName, "logical_block_size" .= topLevelLogicalBlockSize, "device_type" .= topLevelDeviceType, "sata_version" .= topLevelSataVersion, "smart_status" .= topLevelSmartStatus, "interface_speed" .= topLevelInterfaceSpeed, "ata_smart_data" .= topLevelAtaSmartData, "device" .= topLevelDevice, "power_on_time" .= topLevelPowerOnTime, "ata_smart_self_test_log" .= topLevelAtaSmartSelfTestLog, "ata_sct_capabilities" .= topLevelAtaSctCapabilities, "ata_smart_selective_self_test_log" .= topLevelAtaSmartSelectiveSelfTestLog, "ata_smart_error_log" .= topLevelAtaSmartErrorLog, "ata_version" .= topLevelAtaVersion, "rotation_rate" .= topLevelRotationRate, "physical_block_size" .= topLevelPhysicalBlockSize, "json_format_version" .= topLevelJsonFormatVersion, "temperature" .= topLevelTemperature, "scsi_version" .= topLevelScsiVersion, "user_capacity" .= topLevelUserCapacity, "ata_smart_attributes" .= topLevelAtaSmartAttributes, "smartctl" .= topLevelSmartctl, "in_smartctl_database" .= topLevelInSmartctlDatabase, "product" .= topLevelProduct, "revision" .= topLevelRevision, "model_family" .= topLevelModelFamily, "wwn" .= topLevelWwn]
  toEncoding (Smart {..}) = pairs  ("serial_number" .= topLevelSerialNumber<>"power_cycle_count" .= topLevelPowerCycleCount<>"vendor" .= topLevelVendor<>"firmware_version" .= topLevelFirmwareVersion<>"local_time" .= topLevelLocalTime<>"model_name" .= topLevelModelName<>"logical_block_size" .= topLevelLogicalBlockSize<>"device_type" .= topLevelDeviceType<>"sata_version" .= topLevelSataVersion<>"smart_status" .= topLevelSmartStatus<>"interface_speed" .= topLevelInterfaceSpeed<>"ata_smart_data" .= topLevelAtaSmartData<>"device" .= topLevelDevice<>"power_on_time" .= topLevelPowerOnTime<>"ata_smart_self_test_log" .= topLevelAtaSmartSelfTestLog<>"ata_sct_capabilities" .= topLevelAtaSctCapabilities<>"ata_smart_selective_self_test_log" .= topLevelAtaSmartSelectiveSelfTestLog<>"ata_smart_error_log" .= topLevelAtaSmartErrorLog<>"ata_version" .= topLevelAtaVersion<>"rotation_rate" .= topLevelRotationRate<>"physical_block_size" .= topLevelPhysicalBlockSize<>"json_format_version" .= topLevelJsonFormatVersion<>"temperature" .= topLevelTemperature<>"scsi_version" .= topLevelScsiVersion<>"user_capacity" .= topLevelUserCapacity<>"ata_smart_attributes" .= topLevelAtaSmartAttributes<>"smartctl" .= topLevelSmartctl<>"in_smartctl_database" .= topLevelInSmartctlDatabase<>"product" .= topLevelProduct<>"revision" .= topLevelRevision<>"model_family" .= topLevelModelFamily<>"wwn" .= topLevelWwn)
