{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RpgRuleset.Core.World where

import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import RpgRuleset.Core.System (System, systemId)
import RpgRuleset.Core.Types

-- | Represents a game world that uses one or more systems
data World = World
  { -- Identity
    worldId :: !WorldId,
    worldName :: !Text,
    worldDescription :: !(Maybe Text),
    -- Systems used in this world
    worldPrimarySystem :: !SystemId,
    worldAdditionalSystems :: ![SystemId],
    -- Loaded systems (populated at runtime)
    worldSystems :: !(Map SystemId System),
    -- Metadata
    worldVersion :: !Version,
    worldAuthors :: ![Text],
    -- File metadata
    worldRootPath :: !FilePath
  }
  deriving (Show, Eq, Generic)

instance FromJSON World where
  parseJSON = withObject "World" $ \o -> do
    worldId <- o .: "world_id"
    worldName <- o .: "name"
    worldDescription <- o .:? "description"
    worldPrimarySystem <- o .: "primary_system"
    worldAdditionalSystems <- o .:? "additional_systems" .!= []
    let worldSystems = Map.empty -- Systems loaded separately
    worldVersion <- o .:? "version" .!= Version 1 0 0
    worldAuthors <- o .:? "authors" .!= []
    let worldRootPath = ""
    return World {..}

instance ToJSON World where
  toJSON World {..} =
    object
      [ "world_id" .= worldId,
        "name" .= worldName,
        "description" .= worldDescription,
        "primary_system" .= worldPrimarySystem,
        "additional_systems" .= worldAdditionalSystems,
        "version" .= worldVersion,
        "authors" .= worldAuthors
      ]

-- | Create a minimal world with a single system
mkWorld :: WorldId -> Text -> SystemId -> World
mkWorld wid name sysId =
  World
    { worldId = wid,
      worldName = name,
      worldDescription = Nothing,
      worldPrimarySystem = sysId,
      worldAdditionalSystems = [],
      worldSystems = Map.empty,
      worldVersion = Version 1 0 0,
      worldAuthors = [],
      worldRootPath = ""
    }

-- | Get all system IDs used by a world
getAllSystemIds :: World -> [SystemId]
getAllSystemIds w = worldPrimarySystem w : worldAdditionalSystems w

-- | Add a loaded system to the world
addSystem :: System -> World -> World
addSystem sys world =
  world
    { worldSystems = Map.insert (systemId sys) sys (worldSystems world)
    }

-- | Look up a system in the world
lookupSystem :: SystemId -> World -> Maybe System
lookupSystem sid = Map.lookup sid . worldSystems

-- | Get the primary system (if loaded)
getPrimarySystem :: World -> Maybe System
getPrimarySystem w = lookupSystem (worldPrimarySystem w) w
