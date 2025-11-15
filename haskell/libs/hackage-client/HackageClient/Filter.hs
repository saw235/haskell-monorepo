{-# LANGUAGE OverloadedStrings #-}

module HackageClient.Filter
  ( applyFilters,
  )
where

import HackageClient.Types
  ( FilterOptions (..),
    Module (..),
  )

-- | Apply filters to module exports based on filter options
-- If all filters are True, returns module unchanged
-- If any filter is False, removes corresponding entity category
applyFilters :: FilterOptions -> Module -> Module
applyFilters opts mod =
  mod
    { moduleExportedFunctions =
        if filterFunctions opts
          then moduleExportedFunctions mod
          else [],
      moduleExportedTypes =
        if filterTypes opts
          then moduleExportedTypes mod
          else [],
      moduleExportedClasses =
        if filterClasses opts
          then moduleExportedClasses mod
          else []
    }
