-- | Re-export of CLIPS modules

module Clips (
      module Clips.Core
    , module Clips.Error
    , module Clips.Types
    , module Clips.Transformer
    ) where

import           Clips.Core
import           Clips.Error
import           Clips.Transformer
import           Clips.Types
