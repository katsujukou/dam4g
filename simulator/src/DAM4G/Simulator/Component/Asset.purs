module DAM4G.Simulator.Component.Asset where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data AssetURL :: Type

foreign import assetUrls
  :: { icons ::
         { cpu :: AssetURL
         , memory :: AssetURL
         , table :: AssetURL
         , inspector :: AssetURL
         , run :: AssetURL
         , runAll :: AssetURL
         , stepInto :: AssetURL
         }
     }

toString :: AssetURL -> String
toString = unsafeCoerce