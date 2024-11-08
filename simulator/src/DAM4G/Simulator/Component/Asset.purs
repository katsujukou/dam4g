module DAM4G.Simulator.Component.Asset where

import Unsafe.Coerce (unsafeCoerce)

foreign import data AssetURL :: Type

foreign import assetUrls
  :: { icons ::
         { cpu :: AssetURL
         , memory :: AssetURL
         , table :: AssetURL
         , inspector :: AssetURL
         , stack :: AssetURL
         , run :: AssetURL
         , runAndPause :: AssetURL
         , reload :: AssetURL
         , stepInto :: AssetURL
         , eject :: AssetURL
         , compile :: AssetURL
         , github :: AssetURL
         }
     }

toString :: AssetURL -> String
toString = unsafeCoerce