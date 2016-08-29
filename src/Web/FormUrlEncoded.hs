-- |
-- Convert Haskell values to and from @application/xxx-form-urlencoded@ format.
module Web.FormUrlEncoded (
  -- * Classes
  ToForm (..),
  FromForm (..),

  -- * Encoding and decoding @'Form'@s
  encodeAsForm,
  decodeAsForm,

  encodeForm,
  decodeForm,
) where

import Web.Internal.FormUrlEncoded

