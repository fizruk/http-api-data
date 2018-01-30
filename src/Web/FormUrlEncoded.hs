-- |
-- Convert Haskell values to and from @application/xxx-form-urlencoded@ format.
module Web.FormUrlEncoded (
  -- * Classes
  ToForm (..),
  FromForm (..),

  -- ** Keys for 'Form' entries
  ToFormKey(..),
  FromFormKey(..),

  -- * 'Form' type
  Form(..),

  -- * Encoding and decoding @'Form'@s
  urlEncodeAsForm,
  urlEncodeAsFormStable,
  urlDecodeAsForm,

  urlEncodeForm,
  urlEncodeFormStable,
  urlDecodeForm,

  -- * 'Generic's
  genericToForm,
  genericFromForm,

  -- ** Encoding options
  FormOptions(..),
  defaultFormOptions,

  -- * Helpers
  toListStable,
  toEntriesByKey,
  toEntriesByKeyStable,
  fromEntriesByKey,

  lookupAll,
  lookupMaybe,
  lookupUnique,

  parseAll,
  parseMaybe,
  parseUnique,

  urlEncodeParams,
  urlDecodeParams,
) where

import Web.Internal.FormUrlEncoded

