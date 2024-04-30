0.6.1
---

* Support `cookie-0.5.0`, see [#137](https://github.com/fizruk/http-api-data/pull/137).
  Note that `cookie-0.5.0`'s parser now drops removes double quotes around cookie values.

0.6
---

* Use [`text-iso8601`](https://hackage.haskell.org/package/text-iso8601)
  to parse and serialise `time` types. (Instead of `attoparsec-iso8601`).
  Due this change some formats are slightly changed:

  - Space between timezone is not accepted
  - Timezone offset can be between -23:59..23:59
  - Timezone offset is output with colon between hours and minutes

* Require at least GHC-8.2

0.5.1
-----

* Add `toEncodedQueryParam` to `ToHttpApiData` type class. It has default
  implementation using `toQueryParam`, but may be overriden with more efficient
  one.

0.5
---

* Use `attoparsec-iso8601-1.1.0.0`.
  `Day` and `UTCTime` parsers require at least 4 digits now, which is a breaking change.
* Drop GHC-7.8 and GHC-7.10 support.

0.4.3
-----

* Add `Quarter`, `QuarterOfYear` and `Month` instances
* Support `bytestring-0.11`

0.4.2
-----

* Add instances for `Const` and `Identity`

0.4.1.1
-------

* Allow cookie <0.5
* Change to `build-type: Simple`

0.4.1
-----

* Use `time-compat` to provide instances for `DayOfWeek`.

0.4
---

* `NominalDiffTime` instances preserve precision (roundtrip)
* Add `Semigroup.Min`, `Max`, `First`, `Last` instances
* Add `Tagged b a` instances

0.3.10
---

* Fix 'SetCookie' instances
  (see [#86](https://github.com/fizruk/http-api-data/pull/86))
* Add support for `Fixed`
  (see [#78](https://github.com/fizruk/http-api-data/pull/87))

0.3.9
---

* GHC-8.6 support
* Remove dependency on `uri-bytestring` and use functions from `http-types` instead
  (see [#75](https://github.com/fizruk/http-api-data/pull/75))
* Add support for `SetCookie`
  (see [#74](https://github.com/fizruk/http-api-data/pull/74))

0.3.8.1
---

* GHC-8.4.1 support

0.3.8
---

* Minor changes:
    * Stable URL-encoding for `Form`s (see [#67](https://github.com/fizruk/http-api-data/pull/67)):
        * Introduce `urlEncodeParams` and `urlDecodeParams`;
        * Introduce `urlEncodeAsFormStable` and use stable encoding for doctests;
        * Add `toEntriesByKeyStable` and `toListStable`;
    * Add `Semigroup` instance for `Form` (see [#69](https://github.com/fizruk/http-api-data/pull/69));
    * Relax upper bound on Cabal (see [#73](https://github.com/fizruk/http-api-data/pull/73)).

0.3.7.2
---

* Allow http-types-0.12
* .cabal file adjustments

0.3.7.1
---

* GHC-8.2 support (see [#55](https://github.com/fizruk/http-api-data/pull/55)).

0.3.7
---

* Minor changes:
    * Use [`attoparsec-iso8601`](http://hackage.haskell.org/package/attoparsec-iso8601)
      for parsing of time types. Now the accepted formats are the same as by `aeson`,
      i.e. parsers are more lenient
      (see [#41](https://github.com/fizruk/http-api-data/pull/41));
    * Preserve fractions of a second in `ToHttpApiData` instances (see [#53](https://github.com/fizruk/http-api-data/pull/53));
    * Add `ToHttpApiData` and `FromHttpApiData` instances for `TimeOfDay` (see [#53](https://github.com/fizruk/http-api-data/pull/53)).

0.3.6
---

* Minor change:
    * Add `toEncodedUrlPiece` class method for URL-encoded path segments (see [#50](https://github.com/fizruk/http-api-data/pull/50)); use efficient encoding for types whose values don't need URL-encoding.

0.3.5
---

* Minor changes:
    * Add `LenientData` which always succeeds to parse (see [#45](https://github.com/fizruk/http-api-data/pull/45)).

0.3.4
---

* Minor changes:
    * Add support for GHC 8.2, drop support for GHC 7.6 (see [#44](https://github.com/fizruk/http-api-data/pull/44)).

0.3.3
---

* Minor changes:
    * Expose `Form` constructor from `Web.FromUrlEncoded` (see [#40](https://github.com/fizruk/http-api-data/pull/40));
    * Fix example in `FromForm` documentation (see [#39](https://github.com/fizruk/http-api-data/issues/39)).

0.3.2
---

* Minor change:
    * Export `Form` type from `Web.FormUrlEncoded` (see [#37](https://github.com/fizruk/http-api-data/pull/37)).

0.3.1
---

* Minor changes:
    * Add instances for `Data.UUID` (see [#34](https://github.com/fizruk/http-api-data/pull/34)).

0.3
---
* Major changes:
    * Add `Web.FormUrlEncoded` to work with form data (see [#32](https://github.com/fizruk/http-api-data/pull/32)).

* Minor changes:
    * Add instances for `Numeric.Natural` (see [`d944721`](https://github.com/fizruk/http-api-data/commit/d944721ac94929a7ed9e66f25e23221799c08d83)).

0.2.4
---
* Make `parseHeader` total (instead of throwing exceptions on invalid Unicode, see [#30](https://github.com/fizruk/http-api-data/pull/30)).

0.2.3
---
* Add more parser helpers for `Bounded` `Enum` types.

0.2.2
---

* Add instances for more `time` types: `LocalTime`, `ZonedTime`, `UTCTime` and `NominalDiffTime`

0.2.1
---

* Add helpers for multiple URL pieces and query params:
    * `toUrlPieces`, `parseUrlPieces`
    * `toQueryParams`, `parseQueryParams`

0.2
---

* Export helper functions from `Web.HttpApiData`:
    * `parseUrlPieceMaybe`, `parseHeaderMaybe`, `parseQueryParamMaybe`
    * `parseUrlPieceWithPrefix`, `parseHeaderWithPrefix`, `parseQueryParamWithPrefix`
    * `showTextData`, `readTextData`, `parseBoundedTextData`
* Fix AMP related warnings

0.1.1
---

* Add `use-text-show` flag to optionally use more efficient `TextShow` instances
