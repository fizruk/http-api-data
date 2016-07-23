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
