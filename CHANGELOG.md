Unreleased changes
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
