# http-api-data

[![Hackage package](http://img.shields.io/hackage/v/http-api-data.svg)](http://hackage.haskell.org/package/http-api-data)
[![Build Status](https://secure.travis-ci.org/fizruk/http-api-data.png?branch=master)](http://travis-ci.org/fizruk/http-api-data)

This package defines typeclasses used for converting Haskell data types to and from HTTP API data.

### Examples

Booleans:

```
>>> toUrlPiece True
"true"
>>> parseUrlPiece "false" :: Either Text Bool
Right False
>>> parseUrlPieces ["true", "false", "undefined"] :: Either Text [Bool]
Left "could not parse: `undefined'"
```

Numbers:

```
>>> toQueryParam 45.2
"45.2"
>>> parseQueryParam "452" :: Either Text Int
Right 452
>>> toQueryParams [1..5]
["1","2","3","4","5"]
>>> parseQueryParams ["127", "255"] :: Either Text [Int8]
Left "out of bounds: `255' (should be between -128 and 127)"
```

Strings:

```
>>> toHeader "hello"
"hello"
>>> parseHeader "world" :: Either Text String
Right "world"
```

Calendar day:

```
>>> toQueryParam (fromGregorian 2015 10 03)
"2015-10-03"
>>> toGregorian <$> parseQueryParam "2016-12-01"
Right (2016,12,1)
```

## Contributing

Contributions and bug reports are welcome!

