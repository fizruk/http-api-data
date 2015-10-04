# http-api-data

[![Build Status](https://secure.travis-ci.org/fizruk/http-api-data.png?branch=master)](http://travis-ci.org/fizruk/http-api-data)

This package defines typeclasses used for converting Haskell data types to and from HTTP API data.

### Examples

Booleans:

```
>>> toUrlPiece True
"True"
>>> parseUrlPiece "False" :: Either Text Bool
Right False
>>> parseUrlPiece "something else" :: Either Text Bool
Left "could not convert: `something else'"
```

Numbers:

```
>>> toUrlPiece 45.2
"45.2"
>>> parseUrlPiece "452" :: Either Text Int
Right 452
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

