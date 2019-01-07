# json2csv
Json -> CSV conversion utility

json2csv is able to convert arbitrary newline-delimited JSON (.ndjson) into a valid CSV.
Currently, json2csv supports newline-delimited JSON only.

### Build json2csv

You need to install [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

then just execute `stack build`

### Usage

`json2csv-exe in.ndjson out.csv`

### Examples

##### nested arrays in single document

JSON input:

```javascript
{"a": [{"b": "b1", "c": [1, 2]},{"b": "b2", "c": [3, 4]}], "d": [10, 20]}
```

CSV output:

a.$.b|a.$.c.$|d.$
-----|-------|---
b1|1.0|10.0
b1|1.0|20.0
b1|2.0|10.0
b1|2.0|20.0
b2|3.0|10.0
b2|3.0|20.0
b2|4.0|10.0
b2|4.0|20.0

##### various types of inner values

JSON input:

```javascript
{"a": "field a0","b": [{"value": "field b2"}, {"value": "field b xxx"}],"c": [{"value": "field c0"}, {"value": "field c xxx"}]}
{"a": "field a1","b": [{"value": "field b1"}],"c": [{"value": "field c1"}]}
{"a": "field a2","b": [{"value": "field b2"}, {"value": "field b xxx"}],"c": {"value": "field c2"}, "d": 42}
```

CSV output:

a|b.$.value|c.$.value|d|c.value
-|---------|---------|-|-------
field a0|field b2|field c0||
field a0|field b2|field c xxx||
field a0|field b xxx|field c0||
field a0|field b xxx|field c xxx||
field a1|field b1|field c1||
field a2|field b2||42.0|field c2
field a2|field b xxx||42.0|field c2
