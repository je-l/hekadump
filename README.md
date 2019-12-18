# Crawl Heka apartment listings

Program for crawling [Heka apartment listing](https://www.hekaoy.fi/fi/asunnot/kohteet) into csv format

### Development

Tested with OCaml 4.09.0 and dune 2.0.0

Run unit tests:

```sh
dune runtest
```

Build the executable:

```sh
dune build
```

Run the program:

```
dune exec hekadump.exe
```

### Output format

| column | required | description |
| ------ | -------- | ----------- |
| apartment type | x | e.g. "1h + k" |
| apartment size (exact) | | this is found if all apartments have same size |
| apartment size (minimum) | | minimum apartment size if there are multiple |
| apartment size (maximum) | | |
| apartment count | x | count for similar apartments in the building |
| rent (exact) | | rent if it's same for every apartment of this type |
| rent (minimum) | | maximum rent if the apartments have variable rent |
| rent (maximum) | | |
| build year | | |
| floor count (exact) | | |
| floor count (minimum) | | some houses have floor count like "4-6" |
| floor count (maximum) | | |
| identifier | x | labeled as "kohteen tunnus" in the pages |
| district | x | "kaupunginosa" |
| url | x | |
