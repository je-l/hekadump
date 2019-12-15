# Crawl Heka apartment listings

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
./_build/default/main.exe
```

### Output format

| column | required | description |
| ------ | -------- | ----------- |
| apartment type | x | e.g. "1h + k" |
| apartment size (exact) | | This is found if all apartments have same size |
| apartment size (minimum) | | Minimum apartment size if there are multiple |
| apartment size (maximum) | | |
| apartment count | x | how many similar apartments are there |
| rent (exact) | | rent if it's same for every apartment of this type |
| rent (minimum) | | maximum rent if the apartments have variable rent |
| rent (maximum) | | |
| build year | x | |
| floor count | x | |
| identifier | x | Labeled as "kohteen tunnus" in the pages |
| district | x | "kaupunginosa" |
