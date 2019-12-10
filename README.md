# Crawl Heka apartment listings

### Development

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
| build_year | x | |
| floor_count | x | |
| identifier | x | kohteen tunnus |
| district | x | kaupunginosa |
