# Crawl Heka apartment listings

### Development

Build the executable:

```sh
dune build hekadump.exe
```

Run program:

```
./_build/default/hekadump.exe
```

### Output format

| column | required | description |
| ------ | -------- | ----------- |
| build_year | x | |
| floor_count | x | |
| identifier | x | kohteen tunnus |
| district | x | kaupunginosa |
