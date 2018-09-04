# Benchmark

## Dependencies

Install dependencies:

```shell
$ stack install --bench --only-dependencies :bench-speed \
  && stack install --bench --only-dependencies :bench-memory
```

## Speed
We use `Criterion` to bench the speed of the main functions provided by inflections.
The following command will generate the benchmark results.

```
$ stack bench :bench-speed
```

## Memory
We use `Weigh` to bench the memory comsumption of the main functions provided by inflections.
The following command will generate the benchmark results.

```
$ stack bench :bench-memory
```
