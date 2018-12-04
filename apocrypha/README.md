# Apocrypha

This is a complete Haskell implementation of the Apocrypha JSON server.

You can see the original
[Python Implementation](https://github.com/gandalf-/apocrypha) here, and the
official API documentation.

# Features

* *Performance* - up to 12,500 queries per second per CPU, semi linear
  performance scaling with additional CPUs

## ACID

* *Atomicity* - all operations are atomic
* *Consistency* - invariants are enforced by the client application only, there
  are no schemas and no types
* *Strong Isolation* - every write takes a global lock
* *Weak Durability* - writes to disk are done asynchronously

# Usage

* `--headless` - do not log to the console, may make execution up to %30 faster
* `--no-cache` - disable caching, decreases read performance depending on value
  complexity
* `--stateless` - do not write changes back to disk


# Benchmarks

Performance testing done with the `bench-apocrypha` utility, run against the
Haskell binary produced by `make release` and the latest Python implementation.
These results were produced on Ubuntu 16.04, 4 i7-5500 CPU, 16GB RAM.

### Workloads

Seconds to complete

|                     | Python | Haskell | Speedup | Queries      |
|---------------------|--------|---------|---------|--------------|
| Single reader       |        | 5.0264  |         | 100,000      |
| Single reader cache |        | 4.0470  |         | 100,000      |
| Single writer       |        | 5.1017  |         | 100,000      |
|                     |        |         |         |              |
| Multi reader        |        | 18.7190 |         | 10 * 100,000 |
| Multi reader cache  |        | 18.5680 |         | 10 * 100,000 |
|                     |        |         |         |              |
| Many reader         |        | 35.7385 |         | 20 * 100,000 |
| Many reader cache   |        |         |         | 20 * 100,000 |

### Workloads with Tracing

Seconds to complete

|                     | Python   | Haskell | Speedup | Queries      |
|---------------------|----------|---------|---------|--------------|
| Single reader       | 6.6986   | 3.4260  | 195%    | 100,000      |
| Single reader cache | 5.6983   | 3.1076  | 183%    | 100,000      |
| Single writer       | 7.2524   | 3.4295  | 211%    | 100,000      |
|                     |          |         |         |              |
| Multi reader        | 152.0364 | 29.2545 | 519%    | 10 * 100,000 |
| Multi reader cache  | 140.1895 | 26.9502 | 520%    | 10 * 100,000 |
|                     |          |         |         |              |
| Many reader         | 312.3077 | 57.3516 | 544%    | 20 * 100,000 |
| Many reader cache   | 298.4110 | 54.0288 | 552%    | 20 * 100,000 |

### Throughput

Average queries completed per second

|                     | Python | Haskell | Difference | Queries      |
|---------------------|--------|---------|------------|--------------|
| Single reader       | 14,928 | 29,188  | +14,260    | 100,000      |
| Single reader cache | 17,549 | 32,179  | +14,630    | 100,000      |
| Single writer       | 13,788 | 29,158  | +14,370    | 100,000      |
|                     |        |         |            |              |
| Multi reader        | 6,577  | 34,182  | +27,605    | 10 * 100,000 |
| Multi reader cache  | 7,133  | 37,105  | +29,972    | 10 * 100,000 |
|                     |        |         |            |              |
| Many reader         | 6,403  | 34,872  | +28,469    | 20 * 100,000 |
| Many reader cache   | 6,702  | 37,017  | +30,315    | 20 * 100,000 |
