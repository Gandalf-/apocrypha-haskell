# Apocrypha

This is a complete Haskell implementation of the Apocrypha JSON server.

You can see the original
[Python Implementation](https://github.com/gandalf-/apocrypha) here, and the
official API documentation.

# Features

* *Performance* - up to 18,000 queries per second per CPU, semi linear
  performance scaling with additional CPUs. Benchmarking shows 8 CPUS ~= 86,000
  queries/sec using a local Unix Domain Socket.

* *Compatability* - full compatability with the Python implementation. Scripts
  written using the Python API may see a 2x to 5x performance increase. The
  Haskell client API uses the same patterns as the Python API.

* *Easy of Use* - the client API integrates the `Data.Aeson` package for
  straight forward JSON object usage.

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
* `--no-unix` - do not listen on a local Unix Domain Socket, only TCP
* `--database <path>` - use an alternate file on disk for the database. You can
  use this to serve an arbitrary JSON file to the network.


# Benchmarks

Performance testing done with the `bench-apocrypha` utility, run against the
Haskell binary produced by `make release` and the latest Python implementation
available through `pip`.  These results were produced on Ubuntu 16.04, 32
Platinum 8168 CPU @ 2.70GHz

### Workloads

Seconds to complete

|                     | Python  | Haskell | Speedup | Queries      |
|---------------------|---------|---------|---------|--------------|
| Single reader       | 4.0533  | 2.5743  | 157%    | 100,000      |
| Single reader cache | 4.0559  | 2.3132  | 175%    | 100,000      |
| Single writer       | 4.7642  | 3.0133  | 158%    | 100,000      |
|                     |         |         |         |              |
| Multi reader        | 53.3462 | 9.3792  | 568%    | 10 * 100,000 |
| Multi reader cache  | 43.5055 | 8.9738  | 484%    | 10 * 100,000 |
|                     |         |         |         |              |
| Many reader         | 96.2097 | 19.6056 | 490%    | 20 * 100,000 |
| Many reader cache   | 87.5551 | 19.2085 | 455%    | 20 * 100,000 |

### Throughput

Average queries completed per second

|                     | Python | Haskell | Difference | Queries      |
|---------------------|--------|---------|------------|--------------|
| Single reader       | 24,671 | 38,845  | +14,174    | 100,000      |
| Single reader cache | 24,655 | 43,230  | +18,575    | 100,000      |
| Single writer       | 20,989 | 33,186  | +12,197    | 100,000      |
|                     |        |         |            |              |
| Multi reader        | 18,745 | 106,618 | +87,873    | 10 * 100,000 |
| Multi reader cache  | 22,985 | 111,435 | +88,450    | 10 * 100,000 |
|                     |        |         |            |              |
| Many reader         | 20,787 | 102,011 | +81,224    | 20 * 100,000 |
| Many reader cache   | 22,842 | 104,120 | +81,278    | 20 * 100,000 |

# API

This library includes client bindings for applications that want to use an
Apocrypha database natively.

```haskell
type Query = [String]

set    :: (ToJSON a)   => Context -> Query -> a -> IO ()
get    :: (FromJSON a) => Context -> Query -> IO (Maybe a)
keys   :: Context -> Query -> IO [String]
del    :: Context -> Query -> IO ()
pop    :: Context -> Query -> IO (Maybe String)
append :: Context -> Query -> String -> IO ()
```

```haskell
import Apocrypha.Client

main :: IO ()
main = do
    keys' [] >>= mapM_ print

    append' ["nested", "key"] "value"
    value <- pop' ["nested", "key"]

    case value of
      Nothing -> putStrLn "Nothing found"
      Just v  -> putStrLn $ "Found " <> v
```
