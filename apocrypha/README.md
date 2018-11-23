# Apocrypha

This is a complete Haskell implementation of the Apocrypha JSON server.

You can see the original
[Python Implementation](https://github.com/gandalf-/apocrypha) here, and the
official API documentation.


# Benchmarks

|                     | Python   | Haskell | Speedup | Queries      |
|---------------------|----------|---------|---------|--------------|
| Single reader       | 6.6986   | 3.4260  | 195%    | 100,000      |
| Single reader cache | 5.6983   | 3.1076  | 183%    | 100,000      |
| Single writer       | 7.2524   | 3.4295  | 211%    | 100,000      |
|                     |          |         |         |              |
| Multi reader        | 152.0364 | 29.2545 | 519%    | 10 * 100,000 |
| Multi reader cache  | 140.1895 | 26.9502 | 520%    | 10 * 100,000 |
| Multi writer        |          |         |         |              |
