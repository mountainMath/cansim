# Performance benchmarks

## Vector batch materialization

`vector_batching.R` measures the warm-cache path of
`get_cansim_vector_for_latest_periods()`. It uses deterministic cached batch
fixtures and mocks only metadata retrieval and value normalization, keeping the
network outside the benchmark while exercising the public batching workflow.

Run it against isolated package installs:

```sh
R CMD INSTALL -l /tmp/cansim-baseline-lib /path/to/cansim-0.4.5
Rscript benchmarks/vector_batching.R /tmp/cansim-baseline-lib /tmp/baseline.rds

R CMD INSTALL -l /tmp/cansim-candidate-lib .
Rscript benchmarks/vector_batching.R /tmp/cansim-candidate-lib /tmp/candidate.rds
```

Results from five randomized repetitions on R 4.5.0, macOS arm64, dplyr 1.1.4:

| Vectors | Rows | Batches | 0.4.5 median | Candidate median | Saving | Speedup |
|---:|---:|---:|---:|---:|---:|---:|
| 3,000 | 15,000 | 10 | 0.026 s | 0.024 s | 0.002 s | 1.08x |
| 15,000 | 75,000 | 50 | 0.154 s | 0.116 s | 0.038 s | 1.33x |
| 30,000 | 150,000 | 100 | 0.424 s | 0.227 s | 0.197 s | 1.87x |

Speedup is baseline elapsed time divided by candidate elapsed time. Absolute
timings vary by machine. The output SHA-256 digests matched between the
independently installed 0.4.5 baseline and candidate at all three scales.

The mechanism is avoiding repeated copies of the complete accumulated tibble:
each API batch is stored in a preallocated list and all batches are bound once.
