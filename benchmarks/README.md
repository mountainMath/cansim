# CANSIM Performance Benchmarks

This directory contains performance benchmarking scripts for the cansim package, with a focus on database operations.

## Requirements

```r
install.packages("microbenchmark")
```

## Running Benchmarks

### Baseline Benchmarks

To establish baseline performance metrics before optimizations:

```r
source("benchmarks/database_operations_benchmark.R")
```

This will:
- Test database creation, connection, and query performance
- Compare SQLite, Parquet, and Feather formats
- Generate visualizations and summary reports
- Save results to `benchmarks/baseline_results.rds` and `benchmarks/baseline_summary.csv`

### Comparing Before/After

After making optimizations:

1. Run the benchmark script again
2. Results will be saved with current timestamp
3. Compare median times to validate improvements

## Benchmark Categories

1. **Initial Database Creation**: Time to download and convert CSV to database format
2. **Connection Initialization**: Time to open connection to cached database
3. **Index Creation**: Time spent creating indexes (SQLite)
4. **Query Performance**: Time to filter and collect data
5. **Normalization**: Time for `collect_and_normalize()` operation

## Test Tables

- **Small** (23-10-0061): Quick iterations and testing
- **Medium** (20-10-0001): Representative workload
- **Large**: Uncomment census tables for comprehensive testing (slower)

## Output Files

- `baseline_results.rds`: Raw microbenchmark objects
- `baseline_summary.csv`: Summary statistics (min, median, max, mean)
- `connection_time_comparison.png`: Connection time visualization
- `query_time_comparison.png`: Query performance visualization

## Notes

- Benchmarks download real data from Statistics Canada
- First run will be slower due to network downloads
- Subsequent runs use cached data where appropriate
- Clear cache between runs for consistent "cold start" measurements
