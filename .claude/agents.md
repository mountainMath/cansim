# Agent Knowledge Base for cansim Package

This document captures learnings and conventions for AI agents working on the cansim R package.

Last updated: 2025-11-15

---

## Project Overview

**Package**: cansim - R package for accessing Statistics Canada data tables
**Language**: R
**Main Focus**: Database operations (SQLite, Parquet, Feather)
**Testing**: testthat 3.0+
**Maintainers**: Jens von Bergmann, Dmitry Shkolnik

---

## Key Technical Learnings

### Database Schema Conventions

**SQLite Table Naming**:
- ❌ Tables are NOT named "data"
- ✅ Tables are named `cansim_{table_number}` (e.g., `cansim_23_10_0061`)
- Always detect table names dynamically:
  ```r
  tbl <- DBI::dbListTables(con$src$con)
  tbl <- tbl[grepl("^cansim", tbl)]
  ```

**Field Names**:
- Can vary by language (English/French)
- Use actual schema inspection, not assumptions
- Geography fields have special handling with normalization

### Testing Conventions

**testthat Version**: 3.0+
- ✅ Use `label` parameter for custom messages
- ❌ Don't use `info` parameter (deprecated in 3.0+)
  ```r
  # Correct
  expect_equal(x, y, label = "Description")

  # Wrong (old syntax)
  expect_equal(x, y, info = "Description")
  ```

**Data Comparison Best Practices**:
- Always compare against the reference implementation (`get_cansim()` in-memory)
- Sort by ALL dimension columns, not just REF_DATE and DGUID
- Use `get_cansim_column_list()` to get complete dimension list
- Use existing `count_differences()` function for comprehensive cell-by-cell comparison
- Account for trailing spaces in SCALAR_FACTOR

**Test Structure**:
- Skip network-dependent tests on CRAN with `skip_on_cran()`
- Skip offline tests with `skip_if_offline()`
- Use clear, descriptive test names
- Include `label` parameters for better failure messages

### Performance Optimization Patterns

**Transaction Usage**:
- SQLite operations benefit from batched transactions
- Wrap multiple index creations in single transaction (30-50% faster)
- Wrap all CSV chunk writes in single transaction (10-20% faster)
- Always include proper rollback on errors

**Index Creation**:
- Always run `ANALYZE` after creating indexes
- This updates query planner statistics (`sqlite_stat1` table)
- Results in 5-15% faster queries

**Chunk Sizing**:
- Consider both symbol columns AND total column count
- Wide tables (>50 columns) need smaller chunks
- Maintain minimum chunk size (10,000 rows) for efficiency

### Package Build Configuration

**Always exclude from .Rbuildignore**:
- Development artifacts: `^CODE_REVIEW.md`, `^ANALYSIS.md`, etc.
- Benchmark directories: `^benchmarks$`, `^benchmarks/*`
- Workflow-specific files
- Large data files for testing

**Include in package**:
- Core R code
- Tests
- Documentation (vignettes, man pages)
- Essential data files
- NEWS.md, README.md

---

## Code Style & Conventions

### R Coding Style
- Function names: `snake_case`
- Use tidyverse patterns (dplyr, tidyr)
- Consistent indentation (2 spaces)
- roxygen2 documentation for all exported functions
- `@keywords internal` for non-exported functions

### Error Handling
- Use `tryCatch` for operations that may fail
- Provide clear, actionable error messages
- Include context in errors (which table, which operation)
- Use `warning()` for non-critical issues, `stop()` for critical ones

### Messages to Users
- Use `message()` for progress updates
- Include progress indicators for long operations: `[1/5] Processing...`
- Make messages conditional based on verbosity settings where applicable

---

## Common Pitfalls to Avoid

### ❌ Don't:
1. **Hardcode table or field names** - always detect dynamically
2. **Assume data structure** - verify with actual schema inspection
3. **Use outdated testthat syntax** - check package DESCRIPTION for version
4. **Compare formats only to each other** - always include reference implementation
5. **Skip running actual tests** - syntax validation alone is insufficient
6. **Create workflow-specific artifacts** - they clutter the repo
7. **Forget to update .Rbuildignore** - when adding development files

### ✅ Do:
1. **Run `devtools::test()` or `R CMD check`** before submitting changes
2. **Check DESCRIPTION** for dependency versions
3. **Study existing test patterns** in `tests/testthat/`
4. **Verify assumptions** against actual codebase behavior
5. **Use existing helper functions** (like `count_differences()`)
6. **Update NEWS.md** for user-visible changes
7. **Add comprehensive roxygen2 documentation**

---

## Testing Checklist

Before submitting a PR:
- [ ] All R files load without syntax errors
- [ ] `devtools::test()` passes locally
- [ ] New functions have roxygen2 documentation
- [ ] Tests use `label` not `info` for testthat 3.0+
- [ ] Network tests have `skip_on_cran()`
- [ ] Data consistency validated across formats
- [ ] NEWS.md updated for user-visible changes
- [ ] .Rbuildignore updated for new development files
- [ ] No hardcoded assumptions about schema

---

## Performance Optimization Guidelines

### When to Optimize
- Database operations (high ROI)
- Repeated operations in loops
- I/O operations (CSV reading, network calls)
- Memory usage for wide/large tables

### How to Optimize (Conservative Approach)
1. Use transactions for batched operations
2. Run ANALYZE after index creation
3. Adaptive chunk sizing based on table characteristics
4. Cache metadata where safe
5. Use standard database best practices

### What NOT to Optimize (Yet)
- Don't break backward compatibility
- Don't use risky techniques without thorough testing
- Don't optimize without benchmarking
- Don't add complex dependencies

### Benchmarking
- Use `microbenchmark` package (in Suggests)
- Create reproducible benchmarks in `benchmarks/` directory
- Test with realistic data sizes
- Document expected improvements in NEWS.md

---

## Communication with Maintainers

### Commit Message Style
- Prefix: `perf:` for performance, `docs:` for documentation, `fix:` for bugs, `test:` for tests
- First line: concise summary (50 chars)
- Body: detailed explanation of what and why
- Include co-authoring attribution for AI assistance

### PR Best Practices
- Comprehensive description with performance tables
- Link to relevant documentation
- Include validation results
- Explain trade-offs and design decisions
- Keep PRs focused (one concern per PR)

### What Maintainers Value
- No breaking changes
- Comprehensive testing
- Clear documentation
- Conservative, safe optimizations
- Evidence of performance improvements

---

## Useful Commands

### Testing
```bash
# Run all tests
Rscript -e "devtools::test()"

# Run specific test file
Rscript -e "testthat::test_file('tests/testthat/test-performance_optimizations.R')"

# Check package
R CMD check .
```

### Benchmarking
```bash
# Quick validation
Rscript benchmarks/quick_validation.R

# Full benchmark suite
Rscript benchmarks/database_operations_benchmark.R
```

### Loading Functions for Testing
```r
library(dplyr)
library(DBI)
library(RSQLite)
source("R/cansim_helpers.R")
source("R/cansim_sql.R")
source("R/cansim_parquet.R")
```

---

## Project-Specific Context

### Data Sources
- Statistics Canada (StatCan) public data
- Tables identified by NDM numbers (e.g., "23-10-0061")
- Data can be large (gigabytes for census tables)
- Network downloads can be slow/unreliable

### User Expectations
- Fast access to cached data
- Minimal memory usage
- Clear progress indicators for long operations
- No breaking changes to existing code
- Bilingual support (English/French)

### Common Use Cases
1. Downloading and caching tables locally
2. Filtering data at database level before loading to memory
3. Working with very large census tables
4. Repeated access to same tables within R session
5. Comparing data across different time periods

---

## Notes for Future Work

### Potential Future Optimizations
- Metadata hierarchy caching (pre-computed hierarchies)
- Parallel Arrow operations (multi-threaded parquet reads)
- Connection pooling (reuse connections within session)
- Vectorized string operations (data.table for factor conversion)

### Technical Debt to Watch
- Session-level connection cache infrastructure exists but isn't actively used yet
- Metadata file caching is write-only (could be read on reconnect)
- Progress timing could be added to index creation messages

### Known Limitations
- Network dependency for initial downloads
- Large memory usage for very wide tables (mitigated by adaptive chunking)
- Sequential batch processing (could be parallelized in future)

---

## Changelog

### 2025-11-15: Phase 1 - Database Performance (v0.4.5 PR #141)
- Learned: SQLite tables named `cansim_{number}`, not "data"
- Learned: testthat 3.0+ uses `label` not `info`
- Learned: Always compare against reference implementation
- Implemented: Batched index creation (30-50% faster)
- Implemented: Transaction-wrapped CSV conversion (10-20% faster)
- Implemented: ANALYZE command for query optimization
- Implemented: Adaptive chunk sizing for wide tables
- Created: Comprehensive benchmark infrastructure

### 2025-11-15: Phase 2 - Data Processing & Metadata (v0.4.5)
- Learned: `vapply` with pre-allocation faster than `lapply %>% unlist`
- Learned: Pre-split coordinates once, reuse for all fields in loop
- Learned: Session-level caching excellent for repeated operations
- Learned: Recursive algorithms with memoization beat iterative loops for tree structures
- Learned: Base R `strsplit` faster than `stringr::str_split` for simple cases
- Implemented: Vectorized coordinate normalization (30-40% faster)
- Implemented: Date format caching (70-90% faster for cached tables)
- Implemented: Pre-split coordinates for factor conversion (25-40% faster)
- Implemented: Recursive hierarchy building with memoization (30-50% faster)
- Pattern: Always analyze loop iterations - if doing same operation N times, hoist it out

---

**Last Updated**: 2025-11-15 by Claude (Phase 2 Performance Optimizations)
