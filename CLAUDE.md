# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**cansim** is an R package for accessing Statistics Canada data tables and vectors. It retrieves data as tidy data frames with metadata enrichment, supports bilingual (English/French) retrieval, and offers caching via parquet/feather/SQLite for large tables.

## Development Commands

```r
# Load package for development
devtools::load_all()

# Run all tests
devtools::test()

# Run a specific test file
devtools::test_file("tests/testthat/test-data_consistency.R")

# Rebuild documentation (roxygen2)
devtools::document()

# Full package check
devtools::check()
```

## Architecture

### Core Files (in R/)

- **cansim.R** - Main data retrieval functions (`get_cansim()`, `get_cansim_table_info()`, table metadata)
- **cansim_vectors.R** - Vector/time series retrieval (`get_cansim_vector()`, coordinate-based access)
- **cansim_parquet.R** - Parquet/feather caching and lazy database connections (`get_cansim_connection()`, `collect_and_normalize()`)
- **cansim_sql.R** - SQLite database operations (`get_cansim_sqlite()`)
- **cansim_metadata.R** - Metadata parsing from StatCan API responses
- **cansim_helpers.R** - Internal utilities (table number normalization, encoding, etc.)
- **cansim_tables_list.R** - Table discovery (`list_cansim_cubes()`, `search_cansim_cubes()`)
- **user_settings.R** - Cache path management (`set_cansim_cache_path()`)

### Data Flow

```
StatCan API → Raw download → Metadata parsing → Value normalization (scale factors)
           → Date parsing → Factor conversion → Bilingual field names → Tidy tibble
```

### Caching Strategy

- **Session cache**: Temp directory (default)
- **Persistent cache**: Set `CANSIM_CACHE_PATH` environment variable
- **Formats**: parquet (recommended), feather, SQLite
- **Refresh options**: `refresh=TRUE` (force), `refresh="auto"` (if newer available), `refresh=FALSE`

### Bilingual Support

Language parameter affects column names and values:
- English: `VALUE`, `GEO`, `SCALAR_ID`
- French: `VALEUR`, `GÉO`, `IDENTIFICATEUR SCALAIRE`

## Testing Notes

- Tests use `skip_on_cran()` for network-dependent tests
- Some tests require `COMPILE_VIG` environment variable
- Tests verify factor ordering and data format consistency across output types

## Key Implementation Details

- Handles both legacy CANSIM numbers (e.g., "051-0013") and new NDM format (e.g., "17-10-0016")
- Only "-01" base tables are supported (table endings are validated)
- Value normalization scales data by powers of 10 (millions, thousands) and converts percentages to rates
- Census tables use "semi-wide" format with special metadata parsing
