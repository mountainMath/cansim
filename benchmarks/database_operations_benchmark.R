# Database Operations Performance Benchmarks
# This script benchmarks database-related operations in the cansim package
# to establish baseline performance and validate optimizations

library(cansim)
library(microbenchmark)
library(ggplot2)

# Configuration
TEST_TABLES <- list(
  small = "23-10-0061",   # Small table for quick iterations
  medium = "20-10-0001",  # Medium-sized table
  # large tables can be uncommented for comprehensive testing
  # large = "17-10-0005"  # Census table (large)
)

FORMATS <- c("sqlite", "parquet", "feather")
BENCHMARK_TIMES <- 5  # Number of iterations per benchmark

# Helper function to clear cache for clean benchmarks
clear_table_cache <- function(table_number) {
  cache_files <- list_cansim_cached_tables()
  if (!is.null(cache_files) && nrow(cache_files) > 0) {
    table_files <- cache_files[grepl(table_number, cache_files$table_number), ]
    if (nrow(table_files) > 0) {
      for (f in table_files$file_path) {
        if (file.exists(f)) {
          unlink(f)
          message(paste("Removed cache file:", f))
        }
      }
    }
  }
}

# Helper function to ensure table is downloaded (for connection benchmarks)
ensure_table_cached <- function(table_number, format) {
  tryCatch({
    con <- get_cansim_connection(table_number, format = format)
    DBI::dbDisconnect(con)
    message(paste("Table", table_number, "cached in", format, "format"))
  }, error = function(e) {
    message(paste("Error caching table:", e$message))
  })
}

cat("========================================\n")
cat("CANSIM Database Operations Benchmarks\n")
cat("========================================\n\n")

# Store results
results <- list()

#===========================================
# BENCHMARK 1: Initial Database Creation
#===========================================
cat("\n### BENCHMARK 1: Initial Database Creation (CSV to Database)\n")
cat("This measures the time to download and convert a table to database format\n")

for (table in names(TEST_TABLES)) {
  table_number <- TEST_TABLES[[table]]
  cat(paste0("\n-- Testing ", table, " table (", table_number, ") --\n"))

  for (format in FORMATS) {
    cat(paste0("Format: ", format, "\n"))

    # Clear cache before benchmark
    clear_table_cache(table_number)

    # Benchmark the initial creation
    bm <- microbenchmark(
      {
        con <- get_cansim_connection(table_number, format = format)
        DBI::dbDisconnect(con)
      },
      times = 1,  # Only once since it involves download
      unit = "s"
    )

    results[[paste0("creation_", table, "_", format)]] <- bm
    print(summary(bm)[, c("expr", "min", "median", "max")])
  }
}

#===========================================
# BENCHMARK 2: Database Connection Initialization
#===========================================
cat("\n### BENCHMARK 2: Database Connection Initialization (Cached)\n")
cat("This measures connection time when database already exists\n")

for (table in names(TEST_TABLES)) {
  table_number <- TEST_TABLES[[table]]
  cat(paste0("\n-- Testing ", table, " table (", table_number, ") --\n"))

  for (format in FORMATS) {
    # Ensure table is cached
    ensure_table_cached(table_number, format)

    cat(paste0("Format: ", format, "\n"))

    # Benchmark connection initialization
    bm <- microbenchmark(
      {
        con <- get_cansim_connection(table_number, format = format)
        DBI::dbDisconnect(con)
      },
      times = BENCHMARK_TIMES,
      unit = "ms"
    )

    results[[paste0("connection_", table, "_", format)]] <- bm
    print(summary(bm)[, c("expr", "min", "median", "max")])
  }
}

#===========================================
# BENCHMARK 3: Index Creation (SQLite Only)
#===========================================
cat("\n### BENCHMARK 3: Index Creation Time (SQLite)\n")
cat("This measures time spent creating indexes on SQLite databases\n")

# This benchmark requires modifying the code to isolate index creation
# For now, we'll measure it indirectly through connection time differences
# A more direct benchmark will be added after refactoring

for (table in names(TEST_TABLES)) {
  table_number <- TEST_TABLES[[table]]
  cat(paste0("\n-- Testing ", table, " table (", table_number, ") --\n"))

  # Clear SQLite cache
  clear_table_cache(table_number)

  # Create connection and measure total time
  start_time <- Sys.time()
  con <- get_cansim_connection(table_number, format = "sqlite")
  end_time <- Sys.time()

  total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  cat(paste0("Total SQLite creation time: ", round(total_time, 2), " seconds\n"))

  # Get field count (more fields = more indexes)
  fields <- DBI::dbListFields(con, "data")
  cat(paste0("Number of fields (potential indexes): ", length(fields), "\n"))

  DBI::dbDisconnect(con)

  results[[paste0("index_creation_", table)]] <- list(
    total_time = total_time,
    field_count = length(fields)
  )
}

#===========================================
# BENCHMARK 4: Query Performance
#===========================================
cat("\n### BENCHMARK 4: Query Performance (Filtering)\n")
cat("This measures query execution time for filtered data\n")

for (table in names(TEST_TABLES)) {
  table_number <- TEST_TABLES[[table]]
  cat(paste0("\n-- Testing ", table, " table (", table_number, ") --\n"))

  for (format in FORMATS) {
    # Ensure table is cached
    ensure_table_cached(table_number, format)

    cat(paste0("Format: ", format, "\n"))

    # Benchmark a simple filter query
    bm <- microbenchmark(
      {
        con <- get_cansim_connection(table_number, format = format)
        # Apply a filter and collect
        result <- con %>%
          dplyr::filter(REF_DATE >= "2020-01-01") %>%
          dplyr::collect()
        DBI::dbDisconnect(con)
      },
      times = BENCHMARK_TIMES,
      unit = "ms"
    )

    results[[paste0("query_", table, "_", format)]] <- bm
    print(summary(bm)[, c("expr", "min", "median", "max")])
  }
}

#===========================================
# BENCHMARK 5: collect_and_normalize Performance
#===========================================
cat("\n### BENCHMARK 5: collect_and_normalize Performance\n")
cat("This measures the normalization overhead after query\n")

for (table in names(TEST_TABLES)) {
  table_number <- TEST_TABLES[[table]]
  cat(paste0("\n-- Testing ", table, " table (", table_number, ") --\n"))

  for (format in FORMATS) {
    # Ensure table is cached
    ensure_table_cached(table_number, format)

    cat(paste0("Format: ", format, "\n"))

    # Benchmark collect_and_normalize
    bm <- microbenchmark(
      {
        con <- get_cansim_connection(table_number, format = format)
        result <- con %>%
          dplyr::filter(REF_DATE >= "2020-01-01") %>%
          collect_and_normalize(disconnect = TRUE)
      },
      times = BENCHMARK_TIMES,
      unit = "ms"
    )

    results[[paste0("normalize_", table, "_", format)]] <- bm
    print(summary(bm)[, c("expr", "min", "median", "max")])
  }
}

#===========================================
# Save Results
#===========================================
cat("\n### Saving Benchmark Results\n")

# Save raw results
saveRDS(results, "benchmarks/baseline_results.rds")
cat("Raw results saved to: benchmarks/baseline_results.rds\n")

# Create summary report
summary_df <- data.frame()

for (name in names(results)) {
  if (inherits(results[[name]], "microbenchmark")) {
    bm_summary <- summary(results[[name]])
    summary_df <- rbind(summary_df, data.frame(
      benchmark = name,
      min_ms = bm_summary$min,
      median_ms = bm_summary$median,
      max_ms = bm_summary$max,
      mean_ms = bm_summary$mean
    ))
  }
}

write.csv(summary_df, "benchmarks/baseline_summary.csv", row.names = FALSE)
cat("Summary saved to: benchmarks/baseline_summary.csv\n")

#===========================================
# Generate Plots
#===========================================
cat("\n### Generating Visualization\n")

if (nrow(summary_df) > 0) {
  # Parse benchmark names
  summary_df$operation <- sub("_.*", "", summary_df$benchmark)
  summary_df$table_size <- sub(".*_([^_]+)_[^_]+$", "\\1", summary_df$benchmark)
  summary_df$format <- sub(".*_", "", summary_df$benchmark)

  # Plot connection times by format
  connection_data <- summary_df[grepl("^connection", summary_df$benchmark), ]
  if (nrow(connection_data) > 0) {
    p <- ggplot(connection_data, aes(x = table_size, y = median_ms, fill = format)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Database Connection Initialization Time (Cached Tables)",
        subtitle = "Lower is better",
        x = "Table Size",
        y = "Median Time (ms)",
        fill = "Format"
      ) +
      theme_minimal()

    ggsave("benchmarks/connection_time_comparison.png", p, width = 10, height = 6)
    cat("Plot saved to: benchmarks/connection_time_comparison.png\n")
  }

  # Plot query times by format
  query_data <- summary_df[grepl("^query", summary_df$benchmark), ]
  if (nrow(query_data) > 0) {
    p <- ggplot(query_data, aes(x = table_size, y = median_ms, fill = format)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Query Performance (Filtered Data Collection)",
        subtitle = "Lower is better",
        x = "Table Size",
        y = "Median Time (ms)",
        fill = "Format"
      ) +
      theme_minimal()

    ggsave("benchmarks/query_time_comparison.png", p, width = 10, height = 6)
    cat("Plot saved to: benchmarks/query_time_comparison.png\n")
  }
}

cat("\n========================================\n")
cat("Benchmarking Complete!\n")
cat("========================================\n")
cat("\nBaseline benchmarks established. Use these to validate performance improvements.\n")
cat("Results saved in: benchmarks/\n")
