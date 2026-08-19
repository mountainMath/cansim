# Benchmark the warm-cache batching path in get_cansim_vector_for_latest_periods().
# Network, metadata retrieval, and normalization are kept outside the claim so
# the benchmark measures how downloaded batches are materialized into one result.
#
# Usage:
#   Rscript benchmarks/vector_batching.R /path/to/library /tmp/results.rds
#
# Install the baseline or candidate into the supplied isolated library first.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: vector_batching.R LIBRARY OUTPUT_RDS", call. = FALSE)
}

library_path <- normalizePath(args[[1L]], mustWork = TRUE)
output_path <- args[[2L]]
namespace <- loadNamespace("cansim", lib.loc = library_path)
get_vectors <- get("get_cansim_vector_for_latest_periods", envir = namespace)

periods <- 5L
workloads <- data.frame(
  vectors = c(3000L, 15000L, 30000L),
  batches = c(10L, 50L, 100L)
)
repetitions <- 5L

make_cache <- function(vectors) {
  batches <- split(vectors, ceiling(seq_along(vectors) / 300L))
  paths <- character(length(batches))
  for (batch_number in seq_along(batches)) {
    vecs <- batches[[batch_number]]
    vectors_string <- paste0(
      "[",
      paste(
        purrr::map(as.character(vecs), function(x) {
          paste0('{"vectorId":', x, ',"latestN":', periods, "}")
        }),
        collapse = ", "
      ),
      "]"
    )
    paths[[batch_number]] <- file.path(
      tempdir(),
      paste0("cansim_cache_", digest::digest(vectors_string, algo = "md5"), ".rda")
    )

    vector_rows <- rep(vecs, each = periods)
    result <- tibble::tibble(
      REF_DATE = rep(sprintf("202%d", seq_len(periods) - 1L), times = length(vecs)),
      VALUE = as.numeric(vector_rows %% 1000L),
      COORDINATE = paste0((vector_rows %% 97L) + 1L, ".", (vector_rows %% 31L) + 1L),
      VECTOR = paste0("v", vector_rows),
      cansimTableNumber = "34-10-0001"
    )
    saveRDS(result, paths[[batch_number]])
  }
  paths
}

metadata_stub <- function(cansimTableNumber, coordinates, language) {
  tibble::tibble(cansimTableNumber = cansimTableNumber, COORDINATE = coordinates)
}
normalization_stub <- function(data, ...) data

run_order <- sample(rep(seq_len(nrow(workloads)), each = repetitions))
timings <- vector("list", length(run_order))
digests <- character(nrow(workloads))

for (workload_id in seq_len(nrow(workloads))) {
  vectors <- seq_len(workloads$vectors[[workload_id]])
  cache_paths <- make_cache(vectors)
  on.exit(unlink(cache_paths), add = TRUE)
}

for (i in seq_along(run_order)) {
  workload_id <- run_order[[i]]
  vectors <- seq_len(workloads$vectors[[workload_id]])

  gc()
  elapsed <- system.time({
    result <- suppressMessages(testthat::with_mocked_bindings(
      get_vectors(vectors, periods = periods, factors = FALSE),
      metadata_for_coordinates = metadata_stub,
      normalize_cansim_values = normalization_stub,
      .package = "cansim"
    ))
  })[["elapsed"]]

  stopifnot(
    nrow(result) == length(vectors) * periods,
    identical(unique(result$VECTOR), paste0("v", vectors)),
    identical(names(result), c("REF_DATE", "VALUE", "COORDINATE", "VECTOR",
                               "cansimTableNumber"))
  )

  result_digest <- digest::digest(result, algo = "sha256", serialize = TRUE)
  if (digests[[workload_id]] == "") {
    digests[[workload_id]] <- result_digest
  } else {
    stopifnot(identical(digests[[workload_id]], result_digest))
  }

  timings[[i]] <- data.frame(
    vectors = workloads$vectors[[workload_id]],
    rows = nrow(result),
    batches = workloads$batches[[workload_id]],
    repetition = sum(run_order[seq_len(i)] == workload_id),
    elapsed_seconds = elapsed
  )
}

timings <- do.call(rbind, timings)
summary <- aggregate(
  elapsed_seconds ~ vectors + rows + batches,
  timings,
  function(x) c(min = min(x), median = median(x), max = max(x))
)
summary <- cbind(summary[seq_len(3L)], as.data.frame(summary$elapsed_seconds))

result <- list(
  package_version = as.character(utils::packageVersion("cansim", lib.loc = library_path)),
  package_library = library_path,
  r_version = R.version.string,
  platform = R.version$platform,
  dplyr_version = as.character(utils::packageVersion("dplyr")),
  periods = periods,
  repetitions = repetitions,
  output_digests = digests,
  timings = timings,
  summary = summary
)

saveRDS(result, output_path)
print(summary, row.names = FALSE)
