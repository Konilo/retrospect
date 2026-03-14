# Test helper: mocks getSymbols to return fixture CSVs instead of Yahoo Finance.

library(data.table)
library(R6)
library(lubridate)
library(testthat)

project_root <- normalizePath(file.path(getwd(), ".."))

source(file.path(project_root, "src", "core_utils", "Asset.R"))
source(file.path(project_root, "src", "core_utils", "Portfolio.R"))

mock_get_symbols <- function() {
    fixtures_dir <- file.path(project_root, "tests", "fixtures")
    fixture_cache <- list()

    for (f in list.files(fixtures_dir, pattern = "\\.csv$", full.names = TRUE)) {
        ticker <- gsub("_ohlcv\\.csv$", "", basename(f))
        fixture_cache[[ticker]] <- fread(f)
        fixture_cache[[ticker]][, date := as.Date(date)]
    }

    mock_fn <- function(Symbols, src = "yahoo", return.class = "zoo",
                        auto.assign = FALSE, ...) {
        if (!Symbols %in% names(fixture_cache)) {
            stop(paste("No fixture for ticker:", Symbols))
        }
        dt <- copy(fixture_cache[[Symbols]])
        dates <- dt$date
        dt[, date := NULL]
        zoo::zoo(as.matrix(dt), order.by = dates)
    }

    assignInNamespace("getSymbols", mock_fn, ns = "quantmod")
}
