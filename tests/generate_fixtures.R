# Run this script once to generate test fixtures from real Yahoo data.
# The fixtures are committed so tests don't need network access.
#
# Usage: Rscript tests/generate_fixtures.R

library(data.table)
setwd("/workspaces/modern-portfolio-theory")
source("src/core_utils/Asset.R")

# Fetch a small window of real data for two assets
asset_a <- Asset$new("AAPL", "yahoo")
asset_b <- Asset$new("MSFT", "yahoo")

# Keep only 2024 to keep fixtures small
fixture_a <- asset_a$ohlcv[date >= "2024-01-01" & date <= "2024-12-31"]
fixture_b <- asset_b$ohlcv[date >= "2024-01-01" & date <= "2024-12-31"]

fwrite(fixture_a, "tests/fixtures/AAPL_ohlcv.csv")
fwrite(fixture_b, "tests/fixtures/MSFT_ohlcv.csv")
cat("Fixtures written.\n")
