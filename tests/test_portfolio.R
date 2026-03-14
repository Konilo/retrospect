mock_get_symbols()

# Helper to build a 60/40 AAPL/MSFT portfolio
make_portfolio <- function(from = "2024-01-01", to = "2024-12-31") {
    assets <- list(
        c(0.6, Asset$new("AAPL", "yahoo")),
        c(0.4, Asset$new("MSFT", "yahoo"))
    )
    Portfolio$new(
        assets,
        from = from, to = to,
        risk_free_rate = 0.03
    )
}

test_that("Portfolio initializes with correct structure", {
    pf <- make_portfolio()

    expect_s3_class(pf$merged_assets, "data.table")
    expect_true(nrow(pf$merged_assets) > 0)
    expect_true("portfolio_return" %in% colnames(pf$merged_assets))
    expect_true("date" %in% colnames(pf$merged_assets))
})

test_that("Portfolio rejects weights not summing to 1", {
    assets <- list(
        c(0.5, Asset$new("AAPL", "yahoo")),
        c(0.4, Asset$new("MSFT", "yahoo"))
    )
    expect_error(
        Portfolio$new(assets, risk_free_rate = 0.03)
    )
})

test_that("Portfolio rejects negative weights", {
    assets <- list(
        c(-0.1, Asset$new("AAPL", "yahoo")),
        c(1.1, Asset$new("MSFT", "yahoo"))
    )
    expect_error(
        Portfolio$new(assets, risk_free_rate = 0.03)
    )
})

test_that("Portfolio date range filtering works", {
    pf <- make_portfolio(from = "2024-06-01", to = "2024-06-30")
    dates <- pf$merged_assets$date
    expect_true(all(dates >= as.Date("2024-06-01")))
    expect_true(all(dates <= as.Date("2024-06-30")))
})

test_that("portfolio_return is weighted sum of asset returns", {
    pf <- make_portfolio()
    ma <- pf$merged_assets
    complete <- na.omit(ma)

    return_cols <- grep(".Return$", colnames(complete), value = TRUE)
    weights <- c(0.6, 0.4)
    expected <- rowSums(as.matrix(complete[, ..return_cols]) %*% diag(weights))
    expect_equal(complete$portfolio_return, expected, tolerance = 1e-10)
})

test_that("assets_price_comparison returns long-format data", {
    pf <- make_portfolio()
    data <- pf$get_prepared_data("assets_price_comparison", "day")
    expect_true(all(c("date", "asset", "price_pct_change") %in% colnames(data)))
    expect_true("Portfolio" %in% data$asset)
    expect_true("AAPL" %in% data$asset)
    expect_true("MSFT" %in% data$asset)
})

test_that("return_per_time_unit returns correct columns", {
    pf <- make_portfolio()
    data <- pf$get_prepared_data("return_per_time_unit", "month")
    expect_true(all(c("date", "portfolio_pct_return") %in% colnames(data)))
    expect_true(nrow(data) > 0)
})

test_that("drawdown values are non-positive", {
    pf <- make_portfolio()
    data <- pf$get_prepared_data("drawdown", "day")
    expect_true(all(c("date", "drawdown") %in% colnames(data)))
    expect_true(all(data$drawdown <= 0))
})

test_that("assets_returns returns per-asset return columns", {
    pf <- make_portfolio()
    data <- pf$get_prepared_data("assets_returns", "day")
    expect_true("AAPL" %in% colnames(data))
    expect_true("MSFT" %in% colnames(data))
    expect_true("date" %in% colnames(data))
})

test_that("mean_sd_over_time returns annualized metrics", {
    pf <- make_portfolio()
    data <- pf$get_prepared_data(
        "mean_sd_over_time", "month",
        n_trading_days_per_year = 252
    )
    expected_cols <- c(
        "date", "mean_minus_2_sd", "mean_ann_return",
        "ann_volatility", "mean_plus_2_sd"
    )
    expect_true(all(expected_cols %in% colnames(data)))
    expect_true(all(data$ann_volatility >= 0))
    expect_equal(
        data$mean_ann_return + 2 * data$ann_volatility,
        data$mean_plus_2_sd
    )
})

test_that("returns_analysis computes Sharpe ratio", {
    pf <- make_portfolio()
    result <- pf$get_prepared_data(
        "returns_analysis", "day",
        risk_free_rate = 0.03,
        n_trading_days_per_year = 252
    )
    expect_true(is.list(result))
    expect_true("sharpe_ratio" %in% names(result))
    expect_true(is.numeric(result$sharpe_ratio))
})
