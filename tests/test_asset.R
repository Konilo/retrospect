mock_get_symbols()

test_that("Asset initializes with correct structure", {
    asset <- Asset$new("AAPL", "yahoo")

    expect_equal(asset$ticker, "AAPL")
    expect_s3_class(asset$ohlcv, "data.table")
    expect_true(nrow(asset$ohlcv) > 0)
    expect_true("date" %in% colnames(asset$ohlcv))
    expect_true("AAPL.Adjusted" %in% colnames(asset$ohlcv))
    expect_true("AAPL.Return" %in% colnames(asset$ohlcv))
})

test_that("Asset ohlcv dates are sorted chronologically", {
    asset <- Asset$new("AAPL", "yahoo")
    dates <- asset$ohlcv$date
    expect_true(all(diff(dates) >= 0))
})

test_that("Asset ohlcv has no NA values", {
    asset <- Asset$new("AAPL", "yahoo")
    expect_equal(sum(is.na(asset$ohlcv)), 0)
})

test_that("get_prepared_data rejects invalid data_type", {
    asset <- Asset$new("AAPL", "yahoo")
    expect_error(asset$get_prepared_data("invalid", "day"), "Invalid data_type")
})

test_that("get_prepared_data rejects invalid time_unit", {
    asset <- Asset$new("AAPL", "yahoo")
    expect_error(asset$get_prepared_data("ohlcv", "century"), "Invalid time_unit")
})

test_that("ohlcv data returns correct columns for day", {
    asset <- Asset$new("AAPL", "yahoo")
    data <- asset$get_prepared_data("ohlcv", "day")
    expected_cols <- c(
        "date", "open", "high", "low", "close", "volume",
        "adjusted_close", "return"
    )
    expect_true(all(expected_cols %in% colnames(data)))
})

test_that("ohlcv data aggregates correctly for month", {
    asset <- Asset$new("AAPL", "yahoo")
    daily <- asset$get_prepared_data("ohlcv", "day")
    monthly <- asset$get_prepared_data("ohlcv", "month")
    expect_true(nrow(monthly) < nrow(daily))
    expect_true(all(c("date", "open", "high", "low", "adjusted_close") %in%
        colnames(monthly)))
})

test_that("date_range filters data correctly", {
    asset <- Asset$new("AAPL", "yahoo")
    range <- c(as.Date("2024-06-01"), as.Date("2024-06-30"))
    data <- asset$get_prepared_data("ohlcv", "day", range)
    expect_true(all(data$date >= range[1]))
    expect_true(all(data$date <= range[2]))
})

test_that("return_per_time_unit returns date and return columns", {
    asset <- Asset$new("AAPL", "yahoo")
    data <- asset$get_prepared_data("return_per_time_unit", "month")
    expect_true(all(c("date", "return") %in% colnames(data)))
    expect_true(nrow(data) > 0)
    expect_false(any(is.na(data$return)))
})

test_that("drawdown values are non-positive", {
    asset <- Asset$new("AAPL", "yahoo")
    data <- asset$get_prepared_data("drawdown", "day")
    expect_true(all(c("date", "drawdown") %in% colnames(data)))
    expect_true(all(data$drawdown <= 0))
})

test_that("mean_sd_over_time returns annualized metrics", {
    asset <- Asset$new("AAPL", "yahoo")
    data <- asset$get_prepared_data(
        "mean_sd_over_time", "month",
        n_trading_days_per_year = 252
    )
    expected_cols <- c(
        "date", "mean_minus_2_sd", "mean_ann_return",
        "ann_volatility", "mean_plus_2_sd"
    )
    expect_true(all(expected_cols %in% colnames(data)))
    expect_true(all(data$ann_volatility >= 0))
    # Band consistency: mean + 2*vol = mean_plus_2_sd
    expect_equal(
        data$mean_ann_return + 2 * data$ann_volatility,
        data$mean_plus_2_sd
    )
})

test_that("returns_analysis computes Sharpe ratio", {
    asset <- Asset$new("AAPL", "yahoo")
    result <- asset$get_prepared_data(
        "returns_analysis", "day",
        risk_free_rate = 0.03,
        n_trading_days_per_year = 252
    )
    expect_true(is.list(result))
    expect_true("sharpe_ratio" %in% names(result))
    expect_true(is.numeric(result$sharpe_ratio))
    expect_true(is.numeric(result$mean))
    expect_true(is.numeric(result$sd))
})
