library(quantmod)
library(data.table)
library(lubridate)
library(R6)
library(plotly)


Asset <- R6Class("Asset",
    public = list(
        ticker = NULL,
        data_source = NULL,
        colnames_map = NULL,
        ohlcv = NULL,
        initialize = function(ticker, data_source = "yahoo") {
            self$ticker <- ticker
            self$data_source <- data_source
            self$colnames_map <- private$map_colnames(self)
            self$ohlcv <- private$fetch_daily_ohlcv(self)
        },
        get_prepared_data = function(
            data_type,
            time_unit,
            date_range = NULL,
            risk_free_rate = NULL,
            n_trading_days_per_year = NULL
        ) {
            # Checks
            if (
                !data_type %in% c(
                    "ohlcv",
                    "return_per_time_unit",
                    "drawdown",
                    "mean_sd_over_time",
                    "returns_analysis"
                )
            ) {
                stop("Invalid data_type")
            }
            if (!time_unit %in% c(
                "day", "week", "quarter", "halfyear", "month", "year"
            )) {
                stop("Invalid time_unit")
            }
            if (data_type == "drawdown" && time_unit != "day") {
                stop("Drawdowns are only be computed on a daily basis")
            }
            if (data_type == "returns_analysis") {
                if (!is.numeric(risk_free_rate) || risk_free_rate < 0) {
                    stop("Invalid risk_free_rate")
                }
                if (
                    !is.numeric(n_trading_days_per_year) ||
                        n_trading_days_per_year <= 0
                ) {
                    stop("Invalid n_trading_days_per_year")
                }
                if (time_unit != "day") {
                    stop(paste(
                        "Only 'day' time_unit is allowed for data_type",
                        "'returns_analysis'"
                    ))
                }
            }

            switch(
                data_type,
                "ohlcv" = private$get_ohlcv_data(self, time_unit, date_range),
                "return_per_time_unit" = private$get_return_per_time_unit_data(
                    self, time_unit, date_range
                ),
                "drawdown" = private$get_drawdown_data(self, date_range),
                "mean_sd_over_time" = private$get_mean_sd_over_time_data(
                    self, time_unit, date_range
                ),
                "returns_analysis" =
                    private$analyze_returns(
                        self,
                        date_range,
                        risk_free_rate,
                        n_trading_days_per_year
                    )
            )
        }
    ),
    private = list(
        map_colnames = function(self) {
            list(
                "date" = "date",
                "open" = paste0(self$ticker, ".Open"),
                "high" = paste0(self$ticker, ".High"),
                "low" = paste0(self$ticker, ".Low"),
                "close" = paste0(self$ticker, ".Close"),
                "volume" = paste0(self$ticker, ".Volume"),
                "adjusted_close" = paste0(self$ticker, ".Adjusted"),
                "return" = paste0(self$ticker, ".Return")
            )
        },
        fetch_daily_ohlcv = function(self) {
            ohlcv_zoo <- getSymbols(
                self$ticker,
                src = self$data_source,
                return.class = "zoo",
                auto.assign = FALSE
            )
            # Deduplicate index before conversion — crypto tickers
            # (e.g. BTC-EUR) can have duplicate dates, which causes
            # as.data.table to drop date row names in favour of numeric ones.
            ohlcv_zoo <- ohlcv_zoo[!duplicated(index(ohlcv_zoo))]
            ohlcv <- as.data.table(ohlcv_zoo, keep.rownames = "date")

            only_close_subset <- ohlcv[
                get(self$colnames_map[["open"]]) == get(
                    self$colnames_map[["high"]]
                ) &
                    get(self$colnames_map[["high"]]) == get(
                        self$colnames_map[["low"]]
                    ) &
                    get(self$colnames_map[["low"]]) == get(
                        self$colnames_map[["close"]]
                    ),
            ]
            if ((n_days_w_only_close <- only_close_subset[, .N]) > 0) {
                warning(
                    self$ticker, " - Only the close date is known for ",
                    n_days_w_only_close, " days ranging from ",
                    only_close_subset[, min(date)], " to ",
                    only_close_subset[, max(date)]
                )
            }

            ohlcv[
                ,
                self$colnames_map[["date"]] := as_date(
                    get(self$colnames_map[["date"]])
                )
            ]
            message(
                self$ticker, " - Retrieved OHLCV data spans from ",
                ohlcv[, min(date)], " to ", ohlcv[, max(date)]
            )

            ohlcv <- ohlcv[
                ,
                self$colnames_map[["return"]] := (
                    get(self$colnames_map[["adjusted_close"]]) /
                        shift(get(self$colnames_map[["adjusted_close"]])) - 1
                )
            ]

            n_rows_before <- nrow(ohlcv)
            ohlcv <- na.omit(ohlcv)
            if ((n_rows_after <- nrow(ohlcv)) < n_rows_before) {
                message(
                    self$ticker, " - Removed ", n_rows_before - n_rows_after,
                    " rows with missing values"
                )
            }

            # Ensure chronological order
            # It's essential for some logics
            setorderv(ohlcv, self$colnames_map[["date"]])
        },
        get_ohlcv_data = function(self, time_unit, date_range) {
            plot_data <- copy(self$ohlcv)
            setnames(
                plot_data,
                old = unlist(self$colnames_map),
                new = names(self$colnames_map)
            )

            if (time_unit == "day") {
                plot_data[, return := return * 100]
            } else {
                plot_data <- plot_data[
                    ,
                    .(
                        open = .SD[1, open],
                        high = .SD[, max(high)],
                        low = .SD[, min(low)],
                        adjusted_close = .SD[.N, adjusted_close]
                    ),
                    by = .(
                        date = floor_date(
                            date,
                            unit = time_unit,
                            week_start = 1
                        )
                    )
                ]
            }

            if (!is.null(date_range)) {
                plot_data[date %between% date_range]
            } else {
                plot_data
            }
        },
        get_return_per_time_unit_data = function(self, time_unit, date_range) {
            plot_data <- private$get_ohlcv_data(self, time_unit, date_range)
            plot_data[
                ,
                .(
                    date,
                    return = (
                        (adjusted_close / shift(adjusted_close) - 1) * 100
                    ) |> signif(digits = 3)
                )
            ][-1] # Remove the 1st row because it has a NA return
        },
        get_drawdown_data = function(self, date_range) {
            plot_data <- private$get_ohlcv_data(self, "day", date_range)
            plot_data[
                ,
                .(
                    date,
                    drawdown = (
                        (
                            adjusted_close / cummax(adjusted_close) - 1
                        ) * 100
                    ) |> signif(digits = 3)
                )
            ][-1] # Remove the 1st row because it has a NA drawdown
        },
        get_mean_sd_over_time_data = function(self, time_unit, date_range) {
            plot_data <- copy(self$ohlcv)
            if (!is.null(date_range)) {
                plot_data <- plot_data[
                    get(self$colnames_map[["date"]]) %between% date_range
                ]
            }
            plot_data[
                ,
                self$colnames_map[["return"]] := get(
                    self$colnames_map[["return"]]
                ) * 100
            ]
            plot_data <- plot_data[
                ,
                .(
                    mean_daily_return = mean(
                        get(self$colnames_map[["return"]])
                    ),
                    sd_daily_return = sd(get(self$colnames_map[["return"]]))
                ),
                by = .(
                    date = floor_date(
                        get(self$colnames_map[["date"]]),
                        unit = time_unit,
                        week_start = 1
                    )
                )
            ]
            plot_data[
                ,
                .(
                    date,
                    mean_minus_2_sd_daily_return = mean_daily_return - 2 *
                        sd_daily_return,
                    mean_daily_return,
                    sd_daily_return,
                    mean_plus_2_sd_daily_return = mean_daily_return + 2 *
                        sd_daily_return
                )
            ]
        },
        analyze_returns = function(
            self,
            date_range,
            risk_free_rate,
            n_trading_days_per_year
        ) {
            #Checks
            if (!is.numeric(risk_free_rate) || risk_free_rate < 0) {
                stop("Invalid risk_free_rate")
            }
            if (
                !is.numeric(n_trading_days_per_year) ||
                    n_trading_days_per_year <= 0
            ) {
                stop("Invalid n_trading_days_per_year")
            }

            returns <- self$get_prepared_data(
                "ohlcv",
                "day",
                date_range
            )[, .(date, return)]

            mean <- mean(returns[, return])
            sd <- sd(returns[, return])
            normality_test <- shapiro.test(returns[, return])$p.value |>
                signif(digits = 3)

            excess_return_sd <- sd(
                returns[, return] - risk_free_rate / n_trading_days_per_year
            )
            sharpe_ratio <- (
                (
                    mean - risk_free_rate / n_trading_days_per_year
                ) / excess_return_sd * sqrt(n_trading_days_per_year)
            ) |>
                signif(digits = 3)

            n_bins <- 100
            bin_edges <- seq(
                returns[, min(return)],
                returns[, max(return)],
                length.out = n_bins + 1
            )

            plotly_x_bins <- list(
                start = bin_edges[1],
                end = bin_edges[length(bin_edges)],
                size = diff(bin_edges)[1]
            )

            # 1st, pnorm computes cumulative probabilities at each bin edge
            # via the CDF of the normal distribution
            # 2nd, compute bin probabilities
            # = differences between successive cumulative probabilities
            bin_frequencies <- pnorm(bin_edges, mean, sd) |> diff()

            # vector of bin centers
            bin_centers <- (bin_edges[-length(bin_edges)] + bin_edges[-1]) / 2

            normal_return_freqs <- data.table(
                return = bin_centers,
                frequency = bin_frequencies * 100
            )

            list(
                "returns" = returns,
                "mean" = mean |> signif(digits = 3),
                "sd" = sd |> signif(digits = 3),
                "normality_test" = normality_test,
                "normal_return_freqs" = normal_return_freqs,
                "plotly_x_bins" = plotly_x_bins,
                "sharpe_ratio" = sharpe_ratio
            )
        }
    )
)
