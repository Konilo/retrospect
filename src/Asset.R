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
            self$ohlcv <- private$get_ohlcv(self)
        },
        get_plot_data = function(data_type, time_unit, date_range = NULL) {
            if (!data_type %in% c("ohlcv", "return_per_time_unit")) {
                stop("Invalid data_type")
            }
            if (!time_unit %in% c("day", "week", "month", "year")) {
                stop("Invalid time_unit")
            }
            if (!is.null(date_range) && data_type != "ohlcv") {
                stop(
                    "date_range should only be provided if data_type is 'ohlcv'"
                )
            }

            plot_data <- copy(self$ohlcv)
            if (time_unit == "day") {
                setnames(
                    plot_data,
                    old = unlist(self$colnames_map),
                    new = names(self$colnames_map)
                )
                plot_data[, return := return * 100]
            } else if (time_unit %chin% c("week", "month", "year")) {
                plot_data <- plot_data[
                    ,
                    .(
                        open = .SD[1, get(self$colnames_map[["open"]])],
                        high = .SD[, max(get(self$colnames_map[["high"]]))],
                        low = .SD[, min(get(self$colnames_map[["low"]]))],
                        adjusted_close = .SD[
                            .N,
                            get(self$colnames_map[["adjusted_close"]])
                        ]
                    ),
                    by = .(
                        date = floor_date(
                            get(self$colnames_map[["date"]]),
                            unit = time_unit,
                            week_start = 1
                        )
                    )
                ]
            }

            if (data_type == "ohlcv") {
                if (!is.null(date_range)) {
                    plot_data[date %between% date_range]
                } else {
                    plot_data
                }
            } else if (data_type == "return_per_time_unit") {
                plot_data[
                    ,
                    .(
                        date,
                        return = (adjusted_close / shift(adjusted_close) - 1) *
                            100
                    )
                ][-1] # Remove the 1st row because it has a NA return
            }
        },
        get_daily_returns_distrib = function(date_range) {
            returns <- self$get_plot_data(
                "ohlcv",
                "day",
                date_range
            )[, .(date, return)]

            mean <- mean(returns[, return]) |> signif(digits = 3)
            sd <- sd(returns[, return]) |> signif(digits = 3)
            normality_test <- shapiro.test(returns[, return])$p.value |>
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
                "mean" = mean,
                "sd" = sd,
                "normality_test" = normality_test,
                "normal_return_freqs" = normal_return_freqs,
                "plotly_x_bins" = plotly_x_bins
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
        get_ohlcv = function(self) {
            ohlcv <- getSymbols(
                self$ticker,
                src = self$data_source,
                return.class = "zoo",
                auto.assign = FALSE
            ) |>
                # adjustOHLC needed?
                as.data.table(keep.rownames = "date")

            only_close_subset <- ohlcv[
                get(self$colnames_map[["open"]]) == get(self$colnames_map[["high"]]) &
                    get(self$colnames_map[["high"]]) == get(self$colnames_map[["low"]]) &
                    get(self$colnames_map[["low"]]) == get(self$colnames_map[["close"]]),
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

            ohlcv
        }
    )
)
