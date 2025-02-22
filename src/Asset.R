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
        annual_metrics = NULL,
        initialize = function(ticker, data_source = "yahoo") {
            self$ticker <- ticker
            self$data_source <- data_source
            self$colnames_map <- private$map_colnames(self)
            self$ohlcv <- private$get_ohlcv(self)
            self$annual_metrics <- private$get_annual_metrics(self)

        },
        plot_ohlc = function() {
            plot_ly(
                data = self$ohlcv,
                type = "ohlc",
                x = ~get(self$colnames_map[["date"]]),
                open = ~get(self$colnames_map[["open"]]),
                close = ~get(self$colnames_map[["close"]]),
                high = ~get(self$colnames_map[["high"]]),
                low = ~get(self$colnames_map[["low"]])
            ) |>
                layout(
                    title = paste0(self$ticker, " (", self$data_source, ")"),
                    xaxis = list(
                        rangeslider = list(visible = FALSE),
                        title = ""
                    )
                )
        },
        plot_returns_distribution = function() {
            plot_ly(
                data = self$ohlcv,
                type = "histogram",
                x = ~get(self$colnames_map[["return"]]) * 100
            ) |>
                layout(
                    title = paste0(
                        self$ticker, " (", self$data_source,
                        ") - Returns Distribution"
                    ),
                    xaxis = list(title = "Daily Return (%)"),
                    yaxis = list(title = "# Days")
                )
        },
        test_normality = function() {
            shapiro.test(
                self$ohlcv[, get(self$colnames_map[["return"]])]
            )$p.value < 0.05
        },
        plot_annual_metrics = function() {
            plot_ly(
                data = self$annual_metrics,
                type = "bar",
                x = ~year,
                y = ~annual_return * 100
            ) |>
                layout(
                    title = paste0(
                        self$ticker, " (", self$data_source,
                        ") - Annual Returns"
                    ),
                    xaxis = list(title = "Year"),
                    yaxis = list(title = "Annual Return (%)")
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
        },
        get_annual_metrics = function(self) {
            self$ohlcv[
                ,
                .(
                    annual_return = (
                        .SD[
                            .N, get(self$colnames_map[["adjusted_close"]])
                        ] - .SD[1, get(self$colnames_map[["adjusted_close"]])]
                    ) / .SD[1, get(self$colnames_map[["adjusted_close"]])],
                    annual_daily_returns_sd = sd(
                        .SD[, get(self$colnames_map[["adjusted_close"]])]
                    ),
                    annual_daily_returns_mean = mean(
                        .SD[, get(self$colnames_map[["adjusted_close"]])]
                    )
                ),
                year(get(self$colnames_map[["date"]]))
            ]
        }
    )
)
