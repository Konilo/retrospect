library(data.table)
library(lubridate)
library(R6)
library(plotly)
library(GGally)


Portfolio <- R6Class("Portfolio",
    public = list(
        weighted_assets_list = NULL,
        from = NULL,
        to = NULL,
        risk_free_rate = NULL,
        merged_assets = NULL,
        tickers_weights_list = NULL,
        metrics = NULL,

        initialize = function(
            weighted_assets_list,
            from = NULL, to = NULL,
            risk_free_rate # between 0 and 1, not in %
        ) {
            stopifnot(
                sum(sapply(weighted_assets_list, function(x) x[[1]])) == 1,
                all(sapply(weighted_assets_list, function(x) x[[1]]) > 0),
                is.null(from) | as_date(from) == from,
                is.null(to) | as_date(to) == to
            )

            self$weighted_assets_list <- weighted_assets_list
            self$from <- from
            self$to <- to
            self$risk_free_rate <- risk_free_rate
            self$merged_assets <- private$merge_assets(self)
            self$tickers_weights_list <- setNames(
                sapply(self$weighted_assets_list, function(x) x[[1]]),
                sapply(self$weighted_assets_list, function(x) x[[2]]$ticker)
            )
        },
        get_prepared_data = function(
            data_type,
            time_unit,
            risk_free_rate = NULL,
            n_trading_days_per_year = NULL
        ) {
            # The date range is applied at instantiation

            # Checks
            stopifnot(
                data_type %in% c(
                    "assets_price_comparison",
                    "return_per_time_unit",
                    "drawdown",
                    "assets_returns",
                    "mean_sd_over_time",
                    "returns_analysis"
                ),
                time_unit %in% c("day", "week", "month", "quarter", "year")
            )
            if (!time_unit %in% c(
                "day", "week", "quarter", "halfyear", "month", "year"
            )) {
                stop("Invalid time_unit")
            }
            if (
                data_type %chin% c(
                    "assets_price_comparison", "assets_returns", "drawdown"
                ) &&
                    time_unit != "day"
            ) {
                stop("Only 'day' time_unit is allowed for this data_type")
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

            switch(data_type,
                "assets_price_comparison" =
                    private$get_assets_price_comparison(self),
                "return_per_time_unit" =
                    private$get_return_per_time_unit_data(self, time_unit),
                "drawdown" = private$get_drawdown_data(self),
                "assets_returns" =
                    private$get_assets_returns(self),
                "mean_sd_over_time" =
                    private$get_mean_sd_over_time(self, time_unit),
                "returns_analysis" =
                    private$analyze_returns(
                        self,
                        risk_free_rate,
                        n_trading_days_per_year
                    )
            )
        }
    ),
    private = list(
        merge_assets = function(self) {
            # Merge PF assets
            merged_assets <- Reduce(
                function(dt1, dt2) {
                    merge(dt1, dt2, by = "date", all = TRUE)
                },
                lapply(self$weighted_assets_list, function(x) x[[2]]$ohlcv)
            )

            # Filter by date, if requested
            date_colname <- self$weighted_assets_list[[1]][[2]]$colnames_map[[
                "date"
            ]] # Assuming all assets have the same date colname
            if (!is.null(self$from)) {
                merged_assets <- merged_assets[
                    get(date_colname) >= self$from,
                ]
            }
            if (!is.null(self$to)) {
                merged_assets <- merged_assets[
                    get(date_colname) <= self$to,
                ]
            }

            # Compute the weighted PF return
            asset_weights <- sapply(
                self$weighted_assets_list,
                function(x) x[[1]]
            )
            return_cols <- grep(
                ".Return", colnames(merged_assets), value = TRUE
            )
            if (length(return_cols) > 0 && nrow(merged_assets) > 0) {
                merged_assets[
                    ,
                    portfolio_return := rowSums(
                        as.matrix(mapply(`*`, .SD, asset_weights))
                    ),
                    .SDcols = return_cols
                ]
            } else {
                merged_assets[, portfolio_return := numeric(0)]
            }
            merged_assets
        },
        get_assets_price_comparison = function(self) {
            comparison_dt <- copy(self$merged_assets)

            # Only include rows from the earliest complete day
            adj_close_cols <- grep(
                ".Adjusted$",
                colnames(comparison_dt),
                value = TRUE
            )
            complete_rows <- complete.cases(comparison_dt[, ..adj_close_cols])
            if (!any(complete_rows)) {
                return(data.table(
                    date = as.Date(character()),
                    asset = character(),
                    price_pct_change = numeric()
                ))
            }
            earliest_complete_day <- comparison_dt[complete_rows, min(date)]
            comparison_dt <- comparison_dt[date >= earliest_complete_day]

            # Compute the daily price change per asset
            mapply(
                function(asset) {
                    comparison_dt[
                        ,
                        paste0(asset$ticker, "_init_price") :=
                            get(asset$colnames_map[["adjusted_close"]])[1]
                    ]
                    comparison_dt[
                        ,
                        paste0(asset$ticker, "_price_pct_change") := ((
                            get(asset$colnames_map[["adjusted_close"]]) /
                                get(paste0(asset$ticker, "_init_price")) - 1
                        ) * 100) |> round(2)
                    ]
                },
                lapply(
                    self$weighted_assets_list,
                    function(list_element) list_element[[2]]
                )
            )

            # Compute the daily price change for the portfolio (weighted)
            asset_weights <- sapply(
                self$weighted_assets_list,
                function(x) x[[1]]
            )
            price_pct_change_cols <- grep(
                "_price_pct_change$", colnames(comparison_dt),
                value = TRUE
            )
            comparison_dt[
                ,
                portfolio_price_pct_change := rowSums(
                    as.matrix(mapply(`*`, .SD, asset_weights))
                ),
                .SDcols = price_pct_change_cols
            ]

            # Rm no longer needed columns
            cols_to_keep <- grep(
                "(_price_pct_change|date)$",
                colnames(comparison_dt),
                value = TRUE
            )
            comparison_dt <- comparison_dt[, ..cols_to_keep]

            # Wide to long
            comparison_dt <- melt(
                comparison_dt,
                id.vars = 1,
                variable.name = "asset",
                value.name = "price_pct_change"
            )

            # Clean up asset names
            comparison_dt[, asset := gsub("_price_pct_change", "", asset)]
            comparison_dt[, asset := gsub("portfolio", "Portfolio", asset)]

            # Rm rows with NAs, typically when some assets are continually
            # traded and others are not
            na.omit(comparison_dt)
        },
        get_return_per_time_unit_data = function(self, time_unit) {
            # Compute the return per time unit
            plot_data <- copy(self$merged_assets)

            # Only include rows from the earliest complete day
            adj_close_cols <- grep(
                ".Adjusted$",
                colnames(plot_data),
                value = TRUE
            )
            complete_rows <- complete.cases(plot_data[, ..adj_close_cols])
            if (!any(complete_rows)) {
                return(data.table(
                    date = as.Date(character()),
                    portfolio_pct_return = numeric()
                ))
            }
            earliest_complete_day <- plot_data[complete_rows, min(date)]
            plot_data <- plot_data[date >= earliest_complete_day]

            # Compute the assets' returns per time unit
            plot_data <- plot_data[
                ,
                lapply(
                    .SD,
                    function(col) {
                        (
                            col |> na.omit() |> last() /
                                col |> na.omit() |> first() * 100
                        ) - 100
                    }
                ),
                by = .(
                    date = floor_date(
                        date,
                        unit = time_unit,
                        week_start = 1
                    )
                ),
                .SDcols = adj_close_cols
            ]

            # Compute the portfolio's return per time unit by combining the
            # assets' returns
            asset_weights <- sapply(
                self$weighted_assets_list,
                function(x) x[[1]]
            )
            plot_data[
                ,
                portfolio_pct_return := rowSums(
                    as.matrix(mapply(`*`, .SD, asset_weights))
                ) |> round(2),
                .SDcols = adj_close_cols
            ]
        },
        get_drawdown_data = function(self) {
            comparison_dt <- copy(self$merged_assets)

            # Only include rows from the earliest complete day
            adj_close_cols <- grep(
                ".Adjusted$",
                colnames(comparison_dt),
                value = TRUE
            )
            complete_rows <- complete.cases(comparison_dt[, ..adj_close_cols])
            if (!any(complete_rows)) {
                return(data.table(
                    date = as.Date(character()),
                    drawdown = numeric()
                ))
            }
            earliest_complete_day <- comparison_dt[complete_rows, min(date)]
            comparison_dt <- comparison_dt[date >= earliest_complete_day]

            # Compute the daily price change per asset
            mapply(
                function(asset) {
                    comparison_dt[
                        ,
                        paste0(asset$ticker, "_init_price") :=
                            get(asset$colnames_map[["adjusted_close"]])[1]
                    ]
                    comparison_dt[
                        ,
                        paste0(asset$ticker, "_price_pct_change") := ((
                            get(asset$colnames_map[["adjusted_close"]]) /
                                get(paste0(asset$ticker, "_init_price")) - 1
                        ) * 100)
                    ]
                },
                lapply(
                    self$weighted_assets_list,
                    function(list_element) list_element[[2]]
                )
            )

            # Compute the daily price change for the portfolio (weighted)
            asset_weights <- sapply(
                self$weighted_assets_list,
                function(x) x[[1]]
            )
            price_pct_change_cols <- grep(
                "_price_pct_change$", colnames(comparison_dt),
                value = TRUE
            )
            comparison_dt[
                ,
                portfolio_price_pct_change := rowSums(
                    as.matrix(mapply(`*`, .SD, asset_weights))
                ),
                .SDcols = price_pct_change_cols
            ]

            # Remove days during which some assets were not traded
            comparison_dt <- na.omit(comparison_dt)

            comparison_dt[
                ,
                drawdown := (
                    1 - (100 + portfolio_price_pct_change) /
                        (100 + cummax(portfolio_price_pct_change))
                ) * 100 |>
                    signif(3)
            ]

            # No drawdown can be computed before portfolio_price_pct_change
            # has become > 0 at least once
            comparison_dt[
                !is.infinite(drawdown),
                c("date", "drawdown")
            ]
        },
        get_assets_returns = function(self) {
            return_cols <- grep(
                ".Return", colnames(self$merged_assets),
                value = TRUE
            )
            cols_to_keep <- c("date", return_cols)
            plot_data <- self$merged_assets[, ..cols_to_keep]
            tickers <- lapply(
                self$weighted_assets_list,
                function(x) x[[2]]$ticker
            ) |> unlist()
            setnames(plot_data, return_cols, tickers)
            result <- na.omit(plot_data)
            if (nrow(result) < 2) {
                return(data.table(
                    date = as.Date(character()),
                    setNames(
                        replicate(length(tickers), numeric(), simplify = FALSE),
                        tickers
                    )
                ))
            }
            result
        },
        get_mean_sd_over_time = function(self, time_unit) {
            plot_data <- copy(self$merged_assets)
            plot_data <- plot_data[, .(date, portfolio_return)] |> na.omit()
            if (nrow(plot_data) == 0) {
                return(data.table(
                    date = as.Date(character()),
                    mean_minus_2_sd_daily_return = numeric(),
                    mean_daily_return = numeric(),
                    sd_daily_return = numeric(),
                    mean_plus_2_sd_daily_return = numeric()
                ))
            }
            plot_data[
                ,
                portfolio_return := portfolio_return * 100
            ]
            plot_data <- plot_data[
                ,
                .(
                    mean_daily_return = mean(portfolio_return),
                    sd_daily_return = sd(portfolio_return)
                ),
                by = .(
                    date = floor_date(
                        date,
                        unit = time_unit,
                        week_start = 1
                    )
                )
            ] |>
                # sd() returns NA if there's only 1 day in a time unit
                na.omit()
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
            risk_free_rate,
            n_trading_days_per_year
        ) {
            returns <- self$merged_assets[
                ,
                .(
                    date,
                    return = portfolio_return * 100
                )
            ] |>
                na.omit() # Non-trading days for any PF assets are removed

            if (nrow(returns) == 0) {
                return(list(
                    returns = returns,
                    mean = NA_real_,
                    sd = NA_real_,
                    normality_test = NA_real_,
                    normal_return_freqs = data.table(
                        return = numeric(), frequency = numeric()
                    ),
                    plotly_x_bins = list(start = 0, end = 1, size = 0.01),
                    sharpe_ratio = NA_real_
                ))
            }

            n <- nrow(returns)
            mean <- mean(returns[, return])
            sd <- sd(returns[, return])
            normality_test <- if (n >= 3 && n <= 5000) {
                shapiro.test(
                    returns[, return]
                )$p.value |>
                    signif(digits = 3)
            } else {
                NA_real_
            }

            excess_return_sd <- sd(
                returns[, return] - risk_free_rate /
                    n_trading_days_per_year
            )
            sharpe_ratio <- (
                (
                    mean - risk_free_rate / n_trading_days_per_year
                ) / excess_return_sd
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
