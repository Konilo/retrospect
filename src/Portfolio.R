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
        get_plot_data = function(data_type, time_unit) {
            # The date range is applied at initialization

            # Checks
            stopifnot(
                data_type %in% c(
                    "assets_price_comparison",
                    "assets_returns",
                    "mean_sd_over_time"
                ),
                time_unit %in% c("day", "week", "month", "quarter", "year")
            )
            if (
                data_type %chin% c(
                    "assets_price_comparison", "assets_returns"
                ) &&
                    time_unit != "day"
            ) {
                stop(paste(
                    "Only 'day' time_unit is allowed for data_type",
                    "'assets_price_comparison'"
                ))
            }
            if (!time_unit %in% c(
                "day", "week", "quarter", "halfyear", "month", "year"
            )) {
                stop("Invalid time_unit")
            }

            switch(data_type,
                "assets_price_comparison" =
                    private$get_assets_price_comparison(self),
                "assets_returns" =
                    private$get_assets_returns(self),
                "mean_sd_over_time" =
                    private$get_mean_sd_over_time(self, time_unit)
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
            merged_assets[
                ,
                portfolio_return := rowSums(mapply(`*`, .SD, asset_weights)),
                .SDcols = grep(
                    ".Return", colnames(merged_assets), value = TRUE
                )
            ]
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
                        ) * 100) |> round(1)
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
                    mapply(`*`, .SD, asset_weights)
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
        get_assets_returns = function(self) {
            return_cols <- grep(
                ".Return", colnames(self$merged_assets),
                value = TRUE
            )
            cols_to_keep <- c("date", return_cols)
            plot_data <- self$merged_assets[, ..cols_to_keep]
            setnames(
                plot_data,
                return_cols,
                lapply(
                    self$weighted_assets_list,
                    function(x) x[[2]]$ticker
                ) |> unlist()
            )
            na.omit(plot_data)
        },
        get_mean_sd_over_time = function(self, time_unit) {
            plot_data <- copy(self$merged_assets)
            plot_data <- plot_data[, .(date, portfolio_return)] |> na.omit()
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
        }
    )
)
