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
        get_assets_price_comparison = function() {
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

            cols_to_keep <- grep(
                "(_price_pct_change|date)$",
                colnames(comparison_dt),
                value = TRUE
            )
            comparison_dt <- comparison_dt[, ..cols_to_keep]
            comparison_dt <- melt(
                comparison_dt,
                id.vars = 1,
                variable.name = "asset",
                value.name = "price_pct_change"
            )
            comparison_dt[, asset := gsub("_price_pct_change", "", asset)]
            na.omit(comparison_dt)
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
            if (!is.null(self$from)) {
                merged_assets <- merged_assets[
                    get(
                        self$weighted_assets_list[[1]][[2]]$colnames_map[[
                            "date"
                        ]]
                    ) >= self$from,
                ]
            }
            if (!is.null(self$to)) {
                merged_assets <- merged_assets[
                    get(
                        self$weighted_assets_list[[1]][[2]]$colnames_map[[
                            "date"
                        ]]
                    ) <= self$to,
                ]
            }

            # Compute the weighted PF return
            merged_assets[
                ,
                portfolio_weighted_return := rowSums(
                    .SD * sapply(self$weighted_assets_list, function(x) x[[1]])
                ),
                .SDcols = grep(".Return", colnames(merged_assets), value = TRUE)
            ]
        }
    )
)
