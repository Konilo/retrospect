library(data.table)
library(lubridate)
library(R6)
library(plotly)
library(GGally)
source("src/Asset.R")


Portfolio <- R6Class("Portfolio",
    public = list(
        weighted_assets_list = NULL,
        from = NULL,
        to = NULL,
        risk_free_rate = NULL,
        merged_assets = NULL,
        assets_adjusted_return_colnames = NULL,
        metrics = NULL,
        reference_asset_ticker = NULL,
        initialize = function(
            weighted_assets_list,
            from = NULL, to = NULL,
            risk_free_rate,
            reference_asset_ticker
        ) {
            stopifnot(
                sum(sapply(weighted_assets_list, function(x) x[[1]])) == 1
            )
            stopifnot(
                all(sapply(weighted_assets_list, function(x) x[[1]]) > 0)
            )
            stopifnot(
                is.null(from) | as_date(from) == from,
                is.null(to) | as_date(to) == to
            )

            self$weighted_assets_list <- weighted_assets_list
            self$from <- from
            self$to <- to
            self$risk_free_rate <- risk_free_rate
            self$reference_asset_ticker <- reference_asset_ticker
            self$merged_assets <- private$merge_assets(self)
            self$assets_adjusted_return_colnames <- grep(
                ".Return", colnames(self$merged_assets),
                value = TRUE
            )
            self$metrics <- private$get_metrics(self)
        },
        plot_returns_correlation = function() {
            stopifnot(length(self$assets_adjusted_return_colnames) >= 2)

            gg <- ggpairs(
                self$merged_assets[
                    ,
                    self$assets_adjusted_return_colnames,
                    with = FALSE
                ],
                title = "Portfolio Asset Daily Returns Correlation"
            )
            ggplotly(gg)
        },
        plot_returns_distribution = function() {
            plot_data <- melt(
                self$merged_assets[
                    ,
                    c(
                        self$assets_adjusted_return_colnames,
                        "portfolio_weighted_return"
                    ),
                    with = FALSE
                ],
                measure.vars = c(
                    self$assets_adjusted_return_colnames,
                    "portfolio_weighted_return"
                ),
                variable.name = "asset",
                value.name = "daily_return"
            )[
                ,
                `:=`(
                    asset = gsub(
                        "portfolio_weighted_return",
                        "Portfolio",
                        gsub(".Return", "", asset, fixed = TRUE)
                    ),
                    daily_return = daily_return * 100
                )
            ]

            gg <- ggplot(
                plot_data,
                aes(x = daily_return, color = asset)
            ) +
                geom_density(alpha = 1, n = 200) +
                scale_y_continuous(expand = c(0, 0)) +
                theme_minimal()

            ggplotly(gg) |>
                layout(
                    title = "Portfolio Assets Daily Returns Distribution",
                    xaxis = list(title = "Daily Return (%)"),
                    yaxis = list(title = "Density"),
                    legend = list(title = "")
                )
        },
        plot_returns_over_time = function() {
            plot_data <- melt(
                self$merged_assets[
                    ,
                    c(
                        self$assets_adjusted_return_colnames,
                        "portfolio_weighted_return",
                        self$weighted_assets_list[[1]][[2]]$colnames_map[["date"]]
                    ),
                    with = FALSE
                ],
                measure.vars = c(
                    self$assets_adjusted_return_colnames,
                    "portfolio_weighted_return"
                ),
                id.vars = self$weighted_assets_list[[1]][[2]]$colnames_map[["date"]],
                variable.name = "asset",
                value.name = "daily_return"
            )[
                ,
                `:=`(
                    asset = gsub(
                        "portfolio_weighted_return",
                        "Portfolio",
                        gsub(".Return", "", asset, fixed = TRUE)
                    ),
                    daily_return = daily_return * 100
                )
            ]
            gg <- ggplot(
                plot_data,
                aes(
                    x = get(self$weighted_assets_list[[1]][[2]]$colnames_map[["date"]]),
                    y = daily_return,
                    color = asset
                )
            ) +
                geom_path(stat = "identity") +
                facet_grid(asset ~ .) +
                theme_minimal() +
                labs(x = "", y = "Daily Return (%)")
            ggplotly(gg) |>
                layout(
                    title = "Portfolio Assets Daily Returns Over Time",
                    xaxis = list(title = ""),
                    yaxis = list(title = ""),
                    showlegend = FALSE
                )
        },
        plot_pf_vs_reference_performance = function() {
            # Compute cumulative returns
            plot_data <- self$merged_assets[
                ,
                c(
                    "date",
                    "portfolio_weighted_return",
                    "reference_asset_return"
                ),
                with = FALSE
            ][
                ,
                `:=`(
                    cumulative_portfolio_return = (cumprod(
                        1 + portfolio_weighted_return
                    ) - 1) * 100,
                    cumulative_reference_asset_return = (cumprod(
                        1 + reference_asset_return
                    ) - 1) * 100
                )
            ]

            plot_ly(
                plot_data,
                type = "scatter",
                mode = "lines",
                x = ~date,
                y = ~cumulative_portfolio_return,
                name = "Portfolio"
            ) |>
                add_trace(
                    y = ~cumulative_reference_asset_return,
                    name = "Reference Asset"
                ) |>
                layout(
                    title = "Portfolio vs. Reference Asset Performance",
                    xaxis = list(title = ""),
                    yaxis = list(title = "Cumulative Return (%)")
                )
        }
    ),
    private = list(
        merge_assets = function(self) {
            # Merge PF assets
            merged_assets <- Reduce(
                function(dt1, dt2) {
                    merge(dt1, dt2, by = "date", all = FALSE)
                },
                lapply(self$weighted_assets_list, function(x) x[[2]]$ohlcv)
            )

            # Merge reference asset
            reference_asset <- Asset$new(self$reference_asset_ticker)
            reference_asset_adjusted_closes <- reference_asset$ohlcv[
                ,
                c(
                    reference_asset$colnames_map[["date"]],
                    reference_asset$colnames_map[["adjusted_close"]]
                ),
                with = FALSE
            ]
            setnames(
                reference_asset_adjusted_closes,
                reference_asset$colnames_map[["adjusted_close"]],
                "reference_asset_adjusted_close"
            )
            merged_assets <- merge(
                merged_assets,
                reference_asset_adjusted_closes,
                by = reference_asset$colnames_map[["date"]],
                all = FALSE
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

            # Recompute the PF assets' returns, as the merge (inner join)
            # deletes a fraction of the close price variation on dates where
            # some assets are missing.
            pf_asset_objects <- lapply(
                self$weighted_assets_list,
                function(x) x[[2]]
            )
            for (asset in pf_asset_objects) {
                merged_assets[
                    ,
                    paste0(asset$ticker, ".Return") := (
                        get(asset$colnames_map[["adjusted_close"]]) /
                            shift(
                                get(asset$colnames_map[["adjusted_close"]]),
                                1
                            ) - 1
                    )
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

            # Compute the reference asset's returns
            merged_assets[
                ,
                reference_asset_return := (
                    reference_asset_adjusted_close /
                        shift(reference_asset_adjusted_close, 1) - 1
                )
            ]
            merged_assets <- merged_assets[-1]

            message(
                "Merged assets cover ",
                nrow(merged_assets),
                " days in the period ranging from ",
                merged_assets[, min(date)], " to ",
                merged_assets[, max(date)]
            )
            merged_assets
        },
        get_metrics = function(self) {
            prep_data <- melt(
                self$merged_assets[
                    ,
                    c(
                        self$assets_adjusted_return_colnames,
                        "portfolio_weighted_return"
                    ),
                    with = FALSE
                ],
                measure.vars = c(
                    self$assets_adjusted_return_colnames,
                    "portfolio_weighted_return"
                ),
                variable.name = "asset",
                value.name = "daily_return"
            )[
                ,
                `:=`(
                    asset = gsub(
                        "portfolio_weighted_return",
                        "Portfolio",
                        gsub(".Return", "", asset, fixed = TRUE)
                    ),
                    daily_return = daily_return * 100
                )
            ]

            prep_data[
                ,
                .(
                    mean = mean(daily_return),
                    sd = sd(daily_return),
                    sharpe_ratio = (
                        # Considering 252 trading days per year in 
                        # self$merged_assets. That's assuming the PF contains at
                        # least one non-continually traded asset.
                        mean(daily_return) - self$risk_free_rate / 252
                    ) / sd(daily_return),
                    shapiro_test = shapiro.test(daily_return)$p.value < 0.05
                ),
                by = asset
            ]
        }
    )
)
