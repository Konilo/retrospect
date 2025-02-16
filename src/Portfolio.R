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
        assets_adjusted_return_colnames = NULL,
        metrics = NULL,
        initialize = function(
            weighted_assets_list,
            from = NULL, to = NULL,
            risk_free_rate
        ) {
            stopifnot(
                sum(sapply(weighted_assets_list, function(x) x[[1]])) == 1
            )
            stopifnot(
                all(sapply(weighted_assets_list, function(x) x[[1]]) >= 0)
            )
            stopifnot(
                is.null(from) | as_date(from) == from,
                is.null(to) | as_date(to) == to
            )

            self$weighted_assets_list <- weighted_assets_list
            self$from <- from
            self$to <- to
            self$risk_free_rate <- risk_free_rate
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
                        "weighted_return"
                    ),
                    with = FALSE
                ],
                measure.vars = c(
                    self$assets_adjusted_return_colnames,
                    "weighted_return"
                ),
                variable.name = "asset",
                value.name = "daily_return"
            )[
                ,
                `:=`(
                    asset = gsub(
                        "weighted_return",
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
        }
    ),
    private = list(
        merge_assets = function(self) {
            merged_assets <- Reduce(
                function(dt1, dt2) {
                    merge(dt1, dt2, by = "date", all = FALSE)
                },
                lapply(self$weighted_assets_list, function(x) x[[2]]$ohlcv)
            )

            merged_assets <- merged_assets[
                ,
                weighted_return := rowSums(
                    .SD * sapply(self$weighted_assets_list, function(x) x[[1]])
                ),
                .SDcols = grep(
                    ".Return", colnames(merged_assets),
                    value = TRUE
                )
            ]

            if (!is.null(self$from)) {
                merged_assets <- merged_assets[
                    get(
                        self$weighted_assets_list[[1]][[2]]$colnames_map[["date"]]
                    ) >= self$from,
                ]
            }
            if (!is.null(self$to)) {
                merged_assets <- merged_assets[
                    get(
                        self$weighted_assets_list[[1]][[2]]$colnames_map[["date"]]
                    ) <= self$to,
                ]
            }

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
                        "weighted_return"
                    ),
                    with = FALSE
                ],
                measure.vars = c(
                    self$assets_adjusted_return_colnames,
                    "weighted_return"
                ),
                variable.name = "asset",
                value.name = "daily_return"
            )[
                ,
                `:=`(
                    asset = gsub(
                        "weighted_return",
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


# source("src/Asset.R")

# cw8_pa <- Asset$new("CW8.PA")
# # cw8_pa$ohlcv
# # cw8_pa$plot_ohlc()
# # cw8_pa$plot_returns_distribution()
# # cw8_pa$test_normality()
# # cw8_pa$plot_annual_metrics()
# btc_usd <- Asset$new("BTC-USD")
# gc_f <- Asset$new("GC=F")
# # gc_f$plot_ohlc()
# # gc_f$plot_annual_metrics()
# ese_pa <- Asset$new("ESE.PA")

# wal <- list(
#     c(.85, cw8_pa), c(.05, btc_usd), c(.05, gc_f), c(.05, ese_pa)
# )
# wal <- list(
#     c(.9, cw8_pa), c(.05, btc_usd), c(.05, gc_f)
# )

# pf <- Portfolio$new(wal)
# # str(pf$merged_assets)
# # pf$plot_returns_correlation()
# pf$plot_returns_distribution("assets")
