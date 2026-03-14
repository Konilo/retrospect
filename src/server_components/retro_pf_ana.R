library(shiny)
library(data.table)
library(plotly)
library(DT)
library(GGally)
source("core_utils/Asset.R")
source("core_utils/Portfolio.R")


# Initial PF
retro_pf_ana__pf_assets_reac <- reactiveVal(data.table(
    ticker = c("CW8.PA", "IGLN.L", "BTC-EUR"),
    weight = c(90, 5, 5)
))

# Add PF asset
observeEvent(input$retro_pf_ana__add_asset, {
    new_row <- data.table(
        ticker = "",
        weight = 0
    )
    retro_pf_ana__pf_assets_reac(rbind(retro_pf_ana__pf_assets_reac(), new_row))
})

# Remove PF asset
observeEvent(input$retro_pf_ana__remove_asset, {
    retro_pf_ana__pf_assets_reac(retro_pf_ana__pf_assets_reac()[-.N])
})

# Keep the underlying asset data.table up-to-date with user edits
observeEvent(input$retro_pf_ana__pf_assets_cell_edit, {
    cell_edit <- input$retro_pf_ana__pf_assets_cell_edit
    retro_pf_ana__pf_assets_reac(
        editData(
            retro_pf_ana__pf_assets_reac(),
            cell_edit,
            rownames = FALSE
        )
    )
})

output$retro_pf_ana__pf_assets <- renderDT(
    retro_pf_ana__pf_assets_reac(),
    editable = TRUE,
    rownames = FALSE,
    colnames = c("Ticker", "Weight (%)"),
    options = list(
        pageLength = -1,
        dom = "t",
        ordering = FALSE
    )
)

retro_pf_ana__pf <- reactive({
    tryCatch(
        {
            weighted_assets_list <- apply(
                X = retro_pf_ana__pf_assets_reac(),
                MARGIN = 1,
                FUN = function(row) {
                    c(
                        as.numeric(row[["weight"]]) / 100,
                        Asset$new(row[["ticker"]], "yahoo")
                    )
                }
            )
            Portfolio$new(
                weighted_assets_list,
                from = input$retro_pf_ana__date_range[1],
                to = input$retro_pf_ana__date_range[2],
                risk_free_rate = input$retro_pf_ana__risk_free_rate / 100
            )
        },
        error = function(e) {
            validate(paste(
                "Could not load portfolio. Check ticker symbols and",
                "ensure weights sum to 100%."
            ))
        }
    )
}) |>
    bindCache(
        retro_pf_ana__pf_assets_reac(),
        input$retro_pf_ana__date_range,
        input$retro_pf_ana__risk_free_rate,
        input$retro_pf_ana__trading_days_per_year
    ) |>
    bindEvent(input$retro_pf_ana__submit)

retro_pf_ana__kpis <- reactive({
    pf <- retro_pf_ana__pf()
    ma <- pf$merged_assets
    returns <- na.omit(ma[, .(date, portfolio_return)])
    n_trading_days <- as.integer(input$retro_pf_ana__trading_days_per_year)

    if (nrow(returns) < 2) {
        return(list(
            cagr = NA_real_, volatility = NA_real_, max_drawdown = NA_real_
        ))
    }

    n_calendar_days <- as.numeric(
        difftime(returns[.N, date], returns[1, date], units = "days")
    )
    n_years <- n_calendar_days / 365.25

    risk_free_rate <- input$retro_pf_ana__risk_free_rate / 100

    cum_value <- cumprod(1 + returns[, portfolio_return])
    cagr <- cum_value[length(cum_value)]^(1 / n_years) - 1
    volatility <- sd(returns[, portfolio_return]) * sqrt(n_trading_days)
    max_drawdown <- min(cum_value / cummax(cum_value) - 1)
    daily_rf <- risk_free_rate / n_trading_days
    excess_return_sd <- sd(returns[, portfolio_return] - daily_rf)
    sharpe_ratio <- (
        (mean(returns[, portfolio_return]) - daily_rf) /
            excess_return_sd * sqrt(n_trading_days)
    )

    list(
        cagr = (cagr * 100) |> signif(3),
        volatility = (volatility * 100) |> signif(3),
        max_drawdown = (max_drawdown * 100) |> signif(3),
        sharpe_ratio = sharpe_ratio |> signif(3)
    )
}) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__cagr <- renderText({
    paste(retro_pf_ana__kpis()$cagr, "%")
}) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__volatility <- renderText({
    paste(retro_pf_ana__kpis()$volatility, "%")
}) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__max_drawdown <- renderText({
    paste(retro_pf_ana__kpis()$max_drawdown, "%")
}) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__sharpe_ratio <- renderText({
    retro_pf_ana__kpis()$sharpe_ratio
}) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__per_asset_kpis <- renderDT({
    pf <- retro_pf_ana__pf()
    n_trading_days <- as.integer(input$retro_pf_ana__trading_days_per_year)

    kpi_rows <- lapply(pf$weighted_assets_list, function(x) {
        weight <- x[[1]]
        asset <- x[[2]]
        ohlcv <- asset$ohlcv
        if (!is.null(pf$from)) ohlcv <- ohlcv[date >= pf$from]
        if (!is.null(pf$to)) ohlcv <- ohlcv[date <= pf$to]

        prices <- ohlcv[[asset$colnames_map[["adjusted_close"]]]]
        returns <- ohlcv[[asset$colnames_map[["return"]]]]

        if (length(prices) < 2) {
            return(data.table(
                Ticker = asset$ticker,
                `Weight (%)` = (weight * 100) |> round(1),
                `CAGR (%)` = NA_real_,
                `Volatility (%)` = NA_real_,
                `Max Drawdown (%)` = NA_real_
            ))
        }

        dates <- ohlcv[["date"]]
        n_years <- as.numeric(
            difftime(max(dates), min(dates), units = "days")
        ) / 365.25

        cagr <- (prices[length(prices)] / prices[1])^(1 / n_years) - 1
        vol <- sd(returns) * sqrt(n_trading_days)
        max_dd <- min(prices / cummax(prices) - 1)

        data.table(
            Ticker = asset$ticker,
            `Weight (%)` = (weight * 100) |> round(1),
            `CAGR (%)` = (cagr * 100) |> signif(3),
            `Volatility (%)` = (vol * 100) |> signif(3),
            `Max Drawdown (%)` = (max_dd * 100) |> signif(3)
        )
    })

    rbindlist(kpi_rows)
},
    rownames = FALSE,
    options = list(
        pageLength = -1,
        dom = "t",
        ordering = FALSE
    )
) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__assets_price_comp_plot <- renderPlotly({
    plot_data <- retro_pf_ana__pf()$get_prepared_data(
        "assets_price_comparison",
        "day"
    )

    # Avoiding the warning from brewer.pal() when n < 3
    n_assets <- plot_data[, uniqueN(asset)]
    if (plot_data[, uniqueN(asset)] < 3) {
        colors <- c("#66C2A5", "#8DA0CB")[1:n_assets]
    } else {
        colors <- RColorBrewer::brewer.pal(n_assets, "Set2")
    }
    plot_ly(
        data = plot_data,
        type = "scatter",
        mode = "lines",
        x = ~date,
        y = ~price_pct_change,
        color = ~asset,
        colors = colors
    ) |>
        layout(
            title = "",
            xaxis = list(title = ""),
            yaxis = list(title = "Price Change (%)"),
            hovermode = "x"
        )
})

output$retro_pf_ana__return_per_time_unit_plot <- renderPlotly({
    plot_data <- retro_pf_ana__pf()$get_prepared_data(
        "return_per_time_unit",
        input$retro_pf_ana__time_unit
    )

    plot_ly(
        data = plot_data,
        type = "bar",
        x = ~date,
        y = ~portfolio_pct_return
    ) |>
        layout(
            title = "",
            xaxis = list(title = ""),
            yaxis = list(title = "Return (%)"),
            hovermode = "x"
        )
}) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__drawdown_plot <- renderPlotly({
    plot_data <- retro_pf_ana__pf()$get_prepared_data(
        "drawdown",
        "day"
    )

    plot_ly(
        data = plot_data,
        type = "scatter",
        mode = "lines",
        x = ~date,
        y = ~drawdown
    ) |>
        layout(
            title = "",
            xaxis = list(title = ""),
            yaxis = list(title = "Drawdown (%)"),
            hovermode = "x"
        )
})
output$retro_pf_ana__assets_cor_splom <- renderPlotly({
    plot_data <- retro_pf_ana__pf()$get_prepared_data(
        "assets_returns", "day"
    )[, -"date"]

    validate(need(nrow(plot_data) >= 2, "Not enough overlapping data."))

    ggpairs(
        plot_data,
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("points", alpha = 0.3)),
        diag = list(continuous = wrap("barDiag"))
    ) |>
        ggplotly()
})

output$retro_pf_ana__dl_assets_price_comp <- downloadHandler(
    filename = function() "portfolio_assets_performance.csv",
    content = function(file) {
        data <- retro_pf_ana__pf()$get_prepared_data(
            "assets_price_comparison", "day"
        )
        fwrite(data, file)
    }
)

output$retro_pf_ana__dl_return_per_time_unit <- downloadHandler(
    filename = function() "portfolio_return_per_time_unit.csv",
    content = function(file) {
        data <- retro_pf_ana__pf()$get_prepared_data(
            "return_per_time_unit", input$retro_pf_ana__time_unit
        )
        fwrite(data, file)
    }
)

output$retro_pf_ana__dl_drawdown <- downloadHandler(
    filename = function() "portfolio_drawdown.csv",
    content = function(file) {
        data <- retro_pf_ana__pf()$get_prepared_data("drawdown", "day")
        fwrite(data, file)
    }
)

output$retro_pf_ana__dl_ann_vol <- downloadHandler(
    filename = function() "portfolio_ann_vol_per_time_unit.csv",
    content = function(file) {
        data <- retro_pf_ana__pf()$get_prepared_data(
            "mean_sd_over_time",
            ifelse(
                input$retro_pf_ana__time_unit == "day",
                "week",
                input$retro_pf_ana__time_unit
            ),
            n_trading_days_per_year = as.integer(
                input$retro_pf_ana__trading_days_per_year
            )
        )
        fwrite(data, file)
    }
)

output$retro_pf_ana__returns_distrib_per_time_unit_plot <- renderPlotly({
    plot_data <- retro_pf_ana__pf()$get_prepared_data(
        "mean_sd_over_time",
        # Mean & SD of daily returns don't make sense at the scale of days
        # Defaulting to the next time unit so that the single time unit
        # input doesn't cause issues
        ifelse(
            input$retro_pf_ana__time_unit == "day",
            "week",
            input$retro_pf_ana__time_unit
        ),
        n_trading_days_per_year = as.integer(
            input$retro_pf_ana__trading_days_per_year
        )
    )
    plot_ly(data = plot_data, x = ~date) |>
        add_trace(
            y = ~mean_plus_2_sd,
            type = "scatter", mode = "lines",
            line = list(width = 0),
            name = "Mean + 2 Vol.",
            showlegend = FALSE
        ) |>
        add_trace(
            y = ~mean_minus_2_sd,
            type = "scatter", mode = "lines",
            fill = "tonexty",
            fillcolor = "rgba(68,119,170,0.15)",
            line = list(width = 0),
            name = "Mean \u00b1 2 Vol."
        ) |>
        add_trace(
            y = ~mean_ann_return,
            type = "scatter", mode = "lines",
            line = list(color = "rgb(68,119,170)", width = 2),
            name = "Ann. Mean Return"
        ) |>
        add_trace(
            y = ~ann_volatility,
            type = "scatter", mode = "lines",
            line = list(
                color = "rgb(221,132,56)", width = 2, dash = "dash"
            ),
            name = "Ann. Volatility"
        ) |>
        layout(
            title = "",
            xaxis = list(title = ""),
            yaxis = list(title = "Annualized (%)"),
            hovermode = "x"
        )
}) |>
    bindEvent(input$retro_pf_ana__submit)

