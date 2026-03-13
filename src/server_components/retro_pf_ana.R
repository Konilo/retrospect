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
}) |>
    # bindCache(
    #     input$retro_pf_ana__submit,
    #     retro_pf_ana__pf_assets_reac(),
    #     input$retro_pf_ana__risk_free_rate,
    #     input$retro_pf_ana__trading_days_per_year
    # ) |>
    bindEvent(input$retro_pf_ana__submit)

retro_pf_ana__kpis <- reactive({
    pf <- retro_pf_ana__pf()
    ma <- pf$merged_assets
    returns <- na.omit(ma[, .(date, portfolio_return)])
    n_trading_days <- as.integer(input$retro_pf_ana__trading_days_per_year)

    if (nrow(returns) < 2) {
        return(list(cagr = NA_real_, volatility = NA_real_))
    }

    n_calendar_days <- as.numeric(
        difftime(returns[.N, date], returns[1, date], units = "days")
    )
    n_years <- n_calendar_days / 365.25

    cum_value <- cumprod(1 + returns[, portfolio_return])
    cagr <- cum_value[length(cum_value)]^(1 / n_years) - 1
    volatility <- sd(returns[, portfolio_return]) * sqrt(n_trading_days)

    list(
        cagr = (cagr * 100) |> signif(3),
        volatility = (volatility * 100) |> signif(3)
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
        )
    )
    plot_ly(
        data = plot_data,
        type = "scatter",
        mode = "lines",
        x = ~date,
        y = ~mean_plus_2_sd_daily_return,
        name = "Mean Daily Return + 2 Std. Dev."
    ) |>
        add_trace(
            y = ~sd_daily_return,
            name = "Daily Returns Std. Dev."
        ) |>
        add_trace(
            y = ~mean_daily_return,
            name = "Mean Daily Return"
        ) |>
        add_trace(
            y = ~mean_minus_2_sd_daily_return,
            name = "Mean Daily Return - 2 Std. Dev."
        ) |>
        layout(
            title = "",
            xaxis = list(title = ""),
            yaxis = list(title = "Daily Return (%)"),
            barmode = "overlay",
            hovermode = "x"
        )
}) |>
    bindEvent(input$retro_pf_ana__submit)

retro_pf_ana__returns_analysis <- reactive({
    retro_pf_ana__pf()$get_prepared_data(
        "returns_analysis",
        "day",
        input$retro_pf_ana__risk_free_rate,
        as.integer(input$retro_pf_ana__trading_days_per_year)
    )
}) |>
    # bindCache(
    #     retro_pf_ana__pf_assets_reac(),
    #     input$retro_pf_ana__daily_returns_distrib_date_range,
    #     input$retro_pf_ana__risk_free_rate,
    #     input$retro_pf_ana__trading_days_per_year
    # ) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__returns_distrib_plot <- renderPlotly({
    plot_ly(
        data = retro_pf_ana__returns_analysis()$returns,
        type = "histogram",
        x = ~return,
        name = "Actual",
        histnorm = "percent",
        xbins = retro_pf_ana__returns_analysis()$plotly_x_bins
    ) |>
        add_trace(
            data = retro_pf_ana__returns_analysis()$normal_return_freqs,
            x = ~return,
            y = ~frequency,
            name = "Normal",
            type = "scatter",
            mode = "lines+markers",
            histnorm = NULL,
            xbins = NULL
        ) |>
        layout(
            title = paste0(
                retro_pf_ana__returns_analysis()$returns[, min(date)],
                " to ",
                retro_pf_ana__returns_analysis()$returns[, max(date)],
                ", N = ", retro_pf_ana__returns_analysis()$returns[, .N]
            ),
            xaxis = list(title = "Daily Return (%)"),
            yaxis = list(title = "Frequency (%)"),
            barmode = "overlay",
            hovermode = "x"
        )
}) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__returns_distrib_mean <- renderText({
    paste(retro_pf_ana__returns_analysis()$mean, "%")
}) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__returns_distrib_sd <- renderText({
    paste(retro_pf_ana__returns_analysis()$sd, "% points")
}) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__returns_distrib_normality_test <- renderText({
    retro_pf_ana__returns_analysis()$normality_test
}) |>
    bindEvent(input$retro_pf_ana__submit)

output$retro_pf_ana__returns_distrib_sharpe_ratio <- renderText({
    retro_pf_ana__returns_analysis()$sharpe_ratio
}) |>
    bindEvent(input$retro_pf_ana__submit)
