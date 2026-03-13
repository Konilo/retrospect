library(shiny)
source("core_utils/Asset.R")
library(data.table)
library(plotly)
library(lubridate)


retro_asset_ana__asset <- reactive({
    Asset$new(input$retro_asset_ana__ticker, "yahoo")
}) |>
    # bindCache(input$retro_asset_ana__ticker) |>
    bindEvent(input$retro_asset_ana__submit)

output$retro_asset_ana__price_plot <- renderPlotly({
    plot_data <- retro_asset_ana__asset()$get_prepared_data(
        "ohlcv",
        input$retro_asset_ana__time_unit
    )

    validate(need(
        input$retro_asset_ana__price_plot_type %in% c("candles", "close"),
        "Invalid plot type."
    ))

    switch(input$retro_asset_ana__price_plot_type,
        "candles" = {
            plot_ly(
                data = plot_data,
                type = "candlestick",
                x = ~date,
                open = ~open,
                close = ~adjusted_close,
                high = ~high,
                low = ~low
            ) |>
                layout(
                    title = retro_asset_ana__asset()$ticker,
                    xaxis = list(
                        rangeslider = list(visible = FALSE),
                        title = ""
                    ),
                    yaxis = list(title = "Price (OHLC)"),
                    hovermode = "x"
                )
        },
        "close" = {
            plot_ly(
                data = plot_data,
                type = "scatter",
                mode = "lines",
                x = ~date,
                y = ~adjusted_close
            ) |>
                layout(
                    title = retro_asset_ana__asset()$ticker,
                    xaxis = list(title = ""),
                    yaxis = list(title = "Adjusted Close Price"),
                    hovermode = "x"
                )
        }
    )
}) |>
    bindEvent(input$retro_asset_ana__submit)

output$retro_asset_ana__return_per_time_unit_plot <- renderPlotly({
    plot_data <- retro_asset_ana__asset()$get_prepared_data(
        "return_per_time_unit",
        input$retro_asset_ana__time_unit
    )

    plot_ly(
        data = plot_data,
        type = "bar",
        x = ~date,
        y = ~return
    ) |>
        layout(
            title = retro_asset_ana__asset()$ticker,
            xaxis = list(title = ""),
            yaxis = list(title = "Return (%)"),
            hovermode = "x"
        )
}) |>
    bindEvent(input$retro_asset_ana__submit)

output$retro_asset_ana__drawdown_plot <- renderPlotly({
    plot_data <- retro_asset_ana__asset()$get_prepared_data(
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
            title = retro_asset_ana__asset()$ticker,
            xaxis = list(title = ""),
            yaxis = list(title = "Drawdown (%)"),
            hovermode = "x"
        )
}) |>
    bindEvent(input$retro_asset_ana__submit)

retro_asset_ana__returns_analysis <- reactive({
    retro_asset_ana__asset()$get_prepared_data(
        "returns_analysis",
        "day",
        input$retro_asset_ana__daily_returns_distrib_date_range,
        input$retro_asset_ana__risk_free_rate,
        as.integer(input$retro_asset_ana__trading_days_per_year)
    )
}) |>
    bindCache(
        input$retro_asset_ana__ticker,
        input$retro_asset_ana__daily_returns_distrib_date_range,
        input$retro_asset_ana__risk_free_rate,
        input$retro_asset_ana__trading_days_per_year
    ) |>
    bindEvent(input$retro_asset_ana__submit)

output$retro_asset_ana__returns_distrib_plot <- renderPlotly({
    plot_ly(
        data = retro_asset_ana__returns_analysis()$returns,
        type = "histogram",
        x = ~return,
        name = "Actual",
        histnorm = "percent",
        xbins = retro_asset_ana__returns_analysis()$plotly_x_bins
    ) |>
        add_trace(
            data = retro_asset_ana__returns_analysis()$normal_return_freqs,
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
                retro_asset_ana__asset()$ticker, " (",
                retro_asset_ana__returns_analysis()$returns[, min(date)],
                " to ",
                retro_asset_ana__returns_analysis()$returns[, max(date)],
                ", N = ", retro_asset_ana__returns_analysis()$returns[, .N], ")"
            ),
            xaxis = list(title = "Daily Return (%)"),
            yaxis = list(title = "Frequency (%)"),
            barmode = "overlay",
            hovermode = "x"
        )
}) |>
    bindEvent(input$retro_asset_ana__submit)

output$retro_asset_ana__returns_distrib_mean <- renderText({
    paste(retro_asset_ana__returns_analysis()$mean, "%")
}) |>
    bindEvent(input$retro_asset_ana__submit)

output$retro_asset_ana__returns_distrib_sd <- renderText({
    paste(retro_asset_ana__returns_analysis()$sd, "% points")
}) |>
    bindEvent(input$retro_asset_ana__submit)

output$retro_asset_ana__returns_distrib_normality_test <- renderText({
    retro_asset_ana__returns_analysis()$normality_test
}) |>
    bindEvent(input$retro_asset_ana__submit)

output$retro_asset_ana__returns_distrib_sharpe_ratio <- renderText({
    retro_asset_ana__returns_analysis()$sharpe_ratio
}) |>
    bindEvent(input$retro_asset_ana__submit)

output$retro_asset_ana__returns_distrib_per_time_unit_plot <- renderPlotly({
    plot_data <- retro_asset_ana__asset()$get_prepared_data(
        "mean_sd_over_time",
        # Mean & SD of daily returns don't make sense at the scale of days
        # Defaulting to the next time unit so that the single time unit
        # input doesn't cause issues
        ifelse(
            input$retro_asset_ana__time_unit == "day",
            "week",
            input$retro_asset_ana__time_unit
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
            title = retro_asset_ana__asset()$ticker,
            xaxis = list(title = ""),
            yaxis = list(title = "Daily Return (%)"),
            barmode = "overlay",
            hovermode = "x"
        )
}) |>
    bindEvent(input$retro_asset_ana__submit)
