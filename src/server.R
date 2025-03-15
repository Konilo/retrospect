library(shiny)
source("Asset.R")
library(data.table)
library(plotly)
library(lubridate)


server <- function(input, output) {
    retro_ana__asset <- reactive({
        Asset$new(input$retro_ana__ticker, "yahoo")
    }) |>
        bindCache(input$retro_ana__ticker) |>
        bindEvent(input$retro_ana__submit)

    output$retro_ana__price_plot <- renderPlotly({
        plot_data <- retro_ana__asset()$get_plot_data(
            "ohlcv",
            input$retro_ana__time_unit
        )

        validate(need(
            input$retro_ana__price_plot_type %in% c("candles", "close"),
            "Invalid plot type."
        ))

        switch(input$retro_ana__price_plot_type,
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
                        title = retro_ana__asset()$ticker,
                        xaxis = list(
                            rangeslider = list(visible = FALSE),
                            title = ""
                        ),
                        yaxis = list(title = "Price (OHLC)")
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
                        title = retro_ana__asset()$ticker,
                        xaxis = list(title = ""),
                        yaxis = list(title = "Adjusted Close Price")
                    )
            }
        )
    }) |>
        bindEvent(input$retro_ana__submit)

    output$retro_ana__return_per_time_unit_plot <- renderPlotly({
        plot_data <- retro_ana__asset()$get_plot_data(
            "return_per_time_unit",
            input$retro_ana__time_unit
        )

        plot_ly(
            data = plot_data,
            type = "bar",
            x = ~date,
            y = ~return
        ) |>
            layout(
                title = retro_ana__asset()$ticker,
                xaxis = list(title = ""),
                yaxis = list(title = "Return (%)")
            )
    }) |>
        bindEvent(input$retro_ana__submit)

    output$retro_ana__returns_distribution_plot <- renderPlotly({
        daily_returns_distrib <- retro_ana__asset()$get_daily_returns_distrib(
            input$retro_ana__daily_returns_distrib_date_range
        )

        plot_ly(
            data = daily_returns_distrib$returns,
            type = "histogram",
            x = ~ return,
            name = "Actual",
            histnorm = "percent",
            xbins = daily_returns_distrib$plotly_x_bins
        ) |>
            add_trace(
                data = daily_returns_distrib$normal_return_freqs,
                x = ~ return,
                y = ~ frequency,
                name = "Normal",
                type = "scatter",
                mode = "lines+markers",
                histnorm = NULL,
                xbins = NULL
            ) |>
            layout(
                title = paste0(
                    retro_ana__asset()$ticker, ", ",
                    daily_returns_distrib$returns[, min(date)], " to ",
                    daily_returns_distrib$returns[, max(date)],
                    ", N = ", daily_returns_distrib$returns[, .N]
                ),
                xaxis = list(title = "Daily Return (%)"),
                yaxis = list(title = "Frequency (%)"),
                barmode = "overlay"
            )
    }) |>
        bindEvent(input$retro_ana__submit)

    output$retro_ana__returns_distribution_mean <- renderText({
        retro_ana__asset()$get_daily_returns_distrib(
            input$retro_ana__daily_returns_distrib_date_range
        )$mean
    }) |>
        bindEvent(input$retro_ana__submit)

    output$retro_ana__returns_distribution_sd <- renderText({
        retro_ana__asset()$get_daily_returns_distrib(
            input$retro_ana__daily_returns_distrib_date_range
        )$sd
    }) |>
        bindEvent(input$retro_ana__submit)

    output$retro_ana__returns_distribution_normality_test <- renderText({
        retro_ana__asset()$get_daily_returns_distrib(
            input$retro_ana__daily_returns_distrib_date_range
        )$normality_test
    }) |>
        bindEvent(input$retro_ana__submit)
}