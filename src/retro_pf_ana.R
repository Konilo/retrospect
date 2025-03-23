library(shiny)
library(data.table)
library(plotly)
library(DT)
library(GGally)
source("Asset.R")
source("Portfolio.R")


# Initial PF
retro_pf_ana__pf_assets_reac <- reactiveVal(data.table(
    ticker = c("CW8.PA", "IGLN.L", "BTC-USD"),
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

output$retro_pf_ana__assets_price_comp_plot <- renderPlotly({
    plot_data <- retro_pf_ana__pf()$get_assets_price_comparison()

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

library(xtable)
output$retro_pf_ana__assets_cor_matrix <- renderUI({
    cor_matrix <- retro_pf_ana__pf()$analyze_assets_correlation()[[
        "cor_matrix"
    ]]

    M <- print(
        xtable(
            cor_matrix,
            align = rep("c", ncol(cor_matrix) + 1),
            digits = 3
        ),
        floating = FALSE,
        tabular.environment = "array",
        comment = FALSE,
        print.results = FALSE
    )
    html <- paste0("$$", M, "$$")
    list(withMathJax(HTML(html)))
})

output$retro_pf_ana__assets_cor_pval_matrix <- renderUI({
    cor_pval_matrix <- retro_pf_ana__pf()$analyze_assets_correlation()[[
        "cor_pval_matrix"
    ]]

    M <- print(
        xtable(
            cor_pval_matrix,
            align = rep("c", ncol(cor_pval_matrix) + 1),
            digits = 20
        ),
        floating = FALSE,
        tabular.environment = "array",
        comment = FALSE,
        print.results = FALSE
    )
    html <- paste0("$$", M, "$$")
    list(withMathJax(HTML(html)))
})

output$retro_pf_ana__assets_cor_splom <- renderPlotly({
    asset_daily_returns <- retro_pf_ana__pf()$analyze_assets_correlation()[[
        "assets_daily_returns"
    ]]

    dimensions_list <- lapply(
        colnames(asset_daily_returns)[-1],
        function(col) {
            list(
                label = col,
                values = asset_daily_returns[[col]]
            )
        }
    )

    p <- plot_ly(
        data = asset_daily_returns,
        type = "splom",
        dimensions = dimensions_list,
        text = ~date
    ) |>
        layout(
            title = "Assets Daily Returns",
            hovermode = "closest",
            dragmode = "select",
            plot_bgcolor = "rgba(240, 240, 240, 0.95)",
            xaxis = list(
                domain = NULL, showline = FALSE, zeroline = FALSE,
                gridcolor = "#ffff", ticklen = 4
            ),
            yaxis = list(
                domain = NULL, showline = FALSE, zeroline = FALSE,
                gridcolor = "#ffff", ticklen = 4
            )
        ) |>
        style(
            # diagonal = list(visible = FALSE),
            showupperhalf = FALSE
        )

    # Apply the axis settings dynamically
    axis <- list(
        showline = FALSE,
        zeroline = FALSE,
        gridcolor = "#ffff",
        ticklen = 4
    )
    axis_settings <- list()
    for (i in seq_along(dimensions_list)) {
        axis_settings[[paste0("xaxis", i)]] <- axis
        axis_settings[[paste0("yaxis", i)]] <- axis
    }
    p |> layout(axis_settings)
})
