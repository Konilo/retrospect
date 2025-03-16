library(shiny)
source("Asset.R")
library(data.table)
library(plotly)
library(DT)

# Initial PF
retro_pf_ana__pf_assets_reac <- reactiveVal(data.table(
    ticker = c("CW8.PA", "BTC-USD"),
    weight = c(95, 5)
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

output$retro_pf_ana__pf_assets <- renderDT(
    {
        retro_pf_ana__pf_assets_reac()
    },
    editable = TRUE,
    rownames = FALSE,
    colnames = c("Ticker", "Weight (%)"),
    options = list(pageLength = -1, dom = "t")
)
