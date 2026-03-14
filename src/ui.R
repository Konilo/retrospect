library(shiny)
library(bslib)
library(plotly)
library(lubridate)
library(DT)

ann_vol_chart_explanation <- tags$details(
    tags$summary(
        style = "cursor: pointer; color: #6c757d; font-size: 0.85em; margin-top: 8px;",
        "How to read this chart"
    ),
    tags$div(
        style = "font-size: 0.85em; color: #6c757d; margin-top: 6px; line-height: 1.5;",
        tags$p(
            "Daily returns within each time unit are used to",
            "compute the following annualized metrics:"
        ),
        tags$ul(
            tags$li(
                tags$strong("Ann. Mean Return"),
                "(solid blue): the annualized equivalent of the",
                "average daily return for this time unit."
            ),
            tags$li(
                tags$strong("Ann. Volatility"),
                "(dashed orange): the standard deviation of daily",
                "returns for this time unit, scaled to a year."
            ),
            tags$li(
                tags$strong("Mean \u00b1 2 Vol. band"),
                "(shaded blue): assuming normally distributed",
                "returns, ~95.45% of daily returns (annualized)",
                "fall within this range. A wider band signals a",
                "more uncertain period."
            )
        ),
        tags$p(
            "Note: annualization assumes returns scale linearly",
            "(additively), which is an approximation \u2014 in",
            "practice, returns compound."
        )
    )
)

ui <- page_navbar(
    title = "Asset Analysis",
    id = "nav",
    navbar_options = navbar_options(
        bg = "#000000",
        theme = "auto"
    ),
    sidebar = sidebar(
        title = "Parameters",
        width = "20%",
        conditionalPanel(
            "input.nav === 'Retrospective Asset Analysis'",
            textInput(
                "retro_asset_ana__ticker",
                "Ticker",
                value = "CW8.PA"
            ),
            helpText(
                "Financial data from ",
                tags$a(
                    "Yahoo Finance",
                    href = "https://finance.yahoo.com",
                    target = "_blank"
                )
            ),
            dateRangeInput(
                "retro_asset_ana__date_range",
                "Date Range",
                start = today() %m-% years(50),
                end = today(),
                weekstart = 1
            ),
            selectInput(
                "retro_asset_ana__price_plot_type",
                "Price Plot Type",
                choices = list(
                    "Candles" = "candles",
                    "Closing Prices" = "close"
                ),
                selected = "candles"
            ),
            selectInput(
                "retro_asset_ana__time_unit",
                "Time Unit",
                choices = list(
                    "Day" = "day",
                    "Week" = "week",
                    "Month" = "month",
                    "Quarter" = "quarter",
                    "Semester" = "halfyear",
                    "Year" = "year"
                ),
                selected = "month"
            ),
            numericInput(
                "retro_asset_ana__risk_free_rate",
                "Risk-free rate",
                3,
                min = 0,
                max = 100
            ),
            helpText("Annual rate in %"),
            selectInput(
                "retro_asset_ana__trading_days_per_year",
                "Trading Days Per Year",
                choices = list(
                    "252 Days (Equities, etc.)" = 252,
                    "365 Days (Crypto)" = 365
                ),
                selected = 252
            ),
            actionButton("retro_asset_ana__submit", "Submit")
        ),
        conditionalPanel(
            "input.nav === 'Retrospective Portfolio Analysis'",
            p("Portfolio assets"),
            DTOutput("retro_pf_ana__pf_assets"),
            fluidRow(
                column(
                    6,
                    actionButton(
                        "retro_pf_ana__add_asset", HTML("Add</br>Asset")
                    )
                ),
                column(
                    6,
                    actionButton(
                        "retro_pf_ana__remove_asset", "Remove Asset"
                    )
                )
            ),
            dateRangeInput(
                "retro_pf_ana__date_range",
                "Date Range",
                start = today() %m-% years(7),
                end = today(),
                weekstart = 1
            ),
            selectInput(
                "retro_pf_ana__time_unit",
                "Time Unit",
                choices = list(
                    "Day" = "day",
                    "Week" = "week",
                    "Month" = "month",
                    "Quarter" = "quarter",
                    "Semester" = "halfyear",
                    "Year" = "year"
                ),
                selected = "month"
            ),
            numericInput(
                "retro_pf_ana__risk_free_rate",
                "Risk-free rate",
                3,
                min = 0,
                max = 100
            ),
            helpText("Annual rate in %"),
            selectInput(
                "retro_pf_ana__trading_days_per_year",
                "Trading Days Per Year",
                choices = list(
                    "252 Days (Equities, etc.)" = 252,
                    "365 Days (Crypto)" = 365
                ),
                selected = 252
            ),
            helpText(
                "Select the minimum number of trading days per year among the",
                " assets"
            ),
            actionButton("retro_pf_ana__submit", "Submit")
        ),
    ),
    nav_panel(
        title = "Retrospective Asset Analysis", # aka retro_asset_ana
        card(
            card_header("Key Performance Indicators"),
            fill = FALSE,
            card_body(
                layout_column_wrap(
                    value_box(
                        title = tooltip(
                            span(
                                "CAGR",
                                style = "text-decoration: underline dotted; cursor: help;"
                            ),
                            "(End / Start)^(1/Years) - 1"
                        ),
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput("retro_asset_ana__cagr")
                    ),
                    value_box(
                        title = tooltip(
                            span(
                                "Annualized Volatility",
                                style = "text-decoration: underline dotted; cursor: help;"
                            ),
                            "SD(daily returns) * sqrt(trading days/year)"
                        ),
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput("retro_asset_ana__volatility")
                    ),
                    value_box(
                        title = "Max Drawdown",
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput("retro_asset_ana__max_drawdown")
                    ),
                    value_box(
                        title = tooltip(
                            span(
                                "Sharpe Ratio",
                                style = "text-decoration: underline dotted; cursor: help;"
                            ),
                            "(Mean - Rf) / SD(excess) * sqrt(trading days/year)"
                        ),
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput("retro_asset_ana__sharpe_ratio")
                    )
                )
            )
        ),
        card(
            card_header("Prices"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(plotlyOutput("retro_asset_ana__price_plot"))
        ),
        card(
            card_header("Return Per Time Unit"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(plotlyOutput(
                "retro_asset_ana__return_per_time_unit_plot"
            ))
        ),
        card(
            card_header("Daily Close Price Drawdowns"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(plotlyOutput("retro_asset_ana__drawdown_plot"))
        ),
        card(
            card_header("Annualized Return & Volatility Per Time Unit"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(
                plotlyOutput(
                    "retro_asset_ana__returns_distrib_per_time_unit_plot"
                ),
                ann_vol_chart_explanation
            )
        ),
    ),
    nav_panel(
        title = "Retrospective Portfolio Analysis", # aka retro_pf_ana
        card(
            card_header("Key Performance Indicators"),
            fill = FALSE,
            card_body(
                layout_column_wrap(
                    value_box(
                        title = tooltip(
                            span(
                                "CAGR",
                                style = "text-decoration: underline dotted; cursor: help;"
                            ),
                            "(End / Start)^(1/Years) - 1"
                        ),
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput("retro_pf_ana__cagr")
                    ),
                    value_box(
                        title = tooltip(
                            span(
                                "Annualized Volatility",
                                style = "text-decoration: underline dotted; cursor: help;"
                            ),
                            "SD(daily returns) * sqrt(trading days/year)"
                        ),
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput("retro_pf_ana__volatility")
                    ),
                    value_box(
                        title = "Max Drawdown",
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput("retro_pf_ana__max_drawdown")
                    ),
                    value_box(
                        title = tooltip(
                            span(
                                "Sharpe Ratio",
                                style = "text-decoration: underline dotted; cursor: help;"
                            ),
                            "(Mean - Rf) / SD(excess) * sqrt(trading days/year)"
                        ),
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput("retro_pf_ana__sharpe_ratio")
                    )
                )
            )
        ),
        card(
            card_header("Per-Asset KPIs"),
            full_screen = TRUE,
            fill = FALSE,
            max_height = "300px",
            card_body(DTOutput("retro_pf_ana__per_asset_kpis"))
        ),
        card(
            card_header("Portfolio & Assets Performance"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(
                plotlyOutput("retro_pf_ana__assets_price_comp_plot")
            )
        ),
        card(
            card_header("Portfolio Return Per Time Unit"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(
                plotlyOutput("retro_pf_ana__return_per_time_unit_plot")
            )
        ),
        card(
            card_header("Portfolio Drawdowns"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(plotlyOutput("retro_pf_ana__drawdown_plot"))
        ),
        card(
            card_header("Portfolio Assets Correlation"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(
                plotlyOutput("retro_pf_ana__assets_cor_splom")
            )
        ),
        card(
            card_header("Portfolio Annualized Return & Volatility Per Time Unit"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(
                plotlyOutput(
                    "retro_pf_ana__returns_distrib_per_time_unit_plot"
                ),
                ann_vol_chart_explanation
            )
        ),
    ),
    tags$script(HTML(
        "$(document).on('shiny:connected', function() {
            $('#retro_asset_ana__submit').click();
        });"
    )),
    nav_spacer(),
    nav_menu(
        title = "Links",
        align = "right",
        nav_item(
            tags$a(
                "GitHub",
                href = "https://github.com/Konilo/modern-portfolio-theory",
                target = "_blank"
            )
        )
    )
)
