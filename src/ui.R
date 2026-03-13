library(shiny)
library(bslib)
library(plotly)
library(lubridate)
library(DT)


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
            dateRangeInput(
                "retro_asset_ana__daily_returns_distrib_date_range",
                "Daily Returns Distribution Date Range",
                start = today() %m-% years(50),
                end = today(),
                weekstart = 1
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
            card_header("Daily Returns Distribution Per Time Unit"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(plotlyOutput(
                "retro_asset_ana__returns_distrib_per_time_unit_plot"
            ))
        ),
        card(
            card_header("Daily Returns Detailed Analysis"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(
                layout_column_wrap(
                    value_box(
                        title = "Mean",
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput(
                            "retro_asset_ana__returns_distrib_mean"
                        )
                    ),
                    value_box(
                        title = "Standard Deviation",
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput(
                            "retro_asset_ana__returns_distrib_sd"
                        )
                    ),
                    value_box(
                        title = "Shapiro-Wilk Normality Test",
                        min_height = "75px",
                        value = textOutput(
                            "retro_asset_ana__returns_distrib_normality_test"
                        )
                    ),
                    value_box(
                        title = tooltip(
                            span(
                                "Sharpe Ratio",
                                style = "text-decoration: underline dotted; cursor: help;"
                            ),
                            "(Mean return - Risk-free rate) / SD(excess returns)"
                        ),
                        min_height = "75px",
                        value = textOutput(
                            "retro_asset_ana__returns_distrib_sharpe_ratio"
                        )
                    )
                ),
                plotlyOutput("retro_asset_ana__returns_distrib_plot")
            )
        )
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
                    )
                )
            )
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
            card_header("Portfolio Daily Returns Distribution Per Time Unit"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(plotlyOutput(
                "retro_pf_ana__returns_distrib_per_time_unit_plot"
            ))
        ),
        card(
            card_header("Portfolio Daily Returns Detailed Analysis"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(
                layout_column_wrap(
                    value_box(
                        title = "Mean",
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput(
                            "retro_pf_ana__returns_distrib_mean"
                        )
                    ),
                    value_box(
                        title = "Standard Deviation",
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput(
                            "retro_pf_ana__returns_distrib_sd"
                        )
                    ),
                    value_box(
                        title = "Shapiro-Wilk Normality Test",
                        min_height = "75px",
                        value = textOutput(
                            "retro_pf_ana__returns_distrib_normality_test"
                        )
                    ),
                    value_box(
                        title = tooltip(
                            span(
                                "Sharpe Ratio",
                                style = "text-decoration: underline dotted; cursor: help;"
                            ),
                            "(Mean return - Risk-free rate) / SD(excess returns)"
                        ),
                        min_height = "75px",
                        value = textOutput(
                            "retro_pf_ana__returns_distrib_sharpe_ratio"
                        )
                    )
                ),
                plotlyOutput("retro_pf_ana__returns_distrib_plot")
            )
        )
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
