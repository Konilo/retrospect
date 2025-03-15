library(shiny)
library(bslib)
library(plotly)
library(lubridate)

ui <- page_navbar(
    title = "Asset Analysis",
    id = "nav",
    navbar_options = navbar_options(
        bg = "#2D89C8",
        theme = "auto"
    ),

    sidebar = sidebar(
        title = "Parameters",
        conditionalPanel(
            "input.nav === 'Retrospective Analysis'",
            textInput(
                "retro_ana__ticker",
                "Ticker",
                value = "AAPL"
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
                "retro_ana__price_plot_type",
                "Price Plot Type",
                choices = list("Candles" = "candles", "Close" = "close"),
                selected = "candles"
            ),
            selectInput(
                "retro_ana__time_unit",
                "Time Unit",
                choices = list(
                    "Day" = "day",
                    "Week" = "week",
                    "Month" = "month",
                    "Year" = "year"
                ),
                selected = "day"
            ),
            dateRangeInput(
                "retro_ana__daily_returns_distrib_date_range",
                "Daily Returns Distribution Date Range",
                start = today() %m-% years(50),
                end = today(),
                weekstart = 1
            ),
            actionButton("retro_ana__submit", "Submit")
        ),
        conditionalPanel(
            "input.nav === 'Prospective Analysis'",
            p("Prospective Analysis sidebar")
        )
    ),

    nav_panel(
        title = "Retrospective Analysis", # aka retro_ana
        card(
            card_header("Asset Price"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(plotlyOutput("retro_ana__price_plot"))
        ),
        card(
            card_header("Asset Return Per Time Unit"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(plotlyOutput("retro_ana__return_per_time_unit_plot"))
        ),
        card(
            card_header("Daily Asset Returns Distribution"),
            full_screen = TRUE,
            fill = FALSE,
            card_body(
                layout_column_wrap(
                    value_box(
                        title = "Mean",
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput(
                            "retro_ana__returns_distribution_mean"
                        )
                    ),
                    value_box(
                        title = "Standard Deviation",
                        min_height = "75px",
                        fill = FALSE,
                        value = textOutput(
                            "retro_ana__returns_distribution_sd"
                        )
                    ),
                    value_box(
                        title = "Shapiro-Wilk Normality Test",
                        min_height = "75px",
                        value = textOutput(
                            "retro_ana__returns_distribution_normality_test"
                        )
                    )
                ),
                plotlyOutput("retro_ana__returns_distribution_plot")
            )
        )
    ),

    nav_panel(
        title = "Prospective Analysis", # aka pro_ana
        p("Second page content.")
    ),

    nav_spacer(),
    nav_menu(
        title = "Links",
        align = "right",
        nav_item(
            tags$a(
                "GitHub",
                href = "https://github.com/Konilo/modern-portfolio-theory"
            ),
            target = "_blank"
        )
    )
)