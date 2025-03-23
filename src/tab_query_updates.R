library(shiny)


# Support linking to a given tab and update the URL to reflect the current tab
is_session_init_update <- reactiveVal(TRUE)

observeEvent(
    {
        list(input$nav, session$clientData$url_search)
    },
    {
        if (is_session_init_update()) {
            # A session's intial update is handled differently to avoid inifite
            # loops. It's the only case where the tab can be updated based on
            # the URL.
            query <- parseQueryString(session$clientData$url_search)

            if (is.null(query$tab)) {
                new_url <- paste0(
                    "?tab=", URLencode(input$nav, reserved = TRUE)
                )
                updateQueryString(new_url, mode = "push")
            } else {
                updateTabsetPanel(session, "nav", selected = query$tab)
            }

            is_session_init_update(FALSE)
        } else {
            # Outisde of the session's initial update, we only update the URL
            # based on the current tab.
            new_url <- paste0(
                "?tab=", URLencode(input$nav, reserved = TRUE)
            )
            updateQueryString(new_url, mode = "push")
        }
    }
)
