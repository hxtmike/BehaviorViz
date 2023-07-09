tabItem(
    tabName = "endingresults",
    # default file_state_page_output
    uiOutput("file_state_page_endingresults"),

    fluidRow(
        column(
            width = 2,
            uiOutput("er_cols_selector")
        ),
        column(
            width = 2,
            uiOutput("er_viztype_selector")
        ),
        column(
            width = 8,
            uiOutput("er_rows_selector")
        )
    ),
    fluidRow(
        column(
            width = 2,
            uiOutput("er_control1")
        ),
        column(
            width = 2,
            uiOutput("er_control2")
        ),
        column(
            width = 8,
            uiOutput("er_mainplot")
        )
    )

)
