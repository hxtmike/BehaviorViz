tabItem(
    tabName = "datatable",
    # file_state_page_output
    uiOutput("file_state_page_datatable"),

    fluidRow(
        column(
            width = 9,
            uiOutput("metadata")
        ),
        column(
            width = 3,
            uiOutput("worldvars"),
        )
    ),
    fluidRow(
        # panel 1
        column(
            width = 2,
            uiOutput("maindata_panel")
        ),
        # datatable col
        column(
            width = 10,
            uiOutput("maindata_dt")
        ),
    )
)
