tabItem(
    tabName = "datatable",
    # file_state_page_output
    uiOutput("file_state_page_datatable"),

    fluidRow(
        column(
            width = 9,
            uiOutput("dt_metadata")
        ),
        column(
            width = 3,
            uiOutput("dt_worldvars"),
        )
    ),
    fluidRow(
        # panel 1
        column(
            width = 2,
            uiOutput("dt_maindata_panel")
        ),
        # datatable col
        column(
            width = 10,
            uiOutput("dt_maindata_datatable")
        ),
    )
)
