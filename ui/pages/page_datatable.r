tabItem(
    tabName = "datatable",
    # file_state_page_output
    uiOutput("file_state_page_datatable"),

    fluidRow(
        uiOutput("metadata")
    ),
    fluidRow(
        column(
            width = 12,
            uiOutput("worldvar_title"),
            tableOutput("worldvar_table")
        )
    ),
    fluidRow(
        # datatable col
        column(
            width = 8,
            DTOutput("maindata_dt")
        ),
        # panel 1
        column(
            width = 2,
        ),
        # panel 2
        column(
            width = 2,
        )
    )
)
