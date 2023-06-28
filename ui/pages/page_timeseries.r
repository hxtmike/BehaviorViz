tabItem(
    tabName = "timeseries",
    # default file_state_page_output
    uiOutput("file_state_page_timeseries"),

    # visualisation type
    fluidRow(
        column(
            width = 4,
            uiOutput("ts_viztype_radio")
        ),
        column(
            width = 8,
            uiOutput("ts_typenote")
        )
    ),

    # Notes for type
    fluidRow(
        # control panel
        column(
            width = 3,
            uiOutput("ts_control")
        ),
        column(
            width = 9,
            # uiOutput("ts_mainplot")
        )
    )
)