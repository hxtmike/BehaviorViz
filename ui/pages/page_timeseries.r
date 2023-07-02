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
            width = 2,
            uiOutput("ts_control1")
        ),
        column(
            width = 2,
            uiOutput("ts_control2"),
            # uiOutput("ts_control22")
        ),
        column(
            width = 8,
            uiOutput("ts_mainplot")
        )
    )
)