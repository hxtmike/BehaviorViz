dashboardBody(
    tabItems(
        source("./ui/pages/page_upload.r")$value,
        source("./ui/pages/page_datatable.r")$value,
        tabItem(
            tabName = "timeseries",
            h2("Timeseries page")
        ),
        tabItem(
            tabName = "multivariable",
            h2("Multivariable page")
        ),
        source("./ui/pages/page_setting.r")$value
    )
)