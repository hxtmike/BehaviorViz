dashboardBody(
    tabItems(
        source("pages/page_upload.r")$value,
        tabItem(
            tabName = "datatable",
            h2("datatable page")
        ),
        tabItem(
            tabName = "timeseries",
            h2("Timeseries page")
        ),
        tabItem(
            tabName = "multivariable",
            h2("Multivariable page")
        ),
        source("pages/page_setting.r")$value
    )
)