dashboardBody(
    tags$head(
        includeCSS("www/style.css"),
        # includeScript("www/script.js")
    ),
    tabItems(
        source("./ui/pages/page_upload.r")$value,
        source("./ui/pages/page_datatable.r")$value,
        source("./ui/pages/page_timeseries.r")$value,
        source("./ui/pages/page_endingresults.r")$value
    )
)
