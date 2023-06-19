library(shinydashboard)

output$file_state_page_datatable <- renderUI({
    file_state_ui()$page
})
# server code on data table page

## output for metadata
output$metadata <- renderUI({
    if (file_state() != "right_file") {
        return()
    }
    subtitles <- c(
        "NetLogo Version",
        "NetLogo Filename",
        "BehaviorSpace Name",
        "BehaviorSpace Timestamp"
    )
    icons <- c(
        "location-arrow",
        "flask-vial",
        "file-export",
        "clock"
    )
    res <- list(
        column(width = 12, h4("Metadata of the Output File")),
        lapply(
            seq(csv_file_header_info()),
            function(i) {
                valueBox(
                    value = csv_file_header_info()[i],
                    subtitle = subtitles[i],
                    icon = icon(icons[i]),
                    width = 3,
                    color = "light-blue",
                )
            }
        )
    )
    return(res)
})


output$worldvar_title <- renderUI({
    if (file_state() != "right_file") {
        return()
    }
    h4("World Variables of the Output File")
})

output$worldvar_table <- renderTable({
    if (file_state() != "right_file") {
        return()
    }
    worldvars()
})

output$maindata_dt <- renderDT(
    maindata(),
    filter = "top",
    rownames = FALSE,
    options = list(
        autoWidth = TRUE
    ),
)
