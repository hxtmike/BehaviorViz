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
    widths <- c(2, 4, 2, 4)
    res <- list(
        column(width = 12, h4("Metadata of the Output File")),
        lapply(
            seq(csv_file_header_info()),
            function(i) {
                valueBox(
                    value = csv_file_header_info()[i],
                    subtitle = subtitles[i],
                    icon = icon(icons[i]),
                    width = widths[i],
                    color = "light-blue",
                )
            }
        )
    )
    return(res)
})


output$worldvars <- renderUI({
    if (file_state() != "right_file") {
        return()
    }
    list(
        h4("World Variables of the Output File"),
        renderTable(worldvars())
    )
})


output$maindata_panel <- renderUI({
    if (file_state() != "right_file") {
        return()
    }
    box(
        title = "Choose Variables",
        width = 12,
        collapsible = TRUE,
        awesomeCheckboxGroup(
            inputId = "dt_sel_run_step",
            label = "[run number]/[step]",
            choices = maindata()$colstr$run_step,
            selected = maindata()$colstr$run_step
        ),
        awesomeCheckboxGroup(
            inputId = "dt_sel_init_vars",
            label = "Initial Values of the Variables",
            choices = maindata()$colstr$init_vars,
            selected = maindata()$colstr$init_vars
        ),
        awesomeCheckboxGroup(
            inputId = "dt_sel_msr_metrics",
            label = "Measurement Metric Data",
            choices = maindata()$colstr$msr_metrics,
            selected = maindata()$colstr$msr_metrics
        )
    )
})

output$maindata_dt <- renderUI({
    if (file_state() != "right_file") {
        return()
    }
    box(
        title = "Main Data of the Output file",
        width = 12,
        datatable(
            maindata()$data,
            filter = "top",
            rownames = FALSE,
        ) %>%
            formatStyle(
                maindata()$colstr$run_step,
                backgroundColor = "aqua"
            ) %>%
            formatStyle(
                maindata()$colstr$init_vars,
                backgroundColor = "lime"
            ) %>%
            formatStyle(
                maindata()$colstr$msr_metrics,
                backgroundColor = "fuchsia"
            )
    )
})
