endingresult_state <- reactive({
    max(maindata()$data[maindata()$colnum$run]) == nrow(maindata()$data)
    # True: is endingresult
    # False: is timeseries
})
output$file_state_page_timeseries <- renderUI({
    if (file_state() != "right_file") {
       return(file_state_ui()$page)
    }

    # hint for ending result output
    if (endingresult_state()) {
       fluidRow(
            column(
                width = 6, offset = 3,
                box(
                    title = "Notice",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    tags$h4(
                        "The CSV file only contains the ending result of each run. Enable the 'Measure runs at every step' in BehaviorSpace if you want to observe the data for each step", # nolint: line_length_linter.
                        style = "color: orange"
                    )
                )
            )
        )
    }
})

output$ts_viztype_radio <- renderUI({
    if (file_state() != "right_file" | endingresult_state()) {
       return()
    }

    radioGroupButtons(
        inputId = "ts_viztype_choice",
        label = "Choose the visualisation type",
        choices = c("Basic", "Heatmap", "Animation"),
        status = "primary",
        justified = TRUE,
        individual = TRUE
    )
})

output$ts_typenote <- renderUI({
    if (file_state() != "right_file" | endingresult_state()) {
       return()
    }

    if (!is.null(input$ts_viztype_choice) && input$ts_viztype_choice == "Basic") { # nolint: line_length_linter.
        res <- list(
            tags$p(tags$strong("Note on this type of visualisation:")),
            tags$ul(
                tags$li("Only numeric variables in measurement metric data can be visualised in time series form"), # nolint: line_length_linter.
                tags$li("Carefully determine whether different variables need to be presented on different scales before choosing whether to add a second Y-axis") # nolint: line_length_linter.
            )
        )
        return(res)
    }
})

output$ts_control <- renderUI({
    if (file_state() != "right_file" | endingresult_state()) {
       return()
    }

    if (!is.null(input$ts_viztype_choice) && input$ts_viztype_choice == "Basic") { # nolint: line_length_linter.
        box(
            title = "2nd Y-axis",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            materialSwitch(
                inputId = "ts_y2_switch",
                label = "2nd Y-axis",
                status = "primary"
            ),
            prettyRadioButtons(
                inputId = "ts_y2_var",
                label = "Choose Variable for 2nd Y-axis",
                choices = c("a", "b")
                # this choice cannot be NULL
            )
        )
    }
})