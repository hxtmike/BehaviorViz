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

output$ts_control1 <- renderUI({
    if (file_state() != "right_file" | endingresult_state()) {
       return()
    }

    if (!is.null(input$ts_viztype_choice) && input$ts_viztype_choice == "Basic") { # nolint: line_length_linter.
        box(
            width = 12,
            title = "New Input",
            status = "primary",
            numericInput(
                inputId = "ts_basic_add_runnum",
                label = "Choose a Run",
                value = 1,
                min = 1,
                max = 10
            ),
            awesomeRadio(
                inputId = "ts_basic_add_var",
                label = "Choose a Variable",
                choices = c("A", "B", "C")
            ),
            radioGroupButtons(
                inputId = "ts_basic_add_type",
                label = "Choose a Type of Chart",
                choices = c("Line", "Step", "Filling", "Point"),
                direction = "vertical",
                status = "primary"
            ),
            colourInput(
                inputId = "ts_basic_add_col",
                label = "Choose a Color"
            ),
            actionButton(
                inputId = "ts_basic_add_submit",
                label = "Add a New Line",
                class = "btn-primary"
            )
        )
    }# nolint: line_length_linter.
})

ts_inputs_existing <- reactiveValues(stack = list())

ts_add_inputs <- observeEvent(input$ts_basic_add_submit, {
    if (!is.null(input$ts_basic_add_runnum) && input$ts_basic_add_runnum > 10) {
        shinyalert(
            title = "Error",
            text = "The run number is out of range",
            size = "s",
            closeOnEsc = TRUE,
            closeOnClickOutside = FALSE,
            html = FALSE,
            type = "error",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#3C8DBC",
            timer = 0,
            imageUrl = "",
            animation = TRUE
        )
    } else {
        res <- list()
        res$runnum <- input$ts_basic_add_runnum
        res$var <- input$ts_basic_add_var
        res$type <- input$ts_basic_add_type
        res$col <- input$ts_basic_add_col
        res$br <- br()
        ts_inputs_existing$stack <- ts_inputs_existing$stack %>% append(res)
    }
})

output$ts_control21 <- renderUI({
    if (file_state() != "right_file" | endingresult_state()) {
       return()
    }

    if (!is.null(input$ts_viztype_choice) && input$ts_viztype_choice == "Basic") { # nolint: line_length_linter.
        res <- list(
            box(
                width = 12,
                title = "Existing Variables",
                status = "primary",
                p("[placeholder] a list of line here"),
                actionButton(
                    inputId = "ts_basic_empty",
                    label = "Empty the Chart",
                    class = "btn-danger"
                )
            ),
            box(
                width = 12,
                title = "2nd Y-axis",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                materialSwitch(
                    inputId = "ts_basic_y2_switch",
                    label = "2nd Y-axis",
                    status = "primary"
                )
            )
        )
    }
})

output$ts_control22 <- renderUI({
    if (!is.null(input$ts_basic_y2_switch) && input$ts_basic_y2_switch) {
        box(
            width = 12,
            title = "Variable for Axis",
            id = "var_for_axis",
            prettyRadioButtons(
                inputId = "ts_basic_y1_var",
                label = "For the 1st Y-axis",
                choices = c("a", "b")
                #this choice cannot be NULL
            ),
            prettyRadioButtons(
                inputId = "ts_basic_y2_var",
                label = "For the 2nd Y-axis",
                choices = c("a", "b")
                #this choice cannot be NULL
            )
        )
    }
})

output$ts_mainplot <- renderUI({
    return(ts_inputs_existing$stack)
})