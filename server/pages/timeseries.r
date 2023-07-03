# check the state of the file
ts_endingresult_state <- reactive({
    if (file_state() != "right_file") {
        return()
    }
    max(maindata()$data[maindata()$colnum$run]) == nrow(maindata()$data) || length(intersect(maindata()$colnum$msr_metrics, maindata()$colnum$num)) == 0 # nolint: line_length_linter.
    # True: is endingresult
    # False: is timeseries
})

# state page
output$file_state_page_timeseries <- renderUI({
    if (file_state() != "right_file") {
        return(file_state_ui()$page)
    }

    # hint for ending result output
    if (ts_endingresult_state()) {
        fluidRow(
            column(
                width = 6, offset = 3,
                box(
                    title = "Notice",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    tags$h4(
                        "The CSV file only contains the ending result of each run or doesn't contain valid measurement metrics data. Enable the 'Measure runs at every step' in BehaviorSpace or include valid output if you want to observe the data for each step", # nolint
                        style = "color: orange" # nolint
                    )
                )
            )
        )
    }
})

ts_default_state_init <- reactive({
    if (file_state() != "right_file") {
        return(TRUE)
    }

    if (ts_endingresult_state()) {
        return(TRUE)
    }
    return(FALSE)
})

ts_default_state_type <- reactive({
    if (ts_default_state_init()) {
        return(TRUE)
    }

    if (is.null(input$ts_viztype_choice)) {
        return(TRUE)
    }
    return(FALSE)
})

# timeseries data preparation
ts_maindata <- reactive({
    if (ts_default_state_init()) {
        return()
    }

    res <- list()
    res$maxrun <- max(maindata()$data[maindata()$colnum$run])
    res$ptlvar <- intersect(
        maindata()$colstr$msr_metrics,
        maindata()$colstr$num
    )
    res$data <- maindata()$data %>%
        arrange(
            across(
                maindata()$colnum$run
            )
        ) %>%
        select(c(
            maindata()$colnum$run,
            maindata()$colnum$step,
            res$ptlvar
        )) %>%
        pivot_wider(
            names_from = 1,
            values_from = c(3:(2 + length(res$ptlvar))),
            id_cols = `[step]`, # nolint: object_name_linter.
            names_glue = "run_{`[run number]`}_{.value}", # nolint: object_name_linter, line_length_linter.
        )

    print("")
    print("ts_maindata")
    print(res)
    return(res)
})

# Status bar above
output$ts_viztype_radio <- renderUI({
    if (ts_default_state_init()) {
        return()
    }

    radioGroupButtons(
        inputId = "ts_viztype_choice",
        label = "Choose the visualisation type", # nolint: object_name_linter.
        choices = c("Basic", "##Potential Placeholder##"), # nolint: object_name_linter, line_length_linter.
        status = "primary",
        justified = TRUE,
        individual = TRUE
    )
})

output$ts_typenote <- renderUI({
    if (ts_default_state_type()) {
        return()
    }

    res <- list(
        tags$p(tags$strong("Note on this type of visualisation:")) # nolint
    )

    if (input$ts_viztype_choice == "Basic") { # nolint: line_length_linter.
        res[[length(res) + 1]] <- tags$ul(
            tags$li("Only numeric variables in measurement metric data can be visualised in time series form"), # nolint: line_length_linter, object_name_linter, object_length_linter.
            tags$li("Carefully determine whether different variables need to be presented on different scales before choosing whether to add a second Y-axis") # nolint: line_length_linter, object_name_linter, object_length_linter.
        )
        return(res)
    }
})

# control panels 1
output$ts_control1 <- renderUI({
    if (ts_default_state_type()) {
        return()
    }

    if (input$ts_viztype_choice == "Basic") {
        box(
            width = 12,
            title = "New Input",
            status = "primary",
            numericInput(
                inputId = "ts_basic_add_runnum",
                label = "Choose a Run",
                value = 1,
                min = 1,
                max = ts_maindata()$maxrun
            ),
            awesomeRadio(
                inputId = "ts_basic_add_var",
                label = "Choose a Variable",
                choices = ts_maindata()$ptlvar
            ),
            radioGroupButtons(
                inputId = "ts_basic_add_type",
                label = "Choose a Type of Chart",
                choices = c("Line", "Step", "Filling", "Point"),
                direction = "vertical",
                status = "primary"
            ),
            colourInput(
                inputId = "ts_basic_add_colour",
                label = "Choose a Color",
                value = "black"
            ),
            actionButton(
                inputId = "ts_basic_add_submit",
                label = "Add a New Line",
                class = "btn-primary"
            )
        )
    } # nolint: line_length_linter.
})

ts_basic_existing <- reactiveVal(
    data.frame(
        var_name = c(),
        type = c(),
        colour = c(),
        yaxis = c()
    )
)

ts_basic_graph <- reactive({
    if (length(ts_basic_existing()$var_name) == 0) {
        return()
    }
    # renew the graph
    graph <- ts_maindata()$data[
        c("[step]", ts_basic_existing()$var_name)
    ] %>%
        filter(
            if_any(2:(1 + length(ts_basic_existing()$var_name)), ~ !is.na(.)) # nolint: line_length_linter.
        ) %>%
        dygraph()

    for (i in seq_along(ts_basic_existing()$var_name)) {
        graph <- graph %>%
            dySeries(ts_basic_existing()$var_name[i],
                stepPlot = ts_basic_existing()$type[i] == "Step",
                fillGraph = ts_basic_existing()$type[i] == "Filling",
                drawPoints = ts_basic_existing()$type[i] == "Point",
                pointSize = 3,
                color = ts_basic_existing()$colour[i],
                axis = if_else(ts_basic_existing()$yaxis[i] == 1,
                    "y",
                    "y2"
                )
            )
    }
    graph <- graph %>% dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 1
    ) %>%
    dyLegend(show = "follow") %>%
    dyRangeSelector()

    return(graph)
})

observeEvent(input$ts_basic_add_submit, {
    if (!is.null(input$ts_basic_add_runnum) &&
        input$ts_basic_add_runnum > ts_maindata()$maxrun) { # nolint: line_length_linter.
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
            animation = TRUE
        )
    } else {
        new_log <- data.frame(
            var_name = paste0("run_", input$ts_basic_add_runnum, "_", input$ts_basic_add_var), # nolint: line_length_linter.
            type = input$ts_basic_add_type,
            colour = input$ts_basic_add_colour,
            yaxis = 1
        )
        if (new_log$var_name %in% ts_basic_existing()$var_name) {
            shinyalert(
                title = "Warning",
                text = "This input already exist",
                size = "s",
                closeOnEsc = TRUE,
                closeOnClickOutside = FALSE,
                html = FALSE,
                type = "warning",
                showConfirmButton = TRUE,
                showCancelButton = FALSE,
                confirmButtonText = "OK",
                confirmButtonCol = "#AEDEF4",
                timer = 0,
                imageUrl = "",
                animation = TRUE
            )
        } else {
            # renew the value
            ts_basic_existing(
                ts_basic_existing() %>% rbind(new_log) # nolint: line_length_linter.
            )
            print(ts_basic_existing)


        }
    }
})

ts_basic_existing_var_name_y1 <- reactive({
    if (length(ts_basic_existing()) == 0) {
        return("No input yet")
    }

    res <- ts_basic_existing() %>%
        filter(yaxis == 1) %>%
        select(var_name) %>%
        c()

    if (length(res$var_name) == 0) {
        return("No input yet")
    }

    return(res$var_name)
})

ts_basic_existing_var_name_y2 <- reactive({
    if (length(ts_basic_existing()) == 0) {
        return("No input yet")
    }
    res <- ts_basic_existing() %>%
        filter(yaxis == 2) %>%
        select(var_name) %>%
        c()

    if (length(res$var_name) == 0) {
        return("No input yet")
    }

    return(res$var_name)
})

# control panel 2
output$ts_control2 <- renderUI({
    if (ts_default_state_type()) {
        return()
    }

    if (input$ts_viztype_choice == "Basic") {
        list(
            box(
                width = 12,
                title = "Existing Variables",
                status = "primary",
                awesomeRadio(
                    inputId = "ts_basic_y2_choice",
                    label = "Inputs Scaled in 1st Y-axis",
                    choices = ts_basic_existing_var_name_y1()
                ),
                actionButton(
                    inputId = "ts_basic_y2_confirm",
                    label = "Scale this Input in 2nd Y-axis", # nolint: object_name_linter, line_length_linter.
                    class = "btn-primary" # nolint: object_name_linter.
                ),
                awesomeRadio(
                    inputId = "ts_basic_y1_choice",
                    label = "Inputs Scaled in 2nd Y-axis",
                    choices = ts_basic_existing_var_name_y2()
                ),
                actionButton(
                    inputId = "ts_basic_y1_confirm",
                    label = "Scale this Input in 2nd Y-axis", # nolint: object_name_linter, line_length_linter.
                    class = "btn-primary" # nolint: object_name_linter.
                ),
                p("Note: All inputs are scaled on the 2nd Y-axis together."), # nolint: line_length_linter, object_name_linter.
                actionButton(
                    inputId = "ts_basic_empty",
                    label = "Empty the Chart",
                    class = "btn-danger"
                )
            )
        )
    }
})

observeEvent(input$ts_basic_y2_confirm, {
    if (input$ts_basic_y2_choice == "No input yet") {
    } else {
        ts_basic_existing(
            ts_basic_existing() %>%
                mutate(
                    yaxis = if_else(
                        var_name == input$ts_basic_y2_choice,
                        2,
                        yaxis
                    )
                )
        )
    }
})

observeEvent(input$ts_basic_y1_confirm, {
    if (input$ts_basic_y1_choice == "No input yet") {
    } else {
        ts_basic_existing(
            ts_basic_existing() %>%
                mutate(
                    yaxis = if_else(
                        var_name == input$ts_basic_y1_choice,
                        1,
                        yaxis
                    )
                )
        )
    }
})

observeEvent(input$ts_basic_empty, {

    # only input step into dygraph
    ts_basic_existing(
        data.frame(
            var_name = c(),
            type = c(),
            colour = c(),
            yaxis = c()
        )
    )
})

# mainplot
output$ts_mainplot <- renderUI({
    if (ts_default_state_type()) {
        return()
    }

    if (input$ts_viztype_choice == "Basic") {
        res <- box(
            width = 12,
            title = "Time Series Visualisation",
            ts_basic_graph()
        )
        return(res)
    }
})
