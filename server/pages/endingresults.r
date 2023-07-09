output$file_state_page_endingresults <- renderUI({
    if (file_state() != "right_file") {
        return(file_state_ui()$page)
    }
})

er_maindata <- reactive({
    if (file_state() != "right_file") {
        return()
    }

    res <- list()
    res$data <- maindata()$data %>%
        group_by(`[run number]`) %>%
        filter(`[step]` == max(`[step]`)) %>%
        arrange(`[run number]`)
})

output$er_cols_selector <- renderUI({
    if (file_state() != "right_file") {
        return()
    }

    box(
        width = 12,
        title = "Select Columns to Explore",
        collapsible = TRUE,
        status = "primary",
        awesomeCheckboxGroup(
            inputId = "er_page_cols_selected",
            label = "Potential Variables",
            choices = c(
                maindata()$colstr$init_vars,
                maindata()$colstr$step,
                maindata()$colstr$msr_metrics
            ),
        )
    )
})

er_cols_selected <- reactive({
    cols_selected <- input$er_page_cols_selected

    res <- list()

    if (is.null(cols_selected)) {
        res$num_cnt <- 0
        res$fct_cnt <- 0
        return(res)
    }

    res$all_vars <- cols_selected
    res$num_vars <- cols_selected[cols_selected %in% maindata()$colstr$num]
    res$num_cnt <- length(res$num_vars)
    res$fct_vars <- cols_selected[cols_selected %in% maindata()$colstr$fct]
    res$fct_cnt <- length(res$fct_vars)

    return(res)
})

output$er_viztype_selector <- renderUI({
    if (file_state() != "right_file") {
        return()
    }

    potential_viztype_choice <- c()

    # for interaction EDA
    if (er_cols_selected()$fct_cnt + er_cols_selected()$num_cnt >= 2) {

        potential_viztype_choice <- potential_viztype_choice %>%
            append("Pairwise EDA")

    }

    # for independent analysis for nums
    if (er_cols_selected()$num_cnt >= 2 &&
        er_cols_selected()$fct_cnt == 0) {

        potential_viztype_choice <- potential_viztype_choice %>%
            append("Histogram/Density Plot (Separate)") %>%
            append("Box plot/Violin Plot (Separate)")

    }

    # for independent analysis for cats
    if (er_cols_selected()$num_cnt == 0 &&
        er_cols_selected()$fct_cnt >= 2) { # nolint: line_length_linter.

        potential_viztype_choice <- potential_viztype_choice %>%
            append("Barplot (Separate)")

    }

    if (er_cols_selected()$num_cnt == 1 &&
        er_cols_selected()$fct_cnt >= 0 &&
        er_cols_selected()$fct_cnt <= 2) {

        potential_viztype_choice <- potential_viztype_choice %>%
            append("Histogram/Density Plot")
    }

    if (er_cols_selected()$num_cnt == 1 &&
        er_cols_selected()$fct_cnt >= 1 &&
        er_cols_selected()$fct_cnt <= 2) {

        potential_viztype_choice <- potential_viztype_choice %>%
            append("Box plot/Violin Plot")
    }

    if ((er_cols_selected()$num_cnt == 2 && er_cols_selected()$fct_cnt == 0) ||
        (er_cols_selected()$num_cnt == 2 && er_cols_selected()$fct_cnt == 1) ||
        (er_cols_selected()$num_cnt == 3 && er_cols_selected()$fct_cnt == 0) ||
        (er_cols_selected()$num_cnt == 3 && er_cols_selected()$fct_cnt == 1) ||
        (er_cols_selected()$num_cnt == 4 && er_cols_selected()$fct_cnt == 0)) {

        potential_viztype_choice <- potential_viztype_choice %>%
            append("Scatter Plot")
    }

    if ((er_cols_selected()$num_cnt == 2 && er_cols_selected()$fct_cnt == 0) ||
        (er_cols_selected()$num_cnt == 2 && er_cols_selected()$fct_cnt == 1)) {

        potential_viztype_choice <- potential_viztype_choice %>%
            append("2D Density Plot")
    }

    if ((er_cols_selected()$num_cnt == 3 && er_cols_selected()$fct_cnt == 0) ||
        (er_cols_selected()$num_cnt == 3 && er_cols_selected()$fct_cnt == 1) ||
        (er_cols_selected()$num_cnt == 4 && er_cols_selected()$fct_cnt == 0) ||
        (er_cols_selected()$num_cnt == 4 && er_cols_selected()$fct_cnt == 1) ||
        (er_cols_selected()$num_cnt == 5 && er_cols_selected()$fct_cnt == 0)) {

        potential_viztype_choice <- potential_viztype_choice %>%
            append("3D Scatter Plot")
    }

    if (er_cols_selected()$num_cnt == 0 &&
        er_cols_selected()$fct_cnt >= 1 &&
        er_cols_selected()$fct_cnt <= 3
    ) {
        potential_viztype_choice <- potential_viztype_choice %>%
            append("Barplot")
    }



    # the last default choice
    if (length(potential_viztype_choice) == 0) {
        potential_viztype_choice <- c("No Selected Input")
    }

    # debugprinter <- list(
    #     p(strong("debugger, should to be deleted!!")),
    #     p(strong("viztype")),
    #     p(input$er_viztype_selected),
    #     p(strong("#numscount:")),
    #     er_cols_selected()$num_cnt,
    #     p(strong("#fctscount:")),
    #     er_cols_selected()$fct_cnt,
    #     p(strong("#nums:")),
    #     er_cols_selected()$num_vars %>% lapply(p),
    #     p(strong("#fcts:")),
    #     er_cols_selected()$fct_vars %>% lapply(p)
    # )

    box(
        width = 12,
        title = "Select the Visualisation Type",
        status = "primary",
        collapsible = TRUE,
        radioGroupButtons(
            inputId = "er_viztype_selected",
            label = "Potential Visualiation types for Selected Variable(s)",
            status = "primary",
            direction = "vertical",
            choices = potential_viztype_choice
        )
        # debugprinter
    )
})

output$er_rows_selector <- renderUI({
    if (file_state() != "right_file") {
        return()
    }

    box(
        width = 12,
        title = "Select Rows to Visualise",
        collapsible = TRUE,
        status = "primary",
        datatable(
            er_maindata(),
            filter = "top",
            rownames = FALSE,
            elementId = "er_datatable",
            extensions = c("Buttons", "Scroller"),
            options = list(
                pageLength = 5,
                dom = "Bfrt", # check documentation on dom
                buttons = I("colvis"),
                # option for scroller
                deferRender = TRUE,
                scrollY = 140,
                scroller = TRUE
            )
        )
    )
})

er_control_from_viztype <- reactive({
    res <- list()
    if (is.null(input$er_viztype_selected) ||
        input$er_viztype_selected == "No Selected Input"
    ) {
        res$note <- list(
            tags$ul(
                tags$li("Please select some variables") # nolint: line_length_linter.
            )
        )
        res$controller <- list(
            tags$ul(
                tags$li("Please select some variables") # nolint: line_length_linter.
            )
        )
        return(res)
    }

    if (input$er_viztype_selected == "Pairwise EDA") {
        res$note <- list(
            tags$ul(
                tags$li("This visualisation may be VERY TIME COMSUMING"), # nolint
                tags$li("Pairwise EDA explore the relationship between each pair of variables"),  # nolint
                tags$li("An categorical variable can be chosen to group the data in different colours. Normally, this should be the predicted variable") # nolint
            )
        )
        res$controller <- list(
            pickerInput(
                inputId = "er_eda_colour_var",
                label = "Choose an categorical variable for different colours",
                choices = c(er_cols_selected()$fct_vars, "None"),
                options = list(
                    size = 5,
                    `live-search` = TRUE
                )
            )
        )
        return(res)
    }

    res$note <- list(
        tags$ul(
            tags$li("UNDER CONSTRUCTION, TO BE CHANGED!!") # nolint: line_length_linter.
        )
    )
    res$controller <- list(
        tags$ul(
            tags$li("UNDER CONSTRUCTION, TO BE CHANGED!!") # nolint: line_length_linter.
        )
    )
    return(res)

})



output$er_control1 <- renderUI({
    if (file_state() != "right_file") {
        return()
    }

    box(
        width = 12,
        title = "Note on this Visualisation",
        status = "primary",
        solidHeader = TRUE,
        er_control_from_viztype()$note
    )
})

output$er_control2 <- renderUI({
    if (file_state() != "right_file") {
        return()
    }

    box(
        width = 12,
        title = "Parameters this Visualisation",
        status = "primary",
        er_control_from_viztype()$controller,
        actionButton(
            inputId = "er_plot_confirm",
            label = "Submit",
            class = "btn-primary"
        )
    )

})

# prepare selected data
er_maindata_rows_selected <- reactive({
    res <- er_maindata() %>%
        filter(`[run number]` %in% input$er_datatable_rows_selected)
    return(res)
})

# make the graph
er_graph <- eventReactive(input$er_plot_confirm, {
    if (nrow(er_maindata_rows_selected()) == 0) {
        graph <- h3("Please choose some rows to visualise")
        return(graph)
    }

    if (is.null(input$er_viztype_selected) ||
        input$er_viztype_selected == "No Selected Input"
    ) {
        graph <- h3("Please select select some variables")
        return(graph)
    }

    if (input$er_viztype_selected == "Pairwise EDA") {

        if (input$er_eda_colour_var == "None") {
            graph <- er_maindata_rows_selected() %>%
                ggpairs(
                    columns = er_cols_selected()$all_vars
                )
        } else {
            graph <- er_maindata_rows_selected() %>%
                ggpairs(
                    columns = er_cols_selected()$all_vars,
                    mapping = aes(color = .data[[input$er_eda_colour_var]])
                )
        }

        graph <- graph %>%
            ggplotly(width = 800, height = 800)
        return(graph)
    }

    graph <- h3("UNDER CONSTRUCTION, TO BE CHANGED!!")
    return(graph)
})


output$er_mainplot <- renderUI({
    if (file_state() != "right_file") {
        return()
    }

    box(
        id = "er_mainplot_box",
        width = 12,
        # height = 800,
        title = "Parameters this Visualisation",
        status = "primary",
        solidHeader = TRUE,
        er_graph()
    )
})
