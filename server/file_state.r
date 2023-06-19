# figure out the file state
file_state <- reactive({
    if (is.null(input$file)) {
        return("no_file")
    }

    # debug output
    print("###################")
    print(basename(input$file$datapath))

    # if nrow < 8 return
    if (nrow(
            read.csv(file = input$file$datapath, header = FALSE)
        ) < 8) {
        return("wrong_csv")
    }

    identifier_header <- read.csv(
        file = input$file$datapath,
        skip = 6,
        nrow = 1,
        header = FALSE
    )

    identifier_nonrunnumber <- identifier_header[1, 1] != "[run number]" # nolint

    if (is.na(csv_file_header_info()$nl_ver) |
        is.na(csv_file_header_info()$nl_filename) |
        is.na(csv_file_header_info()$bs_time) |
        identifier_nonrunnumber) {
        return("wrong_csv")
    }

    if (identifier_header[1, ] %>%
        c() %>%
        duplicated() %>%
        any()) {
       return("duplicated_colnames")
    }

    identifier_firstrow <- read.csv(
        file = input$file$datapath,
        skip = 6,
        nrow = 1,
        header = TRUE
    )

    if (identifier_firstrow[1, 1] %>%
        mode() != "numeric") {
        return("spreadsheet_output")
    }

    return("right_file")
})

# renderui based on file state
file_state_ui <- reactive({
    res <- list()
    if (file_state() == "no_file") {
        res$h4 <- tags$h4(
            "No file have been uploaded yet", # nolint: object_name_linter.
        )
        res$page <- fluidRow(
            column(
                width = 6, offset = 3,
                box(
                    title = "Notice",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    res$h4
                )
            )
        )
    }

    if (file_state() == "wrong_csv") {
        res$h4 <- tags$h4(
            "The CSV file cannot be parsed correctly. Please upload Table output with right format", # nolint: line_length_linter.
            style = "color: red"
        )
        res$page <- fluidRow(
            column(
                width = 6, offset = 3,
                box(
                    title = "Notice",
                    width = 12,
                    solidHeader = TRUE,
                    status = "danger",
                    res$h4
                )
            )
        )
    }

    if (file_state() == "spreadsheet_output") {
        res$h4 <- tags$h4(
            "The CSV file might be a Spreadsheet output of BehaviorSpace. Please upload Table output instead", # nolint: line_length_linter.
            style = "color: orange"
        )
        res$page <- fluidRow(
            column(
                width = 6, offset = 3,
                box(
                    title = "Notice",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    res$h4
                )
            )
        )
    }

    if (file_state() == "duplicated_colnames") {
        res$h4 <- tags$h4(
            "The CSV file contains duplicate column names, please process them before uploading", # nolint: line_length_linter.
            style = "color: orange"
        )
        res$page <- fluidRow(
            column(
                width = 6, offset = 3,
                box(
                    title = "Notice",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    res$h4
                )
            )
        )
    }

    if (file_state() == "right_file") {
        res$h4 <- tags$h4(
            "The file have been uploaded successfully", # nolint: line_length_linter.
            style = "color: green"
        )
    }

    return(res)
})

output$file_state_ui_h4 <- renderUI({
    file_state_ui()$h4
})
