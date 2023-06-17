regex_netlogo_version <- "BehaviorSpace results \\((NetLogo [0-9.]+)\\)" # nolint
regex_netlogo_filename <- "^(.*\\.nlogo$)" # nolint
regex_behaviorspace_time <- "(\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}:\\d{3} .\\d{4})" # nolint

# load value

## load csv file info

csv_file_header_info <- reactive({
    res <- list()
    csv_file_header <- read.csv(
        text = readLines(input$file$datapath, n = 4),
        header = FALSE
    )
    res$nl_ver <- str_match(
        string = csv_file_header[1, 1],
        pattern = regex_netlogo_version
    )[1, 2]

    res$nl_filename <- str_match(
        string = csv_file_header[2, 1],
        pattern = regex_netlogo_filename
    )[1, 2]

    res$bs_name <- csv_file_header[3, 1]

    res$bs_time <- str_match(
        string = csv_file_header[4, 1],
        pattern = regex_behaviorspace_time
    )[1, 2]

    print(res)

    return(res)
})

## load value for world variables
worldvars <- reactive({
    if (file_state() != "right_file") {
        return()
    }

    worldvars_data <- read.csv(
        file = input$file$datapath,
        skip = 5,
        nrow = 1,
        header = FALSE
    )

    worldvars_header <- read.csv(
        file = input$file$datapath,
        skip = 4,
        nrow = 1,
        header = FALSE
    )
    colnames(worldvars_data) <- worldvars_header
    res <- worldvars_data %>% select_if(~ !all(is.na(.)))

    # for backend debugging
    print(res)

    return(res)
})

maindata <- reactive({
    maindata_header <- read.csv(
        file = input$file$datapath,
        skip = 6,
        nrow = 1,
        header = FALSE
    )

    maindata_data <- read.csv(
        file = input$file$datapath,
        skip = 6,
        header = TRUE
    )
    colnames(maindata_data) <- maindata_header
    res <- maindata_data %>% select_if(~ !all(is.na(.)))

    # for backend debugging
    print(res)

    return(res)
})
