regex_netlogo_version <- "BehaviorSpace results \\((NetLogo [0-9.]+)\\)" # nolint
regex_netlogo_filename <- "^(.*\\.nlogo$)" # nolint
regex_behaviorspace_time <- "(\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}:\\d{3} .\\d{4})" # nolint

# load value

## load csv file info

csv_file_header_info <- reactive({
    if (is.null(input$file)) {
        return()
    }

    if (
        nrow(
            read.csv(file = input$file$datapath, header = FALSE)
        ) < 8
    ) {
        return()
    }

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

    print("--------------------")
    print("metadata")
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
    # when read csv, may occur some all NA column
    res <- worldvars_data %>% select_if(~ !all(is.na(.)))

    # for backend debugging
    print("--------------------")
    print("worldvars")
    print(res)

    return(res)
})

maindata <- reactive({
    if (file_state() != "right_file") {
        return()
    }
    maindata_header <- read.csv(
        file = input$file$datapath,
        skip = 6,
        nrow = 1,
        header = FALSE
    )

    maindata_data <- read.csv(
        file = input$file$datapath,
        skip = 6,
        header = TRUE,
        stringsAsFactors = TRUE
    )
    colnames(maindata_data) <- maindata_header
    # when read csv, may occur some all NA column
    maindata <- maindata_data %>%
        select_if(~ !all(is.na(.)))
    # keep double/int their own way
    # string to factors using stringAsFactors
    # would not be complex or raw
    # need convert "logical" to "factor" type
    res <- list()
    res$data <- maindata %>%
        lapply(function(col) {
            if (is.logical(col)) {
                as.factor(col)
            } else {
                col
            }
        }) %>%
        data.frame()
    colnames(res$data) <- colnames(maindata)

    # different

    # for backend debugging
    print("--------------------")
    print("head(maindata)")
    print(head(res$data))
    print("typeof(maindata)")
    print(head(res$data) %>% sapply(typeof))

    #colnum for init_vars and msr_metric
    res$colnum <- list()
    res$colstr <- list()

    stepcolnum <- which(names(res$data) == "[step]")

    res$colnum$run_step <- c(1, stepcolnum)
    res$colstr$run_step <- colnames(res$data)[res$colnum$run_step]

    if (stepcolnum != 2) {
        res$colnum$init_vars <- c(2 : (stepcolnum - 1))
        res$colstr$init_vars <- colnames(res$data)[res$colnum$init_vars]
    }

    if (stepcolnum != length(res$data)) {
        res$colnum$msr_metrics <- c((stepcolnum + 1):length(res$data))
        res$colstr$msr_metrics <- colnames(res$data)[res$colnum$msr_metrics]
    }
    print("colnums")
    print(res$colnum)
    print("colstr")
    print(res$colstr)

    return(res)
})
