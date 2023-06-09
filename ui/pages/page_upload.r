tabItem(
    tabName = "upload",
    fluidRow(
        column(
            width = 6, offset = 3,
            box(
                title = "Welcome to BehaviorViz",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                p("BehaviorViz is a dashboard tool that allows a user to upload any BehaviorSpace file in the standard format (Table output) and explore the relationships between simulation inputs and output measures. BehaviorSpace is a software tool integrated with NetLogo that allows you to perform experiments with agent-based models."), # nolint: line_length_linter.
                p(
                    "For more information about output files of BehaviorSpace, please visit the ",
                    tags$a(href = "https://ccl.northwestern.edu/netlogo/docs/behaviorspace.html", target = "_blank", "BehaviorSpace Guide"),
                    "or",
                    tags$a(href = "https://ccl.northwestern.edu/netlogo/docs/", target = "_blank", "NetLogo Manual")
                )

            )
        )
    ),
    fluidRow(
        column(
            width = 6, offset = 3,
            box(
                width = 12,
                title = "Upload Table Output",
                status = "primary",
                p("You can upload your own BehaviorSpace Table output file (csv) here."), 
                p("Then, you can observe data at Data Table session or visualise data at Dashboard session"),
                fileInput(
                    inputId = "file",
                    label = "Choose a file to upload",
                    accept = ".csv"
                ),
                uiOutput("file_state_ui_h4")
            )
        )
    ),
)