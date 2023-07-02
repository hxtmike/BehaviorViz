library(colourpicker)
library(shinyalert)
library(dygraphs)


server <- function(input, output) {
    # load file state
    source("./server/info_loader.r", local = TRUE)
    source("./server/file_state.r", local = TRUE)

    # server code for pages
    source("./server/pages/datatable.r", local = TRUE)
    source("./server/pages/timeseries.r", local = TRUE)
    source("./server/pages/endingresults.r", local = TRUE)
}
