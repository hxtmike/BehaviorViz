library(shiny)
library(stringr)
library(dplyr)



server <- function(input, output) {
    # load file state
    source("./server/info_loader.r", local = TRUE)
    source("./server/file_state.r", local = TRUE)
}
