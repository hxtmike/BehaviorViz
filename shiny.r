library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)


source("./ui/ui.r", local = TRUE)
source("./server/server.r", local = TRUE)

shinyApp(ui = ui, server = server)
