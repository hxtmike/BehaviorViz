library(shiny)
library(shinydashboard)
library(tidyverse)


ui_header <- dashboardHeader()

ui_siderbar <- dashboardSidebar()

ui_body <- dashboardBody()

ui <- dashboardPage(
    header = ui_header,
    sidebar = ui_siderbar,
    body = ui_body
)

server <- function(input, output) {

}

shinyApp(
    ui = ui,
    server = server
)
