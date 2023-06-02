library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)


ui_header <- dashboardHeader(
    title = tagList(
        span(class = "logo-lg", "BehaviorViz"),
        icon("location-arrow")
    )
)

ui_siderbar <- source("ui_siderbar.r", local = TRUE)$value

ui_body <- source("ui_body.r", local = TRUE)$value

ui_footer <- dashboardFooter(
     left = "By Xiaotian Han",
     right = "Durham, 2023"
)

ui <- dashboardPage(
    skin = "midnight",
    header = ui_header,
    sidebar = ui_siderbar,
    body = ui_body,
    footer = ui_footer
)

server <- function(input, output) {

}

shinyApp(
    ui = ui,
    server = server
)
