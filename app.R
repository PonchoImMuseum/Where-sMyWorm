# app.R
library(shiny)

shinyApp(ui = source("ui.R")$value,
         server = source("server.R")$value)