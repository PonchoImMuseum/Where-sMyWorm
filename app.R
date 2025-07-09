# app.R
library(shiny)

# Source global.R first to ensure all functions and data are loaded
source("global.R")

# Source the UI and server files in the global environment
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)