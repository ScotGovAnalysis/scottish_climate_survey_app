# app.R - Main application file

# Source global settings and functions
source("global.R")

# Source UI and server
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)