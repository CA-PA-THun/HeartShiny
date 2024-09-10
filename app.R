# Source the necessary components
source("global.R")      # Loads the libraries etc
source("app_ui.R")      # Loads the UI
source("app_server.R")  # Loads the server

# Launch the application
shinyApp(
  ui = app_ui(),
  server = app_server
)
