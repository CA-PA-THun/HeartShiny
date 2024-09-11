############### GLOBAL CODE SECTION ###############################################################################
# Clear the environment
rm(list = ls())

############### LIBRARIES ###############################################################################
# Load required libraries
library(shinydashboard)
library(shiny)
library(DT)
library(tidyverse)
library(kableExtra)
library(reshape2)
library(janitor)
library(zoo)
library(rlang)
library(glue)
library(stringr)
library(plotly)
library(fresh)
library(sass)
library(sf)
library(shinyWidgets)
library(writexl)
library(logger)
library(uuid)
library(fontawesome)

############### GLOBAL SETUP ###############################################################################

# Set up logger to use the same log file already defined in your app
log_appender(appender_file("log_file.log"))

# Global error handler to log any unexpected errors
options(shiny.error = function() {
  log_error(paste("A global error occurred:", geterrmessage()))
})

# Theme for the app
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#333333" # Dark gray for accent colors
  ),
  adminlte_global(
    content_bg = "#FFFFFF", # White background for the main body
    box_bg = "#FFFFFF", # White background for box elements
    info_box_bg = "#FFFFFF" # White background for info boxes
  ),
  adminlte_sidebar(
    dark_bg = "#333333", # Charcoal for sidebar background
    dark_color = "#FFFFFF", # White text for sidebar elements
    dark_hover_bg = "#555555", # Slightly lighter gray for hover effect
    dark_hover_color = "#FFFFFF" # White text for hover state
  )
)

# Load dataset
shiny_df <- readRDS("./data/shiny_df.rds")

# Generate the list of numeric variables
numeric_vars <- names(shiny_df %>% select(starts_with("Num")))
uno_reverso_numeric <- rev(numeric_vars)

# Create a copy of the original dataset
shiny_df_Original <- shiny_df
