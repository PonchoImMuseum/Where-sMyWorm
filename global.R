library(shiny)
library(tidyverse)
library(DT)
library(reactable)
library(purrr)
library(htmltools)

# Constants
MAX_STRINGS <- 40
MAX_STRINGS_PER_ROW <- 10

# Default input file path
default_input_file <- "combined_with_shelving.csv"

# Load default data once and add row_id
input_table <- read_csv(default_input_file, col_types = cols(.default = "c")) %>%
  mutate(row_id = row_number())

# Source your functions
source("functions.R")
