# global.R

library(shiny)
library(tidyverse)
library(DT)

# Constants
MAX_STRINGS <- 20
MAX_STRINGS_PER_ROW <- 10

# Default input file path
# default_input_file <- "/Users/alfonsoaceves/Desktop/script lab/LIB Useful Code/Shelves mapping/combined_with_shelving.csv"

default_input_file <- "combined_with_shelving.csv"

# Load default data
input_table <- read_csv(default_input_file, col_types = cols(.default = "c"))

# You can add global helper functions here or in functions.R later
