library(tidyverse)
library(reactable)
library(htmltools)
library(purrr)

find_catalog_matches_map <- function(data, input_num) {
  pattern <- str_c("(?<=^|\\D)0{0,5}", input_num, "$")
  
  # data must already have row_id column
  
  matched_rows <- data %>%
    filter(str_detect(catalogNumber, regex(pattern)))
  
  if (nrow(matched_rows) == 0) {
    # Return empty tibble with expected columns to avoid errors downstream
    return(tibble())
  }
  
  process_one <- function(match_row) {
    cur_row_id <- match_row$row_id
    cur_shelf <- match_row$shelf
    
    prev_rows <- data %>% filter(row_id < cur_row_id)
    next_rows <- data %>% filter(row_id > cur_row_id)
    
    last_prev_shelf_row <- prev_rows %>%
      filter(shelf != cur_shelf) %>%
      summarise(max_row = max(row_id)) %>%
      pull(max_row)
    
    first_next_shelf_row <- next_rows %>%
      filter(shelf != cur_shelf) %>%
      summarise(min_row = min(row_id)) %>%
      pull(min_row)
    
    rows_before <- ifelse(
      length(last_prev_shelf_row) == 0 || is.na(last_prev_shelf_row),
      NA_integer_,
      max(cur_row_id - last_prev_shelf_row - 1, 0)
    )
    
    rows_after <- ifelse(
      length(first_next_shelf_row) == 0 || is.na(first_next_shelf_row),
      NA_integer_,
      max(first_next_shelf_row - cur_row_id - 1, 0)
    )
    
    match_row %>%
      mutate(
        rows_before = rows_before,
        rows_after = rows_after
      )
  }
  
  results_list <- matched_rows %>%
    split(.$row_id) %>%
    map_dfr(process_one)
  
  return(results_list)
}

# NEW FUNCTION: Find exact catalog number match
find_exact_catalog_match <- function(data, catalog_number) {
  # data must already have row_id column
  
  matched_rows <- data %>%
    filter(catalogNumber == catalog_number)
  
  if (nrow(matched_rows) == 0) {
    # Return empty tibble with expected columns to avoid errors downstream
    return(tibble())
  }
  
  process_one <- function(match_row) {
    cur_row_id <- match_row$row_id
    cur_shelf <- match_row$shelf
    
    prev_rows <- data %>% filter(row_id < cur_row_id)
    next_rows <- data %>% filter(row_id > cur_row_id)
    
    last_prev_shelf_row <- prev_rows %>%
      filter(shelf != cur_shelf) %>%
      summarise(max_row = max(row_id)) %>%
      pull(max_row)
    
    first_next_shelf_row <- next_rows %>%
      filter(shelf != cur_shelf) %>%
      summarise(min_row = min(row_id)) %>%
      pull(min_row)
    
    rows_before <- ifelse(
      length(last_prev_shelf_row) == 0 || is.na(last_prev_shelf_row),
      NA_integer_,
      max(cur_row_id - last_prev_shelf_row - 1, 0)
    )
    
    rows_after <- ifelse(
      length(first_next_shelf_row) == 0 || is.na(first_next_shelf_row),
      NA_integer_,
      max(first_next_shelf_row - cur_row_id - 1, 0)
    )
    
    match_row %>%
      mutate(
        rows_before = rows_before,
        rows_after = rows_after
      )
  }
  
  results_list <- matched_rows %>%
    split(.$row_id) %>%
    map_dfr(process_one)
  
  return(results_list)
}

# Original function (for numerical input)
display_catalog_around_match <- function(data, input_num) {
  # data must already have row_id column
  
  matches <- find_catalog_matches_map(data, input_num)
  
  if (nrow(matches) == 0) stop("No matches found for input: ", input_num)
  
  matched_row <- matches %>% slice(1)
  
  rows_before <- matched_row$rows_before
  rows_after <- matched_row$rows_after
  
  smaller_count <- min(rows_before, rows_after)
  if (is.na(smaller_count)) smaller_count <- 0
  
  if (smaller_count < 5) {
    if (rows_before <= rows_after) {
      display_before <- rows_before
      display_after <- 10
    } else {
      display_before <- 10
      display_after <- rows_after
    }
  } else {
    if (rows_before <= rows_after) {
      display_before <- smaller_count
      display_after <- 5
    } else {
      display_before <- 5
      display_after <- smaller_count
    }
  }
  
  start_row <- max(matched_row$row_id - display_before, 1)
  end_row <- min(matched_row$row_id + display_after, nrow(data))
  
  subset_df <- data %>%
    filter(row_id >= start_row & row_id <= end_row) %>%
    mutate(
      is_matched = (row_id == matched_row$row_id),
      neighbours = row_number() - (display_before + 1)
    ) %>%
    select(-row_id)
  
  css <- "
  .highlighted-row {
    background-color: #fff9c4;
  }
  "
  
  reactable(
    subset_df,
    columns = list(
      is_matched = colDef(show = FALSE),
      neighbours = colDef(name = "Neighbours", align = "center", width = 80)
    ),
    rowClass = function(index) {
      if (subset_df$is_matched[index]) "highlighted-row" else NULL
    },
    highlight = TRUE,
    bordered = TRUE,
    striped = TRUE,
    pagination = FALSE,
    height = 600,
    width = 1200
  ) %>%
    htmltools::browsable() %>%
    htmltools::tagList(
      htmltools::tags$style(css)
    )
}

# # NEW FUNCTION: Display catalog around match using full catalog number
# display_catalog_around_full_match <- function(data, catalog_number) {
#   # data must already have row_id column
#   
#   matches <- find_exact_catalog_match(data, catalog_number)
#   
#   if (nrow(matches) == 0) stop("No matches found for catalog number: ", catalog_number)
#   
#   matched_row <- matches %>% slice(1)
#   
#   rows_before <- matched_row$rows_before
#   rows_after <- matched_row$rows_after
#   
#   smaller_count <- min(rows_before, rows_after)
#   if (is.na(smaller_count)) smaller_count <- 0
#   
#   if (smaller_count < 5) {
#     if (rows_before <= rows_after) {
#       display_before <- rows_before
#       display_after <- 10
#     } else {
#       display_before <- 10
#       display_after <- rows_after
#     }
#   } else {
#     if (rows_before <= rows_after) {
#       display_before <- smaller_count
#       display_after <- 5
#     } else {
#       display_before <- 5
#       display_after <- smaller_count
#     }
#   }
#   
#   start_row <- max(matched_row$row_id - display_before, 1)
#   end_row <- min(matched_row$row_id + display_after, nrow(data))
#   
#   subset_df <- data %>%
#     filter(row_id >= start_row & row_id <= end_row) %>%
#     mutate(
#       is_matched = (row_id == matched_row$row_id),
#       neighbours = row_number() - (display_before + 1)
#     ) %>%
#     select(-row_id)
#   
#   # Determine the closest boundary row index to highlight in green
#   # If rows_before <= rows_after, closest boundary is the first row in subset_df
#   # Else, closest boundary is the last row in subset_df
#   closest_boundary_index <- if (rows_before <= rows_after) 1 else nrow(subset_df)
#   
#   # Get actual column names for reactable columns
#   data_cols <- names(subset_df)
#   
#   columns_list <- list(
#     is_matched = colDef(show = FALSE),
#     neighbours = colDef(name = "Neighbours", align = "center", width = 80)
#   )
#   
#   # Add other columns dynamically
#   if ("catalogNumber" %in% data_cols) columns_list$catalogNumber <- colDef(name = "Catalog Number")
#   if ("FAMILY" %in% data_cols) columns_list$FAMILY <- colDef(name = "Family")
#   if ("GENUS" %in% data_cols) columns_list$GENUS <- colDef(name = "Genus")
#   if ("SPECIES" %in% data_cols) columns_list$SPECIES <- colDef(name = "Species")
#   if ("SUBSPECIES" %in% data_cols) columns_list$SUBSPECIES <- colDef(name = "Subspecies")
#   if ("aisle" %in% data_cols) columns_list$aisle <- colDef(name = "Aisle")
#   if ("shelving_unit" %in% data_cols) columns_list$shelving_unit <- colDef(name = "Shelving Unit")
#   if ("shelf" %in% data_cols) columns_list$shelf <- colDef(name = "Shelf")
#   if ("rows_before" %in% data_cols) columns_list$rows_before <- colDef(name = "Rows Before")
#   if ("rows_after" %in% data_cols) columns_list$rows_after <- colDef(name = "Rows After")
#   
#   reactable(
#     subset_df,
#     columns = columns_list,
#     rowStyle = function(index) {
#       if (subset_df$is_matched[index]) {
#         list(backgroundColor = "#fff9c4", color = "black")  # matched row yellow
#       } else if (index == closest_boundary_index) {
#         list(backgroundColor = "#d0f0c0", color = "black")  # closest boundary row green
#       } else if (index %% 2 == 0) {
#         list(backgroundColor = "#2F6FD9", color = "white")
#       } else {
#         list(backgroundColor = "#3A87FE", color = "white")
#       }
#     },
#     highlight = TRUE,
#     bordered = TRUE,
#     striped = TRUE,
#     pagination = FALSE,
#     height = 600,
#     width = 1200,
#     style = list(
#       backgroundColor = "#3A87FE",
#       color = "white"
#     )
#   )
# }


# UPDATED FUNCTION: Display catalog around match using full catalog number
display_catalog_around_full_match <- function(data, catalog_number) {
  # data must already have row_id column
  
  matches <- find_exact_catalog_match(data, catalog_number)
  
  if (nrow(matches) == 0) stop("No matches found for catalog number: ", catalog_number)
  
  matched_row <- matches %>% slice(1)
  
  rows_before <- matched_row$rows_before
  rows_after <- matched_row$rows_after
  
  smaller_count <- min(rows_before, rows_after)
  if (is.na(smaller_count)) smaller_count <- 0
  
  if (smaller_count < 5) {
    if (rows_before <= rows_after) {
      display_before <- rows_before
      display_after <- 10
    } else {
      display_before <- 10
      display_after <- rows_after
    }
  } else {
    if (rows_before <= rows_after) {
      display_before <- smaller_count
      display_after <- 5
    } else {
      display_before <- 5
      display_after <- smaller_count
    }
  }
  
  start_row <- max(matched_row$row_id - display_before, 1)
  end_row <- min(matched_row$row_id + display_after, nrow(data))
  
  subset_df <- data %>%
    filter(row_id >= start_row & row_id <= end_row) %>%
    mutate(
      is_matched = (row_id == matched_row$row_id),
      neighbours = row_number() - (display_before + 1)
    ) %>%
    select(-row_id)
  
  # Extract aisle, shelving_unit, shelf from the first row for title
  aisle_val <- if ("aisle" %in% colnames(subset_df)) subset_df$aisle[1] else NA
  shelving_unit_val <- if ("shelving_unit" %in% colnames(subset_df)) subset_df$shelving_unit[1] else NA
  shelf_val <- if ("shelf" %in% colnames(subset_df)) subset_df$shelf[1] else NA
  
  # Remove aisle, shelving_unit, shelf columns from subset_df
  subset_df <- subset_df %>%
    select(-any_of(c("aisle", "shelving_unit", "shelf")))
  
  # Determine the closest boundary row index to highlight in green
  closest_boundary_index <- if (rows_before <= rows_after) 1 else nrow(subset_df)
  
  data_cols <- names(subset_df)
  
  columns_list <- list(
    is_matched = colDef(show = FALSE),
    neighbours = colDef(name = "Neighbours", align = "center", width = 80)
  )
  
  # Add other columns dynamically except aisle, shelving_unit, shelf
  if ("catalogNumber" %in% data_cols) columns_list$catalogNumber <- colDef(name = "Catalog Number")
  if ("FAMILY" %in% data_cols) columns_list$FAMILY <- colDef(name = "Family")
  if ("GENUS" %in% data_cols) columns_list$GENUS <- colDef(name = "Genus")
  if ("SPECIES" %in% data_cols) columns_list$SPECIES <- colDef(name = "Species")
  if ("SUBSPECIES" %in% data_cols) columns_list$SUBSPECIES <- colDef(name = "Subspecies")
  if ("rows_before" %in% data_cols) columns_list$rows_before <- colDef(name = "Rows Before")
  if ("rows_after" %in% data_cols) columns_list$rows_after <- colDef(name = "Rows After")
  
  # Create the reactable
  tbl <- reactable(
    subset_df,
    columns = columns_list,
    rowStyle = function(index) {
      if (subset_df$is_matched[index]) {
        list(backgroundColor = "#fff9c4", color = "black")  # matched row yellow
      } else if (index == closest_boundary_index) {
        list(backgroundColor = "#d0f0c0", color = "black")  # closest boundary row green
      } else if (index %% 2 == 0) {
        list(backgroundColor = "#2F6FD9", color = "white")
      } else {
        list(backgroundColor = "#3A87FE", color = "white")
      }
    },
    highlight = TRUE,
    bordered = TRUE,
    striped = TRUE,
    pagination = FALSE,
    height = 600,
    width = 1200,
    compact = TRUE,
    style = list(
      backgroundColor = "#3A87FE",
      color = "white"
    )
  )
  
  # Compose title string from aisle, shelving_unit, shelf values
  title_text <- paste(
    "Aisle:", aisle_val,
    "Shelving Unit:", shelving_unit_val,
    "Shelf:", shelf_val
  )
  
  # Add centered title using add_title from reactablefmtr
  tbl_with_title <- add_title(tbl, title = title_text, align = "center", font_weight = "bold", font_size = 16)
  
  return(tbl_with_title)
}


# Helper function to inspect your data structure (for debugging)
inspect_data_structure <- function(data) {
  cat("Column names in data:\n")
  print(names(data))
  cat("\nFirst few rows:\n")
  print(head(data, 3))
  cat("\nData types:\n")
  print(sapply(data, class))
}