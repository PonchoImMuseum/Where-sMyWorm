
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

# Example usage:
# df <- read_csv("combined_with_shelving.csv") %>% mutate(row_id = row_number())
# matches <- find_catalog_matches_map(df, "0089")
# print(matches)




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


# Example usage:
# df <- read_csv("combined_with_shelving.csv") %>% mutate(row_id = row_number())
# display_catalog_around_match(df, "089")

