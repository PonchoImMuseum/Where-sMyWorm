library(shiny)
library(tidyverse)
library(DT)
library(reactable)
library(shinyjs)
library(bslib)       # For accordion UI
library(purrr)
library(htmltools)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    input_table = input_table,   # Loaded in global.R with row_id
    stored_strings = character(0),
    search_results = NULL,
    error_msg = NULL,
    matched_catalog_numbers = character(0),
    selected_catalog_table = NULL,
    selected_catalog_title = NULL,  # Store title separately
    show_wormfinder = FALSE,
    catalog_observers = list()  # Track observers for cleanup
  )
  
  # Function to sanitize catalog numbers for button IDs
  sanitize_for_id <- function(cat_num) {
    gsub("[^A-Za-z0-9]", "_", cat_num)
  }
  
  add_string <- function(new_str) {
    new_str <- trimws(new_str)
    if (new_str == "") return()
    if (length(rv$stored_strings) >= MAX_STRINGS) {
      rv$error_msg <- "No more worms can be looked for at once"
      return()
    }
    if (!(new_str %in% rv$stored_strings)) {
      rv$stored_strings <- c(rv$stored_strings, new_str)
      rv$error_msg <- NULL
    }
  }
  
  observeEvent(input$add_btn, {
    add_string(input$catalog_input)
    session$sendCustomMessage("clearCustomInput", "catalog_input")
    session$sendCustomMessage("focusInput", "catalog_input")
  })
  
  observeEvent(input$search_btn, {
    if (nzchar(trimws(input$catalog_input))) {
      add_string(input$catalog_input)
      session$sendCustomMessage("clearCustomInput", "catalog_input")
      session$sendCustomMessage("focusInput", "catalog_input")
    }
    if (length(rv$stored_strings) == 0) {
      rv$error_msg <- "No strings to search for. Add some first."
      return()
    }
    perform_search()
    session$sendCustomMessage("clearCustomInput", "catalog_input")
    session$sendCustomMessage("focusInput", "catalog_input")
  })
  
  observeEvent(input$clear_btn, {
    rv$stored_strings <- character(0)
    rv$search_results <- NULL
    rv$error_msg <- NULL
    rv$matched_catalog_numbers <- character(0)
    rv$selected_catalog_table <- NULL
    rv$selected_catalog_title <- NULL  # Clear title
    rv$show_wormfinder <- FALSE
    
    # Clean up observers
    cleanup_catalog_observers()
    
    session$sendCustomMessage("clearCustomInput", "catalog_input")
  })
  
  observeEvent(input$enter_pressed, {
    add_string(input$catalog_input)
    session$sendCustomMessage("clearCustomInput", "catalog_input")
    session$sendCustomMessage("focusInput", "catalog_input")
  })
  
  observeEvent(input$search_pressed, {
    if (length(rv$stored_strings) == 0) {
      rv$error_msg <- "No strings to search for. Add some first."
      return()
    }
    perform_search()
    session$sendCustomMessage("clearCustomInput", "catalog_input")
    session$sendCustomMessage("focusInput", "catalog_input")
  })
  
  perform_search <- function() {
    results_list <- lapply(rv$stored_strings, function(str) {
      matches <- find_catalog_matches_map(rv$input_table, str)
      
      if (nrow(matches) == 0) {
        tibble(
          catalogNumber = str,
          FAMILY = "No",
          GENUS = "Worm",
          SPECIES = "Found",
          SUBSPECIES = "-",
          aisle = "-",
          shelving_unit = "-",
          shelf = "-"
        )
      } else {
        matches
      }
    })
    
    rv$search_results <- bind_rows(results_list)
    rv$error_msg <- NULL
    
    rv$matched_catalog_numbers <- rv$search_results %>%
      filter(!(FAMILY == "No" & GENUS == "Worm")) %>%
      distinct(catalogNumber) %>%
      pull(catalogNumber)
    
    # Clear selected catalog table and title when new search is performed
    rv$selected_catalog_table <- NULL
    rv$selected_catalog_title <- NULL
    rv$show_wormfinder <- FALSE
    
    # Clean up old observers before creating new ones
    cleanup_catalog_observers()
    
    # Set up new observers for the new catalog numbers
    setup_catalog_observers()
  }
  
  # Function to clean up catalog observers
  cleanup_catalog_observers <- function() {
    if (length(rv$catalog_observers) > 0) {
      for (obs in rv$catalog_observers) {
        if (!is.null(obs)) {
          obs$destroy()
        }
      }
      rv$catalog_observers <- list()
    }
  }
  
  # Function to set up catalog observers
  setup_catalog_observers <- function() {
    cleanup_catalog_observers()
    
    if (length(rv$matched_catalog_numbers) > 0) {
      for (i in seq_along(rv$matched_catalog_numbers)) {
        local({
          cat_num <- rv$matched_catalog_numbers[i]
          button_id <- paste0("catalog_btn_", sanitize_for_id(cat_num))
          
          obs <- observeEvent(input[[button_id]], {
            tryCatch({
              matches <- find_exact_catalog_match(rv$input_table, cat_num)
              if (nrow(matches) > 0) {
                matched_row <- matches %>% slice(1)
                
                aisle_val <- if ("aisle" %in% colnames(matched_row)) matched_row$aisle[1] else NA
                shelving_unit_val <- if ("shelving_unit" %in% colnames(matched_row)) matched_row$shelving_unit[1] else NA
                shelf_val <- if ("shelf" %in% colnames(matched_row)) matched_row$shelf[1] else NA
                
                rv$selected_catalog_title <- paste(
                  "Aisle:", aisle_val,
                  "Shelving Unit:", shelving_unit_val,
                  "Shelf:", shelf_val
                )
                
                # Use the no-title version of the function here
                rv$selected_catalog_table <- display_catalog_around_full_match_no_title(rv$input_table, cat_num)
              }
              rv$show_wormfinder <- FALSE
            }, error = function(e) {
              rv$error_msg <- paste("Error displaying catalog", cat_num, ":", e$message)
            })
          }, ignoreInit = TRUE)
          
          rv$catalog_observers[[button_id]] <- obs
        })
      }
    }
  }
  
  # WormFinder button handler - Toggle visibility
  observeEvent(input$wormfinder_btn, {
    rv$show_wormfinder <- !rv$show_wormfinder
    # Clear selected catalog table and title when toggling WormFinder
    if (rv$show_wormfinder) {
      rv$selected_catalog_table <- NULL
      rv$selected_catalog_title <- NULL
    }
  })
  
  # WormFinder UI output
  output$wormfinder_list <- renderUI({
    if (!rv$show_wormfinder || length(rv$matched_catalog_numbers) == 0) {
      return(NULL)
    }
    
    bslib::accordion(
      id = "wormfinder_accordion",
      lapply(rv$matched_catalog_numbers, function(cat_num) {
        button_id <- paste0("catalog_btn_", sanitize_for_id(cat_num))
        bslib::accordion_panel(
          title = cat_num,
          div(
            style = "text-align: center; margin: 10px 0;",
            actionButton(
              button_id, 
              label = paste("Show", cat_num, "location"),
              style = "background-color: #5AC4F6; color: white; border: none; padding: 8px 16px; border-radius: 5px; cursor: pointer;"
            )
          )
        )
      })
    )
  })
  
  output$stored_strings_ui <- renderUI({
    req(rv$stored_strings)
    if (length(rv$stored_strings) == 0) return(NULL)
    
    rows <- split(rv$stored_strings, ceiling(seq_along(rv$stored_strings) / MAX_STRINGS_PER_ROW))
    
    tagList(
      lapply(rows, function(row_strings) {
        div(style = "width: 600px; margin: 0 auto; display: flex; justify-content: center; gap: 20px; color: #BFBFBF; font-size: 18px; user-select:none;",
            lapply(row_strings, function(s) {
              span(s)
            })
        )
      })
    )
  })
  
  output$clear_button_ui <- renderUI({
    if (length(rv$stored_strings) == 0) return(NULL)
    actionButton("clear_btn", label = "Clear",
                 style = "background:none; border:none; color:#D3D3D3; font-weight:normal; font-size:16px; cursor:pointer; user-select:none; padding:0; text-decoration:underline;")
  })
  
  output$wormfinder_button_ui <- renderUI({
    if (length(rv$matched_catalog_numbers) == 0) return(NULL)
    actionButton("wormfinder_btn", label = "WormFinder",
                 style = "background:none; border:none; color:#D3D3D3; font-weight:normal; font-size:16px; cursor:pointer; user-select:none; padding:0; text-decoration:underline;")
  })
  
  output$export_button_ui <- renderUI({
    if (is.null(rv$search_results) || nrow(rv$search_results) == 0) return(NULL)
    downloadButton("export_btn", "Export",
                   style = "background:none; border:none; color:#D3D3D3; font-weight:normal; font-size:16px; cursor:pointer; user-select:none; padding:0; text-decoration:underline;")
  })
  
  # Output for catalog title (separate from reactable)
  output$catalog_title <- renderUI({
    if (is.null(rv$selected_catalog_title)) return(NULL)
    
    div(
      style = "text-align: center; color: white; font-weight: bold; font-size: 18px; margin-bottom: 15px; padding: 10px; background-color: rgba(255,255,255,0.1); border-radius: 5px;",
      rv$selected_catalog_title
    )
  })
  
  # Updated reactable configuration in server.R
  # Replace the output$results_table section with this:
  
  output$results_table <- renderReactable# Replace ONLY the output$results_table section in server.R
  # Keep everything else exactly as it was
  
  output$results_table <- renderReactable({
    # Show selected catalog table if available
    if (!is.null(rv$selected_catalog_table)) {
      return(rv$selected_catalog_table)
    }
    
    if (!is.null(rv$error_msg)) {
      return(
        reactable(
          tibble(Message = rv$error_msg),
          columns = list(Message = colDef(name = "Message")),
          pagination = FALSE,
          bordered = TRUE,
          striped = TRUE
        )
      )
    }
    
    req(rv$search_results)
    
    reactable(
      rv$search_results,
      columns = list(
        catalogNumber = colDef(name = "Catalog Number", sticky = "left"),
        FAMILY = colDef(name = "Family"),
        GENUS = colDef(name = "Genus"),
        SPECIES = colDef(name = "Species"),
        SUBSPECIES = colDef(name = "Subspecies"),
        aisle = colDef(name = "Aisle"),
        shelving_unit = colDef(name = "Shelving Unit"),
        shelf = colDef(name = "Shelf"),
        row_id = colDef(name = "Row ID"),
        rows_before = colDef(name = "Rows Before"),
        rows_after = colDef(name = "Rows After")
      ),
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      pagination = FALSE,
      height = 600,
      width = 1200,
      style = list(
        backgroundColor = "#3A87FE",
        color = "white"
      ),
      rowStyle = function(index) {
        if (index %% 2 == 0) {
          list(backgroundColor = "#2F6FD9")
        } else {
          list(backgroundColor = "#3A87FE")
        }
      }
    )
  })
  
  output$export_btn <- downloadHandler(
    filename = function() {
      paste0("search_results_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S", tz = "Europe/Berlin"), ".csv")
    },
    content = function(file) {
      req(rv$search_results)
      write.csv(rv$search_results, file, row.names = FALSE)
    }
  )
  
  # Clean up observers when session ends
  session$onSessionEnded(function() {
    cleanup_catalog_observers()
  })
  
}