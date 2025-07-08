library(shiny)
library(tidyverse)
library(DT)
library(reactable)  # Loaded here for future use if needed

shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(
    input_table = input_table,   # Loaded in global.R with row_id
    stored_strings = character(0),
    search_results = NULL,
    error_msg = NULL
  )
  
  # Helper function to add a new search string
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
  
  # Add button clicked: add string to stored_strings
  observeEvent(input$add_btn, {
    add_string(input$catalog_input)
    session$sendCustomMessage("clearCustomInput", "catalog_input")
    session$sendCustomMessage("focusInput", "catalog_input")
  })
  
  # Search button clicked: add string if non-empty, then perform search
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
  
  # Clear button clicked: reset stored strings and results
  observeEvent(input$clear_btn, {
    rv$stored_strings <- character(0)
    rv$search_results <- NULL
    rv$error_msg <- NULL
    session$sendCustomMessage("clearCustomInput", "catalog_input")
  })
  
  # Enter key pressed: add string
  observeEvent(input$enter_pressed, {
    add_string(input$catalog_input)
    session$sendCustomMessage("clearCustomInput", "catalog_input")
    session$sendCustomMessage("focusInput", "catalog_input")
  })
  
  # Cmd/Ctrl + Enter pressed: perform search
  observeEvent(input$search_pressed, {
    if (length(rv$stored_strings) == 0) {
      rv$error_msg <- "No strings to search for. Add some first."
      return()
    }
    perform_search()
    session$sendCustomMessage("clearCustomInput", "catalog_input")
    session$sendCustomMessage("focusInput", "catalog_input")
  })
  
  # Updated perform_search: use find_catalog_matches_map for each stored string
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
  }
  
  # Render stored strings UI below search bar
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
  
  # Render Clear button UI conditionally
  output$clear_button_ui <- renderUI({
    if (length(rv$stored_strings) == 0) return(NULL)
    actionButton("clear_btn", label = "Clear",
                 style = "background:none; border:none; color:#D3D3D3; font-weight:normal; font-size:16px; cursor:pointer; user-select:none; padding:0; text-decoration:underline;")
  })
  
  # Render Export button UI conditionally
  output$export_button_ui <- renderUI({
    if (is.null(rv$search_results) || nrow(rv$search_results) == 0) return(NULL)
    downloadButton("export_btn", "Export",
                   style = "background:none; border:none; color:#D3D3D3; font-weight:normal; font-size:16px; cursor:pointer; user-select:none; padding:0; text-decoration:underline;")
  })
  
  # Render results table with all columns from search_results using DT
  output$results_table <- DT::renderDataTable({
    if (!is.null(rv$error_msg)) {
      return(datatable(
        data.frame(Message = rv$error_msg),
        options = list(dom = 't'),
        rownames = FALSE,
        colnames = ""
      ))
    }
    
    req(rv$search_results)
    
    datatable(
      rv$search_results,
      rownames = FALSE,
      options = list(
        pageLength = -1,
        lengthChange = FALSE,
        scrollY = "100%",
        scrollCollapse = TRUE,
        dom = 't'
      ),
      class = "cell-border stripe",
      escape = FALSE
    )
  })
  
  # Export search results to CSV with timestamp
  output$export_btn <- downloadHandler(
    filename = function() {
      paste0("search_results_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S", tz = "Europe/Berlin"), ".csv")
    },
    content = function(file) {
      req(rv$search_results)
      write.csv(rv$search_results, file, row.names = FALSE)
    }
  )
  
})
