# server.R

library(shiny)
library(tidyverse)
library(DT)

shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(
    input_table = NULL,
    stored_strings = character(0),
    search_results = NULL,
    error_msg = NULL
  )
  
  # Load default CSV on startup
  observe({
    default_file <- "combined_with_shelving.csv"
    if (file.exists(default_file)) {
      rv$input_table <- read_csv(default_file, col_types = cols(.default = "c"))
    }
  })
  
  # Trigger hidden file input dialog when title clicked
  observeEvent(input$title_text_click, {
    session$sendCustomMessage("triggerFileInput", "file_input")
  })
  
  # Load CSV when file selected via hidden fileInput
  observeEvent(input$file_input, {
    req(input$file_input)
    file <- input$file_input$datapath
    
    new_data <- tryCatch(
      read_csv(file, col_types = cols(.default = "c")),
      error = function(e) NULL
    )
    if (!is.null(new_data)) {
      rv$input_table <- new_data
      rv$stored_strings <- character(0)
      rv$search_results <- NULL
      rv$error_msg <- NULL
      session$sendCustomMessage("clearCustomInput", "catalog_input")
    } else {
      showNotification("Failed to load uploaded CSV file", type = "error")
    }
  })
  
  # Render Clear button only when stored strings exist
  output$clear_button_ui <- renderUI({
    if (length(rv$stored_strings) == 0) return(NULL)
    actionButton("clear_btn", label = "Clear",
                 style = "background:none; border:none; color:#D3D3D3; font-weight:normal; font-size:16px; cursor:pointer; user-select:none; padding:0; text-decoration:underline;")
  })
  
  # Render Export button only when search results exist
  output$export_button_ui <- renderUI({
    if (is.null(rv$search_results) || nrow(rv$search_results) == 0) return(NULL)
    downloadButton("export_btn", "Export",
                   style = "background:none; border:none; color:#D3D3D3; font-weight:normal; font-size:16px; cursor:pointer; user-select:none; padding:0; text-decoration:underline;")
  })
  
  # Add string helper
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
  
  # Add button clicked
  observeEvent(input$add_btn, {
    add_string(input$catalog_input)
    session$sendCustomMessage("clearCustomInput", "catalog_input")
  })
  
  # Search button clicked
  observeEvent(input$search_btn, {
    if (nzchar(trimws(input$catalog_input))) {
      add_string(input$catalog_input)
      session$sendCustomMessage("clearCustomInput", "catalog_input")
    }
    if (length(rv$stored_strings) == 0) {
      rv$error_msg <- "No strings to search for. Add some first."
      return()
    }
    perform_search()
    session$sendCustomMessage("clearCustomInput", "catalog_input")
  })
  
  # Clear button clicked
  observeEvent(input$clear_btn, {
    rv$stored_strings <- character(0)
    rv$search_results <- NULL
    rv$error_msg <- NULL
    session$sendCustomMessage("clearCustomInput", "catalog_input")
  })
  
  # Enter key pressed (add string)
  observeEvent(input$enter_pressed, {
    add_string(input$catalog_input)
    session$sendCustomMessage("clearCustomInput", "catalog_input")
  })
  
  # Cmd/Ctrl + Enter pressed (search)
  observeEvent(input$search_pressed, {
    if (length(rv$stored_strings) == 0) {
      rv$error_msg <- "No strings to search for. Add some first."
      return()
    }
    perform_search()
    session$sendCustomMessage("clearCustomInput", "catalog_input")
  })
  
  # Export results to CSV with timestamp
  output$export_btn <- downloadHandler(
    filename = function() {
      paste0("search_results_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".csv")
    },
    content = function(file) {
      req(rv$search_results)
      write.csv(rv$search_results, file, row.names = FALSE)
    }
  )
  
  # Perform search helper
  perform_search <- function() {
    results_list <- lapply(rv$stored_strings, function(str) {
      matches <- rv$input_table %>%
        filter(str_detect(tolower(catalogNumber), fixed(tolower(str), ignore_case = TRUE)))
      
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
  
  # Render stored strings UI
  output$stored_strings_ui <- renderUI({
    req(rv$stored_strings)
    if (length(rv$stored_strings) == 0) return(NULL)
    
    rows <- split(rv$stored_strings, ceiling(seq_along(rv$stored_strings) / MAX_STRINGS_PER_ROW))
    
    tagList(
      lapply(rows, function(row_strings) {
        div(style = "width: 600px; margin: 0 auto; display: flex; justify-content: center; gap: 20px; color: #BFBFBF; font-size: 18px;",
            lapply(row_strings, function(s) {
              span(s)
            })
        )
      })
    )
  })
  
  # Render results table
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
    
    df <- rv$search_results %>%
      select(catalogNumber, FAMILY, GENUS, SPECIES, SUBSPECIES, aisle, shelving_unit, shelf) %>%
      rename(shelving = shelving_unit)
    
    datatable(
      df,
      rownames = FALSE,
      colnames = c("Catalog Number", "Family", "Genus", "Species", "Subspecies", "aisle", "shelving", "shelf"),
      options = list(
        pageLength = 10,
        lengthChange = FALSE,
        scrollY = "100%",
        scrollCollapse = TRUE,
        dom = 't'
      ),
      class = "cell-border stripe",
      escape = FALSE
    )
  })
  
})
