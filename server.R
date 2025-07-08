library(shiny)
library(tidyverse)
library(DT)
library(reactable)
library(shinyjs)
library(bslib)       # For accordion UI
library(purrr)
library(htmltools)

shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(
    input_table = input_table,   # Loaded in global.R with row_id
    stored_strings = character(0),
    search_results = NULL,
    error_msg = NULL,
    matched_catalog_numbers = character(0)
  )
  
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
    
    # === Add WormFinder button visibility control here ===
    if (length(rv$matched_catalog_numbers) > 0) {
      shinyjs::show("wormfinder_btn")
    } else {
      shinyjs::hide("wormfinder_btn")
      output$wormfinder_list <- renderUI(NULL)  # Clear WormFinder list UI
    }
  }
  
  # Show or hide WormFinder button based on matches
  output$wormfinder_btn_ui <- renderUI({
    if (length(rv$matched_catalog_numbers) > 0) {
      actionButton("wormfinder_btn", "WormFinder", style = "margin: 10px;")
    }
  })
  
  # Render WormFinder catalog numbers list on button click using bslib accordion
  observeEvent(input$wormfinder_btn, {
    req(rv$matched_catalog_numbers)
    output$wormfinder_list <- renderUI({
      bslib::accordion(
        id = "wormfinder_accordion",
        lapply(rv$matched_catalog_numbers, function(cat_num) {
          bslib::accordion_panel(
            title = cat_num,
            div(id = paste0("table_", gsub("[^A-Za-z0-9]", "_", cat_num)), "Table will load here")
          )
        })
      )
    })
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
  
  output$export_button_ui <- renderUI({
    if (is.null(rv$search_results) || nrow(rv$search_results) == 0) return(NULL)
    downloadButton("export_btn", "Export",
                   style = "background:none; border:none; color:#D3D3D3; font-weight:normal; font-size:16px; cursor:pointer; user-select:none; padding:0; text-decoration:underline;")
  })
  
  output$results_table <- renderReactable({
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
        catalogNumber = colDef(name = "Catalog Number"),
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
  
})
