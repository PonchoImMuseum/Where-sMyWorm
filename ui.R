# ui.R

library(shiny)
library(shinyjs)  # Load shinyjs

shinyUI(
  fluidPage(
    useShinyjs(),  # Initialize shinyjs
    
    tags$head(
      tags$style(HTML("
        body {
          background-color: #3A87FE;
          color: white;
          font-family: Arial, sans-serif;
          text-align: center;
        }
        #title-text {
          font-size: 36px;
          font-weight: bold;
          margin: 20px 0;
          cursor: pointer;
          user-select: none;
        }
        #search-bar {
          width: 600px;
          margin: 0 auto;
          display: flex;
          align-items: center;
          gap: 10px;
          background-color: #FFFFFF;
          border: 4px solid #5AC4F6;
          border-radius: 15px;
          padding: 5px 15px;
          height: 50px;
          box-sizing: border-box;
        }
        #catalog_input {
          flex-grow: 1;
          height: 40px;
          font-size: 20px;
          padding: 0 15px;
          border: none;
          border-radius: 10px;
          text-align: center;
          line-height: 40px;
          box-sizing: border-box;
          color: black;
          outline: none;
          display: block;
        }
        .search-button {
          width: 40px;
          height: 40px;
          border-radius: 10px;
          border: none;
          background-color: #BFBFBF;
          color: black;
          font-weight: bold;
          font-size: 20px;
          cursor: pointer;
          user-select: none;
          flex-shrink: 0;
        }
        #stored-strings {
          color: #BFBFBF;
          margin: 10px auto 10px auto;
          width: 600px;
          min-height: 40px;
          display: flex;
          flex-wrap: wrap;
          justify-content: center;
          gap: 15px;
          font-size: 18px;
          user-select: none;
        }
        #clear-btn-container {
          width: 600px;
          margin: 0 auto 20px auto;
          text-align: center;
        }
        #clear_btn {
          background: none;
          border: none;
          color: #D3D3D3;
          font-weight: normal;
          font-size: 16px;
          cursor: pointer;
          user-select: none;
          padding: 0;
          text-decoration: underline;
        }
        #results-container {
          width: 90%;
          max-width: 900px;
          margin: 0 auto 20px auto;
          overflow-y: auto;
          max-height: 400px;
          background-color: rgba(255,255,255,0.1);
          border-radius: 10px;
          padding: 10px;
          color: white;
          font-size: 16px;
        }
        footer {
          color: white;
          font-size: 14px;
          margin-bottom: 15px;
          user-select: none;
        }
        /* Hide the file input */
        #file_input {
          display: none !important;
        }
      ")),
      
      # JavaScript snippet for enhanced keyboard behavior, title click and clearing input
      tags$script(HTML("
        // Keyboard handling on catalog_input
        $(document).on('keydown', '#catalog_input', function(e) {
          if (e.key === 'Enter') {
            e.preventDefault();
            if (e.metaKey || e.ctrlKey) {
              Shiny.setInputValue('search_pressed', new Date().getTime());
            } else {
              Shiny.setInputValue('enter_pressed', new Date().getTime());
            }
          }
        });

        // Title click triggers hidden file input
        $(document).on('click', '#title-text', function() {
          Shiny.setInputValue('title_text_click', new Date().getTime());
        });

        Shiny.addCustomMessageHandler('triggerFileInput', function(id) {
          var el = document.getElementById(id);
          if(el) el.click();
        });

        Shiny.addCustomMessageHandler('clearCustomInput', function(id) {
          var el = document.getElementById(id);
          if (el) el.value = '';
        });
      "))
    ),
    
    # Title clickable text
    div(id = "title-text", "Where's My Worm?"),
    
    # Hidden file input wrapped in div with display:none
    div(style = "display:none;",
        fileInput("file_input", "Select input CSV file", accept = ".csv")
    ),
    
    # Search bar container with input and buttons
    div(id = "search-bar",
        tags$input(id = "catalog_input", type = "text", placeholder = "Type catalogNumber...", autocomplete = "off"),
        actionButton("add_btn", label = "+", class = "search-button"),
        actionButton("search_btn", label = "\uD83D\uDD0D", class = "search-button")
    ),
    
    # Stored strings display
    div(id = "stored-strings", uiOutput("stored_strings_ui")),
    
    # Clear button container
    div(id = "clear-btn-container",
        uiOutput("clear_button_ui")
    ),
    
    # Results table output
    div(id = "results-container",
        DT::dataTableOutput("results_table")
    ),
    
    # Footer
    tags$footer("Prototype app by El Poncho")
  )
)
