library(shiny)
library(shinyjs)    # For JavaScript enhancements
library(shinyWidgets) # For accordion widget

ui <- div(id = "shiny-app-container",
          fluidPage(
            useShinyjs(),  # Initialize shinyjs
            
            tags$head(
              tags$style(HTML("
            /* Reset full height for html and body */
            html, body {
              height: 100%;
              margin: 0;
              padding: 0;
            }

            /* Main container fills viewport and uses flex column */
            #shiny-app-container {
              display: flex;
              flex-direction: column;
              min-height: 100vh;
              background-color: #3A87FE;
              color: white;
              font-family: Arial, sans-serif;
              text-align: center;
            }

            /* Title styling */
            #title-text {
              font-size: 36px;
              font-weight: bold;
              margin: 20px 0;
              cursor: pointer;
              user-select: none;
            }

            /* Search bar container styling */
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

            /* Search input styling */
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

            /* Search buttons styling */
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

            /* Stored strings display styling */
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

            /* Clear, WormFinder, and Export buttons container styling */
            #clear-btn-container {
              width: 600px;
              margin: 0 auto 20px auto;
              text-align: center;
            }

            #clear-export-buttons {
              display: inline-flex;
              gap: 30px;
              align-items: center;
              justify-content: center;
            }

            /* Clear, WormFinder, and Export buttons styling */
            #clear_btn, #wormfinder_btn, #export_btn {
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

            /* Updated CSS for ui.R - Replace the #results-container section */

            /* Results container - Updated for sticky column support */
           /* Add these CSS rules to your existing ui.R CSS - DO NOT replace existing rules */

            /* Results container - ONLY modify overflow and width */
            #results-container {
              flex-grow: 1;
              min-height: 0;
              overflow-x: auto; 
              overflow-y: auto;
              width: 95%; /* Increased from 90% to allow horizontal scrolling */
              max-width: 1400px; /* Increased from 1000px to accommodate wider table */
              margin: 0 auto;
              box-sizing: border-box;
              padding: 10px 10px 50px 10px;
              background-color: rgba(255,255,255,0.1);
              border-radius: 10px;
              color: white;
              font-size: 16px;
              display: flex;
              flex-direction: column;
            }
            
            /* Ensure reactable sticky columns work properly */
.reactable .rt-table {
  overflow: visible !important;
}

/* Enable sticky headers for vertical scrolling */
.reactable .rt-thead .rt-th {
  position: sticky !important;
  top: 0 !important;
  z-index: 30 !important;
  background-color: #3A87FE !important;
}

/* Fix sticky column positioning with proper background and z-index */
.reactable .rt-td-sticky-left,
.reactable .rt-th-sticky-left {
  position: sticky !important;
  left: 0 !important;
  z-index: 15 !important;
  background-color: inherit !important;
  /* Add border to prevent content bleeding */
  border-right: 1px solid rgba(255, 255, 255, 0.2) !important;
}

/* Ensure sticky header cells have higher z-index and solid background */
.reactable .rt-th-sticky-left {
  z-index: 35 !important;
  /* Ensure header has solid background */
  background-color: #3A87FE !important;
  /* Add box shadow to create separation */
  box-shadow: 2px 0 4px rgba(0, 0, 0, 0.1) !important;
  /* Ensure it's sticky both horizontally and vertically */
  position: sticky !important;
  top: 0 !important;
  left: 0 !important;
}

/* Fix sticky data cells to have proper background based on row styling */
.reactable .rt-td-sticky-left {
  z-index: 20 !important;
  /* Add box shadow for separation */
  box-shadow: 2px 0 4px rgba(0, 0, 0, 0.1) !important;
}

/* Ensure alternating row colors are maintained on sticky cells */
.reactable .rt-tr:nth-child(even) .rt-td-sticky-left {
  background-color: #2F6FD9 !important;
}

.reactable .rt-tr:nth-child(odd) .rt-td-sticky-left {
  background-color: #3A87FE !important;
}

/* Handle special row styling for highlighted rows */
.reactable .rt-tr.highlighted-row .rt-td-sticky-left {
  background-color: #fff9c4 !important;
  color: black !important;
}

/* Handle green boundary row styling - using class-based approach */
.reactable .rt-tr.boundary-row .rt-td-sticky-left {
  background-color: #d0f0c0 !important;
  color: black !important;
}

/* Fix any potential overflow issues */
.reactable .rt-thead,
.reactable .rt-tbody {
  position: relative !important;
  z-index: 10 !important;
}

/* Ensure the table container doesn't interfere */
.reactable {
  position: relative !important;
  z-index: 5 !important;
}
            
            /* Catalog title styling - Updated */
            #catalog_title {
              margin-bottom: 15px;
              position: relative;
              z-index: 5;
            }

            /* WormFinder container styling */
            #wormfinder-container {
              width: 600px;
              margin: 20px auto;
              color: white;
              text-align: left;
            }

            /* Footer fixed near bottom with margin */
            footer {
              position: fixed;
              bottom: 10px;
              left: 10px;
              right: 10px;
              color: white;
              font-size: 14px;
              text-align: center;
              user-select: none;
              background: transparent;
              padding: 5px 0;
              z-index: 1000;
            }

            /* Hide the file input */
            #file_input {
              display: none !important;
            }
          ")),
              
              tags$script(HTML("
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

            Shiny.addCustomMessageHandler('focusInput', function(id) {
              var el = document.getElementById(id);
              if (el) el.focus();
            });
          "))
            ),
            
            # App title
            div(id = "title-text", "Where's My Worm?"),
            
            # Hidden file input for CSV upload
            div(style = "display:none;",
                fileInput("file_input", "Select input CSV file", accept = ".csv")
            ),
            
            # Search bar with input and buttons
            div(id = "search-bar",
                tags$input(id = "catalog_input", type = "text", placeholder = "Type catalogNumber...", autocomplete = "off"),
                actionButton("add_btn", label = "+", class = "search-button"),
                actionButton("search_btn", label = "\uD83D\uDD0D", class = "search-button")
            ),
            
            # Display stored search strings
            div(id = "stored-strings", uiOutput("stored_strings_ui")),
            
            # Clear, WormFinder, and Export buttons container (updated order)
            div(id = "clear-btn-container",
                div(id = "clear-export-buttons",
                    uiOutput("clear_button_ui"),
                    uiOutput("wormfinder_button_ui"),  # WormFinder button in the middle
                    uiOutput("export_button_ui")
                )
            ),
            
            # WormFinder accordion container
            div(id = "wormfinder-container",
                uiOutput("wormfinder_list")
            ),
            
            # Results container with reactable output
            div(id = "results-container",
                # Add the catalog title output here, above the reactable
                uiOutput("catalog_title"),
                reactableOutput("results_table")
            ),
            
            # Footer text
            tags$footer("Prototype app by El Poncho")
          )
)