
## Herramientas

- [RIS to APA converter](https://www.bibtex.com/c/bibtex-to-apa-converter/)
- [RIS to APA converter](https://paperpile.com/t/ris-to-apa-converter/)


## Paquetes

- https://github.com/ropensci/RefManageR
- https://docs.ropensci.org/handlr/reference/bibtex_writer.html

## example of apps

```r
# app.R: Shiny Reader for RIS files with APA7 display

# Load required packages
library(shiny)
library(RefManageR)   # for reading RIS and formatting APA7

# UI
ui <- fluidPage(
  titlePanel("RIS to APA7 Reference Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("risFile", "Upload RIS File",
                accept = c(".ris")),
      actionButton("loadRefs", "Load & Display", class = "btn-primary")
    ),
    mainPanel(
      uiOutput("refListUI"),
      verbatimTextOutput("errorLog")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive: read RIS and format
  refs_apa <- eventReactive(input$loadRefs, {
    req(input$risFile)
    inFile <- input$risFile$datapath
    # Try reading RIS
    bib <- NULL
    errors <- NULL
    tryCatch({
      bib <- ReadRIS(inFile)
    }, error = function(e) {
      errors <<- c(errors, paste0("Failed to read RIS file: ", e$message))
    })
    # If read failed, return list with errors
    if (is.null(bib)) {
      return(list(formatted = NULL, errors = errors))
    }
    # Identify problematic entries: missing author, year, or title
    missing_idx <- which(sapply(bib, function(entry) {
      any(is.null(entry$author) || is.null(entry$year) || is.null(entry$title))
    }))
    if (length(missing_idx) > 0) {
      errors <- c(errors,
                  paste0("References with missing fields at positions: ",
                         paste(missing_idx, collapse = ", ")
                  ))
    }
    # Format all entries in APA7
    formatted <- tryCatch({
      # PrintBibliography returns a character vector
      PrintBibliography(bib, .opts = list(style = "citation", bib.style = "APA"),
                        .newline = "\n", return.ind = FALSE)
    }, error = function(e) {
      errors <<- c(errors, paste0("Failed to format references: ", e$message))
      NULL
    })
    list(formatted = formatted, errors = errors)
  })

  # Render formatted references
  output$refListUI <- renderUI({
    res <- refs_apa()
    if (is.null(res$formatted)) {
      return(div("No references to display."))
    }
    # Show each citation as paragraph
    citation_tags <- lapply(res$formatted, function(txt) {
      tags$p(txt)
    })
    do.call(tagList, citation_tags)
  })

  # Render error messages
  output$errorLog <- renderText({
    res <- refs_apa()
    if (length(res$errors) == 0) return("")
    paste(res$errors, collapse = "\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

```

```r
library(shiny)
library(handlr)
library(RefManageR)
library(DT)
library(shinyjs)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(),
  titlePanel("RIS to APA 7th Edition Converter"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("file", "Upload RIS File", accept = ".ris"),
      helpText("Supported formats: RIS (.ris)"),
      tags$hr(),
      actionButton("apa_info", "APA Format Guidelines", 
                   onclick = "window.open('https://apastyle.apa.org/style-grammar-guidelines/references/examples')")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Formatted References",
                 br(),
                 uiOutput("references_ui")),
        tabPanel("Problematic Entries",
                 br(),
                 DTOutput("error_table")),
        tabPanel("Original RIS Data",
                 br(),
                 verbatimTextOutput("raw_data"))
      )
    )
  )
)

server <- function(input, output, session) {
  bib_entries <- reactiveVal(NULL)
  error_df <- reactiveVal(data.frame(Entry = numeric(), Issue = character()))
  
  # Custom APA formatter with error handling
  format_apa <- function(bib) {
    tryCatch({
      entries <- lapply(bib, function(entry) {
        # Capture APA-formatted citation
        cit <- capture.output(PrintBibliography(entry, .opts = list(style = "apa", first.inits = FALSE)))
        # Add hanging indent using CSS
        div(style = "text-indent: -2em; margin-left: 2em;", HTML(paste(cit, collapse = "<br/>")))
      })
      return(entries)
    }, error = function(e) {
      return(list(div(class = "text-danger", "Formatting error occurred")))
    })
  }
  
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      # Read and convert RIS using handlr
      ris <- ris_reader(input$file$datapath)
      bib_text <- ris$write("bibtex")
      
      # Write to temporary file for RefManageR
      tmp <- tempfile(fileext = ".bib")
      writeLines(bib_text, tmp)
      
      # Read with RefManageR and validate
      bib <- ReadBib(tmp, check = FALSE)
      validate(need(length(bib) > 0, "No valid references found"))
      
      # Check for common issues
      errors <- data.frame()
      for (i in seq_along(bib)) {
        issues <- character()
        entry <- bib[[i]]
        
        if (is.null(entry$author)) issues <- c(issues, "Missing author")
        if (is.null(entry$year)) issues <- c(issues, "Missing year")
        if (is.null(entry$title)) issues <- c(issues, "Missing title")
        if (is.null(entry$journal) && entry$bibtype == "Article") {
          issues <- c(issues, "Missing journal")
        }
        
        if (length(issues) > 0) {
          errors <- rbind(errors, data.frame(Entry = i, Issue = paste(issues, collapse = ", ")))
        }
      }
      
      bib_entries(bib)
      error_df(errors)
      
      if (nrow(errors) > 0) {
        showNotification(paste("Found", nrow(errors), "entries with formatting issues"), 
                         type = "warning")
      }
      
    }, error = function(e) {
      showNotification(paste("Error processing file:", e$message), type = "error")
    })
  })
  
  output$references_ui <- renderUI({
    req(bib_entries())
    apa_list <- tagList()
    formatted <- format_apa(bib_entries())
    
    for (i in seq_along(formatted)) {
      apa_list <- tagAppendChild(apa_list, formatted[[i]])
      apa_list <- tagAppendChild(apa_list, tags$hr())
    }
    
    div(class = "apa-references",
        style = "font-family: Times New Roman; line-height: 1.5;",
        apa_list)
  })
  
  output$error_table <- renderDT({
    req(error_df())
    datatable(error_df(),
              options = list(pageLength = 5, autoWidth = TRUE),
              rownames = FALSE,
              caption = "Entries with missing required fields") %>%
      formatStyle(columns = 1:2, fontSize = '14px')
  })
  
  output$raw_data <- renderPrint({
    req(input$file)
    cat(readLines(input$file$datapath), sep = "\n")
  })
}

shinyApp(ui, server)
```

```r
# Load necessary libraries
# Install them if you haven't already:
# install.packages(c("shiny", "handlr"))

library(shiny)
library(handlr)

# Define supported formats (check handlr documentation/source for the most up-to-date list)
# Based on documentation and common usage:
supported_input_formats <- c(
    "BibTeX" = "bibtex",
    "Citeproc JSON" = "citeproc",
    "Codemeta JSON" = "codemeta",
    "RIS" = "ris",
    "Citation File Format (CFF)" = "cff"
    # Add other formats supported by handlr readers if needed
)

supported_output_formats <- c(
    "BibTeX" = "bibtex",
    "Citeproc JSON" = "citeproc",
    "Codemeta JSON" = "codemeta",
    "RIS" = "ris",
    "Citation File Format (CFF)" = "cff",
    "Schema.org JSON-LD" = "schema_org" # Example of another writer
    # Add other formats supported by handlr writers if needed
)

# Define mapping for file extensions
output_extensions <- c(
    "bibtex" = ".bib",
    "citeproc" = ".json",
    "codemeta" = ".json",
    "ris" = ".ris",
    "cff" = ".cff", # or .yaml, but .cff is specific
    "schema_org" = ".jsonld"
)

# --- User Interface (UI) ---
ui <- fluidPage(
    titlePanel("Citation Format Converter using 'handlr'"),

    sidebarLayout(
        sidebarPanel(
            tags$h4("Input"),
            helpText("Upload your citation file and select its current format."),

            fileInput("inputFile", "1. Upload Citation File:",
                      multiple = FALSE,
                      accept = c(".bib", ".json", ".ris", ".cff", ".yaml", "text/plain") # Common extensions
            ),

            # Input format selection
            selectInput("inputFormat", "2. Select Input Format:",
                        choices = supported_input_formats),

            tags$hr(),

            tags$h4("Output"),
            helpText("Select the desired output format and download the converted file."),

            # Output format selection
            selectInput("outputFormat", "3. Select Output Format:",
                        choices = supported_output_formats),

            # Download button - UI part
            # Disable initially, enable when conversion is ready
            uiOutput("downloadButtonUI")
        ),

        mainPanel(
            tags$h4("Parsed Citation Summary"),
            helpText("A summary of the citation(s) read from the file:"),
            verbatimTextOutput("parsedOutput"),

            tags$hr(),

            tags$h4("Converted Citation Preview"),
            helpText("Preview of the citation(s) in the selected output format:"),
            verbatimTextOutput("convertedOutput")
        )
    )
)

# --- Server Logic ---
server <- function(input, output, session) {

    # Reactive expression to handle file reading and parsing
    citationData <- reactive({
        req(input$inputFile) # Require a file upload
        req(input$inputFormat) # Require input format selection

        # Get the temporary path of the uploaded file
        inFile <- input$inputFile$datapath

        # Use tryCatch to handle potential errors during parsing
        tryCatch({
            # Create a HandlrClient object from the file path
            handlr_obj <- HandlrClient$new(x = inFile)
            # Read the file using the specified format
            handlr_obj$read(format = input$inputFormat)
            # Return the parsed object
            list(success = TRUE, data = handlr_obj)
        }, error = function(e) {
            # If an error occurs, return the error message
            list(success = FALSE, message = paste("Error reading/parsing file:", e$message))
        })
    })

    # Display the summary of the parsed citation object
    output$parsedOutput <- renderPrint({
        result <- citationData()
        if (result$success) {
            # Print the default summary of the handlr object
            print(result$data)
        } else {
            # Show the error message if parsing failed
            cat(result$message)
        }
    })

    # Reactive expression for the converted text
    convertedText <- reactive({
        req(citationData()) # Require successful parsing
        req(input$outputFormat) # Require output format selection

        result <- citationData()
        if (!result$success) {
            return(list(success = FALSE, message = result$message)) # Pass parsing error
        }

        handlr_obj <- result$data

        # Use tryCatch for the conversion/writing process
        tryCatch({
            # Write the citation(s) to the selected output format
            text_output <- handlr_obj$write(format = input$outputFormat)
            list(success = TRUE, text = text_output)
        }, error = function(e) {
            list(success = FALSE, message = paste("Error converting to format:", e$message))
        })
    })

    # Display the preview of the converted citation text
    output$convertedOutput <- renderText({
        result <- convertedText()
        if (result$success) {
            result$text
        } else {
            # Show error message if conversion failed
            result$message
        }
    })

    # Dynamically render the download button only when conversion is successful
    output$downloadButtonUI <- renderUI({
       req(convertedText())
       if(convertedText()$success){
          downloadButton("downloadData", "4. Download Converted File")
       } else {
          # Optionally display a message or disable button appearance
          tags$p(tags$strong("Conversion failed. Cannot download."), style="color:red;")
       }
    })

    # Handle the download
    output$downloadData <- downloadHandler(
        filename = function() {
            req(input$inputFile)
            req(input$outputFormat)

            # Create a filename based on original name and new format
            original_name <- tools::file_path_sans_ext(input$inputFile$name)
            extension <- output_extensions[[input$outputFormat]] # Get extension from mapping
            if (is.null(extension)) extension <- ".txt" # Fallback extension
            paste0(original_name, "_converted", extension)
        },
        content = function(file) {
            result <- convertedText()
            # Only proceed if conversion was successful
            if(result$success){
               # Write the converted text to the file
               writeLines(result$text, file)
            }
            # If not successful, the button shouldn't even be available,
            # but this prevents writing an empty/error file just in case.
        }
    )
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
```