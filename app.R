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