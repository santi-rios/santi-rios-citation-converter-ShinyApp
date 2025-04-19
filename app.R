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