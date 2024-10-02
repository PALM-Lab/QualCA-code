# A lightweight ShinyApp for coding of text data for qualitative analysis
# QualCA (pronounced like Quokka; not yet final)
# Written by William Ngiam
# Started on September 15, 2024
#
# Motivated by the ANTIQUES project
#
# Wishlist
# Take in various corpus input files
# Edit text viewer to show all relevant information - toggle menu to select what is shown
# Add codebook, codes, extracts - have all linked and viewable.
# Collaborative coding <- maybe a google sheet that contains code/extracts...
# Floating menus?
# Dealing with line breaks
# Highlight extracts that are selected in the codebook
# Different colours for each code
# Accept .txt. files
# 
# Glossary
# Corpus: The body of text to be qualitatively analysed
# Document: Each item that makes up the corpus
# Extract: The harvested section of text that is coded
#
# --- #

# Load required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT) # for data presentation purposes
library(tibble)
library(dplyr) # for data wrangling
library(stringr) # for handling strings

# Workaround for Chromium Issue 468227
# Copied from https://shinylive.io/r/examples/#r-file-download
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

### UI ###
ui <- dashboardPage(
  dashboardHeader(title = "Qualitative Coding App"),
  dashboardSidebar(
    fluidPage(
      HTML("<br>To use this app, upload your corpus CSV file using the button below. A menu will appear
              to select the column that contains the text to be coded.<br><br>
           If you are returning to the app, you may upload your previously saved codebook by uploading using the button below.<br><br>
           You may scroll through the documents by pressing the 'previous' and 'next' button, or typing in the numeric value into the bar.<br><br>
           To add an extract to the codebook, highlight the text in the document and press the 'Add Selected Text to Codebook' button. It will appear underneath
           the Extract column. You can then add a Code or Theme by double clicking on a cell within the codebook.<br><br>")),
    fileInput("corpusFile", "Add Corpus CSV File",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), # Upload the corpus of documents to-be-analysed
    uiOutput("documentTextSelector"),
    fileInput("codebookFile", "Add Codebook CSV File",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), # Upload any existing codebook
    uiOutput("codebookSelector"),
    fluidPage("Download Codebook"),
    fluidPage(downloadButton("downloadData",
                             label = "Download Codebook")),
    HTML("<br>"),
    fluidPage("Created by William Ngiam")
  ),
  dashboardBody(
    useShinyjs(),  # Initialize shinyjs
    fluidRow(
      box(title = "Document Viewer", width = 6, solidHeader = TRUE, status = "primary",
          fluidPage(
            fluidRow(
              column(width = 5,
                     numericInput(inputId = "documentID_viewer",
                                  label = "Document #",
                                  value = "currentDocumentID")),
              column(width = 5,
                     offset = 1,
                     HTML("<strong>Document Navigation </strong><br>"),
                     actionButton("prevDocument", "Previous"),
                     actionButton("nextDocument", "Next")))),
          bootstrapPage(
            tags$style(
              "#textDisplay {
                    overflow: auto;
                    max-height: 50vh;
                }"
            ),
            uiOutput("textDisplay"),
            tags$script('
                          function getSelectionText() {
                            var text = "";
                            if (window.getSelection) {
                              text = window.getSelection().toString();
                            } else if (document.selection) {
                              text = document.selection.createRange().text;
                            }
                            return text;
                          }
                          
                          document.onmouseup = document.onkeyup = document.onselectionchange = function() {
                            var selection = getSelectionText();
                            Shiny.onInputChange("extract",selection);
                          };
                          ')
          )),
      box(title = "Quick Look", width = 6, solidHeader = TRUE, status = "primary",
          fluidPage(
            DTOutput("counterTable")
          )
      )
      #box(title = "Themes", width = 3, solidHeader = TRUE, status = "primary")
    ),
    fluidRow(
      box(title = "Codebook", width = 12, solidHeader = TRUE, status = "primary",
          actionButton("addSelectedText", "Add Selected Text as Extract"),
          actionButton("deleteExtract", "Delete Extract from Codebook"),
          DTOutput("codebookTable")))
  )
)

### SERVER ###
server <- function(input, output, session) {
  # Reactive values to store data and codebook
  values <- reactiveValues(
    corpus = NULL, # the text data to be analysed
    documentTextColumn = NULL, # the column which contains document text
    selectedText = NULL,
    codebook = tibble(Theme = as.character(), 
                      Code = as.character(), 
                      Extract = as.character(), 
                      Document_ID = as.numeric()),
    counter = data.frame(),
    currentDocumentIndex = 1,
    nDocuments = 1,
    selection = NULL,
    thisExtract = NULL
  )
  
  ### FUNCTIONS ###
  # Update text display based on currentDocumentIndex and documentTextColumn
  updateTextDisplay <- function() {
    req(values$corpus, values$documentTextColumn)
    text <- values$corpus[[values$documentTextColumn]][values$currentDocumentIndex]
    text <- sub("<span*/span>","",text) # Remove any leftover HTML
    text <- gsub(pattern = "\n", replacement = " ", text) # Remove line breaks
    
    # Add highlights here by adding HTML tags to text
    # Retrieve document ID
    currentDoc <- values$currentDocumentIndex
    
    # Get saved extracts for document ID
    currentExtracts <- values$codebook %>% 
      dplyr::filter(Document_ID == currentDoc)
    oldStrings = currentExtracts$Extract
    
    if (length(oldStrings) > 0) {
      # Detect where extracts start and finish in text
      stringLocs <- as.data.frame(str_locate(text,paste0("\\Q",oldStrings,"\\E")))
      stringLocs <- na.omit(stringLocs) # omit NA where matches don't work with special characters
      
      # Sort these locations by their starting order
      stringLocs <- stringLocs %>% 
        arrange(start)
      
      # If more than one string, check for overlaps (leetcode 56)
      if (length(oldStrings) > 1) {
        # Write the first interval to start result
        reducedStrings <- tibble(start = as.numeric(),
                                 end = as.numeric()) %>% 
          tibble::add_row(start = head(stringLocs$start, n = 1), 
                          end = head(stringLocs$end, n = 1))
        
        for (i in 1:nrow(stringLocs)) {
          # Retrieve the next interval
          thisStringStart <- stringLocs$start[i]
          thisStringEnd <- stringLocs$end[i]
          # Check for overlap
          if (between(thisStringStart,
                      tail(reducedStrings, n = 1)$start,tail(reducedStrings, n = 1)$end)) {
          # If overlap, change value of interval
            reducedStrings[nrow(reducedStrings),ncol(reducedStrings)] = max(thisStringEnd,tail(reducedStrings, n = 1)$end)
          }
          else {
            # If no overlap, add the interval
            reducedStrings <- reducedStrings %>% 
              add_row(start = thisStringStart, end = thisStringEnd)
          }
        }
        # Arrange in descending order for inserting HTML
        stringLocs <- reducedStrings %>% 
          arrange(desc(start))
      }
      
      for (i in 1:nrow(stringLocs)) {
        # Get start and end
        stringStart <- stringLocs$start[i]
        stringEnd <- stringLocs$end[i]
        theString <- str_sub(text,stringStart,stringEnd)
        # Get string
        str_sub(text,stringStart,stringEnd) <- paste0("<span style=\"background-color: powderblue\">",theString,"</span>")
      }
    }
    
    # Hope it works
    output$textDisplay <- renderUI({
      tags$div(id = "textDisplay",
               tags$p(HTML(text), id = "currentText", style = "font-size: 20px"))
    })
  }
  
  # Update document viewer ID number
  updateDocumentID <- function() {
    updateNumericInput(
      session,
      inputId = "documentID_viewer",
      value = values$currentDocumentIndex,
      max = values$nDocuments)
  }
  
  # Save codebook locally
  saveCodebook <- function() {
    req(values$codebook)
    write.csv(values$codebook, "temp_codebook.csv", row.names = FALSE)
  }
  
  # Render codebook
  renderCodebook <- function() {
    output$codebookTable <- renderDT({
      datatable(values$codebook, editable = TRUE, rownames = FALSE, options = list(order = list(3, 'desc')))
    })
  }
  
  # Render code counter
  renderCounter <- function() {
    values$counter <- values$codebook %>% 
      count(Code, name = "Instances")
    
    output$counterTable <- renderDT({
      datatable(values$counter, 
                editable = list(target = "cell",
                                disable = list(columns = 1)),
                rownames = FALSE)
    })
  }
  
  ### UPLOADING DATA ###
  # Load corpus CSV file and update column selector
  observeEvent(input$corpusFile, {
    req(input$corpusFile)
    values$corpus <- read.csv(input$corpusFile$datapath, stringsAsFactors = FALSE)
    colnames <- colnames(values$corpus)
    values$documentTextColumn <- colnames[1]  # Default to the first column
    values$currentDocumentIndex <- 1  # Reset index on new file load
    values$nDocuments <- nrow(values$corpus) # Get number of documents
    
    output$documentTextSelector <- renderUI({
      req(values$corpus)
      selectInput("documentTextColumn", "Select Text Column",
                  choices = colnames,
                  selected = values$documentTextColumn)
    })
    
    updateTextDisplay()
    renderCodebook()
    renderCounter()
    
    updateSelectInput(session, "codebookSelector", choices = values$codebook$Theme, selected = NULL)
  })
  
  # Update codebook on upload
  observeEvent(input$codebookFile, {
    req(input$codebookFile)
    values$codebook <- read.csv(input$codebookFile$datapath, stringsAsFactors = FALSE)
    colnames <- colnames(values$codebook)
    renderCodebook()
    renderCounter()
    updateTextDisplay()
  })
  
  
  ## DOCUMENT NAVIGATION ##
  # Update display after document text column is selected
  observeEvent(input$documentTextColumn, {
    values$documentTextColumn <- input$documentTextColumn
    updateTextDisplay()
  })
  
  # Updated display after document ID is scrolled
  observeEvent(input$documentID_viewer, {
    req(values$corpus)
    if (input$documentID_viewer > nrow(values$corpus)) {
      values$currentDocumentIndex <- nrow(values$corpus)
      updateTextDisplay()
    } else if (values$currentDocumentIndex > 1) {
      values$currentDocumentIndex <- input$documentID_viewer
      # Updated text display
      updateTextDisplay()
    }
  })
  
  # Action button for previous document
  observeEvent(input$prevDocument, {
    req(values$corpus)
    if (values$currentDocumentIndex > 1) {
      values$currentDocumentIndex <- values$currentDocumentIndex - 1
      
      # Updated text display
      updateDocumentID()
      updateTextDisplay()
      
    }
  })
  
  # Action button for next document
  observeEvent(input$nextDocument, {
    req(values$corpus)
    if (values$currentDocumentIndex < nrow(values$corpus)) {
      values$currentDocumentIndex <- values$currentDocumentIndex + 1
      updateDocumentID()
      updateTextDisplay()
    }
  })
  
  ## CODEBOOK ACTIONS ##
  # Save after any edits to the codebook
  observeEvent(input$codebookTable_cell_edit, {
    values$codebook <- editData(values$codebook, input$codebookTable_cell_edit, rownames = FALSE, 'codebookTable')
    saveCodebook()
    renderCounter()
    updateTextDisplay()
  })
  
  # Add selected text as extract
  observeEvent(input$addSelectedText, {
    req(values$codebook,input$extract)
    selectedText <- input$extract # Highlighted text within document to-be-extracted
    
    # Append the selected text to codebook as extract
    values$codebook <- values$codebook %>% 
      add_row(Theme = "",
              Code = "",
              Extract = selectedText,
              Document_ID = values$currentDocumentIndex)
    
    renderCodebook()
    renderCounter()
    updateTextDisplay()
  })
  
  # Delete extract from codebook
  observeEvent(input$deleteExtract, {
    req(input$codebookTable_rows_selected)
    whichRow <- input$codebookTable_rows_selected # highlighed rows in codebook
    values$codebook <- values$codebook[-whichRow,]
    renderCodebook()
    renderCounter()
    updateTextDisplay()
  })
  
  # Save and apply rewording of code
  observeEvent(input$counterTable_cell_edit, {
    # Update counter
    values$counter <- editData(values$counter, input$counterTable_cell_edit, rownames = FALSE, 'counterTable')
    
    ## Get previous code value
    # Recreate previous counter
    oldCounterTable <- values$codebook %>% 
      count(Code, name = "Instances")
    
    # Retrieve old Code
    info = input$counterTable_cell_clicked
    changeRow = info$row
    changeCol = info$col
    oldValue = info$value
    
    # Retrieve new Code
    new_info = input$counterTable_cell_edit
    newValue = new_info$value
    
    # Replace strings in codebook
    values$codebook$Code[values$codebook$Code == oldValue] <- as.character(newValue)
    
    renderCodebook()
  })
  
  # Download handler for the codebook
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("codebook-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$codebook, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)