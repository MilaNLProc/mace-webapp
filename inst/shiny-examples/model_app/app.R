library(shiny)
library("tools")
library(DT)


server <- shinyServer(function(input, output, session) {

  userFile <- reactive({
    validate(need(input$filedata !="", "Please import a data file"))
    input$filedata
  })    
  
  data <- reactive({
    if(is.null(userFile()$datapath)) return(NULL)
    utils::read.table(userFile()$datapath,
                      header = input$header,
                      sep = input$sep,
                      row.names = NULL,
                      #skip = 1,
                      stringsAsFactors = FALSE)
    
  })
  
  output$table <- renderDT(
    data()
  )
  
  output$fileUploaded <- reactive({
    return(!is.null(data()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$annotators <- renderUI(selectInput('annotators', 'Select Annotators column', names(data()), selected = names(data())[2]))
  output$items <- renderUI(selectInput('annotations', 'Select Items column', names(data()), selected = names(data())[3]))
  output$annotations <- renderUI(selectInput('items', 'Select Annotations column', names(data()), selected = names(data())[4]))
  
  output$downloadData <- downloadHandler(
    filename = function () {
      paste("output",file_ext(userFile()$datapath), sep = ".")
    },
    content = function(file) {
      write.table(data(), file, sep = input$sep, row.names = FALSE)
    }
  )
})


ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("MACE"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "filedata",
                label = "Upload data. Choose csv, tsv or txt file",
                multiple = FALSE,
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".tsv")),
      wellPanel(
        h4(helpText("Select the input parameters below")),
        checkboxInput("header", "Header", value = TRUE),
        radioButtons("sep", "Separator",
                     choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''),
                     selected = "\t",
                     inline=TRUE),
        conditionalPanel(
          'output.fileUploaded',
          #h4("Select Annotation column"),
          uiOutput("annotators"),
          uiOutput("items"),
          uiOutput("annotations")
        )
      ),
      conditionalPanel(
        'output.fileUploaded',
        wellPanel(
          h4(helpText("Select the MACE parameters below")),
          sliderInput("iter", "Iterations:",
                      min = 0, max = 5000,
                      value = 2000, step = 500),
          
        ),
        
        downloadButton("downloadData", "Download")
        #actionButton("runButton", "Run Mace", width = '100%',class = "btn-primary"),
      ),
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Input",DTOutput(outputId = "table"))
                  #tabPanel("Confidence Annotators",DTOutput(outputId = "table_confidence"))
      )
      
      
    )
  )
))

shinyApp(ui = ui, server = server)