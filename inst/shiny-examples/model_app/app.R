library(shiny)
library("tools")
library(DT)
library(writexl)
library(stringr)
library(shinyalert)
require(data.table)
library(shinyWidgets)

path_mace = "../../../java/mace.jar"
mace_done = FALSE

server <- shinyServer(function(input, output, session) {

  rv <- reactiveValues(download_flag = 0)

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



  ## Check if file is uploaded to show MACE parameter panel
  output$fileUploaded <- reactive({
    return(!is.null(data()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  ## Check if file is uploaded AND Wide mode is selected to show the specific Wide panel
  output$fileUploadedAndWide <- reactive({
    uploaded = !is.null(data())
    is_wide = input$view == "Wide"
    return(uploaded & is_wide)
  })
  outputOptions(output, 'fileUploadedAndWide', suspendWhenHidden=FALSE)

  ## Check if file is uploaded AND Compact mode is selected to show the specific Compact panel
  output$fileUploadedAndCompact <- reactive({
    uploaded = !is.null(data())
    is_compact = input$view == "Compact"
    return(uploaded & is_compact)
  })
  outputOptions(output, 'fileUploadedAndCompact', suspendWhenHidden=FALSE)

  output$annotators <- renderUI(selectInput('annWide', 'Select Annotators column', names(data()), selected = names(data())[2]))
  output$items <- renderUI(selectInput('itWide', 'Select Items column', names(data())))
  output$annotations <- renderUI(selectInput('annsWide', 'Select Annotations column', names(data()), selected = names(data())[4]))

  output$annotatorsCompact <- renderUI(pickerInput('annCompact', 'Select Annotators columns', names(data()),options = list(`actions-box` = TRUE), selected = names(data())[3], multiple = TRUE))
  output$itemsCompact <- renderUI(selectInput('itCompact', 'Select Items column', c('N/A',names(data()))))


  output$downloadData <- downloadHandler(
    filename = function () {
      paste("output",file_ext(userFile()$datapath), sep = ".")
    },
    content = function(file) {
      write.table(data(), file, sep = input$sep, row.names = FALSE)
    }
  )

  process_input_wide <- function(df){
    print("Wide Table preprocessing")
    setDT(df)
    #f = as.formula(sprintf('%s ~ %s', input$itWide, input$annWide))
    pivot_table = data.table::dcast(df, paste(input$itWide,'~', input$annWide),
                                    value.var = (input$annsWide))

    if (input$itWide != "N/A"){
      pivot_table[,input$itWide] <- NULL # Remove ID column
    }
    #df <- df[input$annCompact]
    #df[is.na(df)] <- NULL
    write.table(df,"temp.csv",sep=",", row.names = FALSE, col.names=FALSE, na = "")
  }

  process_input_compact <- function(df){
    if (input$itCompact != "N/A"){
      df[input$itCompact] <- NULL # Remove ID column
    }
    df <- df[input$annCompact]
    #df[is.na(df)] <- NULL
    write.table(df,"temp.csv",sep=",", row.names = FALSE, col.names=FALSE, na = "")
  }


  mace_output_to_xls <- function(){
    prediction <- read.table('prediction', stringsAsFactors=FALSE,header=FALSE,sep="\t")
    competence <- read.table('competence', stringsAsFactors=FALSE,header=FALSE,sep="\t")
    entropies <- read.csv('entropies', stringsAsFactors=FALSE,header=FALSE)
    colnames(entropies) = "entropy"

    # Sort rows by label name
    pred_ordered = t(apply(prediction, 1, sort))
    col_names = c()

    # For each column, select label name (first word)
    for (i in seq(1,ncol(pred_ordered))){
      value_vector = word(pred_ordered[,i], 1)
      value = unique(value_vector)
      if (length(value) > 1){
        stop("Problem in prediction file processing")
      } else {
        col_names = c(col_names,value)
      }
    }

    # Set label names as column names
    colnames(pred_ordered) = col_names
    # Set table values as the values - remove labels - (second word)
    pred_values_df = pred_ordered
    pred_values_df[] = word(pred_ordered, 2)

    # Add column for predicted label for each row and entropies
    pred_final = cbind(predicted_label = colnames(pred_values_df)[max.col(pred_values_df,ties.method="first")], pred_values_df, entropy = entropies)

    # Process Competences file
    competence_final = t(competence)
    competence_final = as.data.frame(competence_final)
    colnames(competence_final) = "competence"

    return_list <- list("competence" = competence_final, "pred" = pred_final)
    return(return_list)
  }

  observeEvent(rv$download_flag, {
    shinyjs::alert("File downloaded!")
  }, ignoreInit = TRUE)

  output$download <- downloadHandler(

    filename = function() {
      paste("result", "xlsx", sep=".")
    },
    content = function(fname) {
      shiny::withProgress(
        message = paste0("Downloading", input$dataset, " MACE Result"),
        value = 0,
        {

        fs <- c()
        tmpdir <- tempdir()
        setwd(tempdir())
        print (tempdir())

        df = data()
        shiny::incProgress(1/10)
        # convert input in desired format
        if (input$view == "Compact"){
          process_input_compact(df)
        } else {
          process_input_wide(df)
        }

        shiny::incProgress(3/10)

        # run MACE
        cmd_line = paste("java -jar",path_mace,'--distribution','--entropies',
                         "--iterations",input$iter,"--restarts",input$restarts,
                         "--alpha",input$alpha,"--beta",input$beta,'--threshold',input$threshold)
        if  (input$smoothing != "0.01"){
          cmd_line = paste(cmd_line,"--smoothing",input$smoothing)
        }
        system(paste(cmd_line,"temp.csv"))
        shiny::incProgress(7/10)

        return_list = mace_output_to_xls()

        shiny::incProgress(8/10)
        # Write excel file
        list_of_datasets <- list("predictions" = return_list$pred, "competences" = return_list$competence)
        write_xlsx(list_of_datasets, fname)
        })
    },
    contentType="application/xlsx"
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

        radioButtons("view", "View mode",
                     choices = c(Wide='Wide',Compact='Compact'),
                     selected = "Wide",
                     inline=TRUE),
        conditionalPanel(
            'output.fileUploadedAndCompact',
            #h4("Select Annotation column"),
            uiOutput("annotatorsCompact"),
            uiOutput("itemsCompact")
          ),
        conditionalPanel(
          'output.fileUploadedAndWide',
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
          sliderInput("iter", "EM Iterations:",
                      min = 0, max = 1000,
                      value = 50, step = 10),
          sliderInput("alpha", "alpha:",
                      min = 0, max = 1,
                      value = 0.5, step = 0.1),
          sliderInput("beta", "beta:",
                      min = 0, max = 1,
                      value = 0.5, step = 0.1),
          sliderInput("restarts", "Restarts:",
                      min = 0, max = 100,
                      value = 10, step = 5),
          sliderInput("smoothing", "Smoothing:",
                      min = 0, max = 1,
                      value = 0.01, step = 0.01),
          sliderInput("threshold", "Threshold:",
                      min = 0, max = 1,
                      value = 1.0, step = 0.05),

          # no EM as option
          # always run entropy and distribution
          # in a nice spreadsheet

        ),

        #downloadButton("download", "Download"),
        downloadButton("download", "Run Mace", width = '100%',class = "btn-primary"),
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
