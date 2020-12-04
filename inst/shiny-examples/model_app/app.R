library(shiny)
library("tools")
library(DT)
library(writexl)
library(stringr)
library(shinyalert)
require(data.table)
library(shinyWidgets)
library(shinyBS)

path_mace = "~/../opt/app/java/MACE.jar"

mace_done = FALSE

server <- shinyServer(function(input, output, session) {

  download_flag <- FALSE
  name_annotators <- ""
  print(download_flag)

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
    data(), options = list(pageLength = 30, info = TRUE)
  )



  ## Check if file is uploaded to show MACE parameter panel
  output$fileUploaded <- reactive({
    return(!is.null(data()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  ## Check if file is uploaded AND Long mode is selected to show the specific Long panel
  output$fileUploadedAndLong <- reactive({
    uploaded = !is.null(data())
    is_long = input$view == "Long"
    return(uploaded & is_long)
  })
  outputOptions(output, 'fileUploadedAndLong', suspendWhenHidden=FALSE)

  ## Check if file is uploaded AND Wide mode is selected to show the specific Wide panel
  output$fileUploadedAndWide <- reactive({
    uploaded = !is.null(data())
    is_wide = input$view == "Wide"
    return(uploaded & is_wide)
  })
  outputOptions(output, 'fileUploadedAndWide', suspendWhenHidden=FALSE)

  output$fileDownloaded <- reactive({
    return(download_flag)
  })
  outputOptions(output, 'fileDownloaded', suspendWhenHidden=FALSE)

  output$annotators <- renderUI(selectInput('annLong', 'Select Annotators column', names(data()), selected = names(data())[2]))
  output$items <- renderUI(selectInput('itLong', 'Select Items column', names(data())))
  output$annotations <- renderUI(selectInput('annsLong', 'Select Annotations column', names(data()), selected = names(data())[4]))

  output$annotatorsWide <- renderUI(pickerInput('annWide', 'Select Annotators columns', names(data()),options = list(`actions-box` = TRUE), selected = names(data())[3], multiple = TRUE))
  output$itemsWide <- renderUI(selectInput('itWide', 'Select Items column', c('N/A',names(data()))))


  output$downloadData <- downloadHandler(
    filename = function () {
      paste("output",file_ext(userFile()$datapath), sep = ".")
    },
    content = function(file) {
      write.table(data(), file, sep = input$sep, row.names = FALSE)
    }
  )

  process_input_long <- function(df){
    print("Long Table preprocessing")
    setDT(df)
    #f = as.formula(sprintf('%s ~ %s', input$itLong, input$annLong))
    pivot_table = data.table::dcast(df, paste(input$itLong,'~', input$annLong),
                                    value.var = (input$annsLong))

    if (input$itLong != "N/A"){
      pivot_table[,input$itLong] <- NULL # Remove ID column
    }
    #df <- df[input$annWide]
    #df[is.na(df)] <- NULL
    write.table(df,"temp.csv",sep=",", row.names = FALSE, col.names=FALSE, na = "")
    name_annotators <<- input$annLong
  }

  process_input_wide <- function(df){
    if (input$itWide != "N/A"){
      df[input$itWide] <- NULL # Remove ID column
    }
    df <- df[input$annWide]
    #df[is.na(df)] <- NULL
    write.table(df,"temp.csv",sep=",", row.names = FALSE, col.names=FALSE, na = "")
    name_annotators <<- input$annWide
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
    print(name_annotators)
    colnames(competence) = name_annotators
    competence_final = t(competence)
    competence_final = as.data.frame(competence_final)
    competence_final$annotator <- name_annotators
    colnames(competence_final) = c("competence","annotator")
    competence_final <- competence_final[,c("annotator","competence")]

    return_list <- list("competence" = competence_final, "pred" = pred_final)
    return(return_list)
  }

  observeEvent(rv$download_flag, {
    shinyjs::alert("File downloaded!")
  }, ignoreInit = TRUE)

  observeEvent(input$download, {
    print(download_flag)
    download_flag <<- TRUE
  })

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
        if (input$view == "Wide"){
          process_input_wide(df)
        } else {
          process_input_long(df)
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
  #
  # output$plot <- renderPlotly(
  #   print(download_flag),
  #   if(!(download_flag)) {
  #     return(NULL)
  #   } else {
  #     fig <- plot_ly(
  #       y = competence_final$competence,
  #       x = row.names(competence_final),
  #       color = row.names(competence_final),
  #       type = 'bar',
  #     )
  #     fig <- fig %>% layout(title = "Annotator competences",
  #                           xaxis = list(title = "Annotators"),
  #                           yaxis = list(title = "Competence level"))
  #     return(fig)
  #   }
  # )

})


ui <- shinyUI(fluidPage(

  shinydashboard::dashboardPage(
    skin = "black",
    title= "MACE",


    shinydashboard::dashboardHeader(
      title = tags$li(a(href='https://www.dmi.unibocconi.eu/wps/wcm/connect/Cdr/DMI/Home',
                        tags$img(src='logo.jpg', align="left", heiht="40px", width="40px")))

    ),

    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Home",
                                 tabName = "dashboard",
                                 icon = shiny::icon("home")
        ),
        shinydashboard::menuItem("MACE",
                                 icon = shiny::icon("dashboard"),
                                 tabName = "datasets"
        )
        # shinydashboard::menuItem("Competences",
        #                          icon = shiny::icon("bar-chart"),
        #                          tabName = "competences"
        # )
      )
      ),
      shinydashboard::dashboardBody(
        tags$head(
          tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
          tags$title("MACE")
        ),
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "dashboard",
            shiny::fluidRow(
              shiny::column(
                width=5,
                shinydashboard::box(
                  footer =shiny::HTML(''),
                  title = "",
                  status = "primary",
                  solidHeader = TRUE,
                  width = NULL,
                  shiny::img(
                    src = "logo.jpg",
                    height = 50,
                    width = 50
                  ),
                  shiny::h3("MACE (Multi-Annotator Competence Estimation)"),
                  br(),
                  shiny::h5("When evaluating redundant annotations (like those from Amazon's MechanicalTurk), we usually want to"),
                  tags$ol(
                    tags$li("aggregate annotations to recover the most likely answer"),
                    tags$li("find out which annotators are trustworthy"),
                    tags$li("evaluate item and task difficulty")
                  ),
                  shiny::h5("MACE solves all of these problems, by learning competence estimates for each annotators and computing the most likely answer based on those competences."),
                  br(),
                  shiny::h5(
                    "Click",
                    shiny::em("MACE"),
                    " in the sidepanel to get started."
                  ),
                  br(),br(),br(),br(),br(),br(),br(),br()
                )
              ),
              shiny::column(
                width=7,
                shinydashboard::box(
                  title = "",
                  status = "primary",
                  solidHeader = TRUE,
                  width = NULL,
                  tabPanel("About", mainPanel(uiOutput("video")))
                )
              )
            )
          ),
          shinydashboard::tabItem(tabName = "datasets",
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

                  #tags$head(
                    # this changes the size of the popovers
                  #  tags$style(HTML("#view_div .tooltip {width:600px;}"))
                  #),

                  h4(helpText("Select the input parameters below")),
                  checkboxInput("header", "Header", value = TRUE),
                  bsTooltip("header", "Select if file has header.",
                            "right", options = list(container = "body")),
                  radioButtons("sep", "Separator",
                               choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''),
                               selected = "\t",
                               inline=TRUE),
                  radioButtons("view", "View mode",
                               choices = c(Long='Long',Wide='Wide'),
                               selected = "Long",
                               inline=TRUE),
                  bsTooltip("view", "<b>Long view mode</b>: one column for all annotations.<br> <b>Wide view mode</b>: one column for each annotator.",
                            "right", options = list(container = "body")),
                  conditionalPanel(
                      'output.fileUploadedAndWide',
                      uiOutput("annotatorsWide"),
                      bsTooltip("annotatorsWide", "The columns containing annotations (one column = one annotator).",
                                "right", options = list(container = "body")),
                      uiOutput("itemsWide"),
                      bsTooltip("itemsWide", "The column containing the IDs of the items.",
                                "right", options = list(container = "body")),
                    ),
                  conditionalPanel(
                    'output.fileUploadedAndLong',
                    uiOutput("annotators"),
                    bsTooltip("annotators", "The column containing the IDs of annotators.",
                              "right", options = list(container = "body")),
                    uiOutput("items"),
                    bsTooltip("items", "The column containing the IDs of the items.",
                              "right", options = list(container = "body")),
                    uiOutput("annotations"),
                    bsTooltip("annotations", "The column containing annotation.",
                              "right", options = list(container = "body")),
                  )
                ),
                conditionalPanel(
                  'output.fileUploaded',
                  wellPanel(
                    h4(helpText("Select the MACE parameters below")),
                    sliderInput("iter", "EM Iterations:",
                                min = 0, max = 1000,
                                value = 50, step = 10),
                    bsTooltip("iter", "number of iterations for each EM start.",
                              "right", options = list(container = "body")),
                    #sliderInput("alpha", "alpha:",
                    #            min = 0, max = 1,
                    #            value = 0.5, step = 0.1),
                    #sliderInput("beta", "beta:",
                    #            min = 0, max = 1,
                    #            value = 0.5, step = 0.1),

                    numericInput("alpha", "alpha:", 0.5, min = 0, max = 1),
                    bsTooltip("alpha", "first hyper-parameter of beta prior that controls whether an annotator knows or guesses.",
                              "right", options = list(container = "body")),

                    numericInput("beta", "beta:", 0.5, min = 0, max = 1),
                    bsTooltip("beta", "second hyper-parameter of beta prior that controls whether an annotator knows or guesses.",
                              "right", options = list(container = "body")),

                    sliderInput("restarts", "Restarts:",
                                min = 0, max = 100,
                                value = 10, step = 5),
                    bsTooltip("restarts", "number of random restarts to perform.",
                              "right", options = list(container = "body")),

                    numericInput("smoothing", "Smoothing:", 0.01, min = 0, max = 1),
                    bsTooltip("smoothing", "smoothing added to fractional counts before normalization. Higher values mean smaller changes.",
                              "right", options = list(container = "body")),

                    numericInput("threshold", "Threshold:", 1, min = 0, max = 1),
                    bsTooltip("threshold", "only predict the label for instances whose entropy is among the top n%, ignore others.",
                              "right", options = list(container = "body")),


                    #sliderInput("smoothing", "Smoothing:",
                    #            min = 0, max = 1,
                    #            value = 0.01, step = 0.01),
                    #sliderInput("threshold", "Threshold:",
                    #            min = 0, max = 1,
                    #            value = 1.0, step = 0.05),

                    # no EM as option
                    # always run entropy and distribution
                    # in a nice spreadsheet

                  ),

                  #downloadButton("downloadData", "Download"),
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
      ) # end tab MACE

      # shinydashboard::tabItem(tabName = "competences",
      #                         conditionalPanel(
      #                           'output.fileUploaded',
      #                           plotlyOutput('plot')
      #                         )
      #
      #                 ) # end tab competences



)
)
)
)
)

shinyApp(ui = ui, server = server)
