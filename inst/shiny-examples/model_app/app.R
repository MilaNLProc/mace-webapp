library(shiny)
library(shinycssloaders)
library(testpackage)
library(dplyr)
library(ggplot2)

TARGET_VARIABLE <- "median_house_value"

# Define UI for application that draws a histogram
ui <- fluidPage(
        tabsetPanel(
            tabPanel("Data", tableOutput("data_table")),
            tabPanel("Model",
                sidebarLayout(
                    sidebarPanel(
                        selectInput("model_type",
                                    "Type of model:",
                                    choices = "loading...",
                                    selected = "loading..."),
                        checkboxGroupInput("features", "Features included:",
                                           choices = "loading...",
                                           selected = NA)
                    ),

                    # Show a plot of the generated distribution
                    mainPanel(
                        plotOutput("scatter_plot") %>%
                            withSpinner(color="#ee3224")
                    )
                )
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    initialise_model <-
        reactive(Model$new("data/housing.csv", TARGET_VARIABLE))

    # update model choices
    observe({
        model_choices <- initialise_model()$algos

        updateSelectInput(session, "model_type", choices = model_choices,
                          selected = model_choices[1])

        features <- names(initialise_model()$data)
        features <- features[features != TARGET_VARIABLE]

        updateCheckboxGroupInput(session, "features", choices = features,
                                 selected = features)

        })

    output$data_table <- renderTable(initialise_model()$data)

    output$scatter_plot <- renderPlot({
        model <- initialise_model()

        validate(need(input$model_type %in% model$algos,
                 "Algorithm not in algorithm list"))
        model_output <- model$fit_model(input$model_type, input$features)

        model_output$predictions %>%
            ggplot(aes(actual, preds)) +
            geom_point() +
            ggtitle(paste0("Predicted vs Actual, R-Squared: ",
                           round(model_output$r_squared, 2)))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
