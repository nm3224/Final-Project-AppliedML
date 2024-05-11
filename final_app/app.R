library(shiny)

library(shinythemes)

library(shinycssloaders)

library(tidyverse)

library(ggExtra)

library(data.table)

library(caret)
library(tidymodels)
library(haven)
library(visdat)
library(plm)
library(stringr)
library(corrplot)
library(cluster)     
library(factoextra) 
library(vip) 
library(igraph)
library(foreign)
library(MLmetrics)
library(RSNNS)

# Loading data
data_all <- read.csv("data/all_clean.csv", header = TRUE)
data_dem <- read.csv("data/dem_clean.csv", header = TRUE)
data_opin <- read.csv("data/opin_clean.csv", header = TRUE)

rf <- readRDS("models/rf_all.rds")
xg <- readRDS("models/xg_all.rds")

# Define UI for application
ui <- fluidPage(
  titlePanel("Final Project"),
  navbarPage(
    title = "Project Overview",
    theme = shinytheme("flatly"),
    tabPanel("Project Details", icon = icon("info-circle"),
             titlePanel("Predicting Social Class Based on Differing Variables"),
             mainPanel(HTML("STAT 3106: Applied Machine Learning - Final Project:<br><br>
               
                        We decided early on that we would all be interested in a question of the social sciences, 
                        namely in exploring how demographics relate to political opinions and identity. 
                        Because our interest is in understanding these relationships, not only in predicting them, we decided we also wanted to implement interpretable machine learning techniques. 
                        After looking through sites like kaggle and data.gov, we settled on working with data from the General Social Survey (GSS), as it presents extraordinarily extensive data on both demographics and opinions.<br><br>
                        
                        The dataset we started working with was the GSS cumulative dataset, including all variables ever collected from 1972-2022. 
                        With over 72,000 observations on almost 6,700 variables, this dataset was extraordinarily large.<br><br> 
                        
                        Final Research Question: How well can each of our chosen models (XGBoost, Random Forest, and Artificial Neural Network) 
                        predict an individual's independently defined subjective social class based on 3 data subsets:<br><br> 
                            1) solely variables such as demographic information (ex. income, race, occupation),<br><br> 
                            2) solely variables such as opinion data (ex. political opinions),<br><br>
                            3) both these variable datasets combined?"))
    ),
    tabPanel("Our Data", icon = icon("folder-open"),
             titlePanel("Upload Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Dataset:", choices = c("All_Data" = "data_all", 
                                                                "Demographic_Data" = "data_dem", 
                                                                "Opinion_Data" = "data_opin", 
                                                                "Upload your own file")),
                 conditionalPanel(condition = "input.dataset == 'Upload your own file'",
                                  fileInput("file", "Select your files:", accept = c("text/csv",
                                                                                     "text/comma-separated-values,text/plain", ".csv"))
                 )
               ),
               mainPanel(dataTableOutput("data_preview"))
             )
    ),
    tabPanel("Splitting Data",
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Choose a dataset or upload your own:",
                             choices = c("All_Data" = "data_all", 
                                         "Demographic_Data" = "data_dem", 
                                         "Opinion_Data" = "data_opin", 
                                         "Upload your own file")),
                 uiOutput("fileInputUI_split"),  # Dynamic UI for file upload
                 uiOutput("varSelectUI_split"),  # Dynamic UI for selecting variables
                 sliderInput("splitRatio", "Train/Test Split Ratio (%)", min = 0, max = 100, value = 70),
                 actionButton("splitData", "Split Data", icon = icon("cut"))
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Preview", dataTableOutput("preview")),
                   tabPanel("Train Set Preview", dataTableOutput("trainPreview")),
                   tabPanel("Test Set Preview", dataTableOutput("testPreview"))
                 )
               )
             )
    ),
    tabPanel("Pre-Processing", icon = icon("edit"),
             sidebarLayout(
               sidebarPanel(conditionalPanel(
                 condition = "input.preprocess_dataset == 'upload'",
                 fileInput("uploadData", "Upload Dataset:", accept = c(".csv", ".xlsx"))
               ),
               uiOutput("varSelectInput"),  # For selecting variables dynamically
               checkboxInput("removeNA", "Remove All NAs", value = FALSE),
               actionButton("applyPreprocess", "Apply Pre-processing", icon = icon("magic"))
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Preview", dataTableOutput("preview")),
                   tabPanel("Training Set Preview", dataTableOutput("train_preprocess_preview")),
                   tabPanel("Testing Set Preview", dataTableOutput("test_preprocess_preview"))
                 )
               )
             )
             ),
    
    tabPanel("Model Analysis and Summary",
             sidebarLayout(
               sidebarPanel(
                 conditionalPanel(
                   condition = "input.modelDataset == 'upload'",
                   fileInput("uploadModelData", "Upload Dataset:", accept = c(".csv", ".xlsx"))
                 ),
                 selectInput("selectedModel", "Choose a Model", 
                             choices = c("Random Forest" = "rf", 
                                         "XGBoost" = "xgb")),
                 actionButton("loadModel", "Load and Run Model", icon = icon("play-circle"))
               ),
               mainPanel(
                 verbatimTextOutput("modelSummary"),
                 plotOutput("plotActualVsPredicted"),
                 plotOutput("plotResiduals")
               )
             )
    ),
    tabPanel("Prediction Visuals",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plotType", "Select Plot Type",
                             choices = c("Scatterplot", "Residual Plot"))
               ),
               mainPanel(
                 plotOutput("predictionPlot")
               )
             )
    )
    )
)

# Define Server
server <- function(input, output, session) {
  
  split_data <- reactive({
    data <- switch(input$dataset,
                   "data_all" = data_all,
                   "data_dem" = data_dem,
                   "data_opin" = data_opin,
                   "Upload your own file" = {
                     req(input$file)
                     read.csv(input$file$datapath, header = TRUE)
                   })
    
    output$preview <- renderDataTable({
      
      # Read uploaded file if selected
      if (!is.null(input$file)) {
        data <- read.csv(input$file$datapath, header = TRUE)
      } else {
        switch(input$dataset,
               "data_all" = data_all,
               "data_dem" = data_dem,
               "data_opin" = data_opin)
      }
    })
    
    if (!is.null(data)) {
      split_ratio <- input$splitRatio / 100
      index <- round(nrow(data) * split_ratio)
      train_data <- data[1:index, ]
      test_data <- data[(index + 1):nrow(data), ]
      list(train_data = train_data, test_data = test_data)
    } else {
      list(train_data = NULL, test_data = NULL)
    }
  })
  
  # Data preview
  output$data_preview <- renderDataTable({
    # Read uploaded file if selected
    if (!is.null(input$file)) {
      data <- read.csv(input$file$datapath, header = TRUE)
    } else {
      switch(input$dataset,
             "data_all" = data_all,
             "data_dem" = data_dem,
             "data_opin" = data_opin)
    }
  })
  
  # Dynamic UI for file upload in Splitting Data tab
  output$fileInputUI_split <- renderUI({
    if (input$dataset == "Upload your own file") {
      fileInput("file_split", "Select your files:", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
    }
  })
  
  # Dynamic UI for selecting variables in Splitting Data tab
  output$varSelectUI_split <- renderUI({
    if (!is.null(input$file_split)) {
      var_names <- colnames(read.csv(input$file_split$datapath, header = TRUE))
      selectInput("split_variable", "Select Response Variable:", choices = var_names)
    } else {
      var_names <- colnames(get(input$dataset))
      selectInput("split_variable", "Select Response Variable:", choices = var_names)
    }
  })
  
  # Train set preview
  output$trainPreview <- renderDataTable({
    req(split_data()$train_data)
    head(split_data()$train_data)
  })
  
  # Test set preview
  output$testPreview <- renderDataTable({
    req(split_data()$test_data)
    head(split_data()$test_data)
  })
  
  # Function to preprocess data and output a preview
  preprocess_data <- function(train_data, test_data) {
    
    # Apply preprocessing steps
    blueprint <- recipe(split_data()$response_variable ~ ., data = train_data) %>%
      step_other(all_nominal_predictors(),
                 threshold = 0.01,
                 other = "Other") %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors()) %>%
      step_dummy(all_nominal_predictors()) %>%
      step_zv(all_predictors())
    
    preprocessed_train_data <- blueprint %>% prep() %>% bake(new_data = NULL)
    
    preprocessed_test_data <- blueprint %>% prep() %>% bake(new_data = test_data)
    
    list(train_data = head(preprocessed_train_data), test_data = head(preprocessed_test_data))
  }
  
  # Preprocess data and output previews
  output$preprocess_preview <- renderDataTable({
    if (!is.null(split_data()$train_data)) {
      train_data <- split_data()$train_data
      test_data <- split_data()$test_data
      
      preprocessed_data <- preprocess_data(train_data, test_data)
      
      preprocessed_data$train_data
    }
  })
  
  output$test_preprocess_preview <- renderDataTable({
    if (!is.null(split_data()$train_data)) {
      train_data <- split_data()$train_data
      test_data <- split_data()$test_data
      
      preprocessed_data <- preprocess_data(train_data, test_data)
      
      preprocessed_data$test_data
    }
  })
  
  trained_model <- eventReactive(input$loadModel, {
    model <- switch(input$selectedModel,
                    "rf" = rf,  
                    "xgb" = xg)
    return(model)
  }, ignoreNULL = TRUE)
  
  # Generate predictions
  predictions <- reactive({
    predict(trained_model(), newdata = input$split_data$test_data)
  })
  
  # Output model summary
  output$modelSummary <- renderPrint({
    summary(trained_model())
  })
  
  # Update the selected_response reactive value when a response variable is selected
  observe({
    selected_response(input$split_variable)
  })
  
  output$predictionPlot <- renderPlot({
    # Get the response variable from the reactive value
    response_variable <- selected_response()
    
    # Extract actual values from the dataset
    actual_train_values <- train_data[[split_data()$response_variable]]
    actual_test_values <- test_data[[split_data()$response_variable]]
    
    if (input$plotType == "Scatterplot") {
      # Scatterplot of actual vs predicted values
      actual_vs_predicted <- data.frame(Actual = actual_values, Predicted = predicted_values)
      ggplot(actual_vs_predicted, aes(x = Actual, y = Predicted)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        labs(title = "Actual vs. Predicted",
             x = "Actual",
             y = "Predicted")
    } else if (input$plotType == "Residual Plot") {
      # Residual plot
      residuals <- actual_values - predicted_values
      ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
        geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
        labs(title = "Residuals Plot",
             x = "Residuals",
             y = "Frequency")
    }
  })
}

# Run application 
shinyApp(ui = ui, server = server)
