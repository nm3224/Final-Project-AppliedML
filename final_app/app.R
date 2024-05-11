library(shiny)

library(shinythemes)

library(shinycssloaders)

library(tidyverse)

library(ggExtra)

library(data.table)

library(caret)

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
                        predict an individual's independently defined subjective social class based on 3 data subsets: 1) solely variables such as demographic information (ex. income, race, occupation); 2) solely variables such as opinion data (ex. political opinions); 3) both these variable datasets combined."))
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
                   tabPanel("Preview", DTOutput("preview")),
                   tabPanel("Train Set Preview", DTOutput("trainPreview")),
                   tabPanel("Test Set Preview", DTOutput("testPreview"))
                 )
               )
             )
    ),
    tabPanel("Pre-Processing", icon = icon("edit"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("preprocess_dataset", "Choose Dataset or Upload Your Own:", 
                             choices = c("All Data" = "data_all", 
                                         "Demographic Data" = "data_dem", 
                                         "Opinion Data" = "data_opin",
                                         "Upload Your Own File" = "upload")),
                 conditionalPanel(
                   condition = "input.preprocess_dataset == 'upload'",
                   fileInput("uploadData", "Upload Dataset:", accept = c(".csv", ".xlsx"))
                 ),
                 uiOutput("varSelectInput"),  # For selecting variables dynamically
                 checkboxInput("removeNA", "Remove All NAs", value = FALSE),
                 actionButton("applyPreprocess", "Apply Pre-processing", icon = icon("magic"))
               ),
               mainPanel(
                 dataTableOutput("preprocess_preview")
               )
             )
             ),
    
    tabPanel("Model Analysis and Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("modelDataset", "Choose Dataset or Upload Your Own:", 
                             choices = c("All Data" = "data_all", 
                                         "Demographic Data" = "data_dem", 
                                         "Opinion Data" = "data_opin",
                                         "Upload Your Own File" = "upload")),
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
    tabPanel("Prediction Visuals")
    )
)

# Define Server
server <- function(input, output, session) {
  
  # Data preview
  output$data_preview <- renderDataTable({
    # Read uploaded file if selected
    if (!is.null(input$file)) {
      read.csv(input$file$datapath, header = TRUE)
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
  
  # Preview data after splitting
  output$preview <- renderDataTable({
    # Your code for previewing split data
    if (is.null(input$file_split)) {
      data <- get(input$dataset)
    } else {
      data <- read.csv(input$file_split$datapath, header = TRUE)
    }
    # Subset data based on selected variable
    selected_var <- input$split_variable
    data[, selected_var, drop = FALSE]
  })
  
  # Split data into train and test sets
  split_data <- reactive({
    split_ratio <- input$splitRatio / 100
    index <- round(nrow(data()) * split_ratio)
    train_data <- data()[1:index, ]
    test_data <- data()[(index + 1):nrow(data()), ]
    list(train_data = train_data, test_data = test_data)
  })
  
  # Train set preview
  output$trainPreview <- renderDataTable({
    if (!is.null(input$splitData)) {
      head(split_data()$train_data)
    }
  })
  
  # Test set preview
  output$testPreview <- renderDataTable({
    # Your code for previewing test set
    if (!is.null(input$splitData)) {
      head(split_data()$test_data)
    }
  })
  
  # Pre-processing data
  output$preprocess_preview <- renderDataTable({
    # Your code for pre-processing data
    # Example: Remove NA values if selected
    if (input$removeNA) {
      data <- na.omit(get(input$preprocess_dataset))
    } else {
      data <- get(input$preprocess_dataset)
    }
    data
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
