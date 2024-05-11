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
                 selectInput("dataset_split", "Choose a dataset or upload your own:",
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
    tabPanel("Pre-Processing", "Model Content Here"),
    
    tabPanel("Model Predictions and Visuals", "Model Content Here")
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive expression to handle data loading based on user selection or file upload
  File <- reactive({
    if (input$dataset == 'Upload your own file') {
      req(input$file)  # Ensure file is uploaded
      # Read file using data.table's fread for better performance
      df <- data.frame(fread(input$file$datapath), use.names = TRUE, fill = TRUE)
      return(df)
    } else {
      # Return the dataset based on user selection
      switch(input$dataset,
             "All_Data" = data_all,
             "Demographic_Data" = data_dem,
             "Opinion_Data" = data_opin)
    }
  })
  
  # Render data preview table
  output$data_preview <- renderDataTable({
    File()
  })
  
  # Observe changes in the loaded data to update UI elements
  observe({
    df <- req(File())  # Ensure data is loaded
    updateSelectInput(session, "response", choices = names(df))
    updateSelectInput(session, "var", choices = names(df))
  })
  
  # Update the UI elements for selecting variables dynamically based on the dataset loaded
  output$varSelectUI_split <- renderUI({
    if (!is.null(dataset()) && ncol(dataset()) > 0) {
      selectInput("responseVar", "Select Response Variable:", choices = names(dataset()))
    }
  })
  
  # Logic to handle dynamic UI for file uploads
  output$fileInputUI_split <- renderUI({
    if (input$dataset_split == "Upload your own file") {
      fileInput("datafile_split", "Upload CSV File:", accept = ".csv")
    }
  })
  
  # Observe the action button for data splitting
  observeEvent(input$splitData, {
    req(dataset())  # Ensure the dataset is loaded
    data <- dataset()
    responseVar <- input$responseVar  # The variable to predict
    
    # Check if the selected response variable exists in the dataset
    if (responseVar %in% names(data)) {
      # Randomly sample indices for the training set based on the input ratio
      set.seed(123)  # For reproducibility
      trainIndices <- sample(seq_len(nrow(data)), size = floor(nrow(data) * input$splitRatio / 100))
      trainSet <- data[trainIndices, , drop = FALSE]
      testSet <- data[-trainIndices, , drop = FALSE]
      
      # Update data previews
      output$preview <- renderDT({
        datatable(data)
      })
      output$trainPreview <- renderDT({
        datatable(trainSet)
      })
      output$testPreview <- renderDT({
        datatable(testSet)
      })
    } else {
      showNotification("Selected response variable is not in the dataset", type = "error")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
