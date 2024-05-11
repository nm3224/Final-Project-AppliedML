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
                 selectInput("preprocess_dataset", "Choose Dataset:", 
                             choices = c("All Data" = "data_all", 
                                         "Demographic Data" = "data_dem", 
                                         "Opinion Data" = "data_opin")),
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
             "Opinion_Data" = data_opin,
             data_all)
    }
  })
  
  output$data_preview <- renderDataTable({
    
    File()
    
  })
  
  observeEvent(input$applyPreprocess, {
    req(input$file)  # Ensure the dataset is loaded
    
    # Define the recipe
    blueprint <- recipe(~., data = input$file) %>%
      step_string2factor(all_nominal_predictors()) %>%
      step_other(all_nominal_predictors(), threshold = 0.01, other = "Other") %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors()) %>%
      step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
      step_nzv(all_predictors(), freq_cut = 30/100, unique_cut = 20/100) %>%
      step_impute_knn(all_predictors(), neighbors = 5) %>%  # Impute missing values
      prep()  # Prepares the recipe by estimating the required statistics
    
    # Bake the recipe to apply the transformations
    processed_data <- bake(blueprint, new_data = NULL)  # Apply to the entire dataset
    
    # Output the processed data to a DataTable
    output$preprocess_preview <- renderDataTable({
      datatable(processed_data, options = list(scrollX = TRUE, pageLength = 25))
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
