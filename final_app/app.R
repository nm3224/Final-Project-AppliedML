library(shiny)

library(shinythemes)

library(shinycssloaders)

library(tidyverse)

library(ggExtra)

library(data.table)


data_all <- read.csv("data/all_clean.csv", header = TRUE)
data_dem <- read.csv("data/dem_clean.csv", header = TRUE)
data_misc <- read.csv("data/misc_clean.csv", header = TRUE)
data_opin <- read.csv("data/opin_clean.csv", header = TRUE)


# Define UI for application

ui <- fluidPage(
  
  titlePanel("Predicting Social Class Based on Differing Variables"),
  
  navbarPage(
    
    title = ("Project Overview"),
    
    theme = shinytheme("flatly"),
    
    tabPanel("First Panel", icon = icon("info-circle"),
             
             titlePanel("Predicting Social Class Based on Differing Variables"),
             
             mainPanel(
               
               HTML("STAT 3106: Applied Machine Learning - Final Project:<br><br>
               
                        We decided early on that we would all be interested in a question of the social sciences, 
                        namely in exploring how demographics relate to political opinions and identity. 
                        Because our interest is in understanding these relationships, not only in predicting them, we decided we also wanted to implement interpretable machine learning techniques. 
                        After looking through sites like kaggle and data.gov, we settled on working with data from the General Social Survey (GSS), as it presents extraordinarily extensive data on both demographics and opinions.<br><br>
                        
                        The dataset we started working with was the GSS cumulative dataset, including all variables ever collected from 1972-2022. 
                        With over 72,000 observations on almost 6,700 variables, this dataset was extraordinarily large.<br><br> 
                        
                        Final Research Question: How well can each of our chosen models (XGBoost, Random Forest, and Artificial Neural Network) 
                        predict an individual's independently defined subjective social class based on 4 data subsets: 1) solely variables such as demographic information (ex. income, race, occupation); 2) solely variables such as opinion data (ex. political opinions); 4) solely variables pertaining to (fill in misc?); 3) all variables?
"))
             
    ),
    
    tabPanel("Our Data", icon = icon("folder-open"),
             
             titlePanel("Upload Data"),
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput("dataset", "Dataset:", choices = c("All_Data" = "data_all", "Demographic_Data" = "data_dem", "Opinion_Data" = "data_opin", "Misc_Data" = "data_misc", "Upload your own file")),
                 
                 conditionalPanel(condition = "input.dataset == 'Upload your own file'",
                                  
                                  fileInput("file", "Select your files:",
                                            
                                            accept=c("text/csv",
                                                     
                                                     "text/comma-separated-values,text/plain",
                                                     
                                                     ".csv"))  
                                  
                 )
                 
               ),
               
               mainPanel(
                 
                 
                 dataTableOutput("data_preview")
                 
               )
               
             )
             
    ),
    
    tabPanel("Exploratory Visuals",
             
             titlePanel("Scatterplot"),
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput("response", "Response Variable (Y)", choices = NULL), 
                 
                 selectInput("explanatory", "Explanatory Variable (X)", choices = NULL),
                 
                 sliderInput("shade", "Transaparency Rate", min = 0, max = 1, value = 0.5, step = 0.1),
                 
                 checkboxInput("marginal", "Marginal Distributions", value = FALSE)
                 
               ),
               
               mainPanel(
                 
                 tabsetPanel(
                   
                   tabPanel("Scatterplot", 
                            
                            plotOutput("plot1")),
                   
                   tabPanel("Numeric Summary",
                            
                            dataTableOutput("result1"))
                   
                 )
                 
               )
               
               
             )
             
             
             
    ),
    
    
    
    
    tabPanel("Models",
             
             titlePanel("Histogram"),
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectInput("var", "Variable", choices = NULL), 
                 
                 numericInput("bins", "Number of bins", min = 1, max = 50, step = 1, value = 10),
                 
                 radioButtons("color", "Color of bins:",
                              
                              choices = list("Blue" = "blue", "Red" = "red", "Green" = "green"),
                              
                              selected = "blue"),
                 
                 actionButton("click","Submit")
                 
               ),
               
               mainPanel(
                 
                 tabsetPanel(
                   
                   tabPanel("Histogram",
                            
                            plotOutput("plot2"))
                   
                   
                 )
                 
               )
               
               
             )
             
             
             
             
             
    )
    
    
    
  )
  
  
  
)


# Define Server

server <- function(input, output, session) {
  
  ##  
  
  File <- reactive({
    
    if(input$dataset == 'Upload your own file'){
      
      req(input$file)
      
      File <- input$file
      
      df <- data.frame(rbindlist(lapply(File$datapath, fread), use.names = TRUE, fill = TRUE))
      
      return(df)
      
    } else {
      
      return(data_all)
    } 
    
  })
  
  
  ##
  
  observeEvent(File(), {
    
    updateSelectInput(session, "response",
                      
                      choices = names(File()))
  })
  
  
  
  observeEvent(File(), {
    
    updateSelectInput(session, "explanatory",
                      
                      choices = names(File()))
  }) 
  
  
  observeEvent(File(), {
    
    updateSelectInput(session, "var",
                      
                      choices = names(File()))
  })
  
  ##
  
  
  output$data_preview <- renderDataTable({
    
    File()
    
  }) 
  
  
  ##
  output$plot1 <- renderPlot({
    
    p = ggplot(data = File(), aes_string(x = input$explanatory, y = input$response)) +
      
      geom_point(alpha = input$shade) +
      
      theme_minimal() 
    
    
    if(input$marginal) {
      
      p <- ggMarginal(p, type = "histogram")
    }
    
    
    p
    
  })
  
  
  ##
  
  output$result1 <- renderDataTable({
    
    summary_data <- summary(File()[[input$response]])
    
    data.frame(Measure = names(summary_data), Value = as.character(summary_data))
    
  })
  
  
  ##
  
  plot2 <- eventReactive(input$click, 
                         
                         ggplot(data = File(), aes_string(x = input$var)) +
                           
                           geom_histogram(binwidth = diff(range(File()[[input$var]]) / input$bins), fill = input$color, color = "black") +
                           
                           labs(x = input$var, y = "Frequency", title = "Histogram") +
                           
                           theme_minimal()
                         
  )
  
  
  
  output$plot2 <- renderPlot({
    
    plot2() 
    
  })
  
}





# Run the application 

shinyApp(ui = ui, server = server)
