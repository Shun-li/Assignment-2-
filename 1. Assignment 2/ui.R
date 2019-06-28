library(shiny)
library(shinyjs)
library(DT)


ui <- fluidPage(
  
  useShinyjs(),
  
  titlePanel("Logistic Regression Algorithm"),
  
  navbarPage("SHUN LI , 65322005"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("n", "Number of Observations:", min = 1, max = 303, step = 1, value = 3),
      checkboxGroupInput("variables_x_select",
                         label = "Select predictor variables (X) ",
                         choices = list("age","sex","cp", "trestbps", "chol", 
                                        "fbs", "restecg", "thalach",
                                        "exang", "oldpeak", "slope", "ca", "thal"),
                         selected = NULL),
      checkboxGroupInput("variables_y_select",
                         label = "Select response variables (Y) ",
                         choices = list("hd"),
                         selected = NULL)
      
    ),    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data",verbatimTextOutput("DataStr"),DT::dataTableOutput("table")),
        tabPanel("Model", verbatimTextOutput("ModelSummary"),verbatimTextOutput("ModelPseudoR_value")),
        tabPanel("Predict", plotOutput("DataPredict"))
      )
    )
  )
)


