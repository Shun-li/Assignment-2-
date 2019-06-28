library(shiny)
library(shinyjs)
library(DT)
library(ggplot2)
library(cowplot)
library(caret)



######################################################################################################

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)
colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain 
  # 1 = typical angina, 
  # 2 = atypical angina, 
  # 3 = non-anginal pain, 
  # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  # 1 = normal
  # 2 = having ST-T wave abnormality
  # 3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment 
  # 1 = upsloping 
  # 2 = flat 
  # 3 = downsloping 
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease 
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)
data[data == "?"] <- NA
## Now add factors for variables that are factors and clean up the factors
## that had missing data...
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.integer(data$ca) # since this column had "?"s in it 
# R thinks that the levels for the factor are strings, but
# we know they are integers, so first convert the strings to integiers...
data$ca <- as.factor(data$ca)  # ...then convert the integers to factor levels
data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)
## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor
data <- data[!(is.na(data$ca) | is.na(data$thal)),]





server <-  function(input, output) {
  
  ######################################################################################################  
  
  output$table <- DT::renderDataTable({
    data <- head(data, input$n)
    DT::datatable(data[, c(input$variables_x_select,input$variables_y_select), drop = FALSE])
  }) 
  
  ###################################################################################################### 
  
  output$DataStr <- renderPrint({
    data <- data[,c(input$variables_x_select,input$variables_y_select)]
    data <- head(data, input$n)
    str(data)
  }) 
  
  ######################################################################################################
  
  
  ## Now do the actual logistic regression
  output$ModelSummary <- renderPrint({
    form <- sprintf("%s~%s",paste0(input$variables_y_select),paste0(input$variables_x_select,collapse="+"))
    print(form)
    
    data <- data[,c(input$variables_x_select,input$variables_y_select)]
    data <- head(data, input$n)
    logistic <-glm(as.formula(form),family="binomial",data=data)
    print(summary(logistic))
  })
  
  
  ###################################################################################################### 
  
  
  ## Lastly, we can plot the predicted probabilities for each sample having 
  ## heart disease and color by whether or not they actually had heart disease
  output$DataPredict <- renderPlot({
    
    form <- sprintf("%s~%s",paste0(input$variables_y_select),paste0(input$variables_x_select,collapse="+"))
    
    data <- data[,c(input$variables_x_select,input$variables_y_select)]
    data <- head(data, input$n)
    logistic <-glm(as.formula(form),family="binomial",data=data)
    
    
    predicted.data <- data.frame(
      probability.of.hd=logistic$fitted.values,
      hd=data$hd)
    
    predicted.data <- predicted.data[
      order(predicted.data$probability.of.hd, decreasing=FALSE),]
    predicted.data$rank <- 1:nrow(predicted.data)
    
    ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) +
      geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
      xlab("Index") + 
      ylab("Predicted probability of getting heart disease")
  }) 
  
  
  ###################################################################################################### 
  
  
  output$ModelPseudoR_value <- renderPrint({
    
    form <- sprintf("%s~%s",paste0(input$variables_y_select),paste0(input$variables_x_select,collapse="+"))
    
    data <- data[,c(input$variables_x_select,input$variables_y_select)]
    data <- head(data, input$n)
    logistic <-glm(as.formula(form),family="binomial",data=data)
    
    ## Now calculate the overall "Pseudo R-squared" and its p-value
    ll.null <- logistic$null.deviance/-2
    ll.proposed <- logistic$deviance/-2
    
    ## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
    l1 <- (ll.null - ll.proposed) / ll.null
    l1 <- sprintf("McFadden's Pseudo R^2:%s",l1)
    print(l1)
    ## The p-value for the R^2
    l2 <- 1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))
    l2 <- sprintf("The p-value for the R^2:%s",l2)
    print(l2)
    
  }) 
  
}         

