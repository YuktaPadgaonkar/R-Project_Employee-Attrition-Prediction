# Import libraries
install.packages("shiny")
library(shiny)

library(data.table)


# Read in the RF model
model <- readRDS("D:\\SEM8\\R Programming\\R Project\\RPROJECT\\try\\trymodefinal.rds")



####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Employee Attrition Prediction'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Insert your data for prediction :)')),
    numericInput("age", 
                 label = "Age  ", 
                 value = 21),
    numericInput("maritalstatus", 
                 label = "Marital Status ",value=1),
    
    numericInput("overtime", 
                 label = "Overtime ( Enter 1:No and 2:Yes )", 
                 value = 2),
    numericInput("monthlyincome", 
                 label = "Monthly Income ", 
                 value = 5993),
    numericInput("totalworkingyears", 
                 label = "Total Working Years ", 
                 value = 8),
    numericInput("joblevel", 
                 label = "Job level ( Enter in range 1-5 )", 
                 value = 2),
    numericInput("stockoptionlevel", 
                 label = "Stock Option Level ( Enter in range 0-3 )", 
                 value = 0),
    numericInput("environmentsatisfaction", 
                 label = "Environment Satisfaction ( Enter in range 1-4 )", 
                 value = 2),
    numericInput("yearsatcompany", 
                 label = "Years at company", 
                 value = 6),
    
    numericInput("yearsincurrentrole", 
                 label = "Years In Current Role", 
                 value = 5),
                 
    numericInput("yearswithcurrmanager", 
                 label = "Years with Current Manager", 
                 value = 5),
    
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  
  # Input Data
  datasetInput <- reactive({  
    PREDICTION = ''
    model <- readRDS("model.rds")
    print(input$yearsincurrentrole)
    Age = c(input$age)
    MaritalStatus = c(input$maritalstatus)
    OverTime = c(input$overtime)
    MonthlyIncome = c(input$monthlyincome)
    TotalWorkingYears = c(input$totalworkingyears)
    
    JobLevel = c(input$joblevel)
    StockOptionLevel = c(input$stockoptionlevel)
    EnvironmentSatisfaction = c(input$environmentsatisfaction)
    YearsAtCompany = c(input$yearsatcompany)
    
    YearsInCurrentRole = c(input$yearsincurrentrole)
    YearsWithCurrManager = c(input$yearswithcurrmanager)
    #JobRole = c(input$jobrole)
    print(model)
   #  datas = data.frame(Age,MaritalStatus,OverTime,MonthlyIncome,TotalWorkingYears,JobLevel,StockOptionLevel,EnvironmentSatisfaction,YearsAtCompany,YearsInCurrentRole,YearsWithCurrManager)
    datas = data.frame(Age,EnvironmentSatisfaction,JobLevel,MaritalStatus,MonthlyIncome,OverTime,StockOptionLevel,TotalWorkingYears,YearsAtCompany,YearsInCurrentRole,YearsWithCurrManager)
    
    print(datas)
    print(datas)
    
    result = predict(model,datas)
    result1 = predict(model,datas)*100
    print(result)
    print(result1)
    
    PREDICTION1 = paste('The chance of Employee Attrition is',format(round(result1, 2), nsmall = 2),'%')
    ans = ifelse(result > 0.5, 'Higher', 'Low')
    PREDICTION = paste('The chance of Employee Attrition is',ans)
    Output <- data.frame(PREDICTION,PREDICTION1)
    
    
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)