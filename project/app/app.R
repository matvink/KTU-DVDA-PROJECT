library(shiny)
library(tidyverse)
library(h2o)
h2o.init()

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("DVDA 2023 project. Matas ir Ieva",
             actionButton("logout_button", "Logout", style = "color: red; font-weight: bold;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      
      numericInput("amount_current_loan", "Current loan amount", value = 0, min = 0, max = 2000000, step = 500),
      numericInput("yearly_income", "Yearly income", value = 0, min = 0, max = 2000000, step = 500),
      sliderInput("bankruptcies", "Bankruptcies", value = 0, min = 0, max = 10),
      numericInput("monthly_debt", "Monthly debt", value = 0, min = 0, max = 1000000, step = 500),
      numericInput("years_credit_history", "Years credit history", value = 0, min = 0, max = 100, step = 1),
      numericInput("months_since_last_delinquent", "Months since last delinquent", value = 0, min = 0, max = 500, step = 1),
      numericInput("open_accounts", "Open accounts", value = 0, min = 0, max = 70, step = 1),
      sliderInput("credit_problems", "Credit problems", value = 0, min = 0, max = 100),
      numericInput("credit_balance", "Credit balance", value = 0, min = 0, max = 5000000, step = 500),
      numericInput("max_open_credit", "Max open credit", value = 0, min = 0, max = 5000000, step = 500),
      numericInput("years_current_job", "Years in current job", value = 1, min = 1, max = 20, step = 1),
      selectInput("loan_purpose", "Loan purpose", c("debt_consolidation", "other", "home_improvements", "business_loan", "buy_a_car", "medical_bills"), "good"),
      selectInput("home_ownership", "Home ownership", c("mortgage", "rent", "own"), "good"),
      selectInput("credit_score", "Credit score", c("good", "very_good", "fair"), "good"),
      selectInput("term", "Loan term", c("short", "long"), "good"),
      
      actionButton("calculate_button", "Calculate"),
      br(),
      br(),
      a("Logout", href = "javascript:window.location.reload(true);", style = "color: red; font-weight: bold;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Input Data", tableOutput("inputTable")),
        tabPanel("Prediction Results", dataTableOutput("predictionTable")),
        tabPanel("Uploaded File", dataTableOutput("fileTable")),
        tabPanel("Additional Information",
                 fluidRow(
                   column(4,
                          h3("Additional Information"),
                          textInput("AD", "Feedback", value = "")
                   )
                 )
        ),
        tabPanel("Model AUC result",
                 fluidRow(
                   column(4,
                          h3("Model AUC result - 0.82"),
                   )
                 )     
        )
      )
    )
  ),
  tags$style(HTML("
    #calculate_button {
      background-color: #4CAF50; /* Žalia spalva */
      color: white;
      border: none;
      padding: 10px 20px;
      text-align: center;
      text-decoration: none;
      display: inline-block;
      font-size: 16px;
      margin: 4px 2px;
      cursor: pointer;
    }

    .numericInput, .sliderInput, .selectInput, .file-input {
      background-color: #f0f0f0; /* Šviesiai pilka spalva */
      border: 1px solid #ccc;
    }

    .additional-info-column {
      background-color: #d9f7be; /* Šviesiai žalia spalva */
    }
  "))
)

# Define server logic
server <- function(input, output) {
  model <- h2o.loadModel("../4-model/my_best_automlmode")
  
  input_data <- eventReactive(input$calculate_button, {
    data <- data.frame(
      amount_current_loan = input$amount_current_loan,
      yearly_income = input$yearly_income,
      bankruptcies = input$bankruptcies,
      monthly_debt = input$monthly_debt,
      years_credit_history = input$years_credit_history,
      months_since_last_delinquent = input$months_since_last_delinquent,
      open_accounts = input$open_accounts,
      credit_problems = input$credit_problems,
      credit_balance = input$credit_balance,
      max_open_credit = input$max_open_credit,
      years_current_job = input$years_current_job,
      loan_purpose = factor(input$loan_purpose),
      home_ownership = factor(input$home_ownership),
      credit_score = factor(input$credit_score),
      term = factor(input$term),
      AD = input$AD  # Pridėtas naujas stulpelis "Feedback"
    )
    data.frame(
      column = names(data),
      value = unlist(data)
    )
  })
  
  output$inputTable <- renderTable({
    input_data()
  })
  
  prediction_data <- eventReactive(input$calculate_button, {
    inputData <- data.frame(
      amount_current_loan = input$amount_current_loan,
      yearly_income = input$yearly_income,
      bankruptcies = input$bankruptcies,
      monthly_debt = input$monthly_debt,
      years_credit_history = input$years_credit_history,
      months_since_last_delinquent = input$months_since_last_delinquent,
      open_accounts = input$open_accounts,
      credit_problems = input$credit_problems,
      credit_balance = input$credit_balance,
      max_open_credit = input$max_open_credit,
      years_current_job = input$years_current_job,
      loan_purpose = factor(input$loan_purpose),
      home_ownership = factor(input$home_ownership),
      credit_score = factor(input$credit_score),
      term = factor(input$term),
      AD = input$AD  # Pridėtas naujas stulpelis "Feedback"
    )
    
    predictions <- h2o.predict(model, as.h2o(inputData))
    as.data.frame(predictions)
  })
  
  output$predictionTable <- renderDataTable({
    prediction_data()
  })
  
  output$fileTable <- renderDataTable({
    req(input$file)
    test_data <- h2o.importFile(input$file$datapath)
    predictions <- h2o.predict(model, test_data)
    predictions %>%
      as_tibble() %>%
      mutate(id = row_number(), y = p0) %>%
      select(id, y)
  })
  
  
  # Pridėti "Logout" mygtuko logiką
  observeEvent(input$logout_button, {
    session$reload()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)