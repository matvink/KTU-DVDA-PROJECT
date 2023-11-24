library(shinydashboard)
library(shiny)
library(tidyverse)
library(h2o)
h2o.init()

ui <- dashboardPage(
  dashboardHeader(title = "Loan dashboard"),
  dashboardSidebar(fileInput("file", "Ikelti csv faila")),
  dashboardBody(
    valueBox(100, "Basic example"),
    tableOutput("table"),
    dataTableOutput("predictions")
  )
)
server <- function(input, output) {

  model <- h2o.loadModel("../../4-model/rf_model")
  output$table <- renderTable({
    req(input$file)
    table <- read_csv(input$file$datapath) %>%
      group_by(credit_score) %>%
      summarise(n = n()) %>%
      arrange(desc(n))
    table
  })
  
  output$predictions <- renderDataTable({
    req(input$file)
    df_test <- h2o.importFile(input$file$datapath)
    p <- h2o.predict(model, df_test)
    p %>%
      as_tibble() %>%
      mutate(y = predict) %>%
      select(y) %>%
      rownames_to_column("id") %>%
      head(100)
  })
}
shinyApp(ui, server)