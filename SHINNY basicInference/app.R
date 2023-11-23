
# install.packages("C:/Andres/MASTER UPC/OPERATIONS RESEARCH/1Q/R Y SAS/13. CLASE 13/PACKAGE CONTROL/basicInference_0.0.1.tar.gz", repos = NULL, type = "source")

library(shiny)
library(basicInference)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("basicInference package"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: List for the type of random variables ----
      selectInput(inputId="rv", label="Type of variables:", c("Normal", "Student's t", "Exponential"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Literal result of the test ----
      textOutput("text"),
      # Output: Scatter plot ----
      plotOutput(outputId = "distPlot")
    )
  )
)

server <- function(input, output) {
  generate_data <- eventReactive(input$rv, {  
    if (input$rv=="Normal")
    {
      s1 <- rnorm(500)
      s2 <- rnorm(500)
    }
    if (input$rv=="Student's t")
    {
      s1 <- rt(500, 5)
      s2 <- rt(500, 5)
    }
    if (input$rv=="Exponential")
    {
      s1 <- rexp(500)
      s2 <- rexp(500)
    }
    res <- t_test(s1, s2)
    return(res)
  })
  output$text <- renderText({
    as.character(generate_data()[5])})
  output$distPlot <- renderPlot({
    plot(generate_data())
  })
}

shinyApp(ui=ui, server=server)

