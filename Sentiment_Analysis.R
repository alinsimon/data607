install.packages(c("shiny", "tidytext", "dplyr", "ggplot2"))
# Load required libraries
library(shiny)
library(tidytext)
library(dplyr)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Sentiment Analysis App"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text_input", "Enter Text:", rows = 5, placeholder = "Type your text here..."),
      actionButton("analyze", "Analyze Sentiment"),
      hr(),
      h4("Sentiment Results:"),
      verbatimTextOutput("sentiment_summary")
    ),
    
    mainPanel(
      plotOutput("sentiment_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$analyze, {
    
    # Get the input text
    text_data <- data.frame(text = input$text_input)
    
    # Perform sentiment analysis
    sentiment_results <- text_data %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(sentiment) %>%
      mutate(proportion = n / sum(n))
    
    # Display sentiment summary
    output$sentiment_summary <- renderPrint({
      sentiment_results
    })
    
    # Create sentiment plot
    output$sentiment_plot <- renderPlot({
      ggplot(sentiment_results, aes(x = sentiment, y = proportion, fill = sentiment)) +
        geom_col() +
        labs(title = "Sentiment Analysis", x = "Sentiment", y = "Proportion") +
        theme_minimal()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
