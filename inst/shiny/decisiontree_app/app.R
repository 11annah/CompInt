# Load the shiny library
library(shiny)

# Define UI for application
ui <- fluidPage(
  titlePanel("Square Calculator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("number", "Select a number:",
                  min = 1, max = 100, value = 50)
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$result <- renderText({
    input_number <- input$number
    paste("Square of", input_number, "is", input_number^2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
