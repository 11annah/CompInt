library(shiny)
library(bslib)

# Define UI
ui <- page_navbar(
  title="Specifying quantities",
  sidebar = sidebar("Sidebar"),
  nav_panel("What quantitiy do you want?", "Page 1 content"),
  nav_panel("Let's specify it", "Page 2 content"),
  nav_panel("Get your interpretation!", "Page 3content"),
  nav_spacer(),
)

# Define server logic
server <- function(input, output, session) {

}

# Run the application
shinyApp(ui, server)
