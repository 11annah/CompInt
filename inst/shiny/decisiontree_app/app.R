library(shiny)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  # Dashboard header
  dashboardHeader(
    title = "Specifying quantities",
    # Customize the header if needed
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://11annah.github.io/CompInt/",
        tags$img(src = "logo.png", height = "20"),
        "The CompInt package",
        style = "padding-top: 10px; padding-bottom: -20px"
      )
    )
  ),
  
  # Dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      id ="tabs",
      # First group of pages
      menuItem("What quantitiy do you want?", tabName = "page0", icon = icon("gear")),
      menuItem("Let's specify it", tabName = "group1", icon = icon("gears"),startExpanded = TRUE,
               menuSubItem("Choose an assumption", tabName = "page1"),
               menuSubItem("Choose distribution(s)", tabName = "page2"),
               menuSubItem("Additional choices (optional)", tabName = "page3")
      ),
      menuItem("Get your interpretation!", tabName = "page4", icon = icon("globe"))
    )
  ),

  # Dashboard body
  dashboardBody(
    tags$head(tags$script(src="script.js")),
    # Body content (main panel)
    tabItems(
      tabItem(tabName = "page0",
              fluidRow(
                column(12,
                       h2("Choose Your Quantity"),
                       p("Please review the following options and select your preferred quantity."),
                       br(),
                       box(title = "Option A",
                           "Description of Option A goes here.",
                           align = "center",
                           footer = actionButton("gME", "Choose Option A", icon = icon("check-circle"), width = "100%"),
                           width=4
                      ),
                      box(title = "Option B",
                          "Description of Option B goes here.",
                          align = "center",
                          footer = actionButton("IE", "Choose Option B", icon = icon("check-circle"), width = "100%"),
                          width=4
                     ),
                     box(title = "Option C",
                         "Description of Option C goes here.",
                         align = "center",
                         footer = actionButton("IpredD", "Choose Option C", icon = icon("check-circle"), width = "100%"),
                         width=4
                     )
                )
              ),
              actionButton("clear", "Clear",icon = icon("x"), width = "100%"),
              verbatimTextOutput("click_message")
      ),
      # Page 1 content
      tabItem(
        tabName = "page1",
        fluidRow(
          column(12,
                 h2("Choose Your Assumption"),
                 p("Please review the following options and select your preferred assumption"),
                 br(),
                 box(title = "Option A",
                     "Description of Option A goes here.",
                     align = "center",
                     footer = actionButton("A1", "Choose Option A", icon = icon("check-circle"), width = "100%"),
                     width=4
                 ),
                 box(title = "Option B",
                     "Description of Option B goes here.",
                     align = "center",
                     footer = actionButton("A2", "Choose Option B", icon = icon("check-circle"), width = "100%"),
                     width=4
                 ),
                 box(title = "Option C",
                     "Description of Option C goes here.",
                     align = "center",
                     footer = actionButton("A3", "Choose Option C",icon = icon("check-circle"), width = "100%"),
                     width=4
                 )
          )
        ),
        actionButton("clear_assumptions", "Clear", icon = icon("x"), width = "100%"),
        verbatimTextOutput("click_message_assumption")
      ),
      # Page 2 content
      tabItem(
        tabName = "page2",
        fluidRow(
          column(12,
                 h2("Choose Your Distribution"),
                 p("Please review the following options and select your preferred distribution"),
                 br(),
                 box(title = "Option A",
                     "Description of Option A goes here.",
                     align = "center",
                     footer = actionButton("B1", "Choose Option A", icon = icon("check-circle"), width = "100%"),
                     width=4
                 ),
                 box(title = "Option B",
                     "Description of Option B goes here.",
                     align = "center",
                     footer = actionButton("B2", "Choose Option B", icon = icon("check-circle"), width = "100%"),
                     width=4
                 ),
                 box(title = "Option C",
                     "Description of Option C goes here.",
                     align = "center",
                     footer = actionButton("B3", "Choose Option C",icon = icon("check-circle"), width = "100%"),
                     width=4
                 )
          )
        ),
        actionButton("clear_distribution", "Clear", icon = icon("x"), width = "100%"),
        verbatimTextOutput("click_message_distribution")
      ),
      # Page 3 content
      tabItem(
        tabName = "page3",
        fluidRow(
          column(12,
                 h2("Choose Your Choices"),
                 p("Please review the following options and select your preferred choices"),
                 br(),
                 box(title = "Option A",
                     "Description of Option A goes here.",
                     align = "center",
                     footer = checkboxInput("C1", "Choose Option A", value=FALSE, width = "33%"),
                     width=3
                 ),
                 box(title = "Option B",
                     "Description of Option B goes here.",
                     align = "center",
                     footer = checkboxInput("C2", "Choose Option B", value=FALSE, width = "33%"),
                     width=3
                 ),
                 box(title = "Option C",
                     "Description of Option C goes here.",
                     align = "center",
                     footer = checkboxInput("C3", "Choose Option C", value=FALSE, width = "33%"),
                     width=3
                 ),
                 box(title = "Option D",
                     "Description of Option D goes here.",
                     align = "center",
                     footer = checkboxInput("C4", "Choose Option D", value=FALSE, width = "33%"),
                     width=3
                 )
          )
        )
      ),
      # Page 4 content
      tabItem(
        tabName = "page4",
        fluidRow(
          column(12,
                 h2("Result"),
                 HTML("<button id='D1' style='width:100%;' type='button' class='btn btn-default action-button' onclick='create_result()'>Get result</button>"),
                 box(actionButton("D2", "Quantity: ", width = "100%",style="color: #FF0000; background-color: #F8F8F8; border-color: #FF0000"),
                     actionButton("D3", "Assumption: ", width = "100%",style="color: #0000FF; background-color: #F8F8F8; border-color: #0000FF"),
                     actionButton("D4", "Distribution: ", width = "100%",style="color: #008000; background-color: #F8F8F8; border-color: #008000"),
                     actionButton("D4", ":", width = "100%",style="color: #008000; background-color: #F8F8F8; border-color: #008000"),
                     width=3
                 ),
                 box(p(id="result","Please click on the top button for result."),
                     width=9
                 )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Track which button was clicked (quantity)
  button_clicked <- reactiveVal(NULL)
  # Observer to update which button was clicked
  observeEvent(input$gME, {button_clicked("gME")})
  observeEvent(input$IE, {button_clicked("IE")})
  observeEvent(input$IpredD, {button_clicked("IpredD")})
  observeEvent(input$clear, {button_clicked(NULL)})
  # Render the message based on the button clicked
  output$click_message <- renderText({
    if (!is.null(button_clicked())) {
      paste0("Button '", button_clicked(), "' was clicked.")
    } else {
      "No button was clicked."
    }
  })
  
  # Track which button was clicked (assumption)
  button_clicked_assumption <- reactiveVal(NULL)
  # Observer to update which button was clicked
  observeEvent(input$A1, {button_clicked_assumption("A1")})
  observeEvent(input$A2, {button_clicked_assumption("A2")})
  observeEvent(input$A3, {button_clicked_assumption("A3")})
  observeEvent(input$clear_assumptions, {button_clicked_assumption(NULL)})
  # Render the message based on the button clicked
  output$click_message_assumption <- renderText({
    if (!is.null(button_clicked_assumption())) {
      paste0("Button '", button_clicked_assumption(), "' was clicked.")
    } else {
      "No button was clicked."
    }
  })
  
  # Track which button was clicked (distribution)
  button_clicked_distribution <- reactiveVal(NULL)
  # Observer to update which button was clicked
  observeEvent(input$B1, {button_clicked_distribution("B1")})
  observeEvent(input$B2, {button_clicked_distribution("B2")})
  observeEvent(input$B3, {button_clicked_distribution("B3")})
  observeEvent(input$clear_distribution, {button_clicked_distribution(NULL)})
  # Render the message based on the button clicked
  output$click_message_distribution <- renderText({
    if (!is.null(button_clicked_distribution())) {
      paste0("Button '", button_clicked_distribution(), "' was clicked.")
    } else {
      "No button was clicked."
    }
  })
  
  #Track which button was clicked (result)
  observeEvent(input$D2, {updateTabItems(session,"tabs","page0")})
  observeEvent(input$D3, {updateTabItems(session,"tabs","page1")})
  observeEvent(input$D4, {updateTabItems(session,"tabs","page2")})
}

# Run the application
shinyApp(ui = ui, server = server)