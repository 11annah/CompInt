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
      # First group of pages
      menuItem("What quantitiy do you want?", tabName = "page0", icon = icon("gear")),
      menuItem("Let's specify it", tabName = "group1", icon = icon("gears"),startExpanded = TRUE,
               menuSubItem("Choose an assumption", tabName = "page1"),
               menuSubItem("Choose distribution(s)", tabName = "page2"),
               menuSubItem("Additional choices (optional)", tabName = "page")
      ),
      menuItem("Get your interpretation!", tabName = "page3", icon = icon("globe"))
    )
  ),

  # Dashboard body
  dashboardBody(
    # Body content (main panel)
    tabItems(
      tabItem(tabName = "page0",
              fluidRow(
                column(12,
                       h2("Choose Your Quantity"),
                       p("Please review the following options and select your preferred quantity."),
                       br(),
                       fluidRow(
                         column(4,
                                box(
                                  title = "Option A",
                                  "Description of Option A goes here.",
                                  align = "center",
                                  footer = actionButton("gME", "Choose Option A",
                                                        icon = icon("check-circle"), width = "100%")
                                )
                         ),
                         column(4,
                                box(
                                  title = "Option B",
                                  "Description of Option B goes here.",
                                  align = "center",
                                  footer = actionButton("IE", "Choose Option B",
                                                        icon = icon("check-circle"), width = "100%")
                                )
                         ),
                         column(4,
                                box(
                                  title = "Option C",
                                  "Description of Option C goes here.",
                                  align = "center",
                                  footer = actionButton("IpredD", "Choose Option C",
                                                        icon = icon("check-circle"), width = "100%")
                                )
                         )
                       )
                )
              ),
              actionButton("clear", "Clear",
                           icon = icon("x"), width = "100%")
              ,
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
                 fluidRow(
                   column(4,
                          box(
                            title = "Option A",
                            "Description of Option A goes here.",
                            align = "center",
                            footer = actionButton("A1", "Choose Option A",
                                                  icon = icon("check-circle"), width = "100%")
                          )
                   ),
                   column(4,
                          box(
                            title = "Option B",
                            "Description of Option B goes here.",
                            align = "center",
                            footer = actionButton("A2", "Choose Option B",
                                                  icon = icon("check-circle"), width = "100%")
                          )
                   ),
                   column(4,
                          box(
                            title = "Option C",
                            "Description of Option C goes here.",
                            align = "center",
                            footer = actionButton("A3", "Choose Option C",
                                                  icon = icon("check-circle"), width = "100%")
                          )
                   )
                 )
          )
        ),
        actionButton("clear_assumptions", "Clear",
                     icon = icon("x"), width = "100%"),
        verbatimTextOutput("click_message_assumption")
      ),

      # Page 2 content
      tabItem(
        tabName = "page2",
        fluidRow(
          box(
            title = "Page 2 Content",
            "Enter a number for Page 2:",
            numericInput("input_page2", "Input:", value = 0)
          )
        )
      ),

      # Page 3 content
      tabItem(
        tabName = "page3",
        fluidRow(
          box(
            title = "Page 3 Content",
            "Select a date for Page 3:",
            dateInput("input_page3", "Input:", value = Sys.Date())
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Track which button was clicked
  button_clicked <- reactiveVal(NULL)

  # Observer to update which button was clicked
  observeEvent(input$gME, {
    button_clicked("gME")
  })

  observeEvent(input$IE, {
    button_clicked("IE")
  })

  observeEvent(input$IpredD, {
    button_clicked("IpredD")
  })

  observeEvent(input$clear, {
    button_clicked(NULL)
  })

  # Render the message based on the button clicked
  output$click_message <- renderPrint({
    if (!is.null(button_clicked())) {
      paste("Button", button_clicked(), "was clicked.")
    } else {
      "No button was clicked."
    }
  })

  # Track which button was clicked
  button_clicked_assumption <- reactiveVal(NULL)

  # Observer to update which button was clicked
  observeEvent(input$A1, {
    button_clicked_assumption("A1")
  })

  observeEvent(input$A2, {
    button_clicked_assumption("A2")
  })

  observeEvent(input$A3, {
    button_clicked_assumption("A3")
  })

  observeEvent(input$clear, {
    button_clicked_assumption(NULL)
  })

  # Render the message based on the button clicked
  output$click_message_assumption <- renderPrint({
    if (!is.null(button_clicked_assumption())) {
      paste("Button", button_clicked_assumption(), "was clicked.")
    } else {
      "No button was clicked."
    }
  })



}

# Run the application
shinyApp(ui = ui, server = server)
