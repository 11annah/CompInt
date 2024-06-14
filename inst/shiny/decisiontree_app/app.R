library(shiny)
library(shinydashboard)

source("User_Choices.R")

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
      id = "tabs",
      # First group of pages
      menuItem("Upload your model(s)", tabName = "upload_page", icon = icon("upload")),
      menuItem("What quantitiy do you want?", tabName = "page0", icon = icon("gear")),
      menuItem("Let's specify it",
        tabName = "group1", icon = icon("gears"), startExpanded = TRUE,
        menuSubItem("Choose an assumption", tabName = "page1"),
        menuSubItem("Choose distribution(s)", tabName = "page2"),
        menuSubItem("Additional choices (optional)", tabName = "page3")
      ),
      menuItem("Get your interpretation!", tabName = "page4", icon = icon("globe")),
      menuItem("Download your results", tabName = "download_page", icon = icon("download"))
    )
  ),

  # Dashboard body
  dashboardBody(
    tags$head(tags$script(src = "script.js")),
    # Body content (main panel)
    tabItems(
      tabItem(
        tabName = "upload_page",
        fluidPage(column(
          width = 12,
          h3("Placeholder for Upload Content")
        ))
      ),
      tabItem(
        tabName = "page0",
        click_choices_ui("quantity", "Choose Your Quantity", "Please review the following options and select your preferred quantity.",
          noptions = 3,
          names = c("Option A", "Option B", "Option C"),
          descriptions = c("Description of Option A goes here.", "Description of Option B goes here.", "Description of Option C goes here."),
          tags = c("gME", "IE", "IpredD")
        )
      ),
      # Page 1 content
      tabItem(
        tabName = "page1",
        click_choices_ui("assumption", "Choose Your Assumption", "Please review the following options and select your preferred assumption.",
          noptions = 3,
          names = c("Option A", "Option B", "Option C"),
          descriptions = c("Description of Option A goes here.", "Description of Option B goes here.", "Description of Option C goes here."),
          tags = c("A1", "A2", "A3")
        )
      ),
      # Page 2 content
      tabItem(
        tabName = "page2",
        click_choices_ui("distribution", "Choose Your Distribution", "Please review the following options and select your preferred distribution.",
          noptions = 3,
          names = c("Option A", "Option B", "Option C"),
          descriptions = c("Description of Option A goes here.", "Description of Option B goes here.", "Description of Option C goes here."),
          tags = c("B1", "B2", "B3")
        )
      ),
      # Page 3 content
      tabItem(
        tabName = "page3",
        fluidRow(
          column(
            12,
            h2("Choose Your Choices"),
            p("Please review the following options and select your preferred choices"),
            br(),
            box(
              title = "Option A",
              "Description of Option A goes here.",
              align = "center",
              footer = checkboxInput("C1", "Choose Option A", value = FALSE, width = "33%"),
              width = 3
            ),
            box(
              title = "Option B",
              "Description of Option B goes here.",
              align = "center",
              footer = checkboxInput("C2", "Choose Option B", value = FALSE, width = "33%"),
              width = 3
            ),
            box(
              title = "Option C",
              "Description of Option C goes here.",
              align = "center",
              footer = checkboxInput("C3", "Choose Option C", value = FALSE, width = "33%"),
              width = 3
            ),
            box(
              title = "Option D",
              "Description of Option D goes here.",
              align = "center",
              footer = checkboxInput("C4", "Choose Option D", value = FALSE, width = "33%"),
              width = 3
            )
          )
        )
      ),
      # Page 4 content
      tabItem(
        tabName = "page4",
        fluidRow(
          column(
            12,
            h2("Result"),
            HTML("<button id='D1' style='width:100%;' type='button' class='btn btn-default action-button' onclick='create_result()'>Get result</button>"),
            box(
              actionButton("D2", "Quantity: ", width = "100%", style = "color: #FF0000; background-color: #F8F8F8; border-color: #FF0000"),
              actionButton("D3", "Assumption: ", width = "100%", style = "color: #0000FF; background-color: #F8F8F8; border-color: #0000FF"),
              actionButton("D4", "Distribution: ", width = "100%", style = "color: #008000; background-color: #F8F8F8; border-color: #008000"),
              actionButton("D5", "Choice 1:", width = "100%", style = "color: #FFBF00; background-color: #F8F8F8; border-color: #FFBF00"),
              actionButton("D6", "Choice 2:", width = "100%", style = "color: #FFD700; background-color: #F8F8F8; border-color: #FFD700"),
              actionButton("D7", "Choice 3:", width = "100%", style = "color: #C2B280; background-color: #F8F8F8; border-color: #C2B280"),
              actionButton("D8", "Choice 4:", width = "100%", style = "color: #B4C424; background-color: #F8F8F8; border-color: #B4C424"),
              width = 3
            ),
            box(p(id = "result", "Please click on the top button for result."),
              width = 9
            )
          )
        )
      ),
      tabItem(
        tabName = "download_page",
        fluidPage(column(
          width = 12,
          h3("Placeholder for Download Content")
        ))
      )
    )
  )
)

server <- function(input, output, session) {
  display_choice("quantity", c("gME", "IE", "IpredD"))
  display_choice("assumption", c("A1", "A2", "A3"))
  display_choice("distribution", c("B1", "B2", "B3"))

  # Track which button was clicked (result)
  observeEvent(input$D2, {
    updateTabItems(session, "tabs", "page0")
  })
  observeEvent(input$D3, {
    updateTabItems(session, "tabs", "page1")
  })
  observeEvent(input$D4, {
    updateTabItems(session, "tabs", "page2")
  })
  observeEvent(input$D5, {
    updateTabItems(session, "tabs", "page3")
  })
  observeEvent(input$D6, {
    updateTabItems(session, "tabs", "page3")
  })
  observeEvent(input$D7, {
    updateTabItems(session, "tabs", "page3")
  })
  observeEvent(input$D8, {
    updateTabItems(session, "tabs", "page3")
  })
}

shinyApp(ui = ui, server = server)
