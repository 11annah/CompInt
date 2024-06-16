if (length(names) != length(descriptions)) {
  click_choices_ui <- function(id, title, subtitle, noptions, names, descriptions, tags) {
    stop("The length of names and descriptions does not match.")
  }
  if (length(names) != length(tags)) {
    stop("The length of names and tags does not match.")
  }
  if (length(names) != noptions) {
    stop("The length of names number of options does not match.")
  }

  ns <- NS(id)
  ui_elements <- list(
    h2(title),
    p(subtitle),
    fluidRow(
      lapply(1:noptions, function(i) {
        column(
          width = 4,
          div(
            class = "box",
            h3(names[i]),
            p(descriptions[i]),
            actionButton(ns(tags[i]), paste0("Choose ", names[i]), icon = icon("check-circle"), width = "100%")
          )
        )
      }),
      actionButton(ns("clear"), "Clear", icon = icon("x"), width = "100%"),
      verbatimTextOutput(ns("click_message"))
    )
  )
  do.call(tagList, ui_elements)
}


display_choice <- function(id, button_names) {
  moduleServer(id, function(input, output, session) {
    # Track which button was clicked (quantity)
    button_clicked <- reactiveVal(NULL)

    # Create observeEvents dynamically
    lapply(button_names, function(btn_name) {
      observeEvent(input[[btn_name]], {
        button_clicked(btn_name)
      })
    })

    observeEvent(input$clear, {
      button_clicked(NULL)
    })

    # Render the message based on the button clicked
    output$click_message <- renderText({
      if (!is.null(button_clicked())) {
        paste0("Button '", button_clicked(), "' was clicked.")
      } else {
        "No button was clicked."
      }
    })
  })
}
