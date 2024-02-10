# Catcha Generating tool:

# New Colorful Version with Background

library(shiny)
library(stringi)

# Function to generate captcha text
generate_captcha_text <- function(num_chars = 4) {
  paste0(sample(LETTERS, num_chars, replace = TRUE), collapse = "")
}

# UI
ui <- fluidPage(
  titlePanel("Captcha Generator Group 09 by Srikar, Dwijesh & Sweta"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_chars", label = "Number of characters:", value = 4, min = 1, max = 10),
      actionButton("generate_button", "Generate Captcha"),
      selectInput("font_size", label = "Font Size:", choices = c("Small", "Medium", "Large"), selected = "Medium"),
      selectInput("font_weight", label = "Font Weight:", choices = c("Normal", "Bold"), selected = "Normal"),
      selectInput("font_color", label = "Font Color:", choices = c("Black", "Red", "Blue", "Green"), selected = "Black"),
      colourInput("bg_color", label = "Background Color:", value = "#FFFFFF"),
      selectInput("font_family", label = "Font Family:", choices = c("Arial", "Verdana", "Times New Roman"), selected = "Arial"),
      textOutput("captcha_text")
    ),
    mainPanel(
      uiOutput("captcha_image")
    )
  )
)

# Server
server <- function(input, output) {
  captcha_text <- reactiveVal(NULL)
  observeEvent(input$generate_button, {
    captcha_text(generate_captcha_text(input$num_chars))
  })
  output$captcha_text <- renderText({
    if (!is.null(captcha_text())) {
      paste("Captcha Text:", captcha_text())
    }
  })
  output$captcha_image <- renderUI({
    if (!is.null(captcha_text())) {
      font_size <- switch(input$font_size, "Small" = "16px", "Medium" = "24px", "Large" = "32px")
      font_weight <- ifelse(input$font_weight == "Bold", "bold", "normal")
      font_color <- input$font_color
      bg_color <- input$bg_color
      font_family <- input$font_family
      div(
        style = sprintf("padding: 10px; background-color: %s; display: inline-block; font-size: %s; font-weight: %s; color: %s; text-align: center; font-family: '%s';",
                        bg_color, font_size, font_weight, font_color, font_family),
        captcha_text()
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
