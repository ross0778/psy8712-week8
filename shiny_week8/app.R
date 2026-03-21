setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shiny)
library(tidyverse)

shiny_tbl <- read.csv("week8_shiny.csv") %>% 
  mutate(timeEnd = ymd_hms(timeEnd),
         gender = factor(gender,
                         levels = c("Male", "Female")))

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Dropdown to filter by gender; default "All" to show everyone initially
  selectInput("gender", "Select Gender:",
              choices = c("All", "Male", "Female"),
              selected = "All"),
  # Radio buttons to show or hide the error band; shown by default per instructions
  radioButtons("error_band", "Error Band:",
               choices = c("Display Error Band" = "display",
                           "Suppress Error Band" = "suppress"),
               selected = "display"),
  # Checkbox to include participants before July 1 2017; included by default per instructions
  checkboxInput("include_early", "Include participants before July 1, 2017", value = TRUE),
  plotOutput("scatterplot")
)

server <- function(input, output) {
  # Reactive dataset that refilters whenever any of the three inputs change
  filter_tbl <- reactive({
    tbl <- shiny_tbl
    # Filter by gender only when a specific gender is chosen rather than All
    if (input$gender != "All") tbl <- tbl %>% filter(gender == input$gender)
    # Remove pre-July 2017 participants when checkbox is unchecked
    if (!input$include_early) tbl <- tbl %>% filter(timeEnd >= ymd("2017-07-01"))
    tbl
  })
  
  output$scatterplot <- renderPlot({
    # Build scatterplot using filtered data with purple OLS line matching the Rmd plot
    filter_tbl() %>%
      ggplot(aes(x = mq1q6, y = mq8q10)) +
      geom_point() +
      # se maps the radio button string to TRUE/FALSE to toggle the error band
      geom_smooth(method = "lm", color = "purple",
                  se = input$error_band == "display")
  })
}

shinyApp(ui, server)