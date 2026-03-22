library(shiny)
library(tidyverse)

#reads data and stores as a tibble
shiny_tbl <- read.csv("week8_shiny.csv") %>% 
  mutate(
    #converts timeEnd to POSIX format
    timeEnd = ymd_hms(timeEnd),
    #converts gender to a factor
         gender = factor(gender,
                         levels = c("Male", "Female")))

ui <- fluidPage(
  #makes dropdown menu with gender options, default All
  selectInput("gender", "Select Gender:",
              #makes the choices for gender
              choices = c("All", "Male", "Female"),
              #defaults to All
              selected = "All"),
  #creates the radio buttons for the error band display
  radioButtons("error_band", "Error Band:",
               #displays names to user while telling shiny what to use
               choices = c("Display Error Band" = "display",
                           "Suppress Error Band" = "suppress"),
               selected = "display"),
  #makes checkbox that allows users to include or exclude participants
  checkboxInput("include_early", "Include participants before July 1, 2017", value = TRUE),
  #makes room in the UI for the scatterplot
  plotOutput("scatterplot")
)

server <- function(input, output) {
  #used reactive() as it will rerun automatically when input changes
  filter_tbl <- reactive({
    #provides full dataset prior to any filters
    tbl <- shiny_tbl
    #filter by gender only when a specific gender is selected
    if (input$gender != "All") tbl <- tbl %>% filter(gender == input$gender)
    #excludes rows from before 7/17 only when checkbox is unchecked
    if (!input$include_early) tbl <- tbl %>% filter(timeEnd >= ymd("2017-07-01"))
    #returns the filtered tibble to be used by renderPlot
    tbl
  })
  
  output$scatterplot <- renderPlot({
    #builds scatterplot using filtered data
    filter_tbl() %>%
      #maps the x and y axes of scatterplot
      ggplot(aes(x = mq1q6, y = mq8q10)) +
      geom_point() +
      #adds the purple regression line
      geom_smooth(method = "lm", color = "purple",
                  #converts radio string to a logical show/hide band
                  se = input$error_band == "display")
  })
}
#combines the ui and server objects to launch the Shiny app
shinyApp(ui, server)
