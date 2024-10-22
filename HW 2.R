library(shiny)
library(ggplot2)
library(dplyr)
data = read.csv("https://raw.githubusercontent.com/PeterBiiiqy/STAT-436-HW2/main/Spotify%20Most%20Streamed%20Songs.csv")

str(data)

if (is.character(data$streams)) {
  data$streams <- as.numeric(gsub(",", "", data$streams))
}
data <- data[!is.na(data$streams), ]


ui <- fluidPage(
  titlePanel("Spotify Most Streamed Songs Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select a Year", choices = unique(data$released_year)),
      sliderInput("streams_range", "Select a Range of Streams", min = min(data$streams), max = max(data$streams), value = c(min(data$streams), max(data$streams))),
      selectInput("display_type", "Select a Display Type", choices = c("Plot", "Table"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Table", verbatimTextOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(released_year == input$year,
             streams >= input$streams_range[1],
             streams <= input$streams_range[2])
  })
  
  output$plot <- renderPlot({
    ggplot(filtered_data(), aes(x = valence_., y = energy_.)) +
      geom_point() +
      labs(x = "Valence", y = "Energy", title = "Valence vs Energy for Selected Songs")
  })
  
  output$table <- renderPrint({
    summary(filtered_data())
  })
}

shinyApp(ui = ui, server = server)
