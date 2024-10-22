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
      sliderInput("streams_range", "Select a Range of Streams", min = min(data$streams), max = max(data$streams), value = c(min(data$streams), max(data$streams)))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    print(paste("Filtering data for year:", input$year))
    print(paste("Streams range:", input$streams_range))
    filtered <- data %>%
      filter(released_year == input$year,
             streams >= input$streams_range[1],
             streams <= input$streams_range[2])
    print(colnames(filtered))
    return(filtered)
  })
  
  output$plot <- renderPlot({
    print("Generating plot...")
    ggplot(filtered_data(), aes(x = valence_., y = energy_.)) +
      geom_point() +
      labs(x = "Valence", y = "Energy", title = "Valence vs Energy for Selected Songs")
  })
}

shinyApp(ui = ui, server = server)
