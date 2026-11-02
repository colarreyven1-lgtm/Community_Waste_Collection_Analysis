
library(shiny)
library(ggplot2)
library(dplyr)

# Load and prepare the sample dataset
waste <- read.csv("community_waste_collection.csv")
waste$Date <- as.Date(waste$Date)

ui <- fluidPage(
  titlePanel("Community Waste Collection Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "dates",
        "Date range:",
        start = min(waste$Date, na.rm = TRUE),
        end = max(waste$Date, na.rm = TRUE),
        min = min(waste$Date, na.rm = TRUE),
        max = max(waste$Date, na.rm = TRUE)
      ),
      
      selectInput(
        "zone",
        "Zone:",
        choices = c("All", sort(unique(waste$Zone))),
        selected = "All"
      ),
      
      selectInput(
        "area_type",
        "Area type:",
        choices = c("All", sort(unique(waste$AreaType))),
        selected = "All"
      )
    ),
    
    mainPanel(
      h3("Summary"),
      fluidRow(
        column(4, wellPanel(
          h4("Waste collected"),
          textOutput("total_collected")
        )),
        column(4, wellPanel(
          h4("Waste recycled"),
          textOutput("total_recycled")
        )),
        column(4, wellPanel(
          h4("Collection trips"),
          textOutput("total_trips")
        ))
      ),
      
      h3("Waste collected by zone"),
      plotOutput("zone_plot"),
      
      h3("Collected vs. recycled waste"),
      plotOutput("comparison_plot"),
      
      h3("Filtered records"),
      tableOutput("waste_table")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_waste <- reactive({
    data <- waste %>%
      filter(Date >= input$dates[1], Date <= input$dates[2])
    
    if (input$zone != "All") {
      data <- data %>% filter(Zone == input$zone)
    }
    
    if (input$area_type != "All") {
      data <- data %>% filter(AreaType == input$area_type)
    }
    
    data
  })
  
  output$total_collected <- renderText({
    data <- filtered_waste()
    paste0(
      format(sum(data$WasteCollected_kg, na.rm = TRUE), big.mark = ","),
      " kg"
    )
  })
  
  output$total_recycled <- renderText({
    data <- filtered_waste()
    paste0(
      format(sum(data$WasteRecycled_kg, na.rm = TRUE), big.mark = ","),
      " kg"
    )
  })
  
  output$total_trips <- renderText({
    sum(filtered_waste()$CollectionTrips, na.rm = TRUE)
  })
  
  output$zone_plot <- renderPlot({
    data <- filtered_waste()
    validate(need(nrow(data) > 0, "No records match these filters."))
    
    zone_data <- data %>%
      group_by(Zone) %>%
      summarise(
        Total_Collected_kg = sum(WasteCollected_kg, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(zone_data, aes(x = Zone, y = Total_Collected_kg)) +
      geom_col() +
      labs(x = "Zone", y = "Waste collected (kg)") +
      theme_minimal()
  })
  
  output$comparison_plot <- renderPlot({
    data <- filtered_waste()
    validate(need(nrow(data) > 0, "No records match these filters."))
    
    comparison_data <- data %>%
      summarise(
        `Collected` = sum(WasteCollected_kg, na.rm = TRUE),
        `Recycled` = sum(WasteRecycled_kg, na.rm = TRUE)
      ) %>%
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "Waste_Type",
        values_to = "Weight_kg"
      )
    
    ggplot(comparison_data, aes(x = Waste_Type, y = Weight_kg)) +
      geom_col() +
      labs(x = "Waste type", y = "Weight (kg)") +
      theme_minimal()
  })
  
  output$waste_table <- renderTable({
    filtered_waste()
  })
}

shinyApp(ui = ui, server = server)
