

library(shiny)
library(tidyverse)
library(plotly)
library(sf)
library(dplyr)
library(leaflet)
library(rsconnect)

data = read.csv("clean_abortion_data.csv")
data$STATE <- stringr::str_to_title(data$statefip1)

data_clean <- data |>
  group_by(STATE) |>
  summarise(
    total_births = sum(NCHILD, na.rm = TRUE),
    n_obs = n(),
    teen_birth_rate = total_births/n_obs * 1000,
    mean_total_income = mean(INCTOT, na.rm = TRUE),
    mean_wage_income = mean(INCWAGE, na.rm = T),
    mean_age = mean(AGE, na.rm = TRUE),
    ban = max(Abortion.Ban, na.rm = T)
  )

data_clean$STATE <- recode(
  data_clean$STATE,
  "Flordia" = "Florida"
)

states = st_read("us_states_contiguous/states_contiguous.shp") |>
  st_transform(4326)


state_join = left_join(states, data_clean, by = "STATE")

pal <- colorFactor(
  palette = c("blue", "red"),
  domain = state_join$ban
)

# Define UI to make a map of US states
ui <- fluidPage(
  
  titlePanel("US Map - Teen Births"),
  
  fluidRow(
    column(
      4,
      # Make side bar
      tags$div(
        style = "padding:10px; background-color:#f8f9fa; border:1px solid #ddd; border-radius:5px;",
        
        h4("How to use this map"),
        p("Click on a state to view summary statistics."),
        p("Blue = Protected states"),
        p("Red = Ban/hostile states")
      ),
      
      h3("State Summary"),
      verbatimTextOutput("state_info")
    ),
    
    column(
      8,
      leafletOutput("map", height = 600)
    )
  )
)

server <- function(input, output, session) {
  
  # Render map
  output$map <- renderLeaflet({
    
    leaflet(state_join) |>
      addTiles() |>
      addPolygons(
        layerId = ~STATE,
        fillColor = ~pal(ban),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~STATE
      )
  })
  
  # Click event
  observeEvent(input$map_shape_click, {
    
    clicked_state <- input$map_shape_click$id
    
    selected <- state_join |>
      filter(STATE == clicked_state)
    
    output$state_info <- renderPrint({
      #Display stats
      cat(
        "State: ", selected$STATE, "(", selected$ABBREV, ") \n",
        "Total Teen Births:", format(selected$total_births, big.mark = ","), "\n",
        "Total Teens (Observations):", format(selected$n_obs, big.mark = ","), "\n",
        "Birth rate per 100 Teens: ", paste0(round(selected$teen_birth_rate, 2), "%"), "\n",
        "Mean Total Family Income: $", round(selected$mean_total_income, 2), "\n",
        "Mean Family Wage Income: $", round(selected$mean_wage_income, 2), "\n",
        "Mean Age:", round(selected$mean_age, 2), "\n"
      )
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# deploy app
rsconnect::deployApp()
