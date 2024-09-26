library(tidyverse)
library(shiny)
library(bslib)
library(plotly)

d <- readr::read_csv(here::here("data/weather.csv"))

d_vars <- c("Average temp" = "temp_avg",
            "Min temp" = "temp_min",
            "Max temp" = "temp_max",
            "Total precip" = "precip",
            "Snow depth" = "snow",
            "Wind direction" = "wind_direction",
            "Wind speed" = "wind_speed",
            "Air pressure" = "air_press",
            "Total sunshine" = "total_sun")

ui <- page_sidebar(
    title = "Weather Data",
    sidebar = sidebar(
        selectInput(
            "region", "Select a region", 
            choices = c("West", "Midwest", "Northeast", "South")
        ),
        selectInput(
            "name", "Select an airport", choices = c()
        ),
        selectInput(
            "var", "Select a variable",
            choices = d_vars, selected = "temp_avg"
        )
    ),
    card(
        card_header(
            textOutput("title")
        ),
        card_body(
            plotlyOutput("plot")
        )
    ),
    uiOutput("valueboxes")
)

server <- function(input, output, session) {
    # Update the airport selection based on the selected region
    observe({
        req(input$region)  # Ensure region is selected
        updateSelectInput(
            session, "name",
            choices = d |>
                filter(region == input$region) |>
                distinct(name) |>
                pull(name)
        )
    })
    
    d_city <- reactive({
        req(input$name)  # Ensure name is selected
        d |>
            filter(name == input$name)
    })
    
    # Render the value boxes with temperature data
    output$valueboxes <- renderUI({
        req(d_city())  # Ensure data is available
        clean <- function(x) {
            round(x, 1) |> paste("°C")
        }
        
        layout_columns(
            value_box(
                title = "Average Temp",
                value = mean(d_city()$temp_avg, na.rm = TRUE) |> clean(),
                showcase = bsicons::bs_icon("thermometer-half"),
                theme = "success"
            ),
            value_box(
                title = "Minimum Temp",
                value = min(d_city()$temp_min, na.rm = TRUE) |> clean(),
                showcase = bsicons::bs_icon("thermometer-snow"),
                theme = "primary"
            ),
            value_box(
                title = "Maximum Temp",
                value = max(d_city()$temp_max, na.rm = TRUE) |> clean(),
                showcase = bsicons::bs_icon("thermometer-sun"),
                theme = "danger"
            )
        )
    })
    
    # Update the title based on the selected variable
    output$title <- renderText({
        names(d_vars)[d_vars == input$var]
    })
    
    # Render the plot with Plotly
    output$plot <- renderPlotly({
        req(d_city())  # Ensure data is available
        p <- d_city() |>
            ggplot(aes(x = date, y = .data[[input$var]])) +
            geom_line() +
            theme_minimal()
        ggplotly(p)
    })
}

shinyApp(ui = ui, server = server)

