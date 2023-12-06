# Load libraries
library(shiny)
library(tidyverse)
library(caret)
library(e1071)


# Load the dataset
weather <- read_csv("C:/Users/DELL/Downloads/New/R5-Termproject/weather.csv")

# Convert "Yes" and "No" to 1 and 0 in RainTomorrow
weather$RainTomorrow <- ifelse(weather$RainTomorrow == "Yes", 1, 0)

# Create a logistic regression model for rain prediction
model_rain <- glm(RainTomorrow ~ MinTemp + WindGustSpeed + Humidity3pm,
                  data = weather, family = "binomial")

# Create a binary column SnowTomorrow based on MinTemp
weather$SnowTomorrow <- ifelse(weather$MinTemp < 0, 1, 0)

# Create a logistic regression model for snow prediction
model_snow <- glm(SnowTomorrow ~ MinTemp + WindGustSpeed + Humidity3pm,
                  data = weather, family = "binomial")

# Define UI
ui <- fluidPage(
  titlePanel("Weather Prediction"),
  sidebarLayout(
    sidebarPanel(
      h3("Rain Prediction"),
      numericInput("min_temp_rain", "Minimum Temperature:", value = 0, min = -10, max = 30),
      numericInput("wind_speed_rain", "Wind Speed:", value = 20, min = 0, max = 100),
      numericInput("humidity_rain", "Humidity:", value = 50, min = 0, max = 100),
      actionButton("predict_rain_btn", "Predict Rain"),
      
      h3("Snow Prediction"),
      numericInput("min_temp_snow", "Minimum Temperature:", value = 0, min = -10, max = 30),
      numericInput("wind_speed_snow", "Wind Speed:", value = 20, min = 0, max = 100),
      numericInput("humidity_snow", "Humidity:", value = 50, min = 0, max = 100),
      actionButton("predict_snow_btn", "Predict Snow")
    ),
    mainPanel(
      h3("Weather Prediction Result:"),
      verbatimTextOutput("prediction_text_rain"),
      plotOutput("prediction_plot_rain"),
      verbatimTextOutput("prediction_text_snow"),
      plotOutput("prediction_plot_snow")
    )
  )
)


# ... (previous code)

# Define server
server <- function(input, output) {
  observeEvent(input$predict_rain_btn, {
    # Create a data frame with user input for rain prediction
    user_data_rain <- data.frame(
      MinTemp = input$min_temp_rain,
      WindGustSpeed = input$wind_speed_rain,
      Humidity3pm = input$humidity_rain
    )
    
    # Make predictions for rain
    prediction_rain <- predict(model_rain, newdata = user_data_rain, type = "response")
    
    # Display rain prediction result
    output$prediction_text_rain <- renderText({
      if (prediction_rain > 0.5) {
        "It will rain tomorrow."
      } else {
        "It will not rain tomorrow."
      }
    })
    
    # Display a probability plot for rain
    output$prediction_plot_rain <- renderPlot({
      data.frame(Probability = c(0, 0.25, 0.50, 0.75, 1)) %>%
        ggplot(aes(x = 1, y = Probability)) +
        geom_col(fill = "cornflowerblue") +
        ylim(0, 1) +
        labs(title = "Probability of Rain Tomorrow",
             x = NULL,
             y = "Probability")
    })
  })
  
  observeEvent(input$predict_snow_btn, {
    # Create a data frame with user input for snow prediction
    user_data_snow <- data.frame(
      MinTemp = input$min_temp_snow,
      WindGustSpeed = input$wind_speed_snow,
      Humidity3pm = input$humidity_snow
    )
    
    # Additional conditions for snow prediction
    snow_conditions <- user_data_snow$Humidity3pm >= 70 & user_data_snow$WindGustSpeed >= 10
    
    # Make predictions for snow only if conditions are met
    if (all(snow_conditions)) {
      prediction_snow <- predict(model_snow, newdata = user_data_snow, type = "response")
    } else {
      prediction_snow <- 0  # Default to not snowing if conditions are not met
    }
    
    # Display snow prediction result
    output$prediction_text_snow <- renderText({
      if (prediction_snow > 0.5) {
        "It will snow tomorrow."
      } else {
        "It will not snow tomorrow."
      }
    })
    
    # Display a probability plot for snow
    output$prediction_plot_snow <- renderPlot({
      data.frame(Probability = c(0, 0.25, 0.50, 0.75, 1)) %>%
        ggplot(aes(x = 1, y = Probability)) +
        geom_col(fill = "lightblue") +
        ylim(0, 1) +
        labs(title = "Probability of Snow Tomorrow",
             x = NULL,
             y = "Probability")
    })
  })
}



# Run the Shiny app
shinyApp(ui, server)
