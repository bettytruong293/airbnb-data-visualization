library(shiny)
library(leaflet)
library(datasets)
library(ggplot2)

setwd("~/Study/Visualization with R, D3 and Tableau/PE2 R Shiny") 

df <- read.csv("Victoria_Accident_Data_FIT5147S12024PE2.csv", header=T)
############################################
### VIS 1 ####
head(df) 
names(df)
str(df)
vis1_data <- aggregate(ACCIDENT_NO ~ LIGHT_CONDITION_DESC + SPEED_ZONE, data = df, FUN = length)
head(vis1_data) 
dim(vis1_data)

vis1_data$LIGHT_CONDITION_DESC <- factor(vis1_data$LIGHT_CONDITION_DESC)
vis1_data$SPEED_ZONE <- factor(vis1_data$SPEED_ZONE)
str(vis1_data)
vis1_plot <- ggplot(vis1_data, aes(x = LIGHT_CONDITION_DESC, y = ACCIDENT_NO, fill = SPEED_ZONE)) +
  geom_bar(stat = "identity") + labs(title = "Number of Accidents by Light Condition and Speed Zone",
                                     x = "Light Condition",
                                     y = "Number of Accidents",
                                     fill = "Speed Zone")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  guides(fill = guide_legend(title = "Speed Zone")) + # Add legend title 
  scale_fill_brewer(palette = "RdYlBu", direction = -1)
vis1_plot
############################################
### VIS 2
top_speed_zones <- names(sort(table(vis1_data$SPEED_ZONE), decreasing = TRUE))[1:4]
top_speed_zones
vis2_data <- df[df$SPEED_ZONE %in% top_speed_zones, ]
dim(vis2_data)
# Aggregate data by hour and speed zone for VIS 2
vis2_data$ACCIDENT_TIME <- as.POSIXct(vis2_data$ACCIDENT_TIME, format = "%H:%M:%S")
dim(vis2_data)
vis2_data$ACCIDENT_TIME <- as.integer(format(vis2_data$ACCIDENT_TIME, "%H"))
vis2_data$ACCIDENT_TIME <- factor(vis2_data$ACCIDENT_TIME)
dim(vis2_data)
vis2_data <- aggregate(ACCIDENT_NO ~ ACCIDENT_TIME + SPEED_ZONE, data = vis2_data, FUN = length)
vis2_data
vis2_data$ACCIDENT_TIME <- factor(vis2_data$ACCIDENT_TIME)
vis2_data$SPEED_ZONE <- factor(vis2_data$SPEED_ZONE)

plot1 <- ggplot(vis2_data, aes(x = ACCIDENT_TIME, y = ACCIDENT_NO, fill = SPEED_ZONE)) 
vis2_plot <- plot1 + geom_bar(aes(fill = SPEED_ZONE), stat = "identity") +
  labs(title = "Number of Accidents by Time of Day and Speed Zone",
         x = "Time of Day",
         y = "Number of Accidents",
         fill = "Speed Zone") +
  scale_fill_brewer() +
  theme_dark()
vis2_plot
############################################
### MAP

# daynight attribute based on light_condition_desc
df$daynight <- ifelse(df$LIGHT_CONDITION_DESC == "Day", "day",
                      ifelse(df$LIGHT_CONDITION_DESC == "Dusk/Dawn", "dusk/dawn", "night"))

############################################
# UI
ui <- fixedPage(
  fluidRow(
    column(12, h1("Accident Visualization Dashboard", align = "center"))
  ),
  fluidRow(
    column(6, plotOutput("vis1_plot"), textOutput("vis1_desc")),
    column(6, plotOutput("vis2_plot"), textOutput("vis2_desc"))
  ),
  fluidRow(
    column(3, uiOutput("daynight_filter")), 
    column(9, leafletOutput("map_daynight"))
  ),
  fluidRow(
    column(12, textOutput("map_desc"))
  ),
  fluidRow(
    column(12,
           textOutput("data_source"))  # Data source
  )
)

# server 
server <- function(input, output) {
  output$vis1_plot <- renderPlot({
    print(vis1_plot)
  })
  output$vis1_desc <- renderText({
    "
The depicted graph detailing accidents by light condition and speed zone reveals a significant prevalence of accidents transpiring during the day, surpassing 650 incidents. Notably, the most frequent speed zone associated with accidents is 100 km/h, indicating that a majority of daytime accidents occur on highways. Following closely, the second most common speed zone for accidents is 60 km/h, typical of residential and suburban areas.

Conversely, during nighttime, accidents are twice as likely to occur in areas lacking streetlights compared to illuminated areas. Intriguingly, accidents transpiring in poorly lit environments predominantly unfold on freeways with a 100 km/h speed limit."
  })
  output$vis2_plot <- renderPlot({
    print(vis2_plot)
  })
  output$vis2_desc <- renderText({
    "The presented bar graph illustrates the frequency of accidents across various hours and specific speed zones (50, 60, 80, 100). An analysis of the graph reveals that the majority of accidents occur during daytime, particularly between 8 am and 5 pm, with a peak observed at 4 pm. This surge in accidents during the late afternoon coincides with peak commuting hours, as individuals travel to and from work, thus accounting for the heightened accident rate during this time frame. Conversely, the early morning hours from 1 am to 4 am exhibit the lowest incidence of accidents.

Regarding speed zones, accidents are most prevalent within the 100 km/h speed zone, except for 8 PM, where accidents are evenly distributed among the four speed zones. Notably, 12 pm and 4 pm emerge as the peak hours for accidents within the 100 km/h speed zone, indicating heightened risk during these specific time intervals."
  })
  # Leaflet map
  output$map_daynight <- renderLeaflet({
    filtered_df <- df[df$daynight %in% input$daynight_filter, ]
    
    leaflet(data = filtered_df) %>%
      addProviderTiles("CartoDB.Positron") %>%  # Use CartoDB.Positron as the tile provider
      setView(lng = 145.465783, lat = -38.482461, zoom = 10) %>%  # Set the center coordinates and zoom level
      addCircleMarkers(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        popup = ~paste(
          "<b>Accident Date:</b>", ACCIDENT_DATE, "<br>",
          "<b>Accident Type:</b>", ACCIDENT_TYPE_DESC, "<br>",
          "<b>Light Condition:</b>", LIGHT_CONDITION_DESC, "<br>",
          "<b>Road Geometry:</b>", ROAD_GEOMETRY_DESC, "<br>",
          "<b>Speed Zone:</b>", SPEED_ZONE
        ),
        radius = 3,
        fillOpacity = 0.7,
        color = ~ifelse(daynight == "day", "green",
                        ifelse(daynight == "dusk/dawn", "orange", "blue"))
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("green", "orange", "blue"),
        labels = c("day", "dusk/dawn", "night"),
        title = "Day/Night"
      ) 
  })

  output$map_desc <- renderText({
    "
Upon analyzing the map, a conspicuous trend emerges: the majority of accidents manifest during daylight hours. The map distinctly delineates accident occurrences based on light conditions, discernible during daytime, at sunset or sunrise, and during nighttime. Notably, the preponderance of accidents unfolds during the day, particularly clustering along Phillip Island Road and other major highways leading to Phillip Island. Additionally, frequent accident hotspots include Wonthaggi town, Inverloch town, and specific sections of Phillip Island, notably along Thompson Avenue near Cowes. This clustering phenomenon can likely be attributed to the high traffic density, particularly among tourists traversing these locales.

Further examination reveals that within the speed zones of Phillip Island, ranging from 50 km/h to 80 km/h, accidents predominantly occur due to collisions with other vehicles. Accidents occurring at dawn or dusk exhibit a scattered distribution across Phillip Island, Bass Coast Shire, and Wonthaggi. Most of these incidents occur along the major highways in Bass Coast Shire, characterized by a 100 km/h speed limit, and on smaller roads within Phillip Island and Wonthaggi town, where speed limits range from 40 km/h to 80 km/h. Intriguingly, accidents in Phillip Island and Wonthaggi primarily occur at intersections, contrasting with those in Bass Coast Shire, which typically transpire elsewhere.

During nighttime, accidents primarily manifest on highways, particularly in areas devoid of streetlights. Common accident-prone roads include Lynnes Road, Bass Highway, and Phillip Island Road. Noteworthy accident sites include densely populated areas of Phillip Island and Wonthaggi, as well as Cape Woolamai. Interestingly, while accidents on Phillip Island primarily occur on illuminated streets, those on highways typically unfold in areas lacking streetlights. "
  })
  
  # checkbox input
  output$daynight_filter <- renderUI({
    checkboxGroupInput("daynight_filter", "Day/Night Filter", 
                       choices = c("day", "dusk/dawn", "night"), selected = c("day", "dusk/dawn", "night"))
  })
  output$data_source <- renderText({
    "Data source: Victoria_Accident_Data_FIT5147S12024PE2.csv"
  })
}

# Run the application
shinyApp(ui = ui, server = server)

