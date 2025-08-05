# Load necessary libraries
#install.packages("data.table", "R.utils", "plotly", "shiny", "leaflet", "datasets", "ggplot2", "lubridate", "dplyr", "treemapify", "viridis", "shinydashboard", "ggiraph", "geojsonsf", "sf")
library(plotly)
library(shiny)
library(leaflet)
library(datasets)
library(ggplot2)
library(lubridate)
library(dplyr)
library(treemapify)
library(viridis)
library(shinydashboard)
library(ggiraph)
library(geojsonsf)
library(sf)
library(data.table)
library(R.utils)

# Define the URL of the CSV file
url <- "https://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2023-12-12/data/listings.csv.gz"

# Read the CSV file from the URL
df1 <- fread(url)

# Clean and preprocess the data
df1$price_numeric <- as.numeric(gsub("[\\$,]", "", df1$price))
df1 <- df1 %>%
  filter(!is.na(price_numeric))
df1$host_response_rate <- as.numeric(gsub("%", "", df1$host_response_rate))
names(df1)
# Calculate average price for each neighborhood
avg_price_neighborhood <- df1 %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg_price = mean(price_numeric, na.rm = TRUE))

# Merge average price back into original dataframe
df1 <- df1 %>%
  left_join(avg_price_neighborhood, by = "neighbourhood_cleansed")

# Remove outliers
Q1 <- quantile(df1$price_numeric, 0.25)
Q3 <- quantile(df1$price_numeric, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - (1.5 * IQR)
upper_bound <- Q3 + (1.5 * IQR)
df1 <- df1 %>%
  filter(price_numeric >= lower_bound & price_numeric <= upper_bound)
max(df1$price_numeric)
min(df1$price_numeric)

names(df1)

choices <- c("Review Score Rating" = "review_scores_rating",
             "Review Score Accuracy" = "review_scores_accuracy",
             "Review Score Cleanliness" = "review_scores_cleanliness",
             "Review Score Checkin" = "review_scores_checkin",
             "Review Score Communication" = "review_scores_communication",
             "Review Score Location" = "review_scores_location",
             "Review Score Value" = "review_scores_value")

# Calculate average price for each neighborhood
avg_price_neighborhood <- df1 %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg_price = mean(price_numeric, na.rm = TRUE))

# Load the JSON data from the URL
json_url <- "https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php?KAARTLAAG=INDELING_GEBIED&THEMA=gebiedsindeling"
geojson_data <- geojson_sf(json_url)

# Inspect the column names
dim(geojson_data)
names(avg_price_neighborhood)
unique(geojson_data$Gebied)

# Normalize function
normalize_name <- function(name) {
  name <- tolower(name)
  name <- gsub("[[:punct:]]", "", name)
  name <- gsub("\\s+", " ", name)
  return(name)
}

# Normalize the names in both datasets
avg_price_neighborhood$normalized_neighborhood <- sapply(avg_price_neighborhood$neighbourhood_cleansed, normalize_name)
geojson_data$geo_neighborhoods_normalized <- sapply(geojson_data$Gebied, normalize_name)
unique(avg_price_neighborhood$normalized_neighborhood)
unique(geojson_data$geo_neighborhoods_normalized)

# Create a mapping table if necessary
mapping_table <- data.frame(
  airbnb_neighborhood = c("oostelijk havengebied indische buurt",
                          "de baarsjes oudwest", "zuid", "de aker nieuw sloten",
                          "gaasperdam driemond", "gaasperdam driemond"),
  geo_neighborhood = c("indische buurt oostelijk havengebied", 
                       "oudwest de baarsjes", "oudzuid", "de aker sloten nieuwsloten",
                       "gaasperdam", "weesp driemond")
)
mapping_table
# Apply the mapping
avg_price_neighborhood$mapped_neighborhood <- avg_price_neighborhood$normalized_neighborhood
for (i in 1:nrow(mapping_table)) {
  avg_price_neighborhood$mapped_neighborhood[avg_price_neighborhood$normalized_neighborhood == mapping_table$airbnb_neighborhood[i]] <- mapping_table$geo_neighborhood[i]
}
# Adjust the join to ensure correct column names
geojson_data <- geojson_data %>%
  left_join(avg_price_neighborhood, by = c("geo_neighborhoods_normalized" = "mapped_neighborhood"))

# Remove rows with NA in avg_price
geojson_data <- geojson_data %>%
  filter(!is.na(avg_price))

# Check if the join worked correctly
head(geojson_data)

# Define UI
# UI
ui <- dashboardPage(
  dashboardHeader(title = "Airbnb Data Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Price Analysis", tabName = "listing"),
      menuItem("Host Analysis", tabName = "host"),
      menuItem("Data Sources", tabName = "sources")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        fluidRow(
          box(
            title = "Navigating Amsterdam's Airbnb Landscape: Insights for Travelers",
            width = 12,
            p("In the aftermath of the Covid-19 pandemic, Airbnb's resurgence 
            has significantly altered the landscape of tourist accommodations 
            in Europe, now comprising around 25% of the market (Pascoe, 2023). 
            This shift is especially notable in Amsterdam, where 75% of available 
            properties are entire homes, with an average price of €241 which is around $262 
            (BNR, Berg, 2023). As travelers navigate this changing market, finding 
            well-priced accommodations is essential. This report explores different 
            listing's factors that could cause the variations in pricing and offer 
            insights into a good Aribnb hosts next time you plan to choose an 
            Airbnb for your trip, navigating the city's dynamic lodging scene."),
            plotlyOutput("vis1_plot", height = "600px"),
            p("As an avid traveler planning a future trip to Amsterdam, I find the
              appeal of Airbnb accommodations compelling. Striking a balance 
              between affordability and quality is crucial, and Airbnb provides 
              a promising option. In addition to budget concerns, Airbnb aligns 
              with my desire to support local communities and to find hospitable 
              hosts who can enrich my travel experience with their guidance and 
              assistance (Airbnb, 2020)."),
            p("Research Questions:"),
            p("• What factors drive pricing variations in Amsterdam's Airbnb listings?"),
            p("• What attributes characterize successful Airbnb hosts (e.g. responsiveness, listing quality)?")
          )
        )
      ),
      tabItem(
        tabName = "listing",
        fluidRow(
          box(
            title = "Factors that Cause Price Variations of Airbnb listings in Amsterdam",
            width = 8,
            p("Several factors contribute to the price variations of Airbnb 
              accommodations in Amsterdam, including the accommodation capacity, 
              the number of beds and baths, and the neighborhood where the property 
              is located. Let's look at each variable to see how it impacts the price variations.")),
          box(
            title = "Price Range Slider",
            width = 4,
            sliderInput("priceRange", "Price Range:",
                        min = min(df1$price_numeric), 
                        max = max(df1$price_numeric), 
                        value = c(min(df1$price_numeric), max(df1$price_numeric)))
          ),
          box(
            title = "Entire home/apartment is the most common room type but it's also more expensive",
            width = 6,
            p("This histogram of price color coded by room type shows the ditribution 
              of price. The price are mostly in the range from $100 to $350 in general.
              However, the distribution of price for each room type are slightly 
              different. Most expensive but also most common room type is Entire 
              home or apartment. These can range from $40 a night up to 530 a night. 
              The second most common room type is private room offered at $30 to 
              $500 a night but the most common price range is around $60 to $160 
              a night. Hotel room is the least common room type and it is also 
              on the lower range of price from $50 to $350. Shared room are most 
              often offered for cheaper price under $80 but there are options up 
              to $500. "),
            plotlyOutput("price_histogram")
          ),
          box(
            title = "If you don't mind sharing your bathrooms with others, you can rent for cheaper!",
            p("The heat map shows the price distribution according to number of 
              beds and bathrooms. it can be observed from the map that there is 
              a positive relationship between number of beds and bath and price. 
              This means the more beds and baths, the more expensive the property 
              is. While you can rent an Airbnb with up to 20 beds, the most bathrooms 
              you can have is 5. Most properties with one bathroom have the price range of less 
              than $100 to around $200 compared to two beds at around $200-$300 
              a night. It can also be observed from the graph that properties 
              with shared bathrooms are cheaper and also less common than ones 
              with private bathrooms."),
            girafeOutput("vis4_plot", height = 400),
          )),
        fluidRow(
          box(
            title = "Scatter Plot of Price and Accommodates",
            p("This scatter plot shows a positive relationship between price and 
              accommodation capacity. This means the more people the property can 
              accommodate, the more expensive it is. This trend, however, is 
              stronger in properties with smaller accommodation capacity. 
              This phenomenon can be explained by couple of reasons. Amsterdam 
              has a diverse range of tourists, but the demand for extremely large 
              accommodations is likely lower. Most travelers visit in small groups, 
              couples, or as individuals, so the market for very large Airbnbs 
              is limited. Secondly, in a city like Amsterdam, location and 
              amenities can significantly impact pricing. A well-located smaller 
              apartment with high-end amenities might fetch a higher price than 
              a larger but less well-equipped or less centrally located property."),
            plotOutput("scatter_plot")
          ),
          box(
            title = "Top 5 Most Popular Neighborhoods for Airbnbs in Amsterdam",
            width = 6,
            p("This graph shows the top 5 most popular neighborhoods with their 
              average price. These neighboroods are either in the city center or
              in areas surrounding the city center. These locations are widely 
              sought after by tourists, thus opening up opportunities for Airbnbs 
              for locals in Amsterdam. The district with the most Airbnb listing is De 
              Baarsjes - Oud-West with over 1390 listings. Centrum West, Centrum 
              Oost are the city center neighborhoods, thus making them the most sought after 
              locations for Airbnb accomodation. Westerpark is located at the upper 
              west side of centrum with more than 558 Airbnb listings. Lastly, the most expensive neighborhood to 
              stay at is  De Pijp, Rivierenbuurt with 910 listings and their average price at $240."),
            plotOutput("vis2_plot", width = "100%")
          ),
          box(
            title = "De Pijp, Rivierenbuurt is the most expensive neighborhood for Airbnb renters, not the city centre",
            width = 12,
            p("On the contrary to the common belief that most expensive airbnb 
              would locate in the city centre, De Pijp, Rivierenbuurt is the most 
              expensive neighborhood with their average price at $240 a night.
              There are some of the factors that can conrtribute to this phenomenon.
              Firstly, De Pijp and Rivierenbuurt are known for their vibrant, trendy 
              atmospheres. De Pijp, in particular, is famous for its bustling 
              markets (such as Albert Cuyp Market), diverse restaurants, cafes, and 
              nightlife, making it highly desirable for tourists seeking an 
              authentic, lively experience. Another reason can be that these 
              neighborhoods offer a more residential feel compared to the highly 
              tourist-centric city center. Many visitors prefer staying in areas 
              that offer a local, less touristy vibe while still being close to 
              major attractions."),
            leafletOutput("vis7_map", width = "100%", height = 600)
          )
        )
      ),
      tabItem(
        tabName = "host",
        fluidRow(
          box(
            title = "Hosts can make a difference in your Airbnb and trip experience",
            width = 8,
            height = 300,
            p("When searching for an Airbnb in Amsterdam, it's essential to find
              a good host to enhance your overall experience. A good host can 
              provide valuable local insights, recommendations, and personalized 
              touches that make your stay memorable. They can offer assistance 
              and support throughout your trip, ensuring a smooth and enjoyable 
              visit. By choosing a reputable host, you not only gain access to 
              comfortable accommodations but also tap into a wealth of knowledge 
              about the city, its culture, and hidden gems. To find a good host, 
              it's helpful to read reviews from previous guests on the Airbnb 
              platform, paying attention to comments about responsiveness, 
              cleanliness, friendliness, and helpfulness. Additionally, hosts 
              with Superhost status, indicating a track record of excellent 
              service, can be a reliable choice. Researching the host's profile,
              including their response rate and verified identity, can also offer
              valuable insights into their reliability and commitment to 
              hospitality.")
            ),
          box(
            title = "Superhost Filter",
            width = 4,
            height = 150,
            checkboxGroupInput("hostFilter", "Filter Hosts:",
                               choices = c("Superhosts", "Non-Superhosts"),
                               selected = c("Superhosts", "Non-Superhosts"))
          ),
          box(
            title = "Select Review Score:",
            width = 4,
            height = 150,
            selectInput("reviewScore", "Select Review Score:",
                        choices = choices)
          )),
        fluidRow(
          box(
            title = "Superhosts tend to have better review score than non-superhosts in general",
            p("Successful host tend to have good review rating scores for their 
              listings. In fact, listings under superhosts always have higher 
              percentage of excellent rating of more than 4.8/5.0. The most 
              contrasting review score catergory between two host type is review 
              score for cleanliness. While there are  71.52 % of listings from 
              Superhosts have review score over 4.8 stars, only around half of 
              the listings (49.65 %) by non-superhosts are scored 4.8 and over."),
            girafeOutput("vis5_plot", height = 400)
          ),
          box(
            title = "If a Superhost has not responded to you after a few days, chances are they never will",
            p("Response rate and response time are important indicators of a 
              successful Airbnb host because they directly reflect the host's 
              communication and attentiveness to guests' needs. In general, most 
              hosts who response within a day or earlier will have more than 50% 
              response rate. For superhosts, if it is within an hour, 75%-100% 
              they will always response to you. On the other hand, if they haven't responded back 
              to you after couple of days, it is likely that you would not 
              receive any response. Surprsingly, this trend is even more prevalent 
              among Supehosts with less than 25% response rate"),
            plotOutput("vis9_plot")
          ),
          box(
            title = "Response Rate and Value Rating vs Price",
            width = 12,
            p("This graph is to provide a different point of view to look into 
              response rate and Review score for value. The graphs clearly show 
              that while superhosts tend to have more uniform good attributes, 
              in this case response rate and value rating, non-superhosts tends 
              to have more variations of their listing qualities and response 
              rate. Therefore, it can be concluded that superhosts is a good 
              indicator of a successful host."),
            plotOutput("vis6_plot")
        ))
      ),
tabItem(
  tabName = "sources",
  fluidRow(
    box(
      title = "Data Sources",
      width = 12,
      p(tags$b("Listings:")),
      p(HTML(
        "The dataset for analyzing Airbnb listings in Amsterdam spans from 
        <i>2009 to 2023</i> and is available in a <span style='color:blue;'>CSV file</span>. This extensive dataset 
        contains <b>8,739 rows</b> and <b>75 columns</b>, providing detailed information on 
        all Airbnb listings in Amsterdam. It includes comprehensive details 
        about listings and hosts. Listing attributes encompass property types,
        room setups, amenities, prices, and availability. Review information 
        such as review counts, ratings, and dates is also included. Additionally, 
        the dataset covers licensing statuses and other details like availability 
        metrics and calendar updates. Overall, it is a rich resource for 
        understanding the dynamics of Airbnb in Amsterdam. The attributes 
        include both nominal and numerical data types, allowing for in-depth 
        analysis of various factors influencing the short-term rental market 
        in the city. The temporal information in the dataset extends up to 
        <b>December 12, 2023</b>, offering insights into the trends of Airbnb listings 
        in Amsterdam over the past decade."
      )),
      p("Direct URL: ", tags$a(href = "https://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2023-12-12/data/listings.csv.gz", "https://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2023-12-12/data/listings.csv.gz")),
      p(tags$b("Geospatial data of Amsterdam districts (neighborhood):")),
      p(HTML(
        "This is a comprehensive geospatial dataset initially provided in 
        <span style='color:green;'>JSON format</span> in April 2022. Through data wrangling, I restructured this dataset 
        into a Simple Feature Collection (<i>sf object</i>) to facilitate analysis 
        and visualization. This refined dataset includes various attributes 
        that describe each neighborhood and their corresponding geometries. 
        Key fields in the dataset comprise unique neighborhood codes 
        (<b>Gebiedcode</b>), neighborhood names (<b>Gebied</b>), district codes (<b>Stadsdeelcode</b>), 
        district names (<b>Stadsdeel</b>), and the area of each neighborhood in 
        square meters (<b>Oppervlakte_m2</b>). The 'geometry' field includes 
        polygon geometries in the WGS 84 coordinate reference system, 
        defining the geographical boundaries necessary for visual mapping. 
        By leveraging this detailed geospatial data, the project aims to 
        visually analyze and present price variations across different 
        neighborhoods, providing valuable insights into the spatial 
        distribution of average prices."
      )),
      p("Direct URL: ", tags$a(href = "https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php?KAARTLAAG=INDELING_GEBIED&THEMA=gebiedsindeling", "https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php?KAARTLAAG=INDELING_GEBIED&THEMA=gebiedsindeling"))
    )
  )
))))

# Define server
server <- function(input, output) {
  
  # Reactive expression to filter data based on the price range slider input
  filtered_data <- reactive({
    df1 %>%
      filter(price_numeric >= input$priceRange[1], price_numeric <= input$priceRange[2])
  })
  
  output$vis1_plot <- renderPlotly({
    df_filtered <- df1
    
    df_filtered$first_review <- as.Date(df_filtered$first_review, format = "%Y-%m-%d")
    df_filtered$first_review_year <- year(df_filtered$first_review)
    
    vis1_data <- df_filtered %>%
      group_by(first_review_year) %>%
      summarize(count = n(),
                avg_price = mean(price_numeric, na.rm = TRUE))
    
    p <- ggplot(vis1_data, aes(x = first_review_year)) +
      geom_line(aes(y = count, color = "Count of Listings"), size = 1) +
      geom_point(aes(y = count, color = "Count of Listings", text = paste("Year:", first_review_year, "<br>Count:", count))) +
      geom_line(aes(y = avg_price * 10, color = "Average Price (x10)"), size = 1) +
      geom_point(aes(y = avg_price * 10, color = "Average Price (x10)", text = paste("Year:", first_review_year, "<br>Avg Price:", round(avg_price, 2)))) +
      scale_y_continuous(
        name = "Count of Listings",
        sec.axis = sec_axis(~./10, name = "Average Price")
      ) +
      scale_color_manual(
        name = "Legend",
        values = c("Count of Listings" = "#f1a226", "Average Price (x10)" = "#298c8c")
      ) +
      labs(title = "Annual Count of Airbnb Listings and Average Price per Year",
           x = "Year") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$price_histogram <- renderPlotly({
    filtered_data <- df1 %>%
      filter(price_numeric >= input$priceRange[1] & price_numeric <= input$priceRange[2])
    
    p <- ggplot(filtered_data, aes(x = price_numeric, fill = room_type)) +
      geom_histogram(binwidth = 10, color = "white", position = "stack") +
      labs(title = "Price Distribution by Room Type", x = "Price", y = "Count", fill = "Room Type") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  ## tree map of neighborhood
  output$vis2_plot <- renderPlot({
    df_filtered <- filtered_data()
    
    vis2_data <- df_filtered %>%
      group_by(neighbourhood_cleansed) %>%
      summarize(
        mean_price = mean(price_numeric, na.rm = TRUE),
        count_id = n()
      ) %>%
      arrange(desc(count_id)) %>%
      head(5)
    
    vis2_data$neighbourhood_cleansed <- factor(vis2_data$neighbourhood_cleansed, levels = vis2_data$neighbourhood_cleansed)
    
    ggplot(vis2_data, aes(area = count_id, fill = neighbourhood_cleansed, 
                          label = paste(neighbourhood_cleansed, "\nCount: ", count_id, "\nAvg. Price: ", round(mean_price)))) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
      scale_fill_viridis_d() +
      theme_minimal() +
      theme(legend.position = "none")
  })
  ##############################################################################
  ## heat map of beds and baths
  output$vis4_plot <- renderGirafe({
    df_filtered <- filtered_data()
    vis4_data <- df_filtered %>%
      filter(!is.na(beds)) %>%
      group_by(beds, bathrooms_text) %>%
      summarize(mean_price = mean(price_numeric, na.rm = TRUE),
                count = n())
    
    vis4_data$beds <- factor(vis4_data$beds)
    vis4_data$bathrooms_text <- factor(vis4_data$bathrooms_text)
    
    p <- ggplot(vis4_data, aes(x = beds, y = bathrooms_text, fill = mean_price, tooltip = paste("Average Price:", round(mean_price, 2), "<br>Count:", count))) +
      geom_tile_interactive() +
      scale_fill_viridis() + 
      labs(title = "Average Price Heatmap according to Number of Beds and Bathsrooms",
           x = "Beds", y = "Baths", fill = "Average Price") +
      theme_minimal()
    
    girafe(code = print(p))
  })
  ### Pie chart of review scores
  output$vis5_plot <- renderGirafe({
    # Filter data based on selected review score and host filter
    filtered_data <- df1
    if ("Superhosts" %in% input$hostFilter && "Non-Superhosts" %in% input$hostFilter) {
      filtered_data <- filtered_data
    } else {
      if ("Superhosts" %in% input$hostFilter) {
        filtered_data <- filtered_data %>%
          filter(host_is_superhost == "t")
      }
      if ("Non-Superhosts" %in% input$hostFilter) {
        filtered_data <- filtered_data %>%
          filter(host_is_superhost == "f")
      }
    }
    
    # Create bins for review scores
    review_score_bins <- seq(0, 5, by = 0.2)
    bin_labels <- cut(filtered_data[[input$reviewScore]], review_score_bins, include.lowest = TRUE)
    
    # Convert bin labels to factor and add as a new column
    filtered_data$rating_category <- factor(bin_labels)
    
    # Create the data for the plot
    category_counts <- table(filtered_data$rating_category)
    vis5_data <- data.frame(
      category = names(category_counts),
      value = as.numeric(category_counts)
    )
    # Calculate percentage of listing count compared to total listing count
    total_listings <- nrow(filtered_data)
    vis5_data$percentage <- (vis5_data$value / total_listings) * 100
    
    # Find the top category
    top_category <- vis5_data %>% slice_max(value, n = 1)
    
    # Create pie chart
    vis5_plot <- ggplot(vis5_data, aes(x = "", y = value, fill = category,
                                       tooltip = paste("Review Score:", category, "\n",
                                                       "Listing Count:", value, "\n",
                                                       "Percentage:", round(percentage, 2), "%"))) +
      geom_bar_interactive(stat = "identity", width = 1, color = "white") +
      scale_fill_viridis_d() +
      coord_polar("y", start = 0) +
      geom_text_interactive(data = top_category, aes(label = paste("Review Score:", category, "\n",
                                                                   "Listing Count:", value, "\n",
                                                                   "Percentage:", round(percentage, 2), "%")),
                            size = 4, color = "black", position = position_stack(vjust = 0.5)) +
      theme_void() +
      theme(legend.position = "none")
    
    # Render girafe object
    girafe(code = print(vis5_plot)) %>%
      girafe_options(opts_hover(css = "fill: white;"))
    
  })
### scatterplot of value rating and response rate
  output$vis6_plot <- renderPlot({
    # Filter data based on selected review score and host filter
    filtered_data <- df1
    if ("Superhosts" %in% input$hostFilter && "Non-Superhosts" %in% input$hostFilter) {
      filtered_data <- filtered_data
    } else {
      if ("Superhosts" %in% input$hostFilter) {
        filtered_data <- filtered_data %>%
          filter(host_is_superhost == "t")
      }
      if ("Non-Superhosts" %in% input$hostFilter) {
        filtered_data <- filtered_data %>%
          filter(host_is_superhost == "f")
      }
    }
    vis6_data <- filtered_data %>%
      select(price_numeric, review_scores_rating, host_is_superhost, host_response_rate) %>%
      filter(!is.na(host_response_rate), !is.na(review_scores_rating)) %>%
      mutate(host_is_superhost = recode(host_is_superhost, `f` = "No", `t` = "Yes", .missing = "Unknown"))
    
    vis6_data_long <- rbind(
      data.frame(x = vis6_data$host_response_rate, y = vis6_data$price_numeric, type = "Response Rate", host_is_superhost = vis6_data$host_is_superhost),
      data.frame(x = vis6_data$review_scores_rating, y = vis6_data$price_numeric, type = "Value Rating", host_is_superhost = vis6_data$host_is_superhost)
    )
    
    ggplot(vis6_data_long, aes(x = x, y = y, color = host_is_superhost)) +
      geom_point() +
      facet_wrap(~type, scales = "free_x") +
      labs(title = "Response Rate and Value Rating vs Price", x = "Rate", y = "Price", color = "Superhost") +
      theme_minimal()
  })
  ##############################################################
  ### choropleth map of neighborhood
  output$vis7_map <- renderLeaflet({
    # Filter data based on selected price range
    vis7_data <- filtered_data()
    
    
    # Calculate range of avg_price excluding NA values
    avg_price_range <- range(geojson_data$avg_price, na.rm = TRUE)
    
    # Generate color bins
    n_colors <- 5
    color_pal <- colorBin("YlOrRd", domain = avg_price_range, bins = n_colors)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = geojson_data,
                  fillColor = ~color_pal(avg_price),
                  fillOpacity = 0.7,
                  color = "white",
                  weight = 1,
                  popup = ~paste("Neighborhood:", Gebied, "<br>Average Price:", round(avg_price, 2))) %>%
      addCircleMarkers(data = vis7_data,
                       lng = ~longitude,
                       lat = ~latitude,
                       popup = ~paste(
                         "<b>Neighborhood:</b>", neighbourhood_cleansed, "<br>",
                         "<b>Price:</b>", round(price_numeric, 2), "<br>"
                       ),
                       radius = 5,
                       fillOpacity = 0.7,
                       color = ~colorFactor(palette = "Set1", domain = vis7_data$neighbourhood_cleansed)(neighbourhood_cleansed),
                       clusterOptions = markerClusterOptions()) %>%
      addLegend(position = "bottomright",
                pal = color_pal,
                values = geojson_data$avg_price,
                title = "Average Price",
                opacity = 0.7)
  })
 ######################################################################
  ### Violin chart of reponse rate and response time
  output$vis9_plot <- renderPlot({
    # Filter data based on selected review score and host filter
    filtered_data <- df1
    if ("Superhosts" %in% input$hostFilter && "Non-Superhosts" %in% input$hostFilter) {
      filtered_data <- filtered_data
    } else {
      if ("Superhosts" %in% input$hostFilter) {
        filtered_data <- filtered_data %>%
          filter(host_is_superhost == "t")
      }
      if ("Non-Superhosts" %in% input$hostFilter) {
        filtered_data <- filtered_data %>%
          filter(host_is_superhost == "f")
      }
    }
    vis9_data <- data.frame(response_rate = as.integer(filtered_data$host_response_rate), response_time = factor(filtered_data$host_response_time))
    vis9_data
    colSums(is.na(vis9_data))
    vis9_data <- vis9_data %>%
      filter(!is.na(response_rate))
    vis9_data
    ggplot(vis9_data, aes(x = response_time, y = response_rate, fill = response_time)) +
      geom_violin() +
      labs(title = "Relationship between Response Time and Response Rate", x = "Response Time", y = "Response Rate", fill = "Response Time") +
      theme_minimal()
  })
  #################################################################
  ### Scatter plot of price and accommodation capaity
  output$scatter_plot <- renderPlot({
    df_filtered <- filtered_data()
    scatter_plot <- ggplot(df_filtered, aes(x = accommodates, y = price_numeric)) +
      geom_point(color = "#1a80bb") +                  # Scatter plot
      geom_smooth(method = "lm", color = "#ea801c") +    # Add regression line
      labs(title = "Scatter Plot of Price and Accommodates",
           x = "Accommodates",
           y = "Price")
    
    # Print the plot
    print(scatter_plot)
  })
}

# Run the application
shinyApp(ui = ui, server = server)