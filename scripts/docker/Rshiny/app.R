pkgs=c('shiny', 'shinythemes', 'leaflet', 'sf', 'ggplot2', 'dplyr', 'lubridate', 'rnaturalearth', 'ggrepel', 'meteospain', 'sf')



#install.packages("rnaturalearthdata")
library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rnaturalearth)
library(ggrepel)
#library(dplyr)

# geopackage_path <- "C:/Users/yagmur/work/Spain/data/"
geopackage_path <- "./data"

years <- 2016:2022
sf_data <- purrr::map_dfr(years, function(year) {
  filename <- paste0("meteo_andalucia_", year, ".gpkg")
  filepath <- file.path(geopackage_path, filename)
  st_read(filepath)
})

# Select unique locations
#unique_locations <- sf_data %>% distinct(geom, station_name)
unique_locations <- sf_data %>%
  group_by(station_id, station_name) %>%
  slice(1) %>%
  ungroup() %>%
  select(station_id, station_name, geom)

# Prep Plot B: Map visualization 
# (Need to clean this code)
for (year in 2010:2022) {
  filename <- paste0("meteo_andalucia_", year, ".gpkg")
  filepath <- file.path(geopackage_path, filename)
  df_name <- paste0("df_", year)
  assign(df_name, st_read(filepath))
}

# Create a list with the names of all the dataframes
df_list <- list(df_2010, df_2011, df_2012, df_2013, df_2014, df_2015, df_2016, df_2017, df_2018, df_2019, df_2020, df_2021, df_2022)

# Merge the data frames using bind_rows
df_2010_2022 <- do.call(bind_rows, df_list)

#### Calculate mean per month of the year
# Convert the 'timestamp' column to a date format
df_2010_2022$timestamp <- as.Date(df_2010_2022$timestamp)

# Extract the month and year from the timestamp
df_2010_2022$year <- format(df_2010_2022$timestamp, "%Y")

# Group the data by year and calculate the mean precipitation
yearly_means_2010_2022 <- df_2010_2022 |> 
  group_by(year, station_name) |>
  summarise(mean_precipitation = mean(precipitation, na.rm = TRUE)) |>
  as.data.frame()

# Extract unique stations from df_2010_2022 and calculate mean precipitation for each of those stations
mean_precipitation <- df_2010_2022 |>
  group_by(station_name) |>
  summarize(mean_precipitation = mean(as.numeric(precipitation), na.rm = TRUE))

# Download country shapes
world <- ne_countries(scale = "medium", returnclass = "sf")

# Download ocean layer
ocean <- ne_download(category = "physical", scale = "medium", type = "ocean", returnclass = "sf")

# Add names of seas
ocean_names <- data.frame(
  name = c("Alboran Sea", "Gulf of Cádiz"),
  lon = c(-3.5, -7.8),
  lat = c(35.8, 36.0)
)

# Add point geometries for cities in the south of Spain / Portugal
spain_cities <- data.frame(
  state = c(rep("Andalusia", 3), rep("Algarve", 2)),
  city = c("Seville", "Malaga", "Cadiz", "Faro", "Portimao"),
  lat = c(37.3890924, 36.7212746, 36.5297706, 37.019355, 37.1360597),
  lon = c(-5.9844589, -4.4213988, -6.252567, -7.930440, -8.5379872)
)

spain_cities <- st_as_sf(spain_cities, coords = c("lon", "lat"), remove = FALSE, 
                         crs = 4326, agr = "constant")

# Function for plotting the precipitation map (plot B)
plot_precipitation_map <- function(year) {
  # Filter the data for the specified year
  yearly_means_filtered <- filter(yearly_means_2010_2022, year == year)
  
  # Create the ggplot with the filtered data
  plot <- ggplot(data = world) + 
    geom_sf() +
    geom_sf(data = ocean, fill = "lightblue") +
    geom_text(data = ocean_names, aes(x = lon, y = lat, label = name),
              color = "darkblue", size = 3, angle = c(15, -10)) +
    geom_sf(data = yearly_means_filtered$geom, aes(color = yearly_means_filtered$mean_precipitation), size = 2.5, shape = 19, alpha = 0.9) + 
    geom_sf(data = spain_cities, col = "red") +
    geom_text_repel(data = spain_cities, aes(x = lon, y = lat, label = city), 
                    fontface = "bold", nudge_x = c(-1, 0, -0.25, -1, 0), nudge_y = c(1, -0.75, -0.5, -0.5, 0)) +
    coord_sf(xlim = c(-9.248333, 0.2297222), ylim = c(34.285, 40.49611), expand = FALSE) +
    labs(color = "Mean Precipitation in L/m²") +
    ggtitle(paste("Meteorological stations in Andalusia, Spain: mean precipitation per station", year))
  
  return(plot)
}

### UI section

ui <- fluidPage(
  theme=shinytheme("paper"),
  
  navbar <- navbarPage(
    title = "Andalusia Living Lab",
    id = "navbar",
    
    # Page 1 "Home" content
    tabPanel(title = "Home", value = "tab1",
             
             mainPanel(
               img(src="logo.png", style = "height: 50%; width: 50%; margin: 1px 1px;"),
               h1("Welcome"),
               p("This webpage exhibits some first examples for the visualization of climate and meteorological data for Living Lab Andalusia in Spain 🇪🇸 in the I-CISK project."),
               p("Navigate through the top bar to explore the different visualizations.")
             )),
    
    # Page 2 "Plot A" content
    tabPanel( title = "Plot A", value = "tab2",
              
      mainPanel(
        fluidRow(
          column(width = 4,
                 selectInput("station", "Select a Station", choices = unique_locations$station_name),
                 checkboxGroupInput("years", "Select Years", choices = years),
                 checkboxGroupInput("metrics", "Select Metrics", choices = c("Mean", "Min", "Max")),
                 actionButton("compare", "Compare Temperature")
          ),
          column(width = 8,
                 leafletOutput("map")
          )
        ),
        plotOutput("graphA")
      )
    ),
    
    # Page 3 "Plot B" content
    tabPanel(title = "Plot B", value = "tab3",
             
             mainPanel(
               h1("Past vs. Future"),
               h3("Precipitation in Andalusia"),
               p("The two maps below show precipitation values in L/m2 for each individual station."),
               p("The first map shows the average historic precipitation. In the second map, you can select which year you would like to compare it with."),
               plotOutput("graphB_hist"),
               selectInput("year", "Select Year", choices = 2010:2022, selected = 2014),
               plotOutput("graphB")
             )),
    
    # Page 4 "Plot C" content
    tabPanel(title = "Plot C", value = "tab4",
             
             mainPanel(
               h1("Welcome to plot C")
             )),
    
    # Page 5 "Plot D" content
    tabPanel(title = "Plot D", value = "tab5",
             
             mainPanel(
               h1("Welcome to plot D")
             ))
    
  )
)

server <- function(input, output, session) {
  
  ### tab 2 Plot A logic 
  # Initialize the selected station as NULL
  selected_station <- reactiveVal(NULL)
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = unique_locations,
                       radius = 4,
                       color = "red",
                       fillOpacity = 0.8,
                       layerId = ~station_name,
                       label = ~station_name)
  })
  
  # Create a reactive expression for selected data
  selected_data <- reactive({
    if (!is.null(input$station) && !is.null(input$years) && !is.null(input$metrics)) {
      data_selected <- sf_data %>%
        filter(station_name == input$station & year(timestamp) %in% input$years) %>%
        mutate(month = month(timestamp, label = TRUE, abbr = FALSE))
      
      if ("Mean" %in% input$metrics) {
        data_selected <- data_selected %>% mutate(mean_temperature = mean_temperature)
      }
      if ("Min" %in% input$metrics) {
        data_selected <- data_selected %>% mutate(min_temperature = min_temperature)
      }
      if ("Max" %in% input$metrics) {
        data_selected <- data_selected %>% mutate(max_temperature = max_temperature)
      }
      
      return(data_selected)
    }
  })
  
  # Render the graph based on the selected station, years, and metrics
  output$graphA <- renderPlot({
    data_selected <- selected_data()
    
    if (!is.null(data_selected)) {
      ggplot(data_selected, aes(x = month, group = as.factor(year(timestamp)))) +
        geom_line(aes(y = mean_temperature, color = ifelse("Mean" %in% input$metrics, "Mean", NA))) +
        geom_line(aes(y = min_temperature, color = ifelse("Min" %in% input$metrics, "Min", NA))) +
        geom_line(aes(y = max_temperature, color = ifelse("Max" %in% input$metrics, "Max", NA))) +
        labs(x = "Month", y = "Temperature") +
        theme_minimal()
    }
  })
  
  # Update the selected station when a marker is clicked
  observeEvent(input$map_marker_click, {
    clicked_marker <- input$map_marker_click
    clicked_station <- clicked_marker$id
    selected_station(clicked_station)
    
    # Update the select input with the clicked station
    updateSelectInput(session, "station", selected = clicked_station)
  })
  ### END tab 2 plot A logic
  
  ### Tab 3 plot B logic  
  output$graphB_hist <- renderPlot({
    # Historical plot
    ggplot(data = world) + 
      geom_sf() +
      geom_sf(data = ocean, fill = "lightblue") +
      geom_text(data = ocean_names, aes(x = lon, y = lat, label = name),
                color = "darkblue", size = 3, angle = c(15, -10)) +
      geom_sf(data = mean_precipitation$geom, aes(color = mean_precipitation$mean_precipitation), size=2.5, shape=19, alpha=0.9) + 
      geom_sf(data = spain_cities, col = "red") +
      geom_text_repel(data = spain_cities, aes(x = lon, y = lat, label = city), 
                      fontface = "bold", nudge_x = c(-1, 0, -0.25, -1, 0), nudge_y = c(1, -0.75, -0.5, -0.5, 0)) +
      coord_sf(xlim=c(-9.248333, 0.2297222), ylim=c(34.285, 40.49611), expand = FALSE) +
      labs(color = "Mean Precipitation in L/m²") +
      ggtitle("Meteorological stations in Andalusia, Spain: mean precipitation per station 2010-2022") 
  })
  
  # Update the map plot based on the selected year
  output$graphB <- renderPlot({
    plot_precipitation_map(input$year)
  })
  
  ### END Tab 3 plot B logic
}

options(shiny.host = 10.0.1.5)
options(shiny.port = 7777)
runApp(shinyApp(ui, server))
