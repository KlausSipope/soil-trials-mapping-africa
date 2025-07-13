# Name: Mr. Klaus Sipope
# Position: Junior Data Scientist
# Description: Create map for all countries
# Date first written: Sun, 27-Oct-2024
# Date last updated: Sun, 27-Oct-2024


## UI
# Load packages
library(shiny)
library(shinyjs)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(dplyr)
library(readxl)
library(RColorBrewer)

# Define UI
ui <- fluidPage(
  shinyjs::useShinyjs(), # Initialize shinyjs
  titlePanel("SOILS Trials"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryFocus", "Focus Map On:",
                  choices = c("All Countries", "Mozambique", "Tanzania", "Uganda"),
                  selected = "All Countries"),
      selectInput("selectedDepth", "Select Depth:",
                  choices = NULL,
                  selected = NULL),
      checkboxGroupInput("selectedTrials", "Select trials:",
                         choices = NULL,
                         selected = NULL),
      checkboxGroupInput("selectedCrops", "Select crops:",
                         choices = NULL,
                         selected = NULL),
      selectInput("colorVariable", "Color by:", 
                  choices = c(
                    "pH" = "pH",
                    "Phosphorus (P ppm)" = "P (ppm)",
                    "Potassium (K ppm)" = "K (ppm)",
                    "Calcium (Ca ppm)" = "Ca (ppm)",
                    "Magnesium (Mg ppm)" = "Mg (ppm)",
                    "Sulfur (S ppm)" = "S (ppm)",
                    "Sodium (Na ppm)" = "Na (ppm)",
                    "Manganese (Mn ppm)" = "Mn (ppm)",
                    "Boron (B ppm)" = "B (ppm)",
                    "Zinc (Zn ppm)" = "Zn (ppm)",
                    "Copper (Cu ppm)" = "Cu (ppm)",
                    "Sand Percentage (%Sand)" = "%Sand (%)",
                    "Silt Percentage (%Silt)" = "%Silt (%)",
                    "Clay Percentage (%Clay)" = "%Clay (%)",
                    "Carbon Content (%C)" = "C (%)",
                    "Nitrogen Content (%N)" = "N (%)",
                    "Cation Exchange Capacity (C.E.C meq/100g)" = "C.E.C (meq/100g)",
                    "Exchangeable Aluminum (Exch.Al meq/100g)" = "Exch.Al (meq/100g)"
                  ),
                  selected = "pH")
    ),
    mainPanel(
      leafletOutput("map", height = 700)
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Paths to the dataset (make sure this path is correct)
  path_season <- "general_dataset.xlsx"
  
  # Read the data
  data <- read_xlsx(path_season)
  
  # Preprocess data to handle special numeric cases
  handle_special_numeric <- function(x) {
    numeric_values <- as.numeric(gsub("[^0-9.]", "", x))
    adjusted_values <- ifelse(grepl("^<", x), numeric_values - 0.05, numeric_values)
    return(adjusted_values)
  }
  
  # Apply special numeric handling and convert ppm to mmol/kg where applicable
  data <- data %>%
    mutate(across(c("P (ppm)", "K (ppm)", "Ca (ppm)", "Mg (ppm)", "S (ppm)", 
                    "Na (ppm)", "Mn (ppm)", "B (ppm)", "Zn (ppm)", "Cu (ppm)", 
                    "%Sand (%)", "%Silt (%)", "%Clay (%)", "C (%)", "N (%)", 
                    "C.E.C (meq/100g)", "Exch.Al (meq/100g)"),
                  ~handle_special_numeric(as.character(.)))) %>%
    mutate(
      `K (mmol/kg)` = `K (ppm)` / 39.1,
      `Ca (mmol/kg)` = `Ca (ppm)` / 40.08,
      `Mg (mmol/kg)` = `Mg (ppm)` / 24.31,
      `S (mmol/kg)` = `S (ppm)` / 32.07,
      `Na (mmol/kg)` = `Na (ppm)` / 22.99,
      `Mn (mmol/kg)` = `Mn (ppm)` / 54.94,
      `B (mmol/kg)` = `B (ppm)` / 10.81,
      `Zn (mmol/kg)` = `Zn (ppm)` / 65.38,
      `Cu (mmol/kg)` = `Cu (ppm)` / 63.55,
      `C.E.C (meq/kg)` = `C.E.C (meq/100g)` * 10,
      `Exch.Al (meq/kg)` = `Exch.Al (meq/100g)` * 10
    )
  
  # Convert cleaned data to sf object for spatial operations
  data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Load Africa administrative boundaries
  africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
  
  # Define tighter focus and boundaries for each country
  country_centers <- list(
    "Mozambique" = list(lng = 35.5296, lat = -18.6657, zoom = 6),
    "Tanzania" = list(lng = 34.8888, lat = -6.3690, zoom = 6),
    "Uganda" = list(lng = 32.2903, lat = 1.3733, zoom = 6)
  )
  
  # Dynamically update depth selection
  observe({
    depth_values <- unique(data_sf$Depth)
    updateSelectInput(session, "selectedDepth", choices = depth_values, selected = depth_values[1])
  })
  
  # Dynamically update trials and crops when the country changes
  observe({
    # Filter the data based on the selected country
    filtered_data <- if (input$countryFocus != "All Countries") {
      data_sf %>% filter(Country == input$countryFocus)
    } else {
      data_sf
    }
    
    # Update trials based on filtered data
    trials <- unique(filtered_data$Trial)
    updateCheckboxGroupInput(session, "selectedTrials", choices = trials, selected = trials)
    
    # Update crops based on filtered data
    crops <- unique(filtered_data$Crop)
    updateCheckboxGroupInput(session, "selectedCrops", choices = crops, selected = crops)
  })
  
  # Dynamically update trials based on selected depth
  observe({
    filtered_data_by_depth <- data_sf %>% filter(Depth %in% input$selectedDepth)
    trials <- unique(filtered_data_by_depth$Trial)
    updateCheckboxGroupInput(session, "selectedTrials", choices = trials, selected = trials)
  })
  
  # Update crops checkbox input dynamically based on both depth and trials
  observe({
    filtered_data_by_depth_and_trial <- data_sf %>%
      filter(Depth %in% input$selectedDepth, Trial %in% input$selectedTrials)
    crops <- unique(filtered_data_by_depth_and_trial$Crop)
    updateCheckboxGroupInput(session, "selectedCrops", choices = crops, selected = crops)
  })
  
  # Function to generate color palette based on the selected variable
  getColorPalette <- function(data, variable) {
    domain_range <- range(data[[variable]], na.rm = TRUE)
    if (variable == "pH") {
      return(colorNumeric(palette = c("red", "yellow", "green"), domain = domain_range))
    } else {
      return(colorNumeric(palette = c("red", "orange", "yellow"), domain = domain_range))
    }
  }
  
  # Generate and render the map
  output$map <- renderLeaflet({
    req(input$selectedDepth, input$selectedTrials, input$selectedCrops, input$colorVariable)
    
    # Filter data for the selected country if not "All Countries"
    filtered_data <- data_sf
    if (input$countryFocus != "All Countries") {
      filtered_data <- filtered_data %>% filter(Country == input$countryFocus)
    }
    
    filtered_data <- filtered_data %>%
      filter(Depth %in% input$selectedDepth, Trial %in% input$selectedTrials, Crop %in% input$selectedCrops)
    
    # Handle empty dataset
    if (nrow(filtered_data) == 0) {
      return(leaflet() %>%
               addProviderTiles(providers$CartoDB.Positron) %>%
               setView(lng = 20.0, lat = 2.0, zoom = 3))
    }
    
    filtered_data[[input$colorVariable]] <- as.numeric(filtered_data[[input$colorVariable]])
    
    colorPalette <- getColorPalette(filtered_data, input$colorVariable)
    
    map <- leaflet(filtered_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = africa, fillColor = "transparent", color = "#444444", weight = 1) %>%
      addCircleMarkers(
        color = ~colorPalette(get(input$colorVariable)),
        fillColor = ~colorPalette(get(input$colorVariable)),
        fillOpacity = 0.8, radius = 5,
        popup = ~paste("Location:", Country, "<br>",
                       "Demo:", Trial, "<br>",
                       "Crop:", Crop, "<br>",
                       paste0(input$colorVariable, ": "), get(input$colorVariable))
      ) %>%
      addLegend("bottomright",
                pal = colorPalette,
                values = ~get(input$colorVariable),
                title = paste("Legend -", input$colorVariable),
                opacity = 1)
    
    if (input$countryFocus != "All Countries") {
      focus <- country_centers[[input$countryFocus]]
      map <- map %>%
        setView(lng = focus$lng, lat = focus$lat, zoom = focus$zoom)
    } else {
      map <- map %>%
        setView(lng = 20.0, lat = 2.0, zoom = 3)
    }
    
    map
  })
}

# Run the application
shinyApp(ui = ui, server = server)
