#title: "Site table shint"
#author: "Joshua G. Smith"
#date: "5/30/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse)

#Directories
basedir <- "/home/shares/ca-mpa/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Read site locations
sites <- readxl::read_excel(file.path(basedir, "monitoring/ltm-site-table-cleaned.xlsx")) %>%
              filter(!(is.na(Data))) %>%
              janitor::clean_names()

################################################################################

#MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)

# Region labels
region_labels <- tibble(long_dd=c(-123.9, -122.9, -121, -118, -119.5),
                        lat_dd=c(40.5, 38.7, 36, 34.1, 34.8),
                        label=c("North\n(Dec 2012)", "North Central\n(May 2010)", "Central\n(Sep 2007)", "South\n(Jan 2012)", "N. Channel\nIslands (2003)"))

# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey40", lwd=0.1) +
  # Plot sites
  geom_point(data=sites, mapping=aes(x=lon_wgs84, y=lat_wgs84, color = Group)) +
  # Plot region labels
  geom_text(data=region_labels, mapping=aes(x=long_dd, y=lat_dd, label=label), hjust=0, size=2.3) +
  # Labels
  labs(x="", y="", tag="A") +
  scale_size_continuous(name="# of habitats monitored") +
  # Axes
  scale_y_continuous(breaks=32:42) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(0.4, "cm"))
g1


###############################################################################
# Define UI
ui <- fluidPage(
  titlePanel("Sites by Year"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year", min = min(sites$year), max = max(sites$year),
                  value = min(sites$year), step = 1)
    ),
    mainPanel(
      plotOutput("site_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  output$site_plot <- renderPlot({
    # Filter sites based on selected year
    filtered_sites <- subset(sites, year == input$year)
    
    # Plot data
    ggplot() +
      # Plot regions
      geom_hline(mapping = aes(yintercept = region_lats)) +
      # Plot land
      geom_sf(data = foreign, fill = "grey80", color = "white", lwd = 0.3) +
      geom_sf(data = usa, fill = "grey80", color = "white", lwd = 0.3) +
      # Plot state waters
      geom_sf(data = state_waters_line, color = "grey40", lwd = 0.1) +
      # Plot sites
      geom_point(data = filtered_sites, mapping = aes(x = lon_wgs84, y = lat_wgs84, color = group)) +
      # Plot region labels
      geom_text(data = region_labels, mapping = aes(x = long_dd, y = lat_dd, label = label), hjust = 0, size = 2.3) +
      # Labels
      labs(x = "", y = "") +
      scale_size_continuous(name = "# of habitats monitored") +
      # Axes
      scale_y_continuous(breaks = 32:42) +
      # Crop
      coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
      # Theme
      theme_bw() +
      theme(
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title = element_blank(),
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(0.4, "cm")
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)













library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("Sites by Year"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year", min = min(sites$year), max = max(sites$year),
                  value = min(sites$year), step = 1)
    ),
    mainPanel(
      leafletOutput("site_plot"),
      leafletOutput("legend")  # Add leaflet output for legend
    )
  )
)

# Define server
server <- function(input, output) {
  output$site_plot <- renderLeaflet({
    # Filter sites based on selected year
    filtered_sites <- subset(sites, year == input$year)
    
    # Convert group variable to factor if needed
    filtered_sites$group <- as.factor(filtered_sites$group)
    
    # Create color palette for groups
    color_palette <- colorFactor(palette = "Set1", domain = filtered_sites$group)
    
    # Create leaflet map
    m <- leaflet() %>%
      # Set initial view to California
      setView(lng = -119.4179, lat = 36.7783, zoom = 5.2) %>%
      # Add map tiles
      addTiles() %>%
      # Add site markers with fill colors based on group
      addCircleMarkers(data = filtered_sites, lng = ~lon_wgs84, lat = ~lat_wgs84,
                       color = ~color_palette(group), radius = 5, stroke = FALSE, fillOpacity = 0.7)
    
    m
  })
  
  # Create separate leaflet object for legend
  output$legend <- renderLeaflet({
    # Create color palette for groups
    color_palette <- colorFactor(palette = "Set1", domain = sites$group)
    
    # Create leaflet map for legend
    legend_map <- leaflet() %>%
      addTiles() %>%
      addLegend(position = "bottomright", pal = color_palette, values = sites$group,
                title = "Group", opacity = 1)
    
    legend_map
  })
}

# Run the app
shinyApp(ui = ui, server = server)
