setwd("~/Desktop/USGS/hurricane_viz_R/data")
library(jsonlite)
library(sf)
library(geojsonsf)
library(leaflet)
library(dygraphs)
library(reshape2)
library(xts)
library(shiny)


# Load data
precip_data <- readRDS("precip_values_thinned.rds", refhook = NULL)
precip_spatial <- readRDS("precip_spatial.rds", refhook = NULL)
gages <- read.csv("all_sites.csv")
streamdata <- read.csv("streamdata.csv")
pts <- read_sf("AL062018_pts.shp")



# Filter the gages
included <- c(02096500, 02096960, 02099000, 02100500, 02102000, 02102500, 02102908, 02103000,
              02104000, 02104220, 02105500, 02105769, 02106500, 02108000, 02108566)
streamdata_filtered <- subset(streamdata, site_no %in% included)[,c('site_no','dateTime','X_00065_00000')]
gages_filtered <- subset(gages, site_no %in% included)



# Transform data
spatial_wgs84 <- st_transform(precip_spatial, "+init=epsg:4326") # Add CRS coordinates
line_wgs84 <- st_transform(line, "+init=epsg:4326") # Add CRS coordinates
pts_wgs84 <- st_transform(pts, "+init=epsg:4326") # Add CRS coordinates
# Add time dimension to pts
pts_wgs84$DTG <- as.character(pts_wgs84$DTG)
pts_wgs84$dateTime <- as.POSIXct(pts_wgs84$DTG, format="%Y%m%d%H",tz=Sys.timezone())
# Duplicate and group points
pts_wgs84_dup = pts_wgs84[-1,]
pts_wgs84$group <- 1:nrow(pts_wgs84) 
pts_wgs84_dup$group <- 1:nrow(pts_wgs84_dup) 
pts_wgs84_groups <- rbind(pts_wgs84,pts_wgs84_dup)
pts_wgs84_groups <- pts_wgs84_groups[order(pts_wgs84_groups$group),]
pts_wgs84_groups = pts_wgs84_groups[-145,]

# Normalize the stream data by gage flood level
streamdata_filtered$flood_norm <- streamdata_filtered$X_00065_00000 - subset(gages, site_no == streamdata_filtered$site_no)$flood_stage
streamdata_filtered$dateTime <- as.POSIXct(streamdata_filtered$dateTime, format="%Y-%m-%dT%H:%M:%S",tz=Sys.timezone())
stream_cast <- dcast(streamdata_filtered, dateTime ~ site_no)
stream_ts <- xts(stream_cast, order.by=stream_cast$dateTime)
stream_ts <- stream_ts[ , !(names(stream_ts) == 'dateTime')]

# Remove 0 precip from data
precip_data_sub <- subset(precip_data, precip != 0)
# Link the precip data to precip spatial
precip_merge <- merge(precip_data_sub, spatial_wgs84, by = 'id', all = TRUE)
precip_merge <- subset(precip_merge, !is.na(precip))
precip_merge <- st_sf(precip_merge)
precip_merge$id <- as.character(precip_merge$id)


# Output in Shiny app
s <- NULL

ui <- bootstrapPage(
    # titlePanel("Hurricane Florence"),
    tags$style(type = "text/css", "html, body, #map {width:100%;height:calc(100vh)}"),
    leafletOutput("map", width="100%", height="100vh"),
    absolutePanel(top = 30, right = 50,
                  # style='background-color: #ffffff',
                  width = 270, height = "auto",
                  sliderInput("time", "date/time", min(precip_merge$time),
                              max(precip_merge$time),
                              value = min(precip_merge$time),
                              step=21600, # 1 hour is 3600
                              animate=T),
                  dygraphOutput("graph")
    )
)

server <- function(input, output, session) {
    
    precipColor <- colorBin(palette = c('#edf5e5', '#bcdec9', '#aec8d5', '#8c6bb1', '#9c90ba', '#5d426d', '#361e44'),
                            bins = 7, pretty = TRUE,
                            domain = precip_merge$precip)
    
    output$map <- renderLeaflet({ # Build map
        leaflet(options = leafletOptions(zoomControl=FALSE, height="100vh")) %>%
            addProviderTiles(providers$Esri.WorldTopoMap,
                             options = providerTileOptions(opacity = 1)) %>%
            # addMarkers(lng=-81.1637, lat=33.8361, popup="Test popup") %>%
            setView(lng=-76.1637, lat=33.8361, zoom=7)
    })
    
    output$graph <- renderDygraph({ # Build flooding graphs
        # All selected on one graph
        dygraph(stream_ts, main = "Water at selected USGS gages", width = '270', height = '700') %>%
            dyAxis("y", valueRange = c(-18,50)) %>%
            dyRangeSelector(dateWindow = c("2018-09-11 04:00:00", "2018-09-19 14:00:00"), height = 20) %>%
            dyLegend(show="never") %>%
            dyOptions(colors = '#000') %>% 
            dyHighlight(highlightSeriesBackgroundAlpha = 1) -> d1
    })
    
    observe({ # Add precip polygons
        leafletProxy("map", data = subset(precip_merge, time == input$time)) %>%
            removeShape(s) %>%
            addPolygons(color = ~precipColor(precip), weight = 0, 
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.75, layerId=~id)
        s <<- subset(precip_merge, time == input$time)$id
        # cat(file=stderr(), "debug ", s, "\n")
    })
    
    observe({ # Add gages
        leafletProxy("map", data = gages_filtered) %>%
        addCircles(lng = ~dec_long_va, lat = ~dec_lat_va, weight = 5,
                   radius = 20, popup = ~station_nm, layerId=~station_nm)
    })

    observe({ # Add hurricane path
        for (i in unique(pts_wgs84_groups$group)) {
            group_sub <- pts_wgs84_groups[which(pts_wgs84_groups$group == i), ]
            if(group_sub[2,]$dateTime <= input$time) {
                leafletProxy("map", data = group_sub) %>%
                addPolylines(lng = ~LON, lat = ~LAT, weight = ~INTENSITY/10,
                             popup = ~STORMTYPE, layerId=~dateTime, opacity = 1.0)
            }
        }
    })
    
    observe({
        input$time # Update the time series to align with the map
        updated <- stream_ts[paste('2018/',as.Date(input$time),sep="")]
        output$graph <- renderDygraph({
            dygraph(updated, main = "Water at selected USGS gages", width = '270', height = '700') %>%
                dyAxis("y", valueRange = c(-18,50)) %>%
                dyRangeSelector(dateWindow = c("2018-09-11 04:00:00", "2018-09-19 14:00:00"), height = 20) %>%
                dyLegend(show="never") %>%
                dyOptions(colors = '#000') %>% 
                dyHighlight(highlightSeriesBackgroundAlpha = 1) -> d1
            
        })
    })


    # Add interaction events
    observeEvent(input$map_shape_click, { # This could identify which line goes to which gage (also on hover)
        # on our click let's update the dygraph to only show the time series for the clicked
        updated <- stream_ts[paste('2018/',as.Date(input$time),sep="")]
        output$graph <- renderDygraph({
            dygraph(updated, main = "Water at selected USGS gages", width = '270', height = '700') %>%
                dyAxis("y", valueRange = c(-18,50)) %>%
                dyRangeSelector(dateWindow = c("2018-09-11 04:00:00", "2018-09-19 14:00:00"), height = 20) %>%
                dyLegend(show="never") %>%
                dyOptions(colors = '#000')
        })
    })
        
}

shinyApp(ui, server)


# NOTES
# Add colors to the precip data
# Add a key for the precip data colors
# Add a key for the hurricane path width data
# Connect the gages to the lines in the dygraph
# Make all the lines in the dygraph the same color
# Remove hurricane path when going back in time on the time scrubber
