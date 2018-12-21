setwd("~/Desktop/USGS/hurricane_viz_R/data")
library(jsonlite)
library(sf)
library(geojsonsf)
library(leaflet)
library(dygraphs)
library(reshape2)
library(xts)
library(shiny)
library(leaflet.minicharts)
library(sp)
library(maptools)

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

pts_sm <- pts_wgs84[, c("LON", "LAT", "INTENSITY", "STORMTYPE", "dateTime")]

# Add time dimension to pts
pts_wgs84$DTG <- as.character(pts_wgs84$DTG)
pts_wgs84$dateTime <- as.POSIXct(pts_wgs84$DTG, format="%Y%m%d%H",tz=Sys.timezone())

# Duplicate and group points
pts_wgs84_dup <- pts_wgs84[-1,]
pts_wgs84$group <- 1:nrow(pts_wgs84) 
pts_wgs84_dup$group <- 1:nrow(pts_wgs84_dup) 
pts_wgs84_groups <- rbind(pts_wgs84,pts_wgs84_dup)
pts_wgs84_groups <- pts_wgs84_groups[order(pts_wgs84_groups$group),]
pts_wgs84_groups <- pts_wgs84_groups[-145,]

# Normalize the stream data by gage flood level
streamdata_filtered$flood_norm <- streamdata_filtered$X_00065_00000 - subset(gages, site_no == streamdata_filtered$site_no)$flood_stage
streamdata_filtered$dateTime <- as.POSIXct(streamdata_filtered$dateTime, format="%Y-%m-%dT%H:%M:%S",tz=Sys.timezone())
stream_cast <- dcast(streamdata_filtered, dateTime ~ site_no)
stream_ts <- xts(stream_cast, order.by=stream_cast$dateTime)
stream_ts <- stream_ts[ , !(names(stream_ts) == 'dateTime')]

# Add lat/lon and name to the streamdata_filtered -- then draw these points and animate shape/color over time
streamdata_time <- merge(streamdata_filtered, gages_filtered, by="site_no")

# Remove 0 precip from data
precip_data_sub <- subset(precip_data, precip != 0)

# Link the precip data to precip spatial
precip_merge <- merge(precip_data_sub, spatial_wgs84, by = 'id', all = TRUE)
precip_merge <- subset(precip_merge, !is.na(precip))
precip_merge <- st_sf(precip_merge)
precip_merge$id <- as.character(precip_merge$id)

new <- data.frame(matrix(ncol = 8, nrow = 0))
columns <- c("site_no", "dateTime", "X_00065_00000", "flood_norm", "station_nm", "dec_lat_va", "dec_long_va", "flood_stage")
colnames(new) <- columns


for (i in unique(streamdata_time$dateTime)) {
    for(a in unique(streamdata_time$site_no)) {
        if(nrow(subset(streamdata_time, dateTime == i & site_no == a))==0) {
            gage <- subset(gages, site_no == a)
            row <- c(a, i, NA, NA, NA, gage$dec_lat_va, gage$dec_long_va, 0)
            new[nrow(new)+1,] <- row
        }    
    }
}
new$dateTime <- as.POSIXct(new$dateTime, origin = "1970-01-01")
streamdata_time <- rbind(streamdata_time, new)


# Output in Shiny app
s <- NULL

ui <- bootstrapPage(
    # titlePanel("Hurricane Florence"),
    tags$style(type = "text/css", "
               html, body, #map {width:100%;height:calc(100vh)}
               .irs {width: 300px; float: left; display: inline-block;}
               .play {font-size: 18px !important; color: #0f284a !important;}
               .slider-animate-container {float: right; display: inline-block; height: 60px; width: 50px; margin-top: 20px !important; text-align: center !important;}
               .irs-bar {width: 300px; height: 10px; background: black; border: none;}
               .irs-bar-edge {background: black; border: none; height: 10px; border-radius: 50px; width: 20px;}
               .irs-line {border: none; height: 10px; border-radius: 50px;}
               .irs-grid-text {font-family: 'arial'; color: transparent; bottom: 17px; z-index: 1;}
               .irs-grid-pol {display: none;}
               .irs-max {font-family: 'arial'; color: black; visibility: hidden !important;}
               .irs-min {font-family: 'arial'; color: black; visibility: hidden !important;}
               .irs-single {color:black; background:transparent; font-size: 14px !important; left: 0 !important;}
               .irs-slider {width: 18px; height: 18px; top: 20px;}
               .dygraph-rangesel-bgcanvas {display: none;}
               .dygraph-rangesel-fgcanvas {display: none;}
               .dygraph-rangesel-zoomhandle {display: none;}
               .dygraph-axis-label-x {display: none;}
               .dygraph-axis-label-y {font-size: 12px;}
               .dygraph-title {font-size: 16px; margin-bottom: 10px; text-align: left; padding-left: 20px;}
               .form-group {padding-left: 26px;}
               "),
    leafletOutput("map", width="100%", height="100vh"),
    absolutePanel(top = 100, right = 50, fixed = TRUE,
                  width = 400, height = 100,
                  style = "margin-left: auto;margin-right: auto;",
                  dygraphOutput("graph", width = "100%", height = "200px"),
                  sliderInput("time", "date/time", 
                              min = as.POSIXct("2018-09-13 00:00:00"),
                              max = as.POSIXct("2018-09-19 11:00:00"),
                              value = as.POSIXct("2018-09-13 00:00:00"),
                              step = 21600, # 1 hour is 3600
                              animate = T, width = "100%",
                              ticks = T, timeFormat = "%a %b %o %I%P",
                              label = NULL)
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
            setView(lng=-76.1637, lat=33.8361, zoom=7) %>%
            addMinicharts(lng = streamdata_time$dec_long_va, 
                          lat = streamdata_time$dec_lat_va, 
                          layerId = streamdata_time$station_nm,
                          type = "bar", maxValues = 42, 
                          width = 15, height = 120, fillColor = "#0f284a")
    })
    
    observe({ # Add precip polygons
        leafletProxy("map", data = subset(precip_merge, time == input$time)) %>%
            removeShape(s) %>%
            addPolygons(color = ~precipColor(precip), weight = 0, 
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.75, layerId=~id)
        s <<- subset(precip_merge, time == input$time)$id
    })
    
    observe({ # Add gages
        data <- subset(streamdata_time, dateTime == input$time)
        leafletProxy("map") %>%
            updateMinicharts(
                layerId = data$station_nm,
                chartdata = data$flood_norm
            )
    })
    
    observe({ # Add hurricane path
        dat <- subset(pts_wgs84, dateTime <= input$time)
        d <- points_to_line(dat, "LON", "LAT", sort_field = "dateTime")
        leafletProxy("map", data = d) %>%
            addPolylines(color="#0f284a", opacity=1, layerId='hurricanePath')
            # addPolylines(color="#0f284a", opacity=1, weight=~LON/10)
    })
    
    observe({
        input$time # Update the time series to align with the map
        updated <- stream_ts[paste('2018/',input$time,sep="")]
        output$graph <- renderDygraph({
            dygraph(updated, main = "Water level at selected USGS gages", width = '270', height = '700') %>%
                dyAxis("y", valueRange = c(-18,50), axisLabelWidth = 20) %>%
                dyAxis("x", drawGrid = FALSE) %>%
                dyRangeSelector(dateWindow = c("2018-09-13 00:00:00", "2018-09-19 11:00:00"), height = 20) %>%
                dyLegend(show="never") %>%
                dyOptions(colors = '#000', drawGrid = FALSE) %>%
                dyShading(from = "-20", to = "0", color = "#EFEFEF", axis = "y")
            
        })
    })
    
    
    # Add interaction events
    observeEvent(input$graph_click$x, { # This could identify which line goes to which gage (also on hover)
        cat(file=stderr(), "debug ", input$graph_click$series, "\n")
        # on our click let's update the dygraph to only show the time series for the clicked
        # updated <- stream_ts[paste('2018/',as.Date(input$time),sep="")]
        # output$graph <- renderDygraph({
        #     dygraph(updated, main = "Water at selected USGS gages", width = '270', height = '700') %>%
        #         dyAxis("y", valueRange = c(-18,50)) %>%
        #         dyRangeSelector(dateWindow = c("2018-09-11 04:00:00", "2018-09-19 14:00:00"), height = 20) %>%
        #         dyLegend(show="never") %>%
        #         dyOptions(colors = '#000')
        # })
    })
    
}

shinyApp(ui, server)


points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
    
    data <- data.frame(data)
    
    # Convert to SpatialPointsDataFrame
    coordinates(data) <- c(long, lat)
    
    # If there is a sort field...
    if (!is.null(sort_field)) {
        if (!is.null(id_field)) {
            data <- data[order(data[[id_field]], data[[sort_field]]), ]
        } else {
            data <- data[order(data[[sort_field]]), ]
        }
    }
    
    # If there is only one path...
    if (is.null(id_field)) {
        
        lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
        
        return(lines)
        
        # Now, if we have multiple lines...
    } else if (!is.null(id_field)) {  
        
        # Split into a list by ID field
        paths <- sp::split(data, data[[id_field]])
        
        sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
        
        # I like for loops, what can I say...
        for (p in 2:length(paths)) {
            id <- paste0("line", as.character(p))
            l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
            sp_lines <- spRbind(sp_lines, l)
        }
        
        return(sp_lines)
    }
}

# NOTES
# Add colors to the precip data
# 0f284a
# Add a key for the precip data colors
# Add a key for the hurricane path width data
# Connect the gages to the lines in the dygraph
# Make all the lines in the dygraph the same color
