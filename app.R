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

# Add lat/lon and name to the streamdata_filtered -- then draw these points and animate shape/color over time
streamdata_time <- merge(streamdata_filtered, gages_filtered, by="site_no")
streamdata_time$flood_norm <- streamdata_time$X_00065_00000 - streamdata_time$flood_stage
streamdata_time$dateTime <- as.POSIXct(streamdata_time$dateTime, format="%Y-%m-%dT%H:%M:%S",tz=Sys.timezone())

# Normalize the stream data by gage flood level
streamdata_filtered <- streamdata_time

# Split into positive and negative series for display in the dygraph
streamdata_filtered$positive <- NA
streamdata_filtered[!is.na(streamdata_filtered$flood_norm) & streamdata_filtered$flood_norm >= 0, "positive"] <- 'A'
streamdata_filtered[!is.na(streamdata_filtered$flood_norm) & streamdata_filtered$flood_norm < 0, "positive"] <- 'B'
streamdata_filtered$series <- paste(streamdata_filtered$site_no, streamdata_filtered$positive, sep="")

stream_cast <- dcast(streamdata_filtered, dateTime ~ series, value.var = 'flood_norm')
stream_ts <- xts(stream_cast, order.by=stream_cast$dateTime)
stream_ts <- stream_ts[ , !(names(stream_ts) == 'dateTime')]

# Remove 0 precip from data
precip_data_sub <- subset(precip_data, precip != 0)

# Link the precip data to precip spatial
precip_merge <- merge(precip_data_sub, spatial_wgs84, by = 'id', all = TRUE)
precip_merge <- subset(precip_merge, !is.na(precip))
precip_merge <- st_sf(precip_merge)
precip_merge$id <- as.character(precip_merge$id)

new <- data.frame(matrix(ncol = 8, nrow = 0))
columns <- c("site_no", "dateTime", "X_00065_00000", "station_nm", "dec_lat_va", "dec_long_va", "flood_stage", "flood_norm")
colnames(new) <- columns


for (i in unique(streamdata_time$dateTime)) {
    for(a in unique(streamdata_time$site_no)) {
        if(nrow(subset(streamdata_time, dateTime == i & site_no == a))==0) {
            gage <- subset(gages_filtered, site_no == a)
            row <- c(a, i, NA, NA, gage$dec_lat_va, gage$dec_long_va, 0, NA)
            new[nrow(new)+1,] <- row
        }    
    }
}

new$dateTime <- as.POSIXct(new$dateTime, origin = "1970-01-01")
streamdata_time_new <- rbind(streamdata_time, new)


# Output in Shiny app
s <- NULL
a <- 0

ui <- bootstrapPage(
    # titlePanel("Hurricane Florence"), # remove on mobile
    tags$style(type = "text/css", "
               html, body, #map {width:100%;height:calc(100vh)}
               .irs {width: 300px; float: left; display: inline-block;}
               .play {font-size: 18px !important; color: #414042 !important;}
               .pause {font-size: 18px !important; color: #414042 !important;}
               .slider-animate-container {float: right; display: inline-block; height: 60px; width: 50px; margin-top: 18px !important; text-align: left !important;}
               .irs-bar {width: 300px; height: 10px; background: #6d6e71; border: none;}
               .irs-bar-edge {background: #6d6e71; border: none; height: 10px; border-radius: 50px; width: 20px;}
               .irs-line {border: none; height: 10px; border-radius: 50px;}
               .irs-grid-text {font-family: 'arial'; color: transparent; bottom: 17px; z-index: 1;}
               .irs-grid-pol {display: none;}
               .irs-max {font-family: 'arial'; color: black; visibility: hidden !important;}
               .irs-min {font-family: 'arial'; color: black; visibility: hidden !important;}
               .irs-single {color:#333; background:transparent; font-size: 14px !important; left: 0 !important; font-weight: 600;}
               .irs-slider {width: 18px; height: 18px; top: 20px;}
               .dygraph-rangesel-bgcanvas {display: none;}
               .dygraph-rangesel-fgcanvas {display: none;}
               .dygraph-rangesel-zoomhandle {display: none;}
               .dygraph-axis-label-x {display: none;}
               .dygraph-axis-label-y {font-size: 12px;}
               .dygraph-title {font-size: 14px; margin-bottom: 10px; text-align: left; padding-left: 20px;}
               .form-group {padding-left: 18px;}
               .shiny-input-container {height: 50px;}
               "),
    # leafletOutput("map", width="100%", height="100vh"),
    leafletOutput("map", width=340, height=400), # mobile version
    # absolutePanel(top = 230, right = 50, fixed = TRUE,
    #               width = 400, height = "auto",
    absolutePanel(top = 280, left = 10, fixed = TRUE, # mobile version
                  width = 300, height = 100, # mobile version
                  style = "margin-left: auto;margin-right: auto;",
                  sliderInput("time", "date/time", 
                         min = as.POSIXct("2018-09-13 00:00:00"),
                         max = as.POSIXct("2018-09-19 11:00:00"),
                         value = as.POSIXct("2018-09-13 00:00:00"),
                         step = 21600, # 1 hour is 3600
                         animate = T, width = "100%",
                         ticks = T, timeFormat = "%a %b %o %I%P",
                         label = NULL)
                  # dygraphOutput("graph", width = "100%", height = "200px") # remove on mobile
    )
)

server <- function(input, output, session) {
    
    precipColor <- colorBin(palette = c('#ccdb96', '#abd0a5', '#89bad3', '#7d88b5', '#67508e', '#5a396d', '#371437'),
                            bins = 7, pretty = TRUE,
                            domain = precip_merge$precip)
    
    output$map <- renderLeaflet({ # Build map
        leaflet() %>%
            addMapPane(name = "polygons", zIndex = 410) %>% 
            addMapPane(name = "maplabels", zIndex = 420) %>%
            addProviderTiles(providers$CartoDB.PositronNoLabels,
            # addProviderTiles(providers$Esri.WorldTopoMap,
                             options = providerTileOptions(opacity = 0.6)) %>%
            addProviderTiles("CartoDB.PositronOnlyLabels", 
                             options = leafletOptions(pane = "maplabels"),
                             group = "map labels") %>%
            # setView(lng=-78.497110, lat=34.643180, zoom=8) %>%
            setView(lng=-77.8868, lat=33.2, zoom=6) %>% # mobile version
            addMinicharts(lng = streamdata_time_new$dec_long_va,
                          lat = streamdata_time_new$dec_lat_va,
                          layerId = streamdata_time_new$station_nm,
                          type = "bar", maxValues = 42,
                          width = 20, height = 120, fillColor = "#226eae")
    })
    
    
    observe({ # Add hurricane path
        dat <- pts_wgs84[ which(pts_wgs84$dateTime <= input$time), ]
        for (i in 1:nrow(dat)) {
            dat.sub <- dat[i:(i+1),]
            leafletProxy("map", data = dat.sub) %>%
                addPolylines(lng=~LON, lat=~LAT, color="#bcbec0", 
                             opacity=1, options = leafletOptions(pane = "polygons"),
                             group = paste("hurricane path",a,sep="_"),
                             weight=~ifelse(STORMTYPE == 'DB', 1, 
                                           ifelse(STORMTYPE == 'LO', 1,
                                                  ifelse(STORMTYPE =='TD', 1,
                                                         ifelse(STORMTYPE == 'TS', 6, 24)))))
        }
        leafletProxy("map") %>% clearGroup(paste("hurricane path",a-1,sep="_"))
        a <<- a + 1
    })
    
    observe({ # Add precip polygons
        leafletProxy("map", data = subset(precip_merge, time == input$time)) %>%
            removeShape(s) %>%
            addPolygons(color = ~precipColor(precip), weight = 0, 
                        smoothFactor = 0.5, opacity = 0.35, fillOpacity = 0.35, layerId=~id,
                        options = leafletOptions(pane = "polygons"))
        s <<- subset(precip_merge, time == input$time)$id
    })
    
    observe({ # Add gages
        data <- subset(streamdata_time_new, dateTime == input$time)
        leafletProxy("map") %>%
            updateMinicharts(
                layerId = data$station_nm,
                chartdata = data$flood_norm,
                opacity = ifelse(data$flood_norm < 0, 0.2, 1)
            )
    })
    
    # observe({ # remove on mobile version
    #     input$time # Update the time series to align with the map
    #     updated <- stream_ts[paste('2018/',input$time,sep="")]
    #     output$graph <- renderDygraph({
    #         dygraph(updated, main = "Water level at selected USGS gages", width = '270', height = '700') %>%
    #             dyAxis("y", valueRange = c(-18,50), axisLabelWidth = 20) %>%
    #             dyAxis("x", drawGrid = FALSE) %>%
    #             dyRangeSelector(dateWindow = c("2018-09-13 00:00:00", "2018-09-19 11:00:00"), height = 20) %>%
    #             dyLegend(show="never") %>%
    #             dyOptions(drawGrid = FALSE) %>%
    #             dyLimit(limit=0, label = "flooding level", labelLoc = "left",
    #                     color = "#414042", strokePattern = "solid") %>%
    #             dyHighlight(highlightCircleSize = 0, highlightSeriesBackgroundAlpha = 1)  %>%
    #             # Below 0 is #a4b5d8
    #             dySeries("2096500B", color = "#a4b5d8", strokePattern = "dotted") %>%
    #             dySeries("2096960B", color = "#a4b5d8", strokePattern = "dotted") %>%
    #             dySeries("2100500B", color = "#a4b5d8", strokePattern = "dotted") %>%
    #             dySeries("2102000B", color = "#a4b5d8", strokePattern = "dotted") %>%
    #             dySeries("2102500B", color = "#a4b5d8", strokePattern = "dotted") %>%
    #             dySeries("2103000B", color = "#a4b5d8", strokePattern = "dotted") %>%
    #             dySeries("2104000B", color = "#a4b5d8", strokePattern = "dotted") %>%
    #             dySeries("2105769B", color = "#a4b5d8", strokePattern = "dotted") %>%
    #             dySeries("2106500B", color = "#a4b5d8", strokePattern = "dotted") %>%
    #             dySeries("2108000B", color = "#a4b5d8", strokePattern = "dotted") %>%
    #             dySeries("2108566B", color = "#a4b5d8", strokePattern = "dotted") %>%
    #             # Above 0 is #226eae
    #             dySeries("2096500A", color = "#226eae") %>%
    #             dySeries("2096960A", color = "#226eae") %>%
    #             dySeries("2100500A", color = "#226eae") %>%
    #             dySeries("2102000A", color = "#226eae") %>%
    #             dySeries("2102500A", color = "#226eae") %>%
    #             dySeries("2103000A", color = "#226eae") %>%
    #             dySeries("2104000A", color = "#226eae") %>%
    #             dySeries("2105769A", color = "#226eae") %>%
    #             dySeries("2106500A", color = "#226eae") %>%
    #             dySeries("2108000A", color = "#226eae") %>%
    #             dySeries("2108566A", color = "#226eae") 
    # 
    #     })
    # })

    
}

shinyApp(ui, server)



# NOTES
# Add a key for the precip data colors
# Add a key for the hurricane path width data