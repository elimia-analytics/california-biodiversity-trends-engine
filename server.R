#' ---
#' title: California Biodiversity Trends Engine
#' ---
#'
#' # Server setup
#' # Load libraries
# app_libraries <- c("tidyverse", ### data manipulation
#                   "shiny", "shinyjs", "shinyWidgets", "shinydashboard", "shinycssloaders", "shinyBS", "shinybusy",  ### shiny
#                  "sf", "terra", "leaflet", "leaflet.extras", "leaflet.minicharts", "leafpm", "h3jsr", "esri2sf", "leafgl",   ### spatial
#                  "plotly", "htmltools", "htmlwidgets", "sortable", "DT", "flexdashboard", "dygraphs", "bslib",   ### interactive
#                  "natserv", "duckdbfs", "picante", "ecoCopula", "mvabund", ### data 
#                  "units", "memoise", "glue"
# )
# lapply(app_libraries, library, character.only = TRUE)
library(tidyverse)
library(shiny) 
library(shinyjs) 
library(shinyWidgets) 
library(shinydashboard) 
library(shinycssloaders) 
library(shinyBS) 
library(shinybusy) 
library(sf) 
library(terra) 
library(leaflet) 
library(leaflet.extras) 
library(leaflet.minicharts) 
library(leafpm) 
library(h3jsr) 
library(esri2sf) 
library(leafgl)   
library(plotly) 
library(htmltools) 
library(htmlwidgets) 
library(sortable) 
library(DT) 
library(flexdashboard) 
library(dygraphs) 
library(bslib) 
library(natserv) 
library(duckdbfs) 
library(picante) 
library(ecoCopula) 
library(mvabund)
library(units) 
library(memoise) 
library(glue)
#'
#' # Load data
#' ## California boundary shapefile
ca_boundary <- readRDS("data/boundaries/ca_boundary.rds")
#' ## Pre-loaded areas of interest polygons
aoi_polygons <- readRDS("data/boundaries/aoi_polygons.rds") %>% 
  dplyr::filter(aoi_name %in% gsub("_data.rds", "", list.files("data/outputs")))
#'
aoi_available_outputs <- fromJSON(content(GET("https://api.github.com/repos/elimia-analytics/california-biodiversity-trends-engine/contents/data/outputs"), "text", encoding = "UTF-8"))
aoi_available_outputs <- aoi_available_outputs$name[grepl("\\.rds$", aoi_available_outputs$name)]
aoi_available_outputs

#' ## Load custom functions to support app functionality 
source("biodiversity_trends_engine_functions.R")
#' 
#' 
#' # Define server logic
function(input, output, session) {
  
  # Create reactive objects and functions
  ## Set up reactive objects
  ### Reactive object to store all data for area of interest
  area_of_interest <- reactiveValues(
    boundary = NULL,
    baseline = NULL,
    bbox = NULL,
    baseline_bbox = NULL,
    area = NULL,
    hexes = NULL,
    gbif_data = NULL,
    gbif_data_filtered = NULL,
    species_associations = NULL,
    species_associations_matrix = NULL,
    points_selected = NULL,
    metric_count_hexes = NULL,
    metric_table = NULL,
    records_table = NULL,
    species_table = NULL,
    observers_table = NULL,
    visits_table = NULL,
    completeness_table = NULL,
    trends_table = NULL,
    species_trends_list = NULL,
    biggest_movers_table = NULL,
    focal_species_trends_table = NULL
  )

  ### Objects to store clicks and center values from the taxon sunburst chart
  clicked_taxa <- reactiveValues(taxon = vector(mode = "character"))
  center_taxon <- reactiveValues(name = "Life")
  current_taxon <- reactiveVal("Life")
  
  ### Objects to detect actions from map
  #### Reactive value to store current map values
  current_res <- reactiveVal(6)
  current_metric <- reactiveVal("Records")
  current_bounds <- reactiveValues(
    xmin = -180,
    ymin = -90,
    xmax = 180,
    ymax = 90
  )
  current_species <- reactiveVal(NULL)
  selected_records <- reactiveValues(points = NULL)
  map_initial_zoom <- reactiveValues(value = NULL)
  map_zoom_cross <- reactiveValues(value = FALSE)

  ## Object to detect "redo search in this area" button presses
  redo_search_button_presses <- reactiveValues(values = 0)

  # Set up reactive functions
  ## Function to load user-uploaded data
  url_place <- reactive({
    
    if (grepl("arcgis", input$get_aoi_from_url)){
      out <- input$get_aoi_from_url %>% 
        esri2sf::esri2sf()
    }
    
    out
    
  })
  
  ## Function to capture each click from the taxon sunburst chart
  clickData <- reactive({
    currentEventData <- unlist(event_data(event = "plotly_sunburstclick", source = "taxa_plot", priority = "event"))
    currentEventData
  })
  
  ## Function to filter data based on filters
  filtered_data <- reactive({

    if (!is.null(input$select_species)){

      out <- area_of_interest$gbif_data %>%
        dplyr::filter(
          !(species %in% setdiff(species, input$select_species))
          )
      
    } else {
      
      if (center_taxon$name != "Life"){

      out <- area_of_interest$gbif_data %>%
        dplyr::filter_all(any_vars(. %in% center_taxon$name))

    } else {
        out <- area_of_interest$gbif_data
    }
    }

    out

  })
  
  # Create web map and add basic elements and functionality
  output$main_map <- leaflet::renderLeaflet({

    m <- leaflet::leaflet(options = leafletOptions(zoomDelta = 0.5, zoomSnap = 0, attributionControl = FALSE, worldCopyJump = FALSE)) %>% # Open new leaflet web map
      leaflet::fitBounds(lng1 = -124.482, lat1 = 32.52883, lng2 = -114.1312, lat2 = 42.0095) %>% 
      leaflet::addMapPane("basemap1", zIndex = -100) %>% # Add basemap 1
      leaflet::addProviderTiles(providers$Esri.WorldTerrain, group = "Esri World Terrain", options = list(pathOptions(pane = "basemap1")),
                                providerTileOptions(
                                  updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                                  updateWhenIdle = TRUE           # map won't load new tiles when panning
                                )) %>%
      leaflet::addMapPane("basemap2", zIndex = -100) %>% # Add basemap 2
      leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = list(pathOptions(pane = "basemap2"))) %>%
      leaflet::addMapPane("basemap3", zIndex = -100) %>% # Add basemap 3
      leaflet::addProviderTiles(providers$OpenStreetMap, group = "Open Street Map", options = list(pathOptions(pane = "basemap3"))) %>%
      leaflet::addMapPane("basemap4", zIndex = -100) %>% # Add basemap 4
      leaflet::addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri World Street Map", options = list(pathOptions(pane = "basemap4"))) %>%
      leaflet::addScaleBar(position = "bottomleft") %>% # Add scale bar
      leaflet.extras::addResetMapButton() %>% # Add button to reset map bounds
      leaflet::addLayersControl(baseGroups = c("Esri World Street Map", "Open Street Map", "Esri World Terrain", "Esri World Imagery"), # Add layers control widget
                                options = layersControlOptions(collapsed = TRUE), position = "topleft") %>% 
      leafpm::addPmToolbar(toolbarOptions = leafpm::pmToolbarOptions(drawCircle = FALSE, drawMarker = FALSE, drawPolygon = FALSE, drawPolyline = FALSE, editMode = FALSE, cutPolygon = FALSE, removalMode = FALSE), # Add point/polygon drawing tools
                           drawOptions = leafpm::pmDrawOptions(snappable = FALSE, markerStyle = list(draggable = FALSE))
      ) %>% 
      leaflet::addMapPane("preloaded_areas_of_interest", zIndex = 200) %>%
      leaflet::addPolygons(
        data = aoi_polygons,
        color = grey(.5),
        fill = TRUE,
        fillOpacity = 0.1,
        highlightOptions = highlightOptions(color = grey(.2), fill = TRUE, fillOpacity = 0.1, bringToFront = TRUE),
        options = pathOptions(pane = "preloaded_areas_of_interest"), 
        layerId = ~aoi_name,
        group = "Preloaded Areas of Interest",
        label = aoi_polygons$aoi_name, 
        labelOptions = labelOptions(textOnly = TRUE, direction = "center", textsize = "15px", sticky = FALSE, style = list("color" = "black"))
      )
    
    m
    
  })
  
  # Set up actions following "Explore place" button press
  observeEvent(input$select_map_aoi, {
   
    shinyjs::hide("time_plot_panel")
    shinyjs::hide("data_output")
    
    if (input$pages != "DATA"){
      
      updateTabsetPanel(inputId = "pages", selected = "DATA")
      
    }
    
    if (!is.null(input$select_map_aoi) & input$select_map_aoi != ""){
      
      selected_map_aoi <- aoi_polygons[aoi_polygons$aoi_name == input$select_map_aoi, ]
      
      area_of_interest$boundary <- selected_map_aoi
      
      area_of_interest$bbox <- area_of_interest$boundary %>% 
        sf::st_bbox()
      
      m <- leafletProxy("main_map") %>%
        clearMarkerClusters() %>%
        clearMarkers() %>%
        clearControls() %>% 
        leaflet::flyToBounds(area_of_interest$bbox[[1]],
                             area_of_interest$bbox[[2]],
                             area_of_interest$bbox[[3]],
                             area_of_interest$bbox[[4]], 
                             options = list(animate = TRUE, duration = 1, easeLinearity = 0.1, noMoveStart = TRUE)
        ) 
      m
      
    }
     
  })

  # React to map click on preloaded areas of interest
  observeEvent(input$main_map_shape_click, {

    bslib::sidebar_toggle(id = "place_sidebar", open = TRUE)
    
    selected_map_aoi <- aoi_polygons[aoi_polygons$aoi_name == input$main_map_shape_click$id, ]
    
    area_of_interest$boundary <- selected_map_aoi
      
    area_of_interest$bbox <- area_of_interest$boundary %>% 
      sf::st_bbox()
    
    updateSelectizeInput(session = session, 
                         inputId = "select_map_aoi", 
                         selected = input$main_map_shape_click$id
    )
    
    m <- leafletProxy("main_map") %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      clearControls() %>% 
      leaflet::flyToBounds(area_of_interest$bbox[[1]],
                           area_of_interest$bbox[[2]],
                           area_of_interest$bbox[[3]],
                           area_of_interest$bbox[[4]], 
                           options = list(animate = TRUE, duration = 1, easeLinearity = 0.1, noMoveStart = TRUE)
      ) 
    m
    
  })
  
  # React to area of interest URL input
  observeEvent({
    input$get_aoi_from_url
  }, {

    if (!is.null(input$get_aoi_from_url) & input$get_aoi_from_url != ""){
      area_of_interest$boundary <- url_place()
    
    area_of_interest$bbox <- area_of_interest$boundary %>% 
      sf::st_bbox()
    
    m <- leafletProxy("main_map") %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      clearControls() %>% 
      leaflet::flyToBounds(area_of_interest$bbox[[1]],
                           area_of_interest$bbox[[2]],
                           area_of_interest$bbox[[3]],
                           area_of_interest$bbox[[4]], 
                           options = list(animate = TRUE, duration = 1, easeLinearity = 0.1, noMoveStart = TRUE)
      ) %>% 
      leaflet::addMapPane("area_of_interest", zIndex = 200) %>%
      leaflet::addPolygons(
        data = area_of_interest$boundary,
        color = grey(.2),
        fillOpacity = 0.1,
        fill = TRUE,
        options = pathOptions(pane = "area_of_interest"),
        group = "Area of Interest"
      )
    
    m
    
  }
  })
  
  # Set up data download and analysis following "Explore place" button press
  observeEvent(input$aoi_go, {
    
    if (!is.null(area_of_interest$boundary)){

    # Load cached data, if preloaded area of interest
    # if (input$select_map_aoi %in% gsub("_data.rds", "", list.files("data/outputs"))){
      if (input$select_map_aoi %in% gsub("_data.rds", "", aoi_available_outputs)){ 
      
      # aoi <- readRDS(paste0("data/outputs/", input$select_map_aoi, "_data.rds"))
      
      aoi <- read_github_rds(
          owner = "elimia-analytics",
          repo  = "california-biodiversity-trends-engine",
          path  = "data/outputs",
          file  = gsub(" ", "%20", paste0(input$select_map_aoi, "_data.rds"))
        )
      
      area_of_interest$boundary <- aoi$boundary
      area_of_interest$baseline <- aoi$baseline
      area_of_interest$bbox <- aoi$bbox
      area_of_interest$baseline_bbox <-  aoi$baseline_bbox
      area_of_interest$area <- aoi$area
      area_of_interest$hexes <- aoi$hexes
      area_of_interest$gbif_data <- aoi$gbif_data
      area_of_interest$gbif_data_filtered <- aoi$gbif_data
      area_of_interest$records_table <- aoi$records_table
      area_of_interest$species_table <- aoi$species_table
      area_of_interest$observers_table <- aoi$observers_table
      area_of_interest$visits_table <- aoi$visits_table
      area_of_interest$completeness_table <- aoi$completeness_table
      # area_of_interest$trends_table <- aoi$trends_table
      # area_of_interest$species_trends_list <- aoi$species_trends_list
      # area_of_interest$biggest_movers_table <- aoi$biggest_movers_table
      # area_of_interest$focal_species_trends_table <- aoi$focal_species_trends_table

    } else { 
      
    # Set up correct AOI workflow
    # area_of_interest <- 
      
    }
      
    updateSelectizeInput(inputId = "select_species", session = session,
                         choices = area_of_interest$gbif_data_filtered %>%
                           dplyr::pull(species) %>%
                           as.character() %>%
                           unique() %>%
                           sort(),
                         server = TRUE)
    
    shinyjs::show("time_plot_panel")
    shinyjs::show("data_output")
    
    } else {
      
      sendSweetAlert(session, type = "warning", title = "Oops!", text = "You need to select an area of interest!", closeOnClickOutside = TRUE)
      
    }
    
  })
  
  # Collapse sidebar before displaying downloaded data
  observe({
    if (!is.null(area_of_interest$gbif_data)){
    bslib::sidebar_toggle(id = "place_sidebar", open = FALSE)
    }
  }, autoDestroy = TRUE)

  observeEvent(input$select_species, {

    if (!is.null(input$select_species)){
      current_bounds$xmin <- current_bounds$xmin - 0.2
    } else {
      current_bounds$xmin <- current_bounds$xmin + 0.2
    }

  }, ignoreNULL = FALSE)
  
  # Display data downloaded on DATA tab
  observe({
    
    if (!is.null(area_of_interest$gbif_data) & input$pages == "DATA"){
    
    zoom <- input$main_map_zoom
    
    # Set up h3 hex resolution corresponding to each map zoom level
    res <- case_when(
      zoom <= 10 ~ 5,
      zoom <= 12 ~ 6,
      zoom <= 13 ~ 7,
      zoom <= 15 ~ 8,
      TRUE ~ 9
    )
    
    # Implement additional taxonomic/spatial filters on data displayed
    area_of_interest$gbif_data_filtered <- filtered_data()
    
    # Observe user actions that should lead to updates to data displayed
    if (res != current_res() | # Respond to zoom/spatial resolution changes
        input$metric_switch != current_metric() | # Respond to metric switch
        input$main_map_bounds$west != current_bounds$xmin | # Respond to change map bounds
        input$main_map_bounds$south != current_bounds$ymin | # Respond to change map bounds
        input$main_map_bounds$east != current_bounds$xmax | # Respond to change map bounds
        input$main_map_bounds$north != current_bounds$ymax | # Respond to change map bounds
        current_taxon() != center_taxon$name # | # Respond to taxon sunburst chart clicks 
        # !is.null(input$select_species) 
        ) {
    
      current_res(res)
      current_metric(input$metric_switch)
      current_bounds$xmin <- input$main_map_bounds$west
      current_bounds$ymin <- input$main_map_bounds$south
      current_bounds$xmax <- input$main_map_bounds$east
      current_bounds$ymax <- input$main_map_bounds$north
      current_taxon(center_taxon$name)
      
      bbox_coords <- matrix(
        c(max(c(area_of_interest$baseline_bbox[["xmin"]], input$main_map_bounds$west)),
          max(c(area_of_interest$baseline_bbox[["ymin"]], input$main_map_bounds$south)),
          max(c(area_of_interest$baseline_bbox[["xmax"]], input$main_map_bounds$east)),
          max(c(area_of_interest$baseline_bbox[["ymax"]], input$main_map_bounds$north))
          ),
        ncol = 2, byrow = TRUE)

      bbox_poly <- st_polygon(list(rbind(c(bbox_coords[1,1], bbox_coords[1,2]),
                                         c(bbox_coords[2,1], bbox_coords[1,2]),
                                         c(bbox_coords[2,1], bbox_coords[2,2]),
                                         c(bbox_coords[1,1], bbox_coords[2,2]),
                                         c(bbox_coords[1,1], bbox_coords[1,2])))) %>%
        st_sfc(crs = 4326)
      
      # Identify h3 hexes corresponding to baseline area at current map resolution
      h3_sf <- generate_h3_cells(bbox_poly, res)
      
      # Implement additional taxonomic/spatial filters on data displayed
      area_of_interest$gbif_data_filtered <- filtered_data()
      
      # if (!is.null(input$select_species)){
      #   center_taxon$name <- area_of_interest$gbif_data_filtered %>% 
      #     dplyr::filter(species %in% input$select_species) %>% 
      #     dplyr::pull(genus) %>% 
      #     head(1)
      #   current_species(input$select_species)
      # } else {
      #   # center_taxon$name <- "Life"
      #   current_taxon(center_taxon$name)
      # }
      
      # Count user-selected metric across hexes displayed
      area_of_interest$metric_count_hexes <- get_count_summary(
        records = area_of_interest$gbif_data_filtered %>% 
          sf::st_set_geometry(NULL) %>% 
          dplyr::filter(decimallatitude >= input$main_map_bounds$south & decimallatitude <= input$main_map_bounds$north & decimallongitude >= input$main_map_bounds$west & decimallongitude <= input$main_map_bounds$east),
        base_hexes = h3_sf,
        metric = input$metric_switch,
        resolution = paste0("h", res)
      )
      
      area_of_interest$metric_count_hexes <- area_of_interest$metric_count_hexes %>% 
        dplyr::filter(!is.na(metric)) 
      
      if (sum(!is.na(area_of_interest$metric_count_hexes$metric)) > 0){
      
      pal <- colorNumeric(palette = "Reds", domain = area_of_interest$metric_count_hexes$metric)
        
      m <- leafletProxy("main_map") %>%
        clearControls() %>% 
        clearGroup("h3") %>%
        clearGroup("Records") %>% 
        addPolygons(data = area_of_interest$metric_count_hexes,
                    fillColor = ~pal(area_of_interest$metric_count_hexes$metric),
                    fillOpacity = 0.8,
                    color = "transparent",
                    opacity = 0,
                    group = "h3") 

      
      if (input$metric_switch == "Locations"){
        m <- m %>%
          addLegend(
            position = "topleft",
            pal = pal,
            bins = 2,
            values = c(min(area_of_interest$metric_count_hexes$metric, na.rm = TRUE), max(area_of_interest$metric_count_hexes$metric, na.rm = TRUE)), 
            labels = c(min(area_of_interest$metric_count_hexes$metric, na.rm = TRUE), max(area_of_interest$metric_count_hexes$metric, na.rm = TRUE)),
            title = paste0("Species Inventory Completeness (%)")
          )
      } else {
        m <- m %>%
          addLegend(
            position = "topleft",
            pal = pal, 
            bins = 3,
            values = c(min(area_of_interest$metric_count_hexes$metric, na.rm = TRUE), max(area_of_interest$metric_count_hexes$metric, na.rm = TRUE)), 
            labels = c(min(area_of_interest$metric_count_hexes$metric, na.rm = TRUE), max(area_of_interest$metric_count_hexes$metric, na.rm = TRUE)),
            title = paste0("Number of ", input$metric_switch)
          )
       }
      } else {
        
        m <- leafletProxy("main_map") %>%
          clearMarkerClusters() %>%
          clearControls() %>% 
          clearMarkers() %>%  
          clearGroup("h3") %>% 
          clearGroup("Records")
        
      }

      m

    }

    # Map actual occurrences above a certain zoom level
    if (input$main_map_zoom > 15) {
      
      map_occ <- area_of_interest$gbif_data_filtered %>%
        dplyr::filter(decimallatitude >= input$main_map_bounds$south & decimallatitude <= input$main_map_bounds$north & decimallongitude >= input$main_map_bounds$west & decimallongitude <= input$main_map_bounds$east) %>%
        dplyr::mutate(
          gbifid = paste0("<a href='https://www.gbif.org/occurrence/", gbifid, "' target='_blank' onmousedown='event.stopPropagation();'>", gbifid, "</a>"),
          URL = paste0("<a href='https://www.gbif.org/occurrence/", gbifid, "' target='_blank' onmousedown='event.stopPropagation();'>", paste0("https://www.gbif.org/occurrence/", gbifid), "</a>"),
          point_color = case_when( # Color occurrences to reflect runburst chart colors
            kingdom == "Animalia" ~ "#FF7F0F90", 
            kingdom == "Plantae" ~ "#2CA02C90",
            kingdom == "Fungi" ~ "#D6272890",
            kingdom == "Chromista" ~ "#1F77B490",
            .default = "#66666680"
          )
          )
      
      if (nrow(map_occ) > 0){
        
        # Add occurrences to map
        m <- leafletProxy("main_map") %>%
          clearShapes() %>%
          clearControls() %>%
          clearGroup("Records") %>% 
          clearGroup("h3") %>%
          leaflet::addMapPane("records", zIndex = 400) %>%
        addGlPoints(
          data = map_occ, 
          layerId = "gbifid", 
          popup = TRUE,
          fillColor = map_occ$point_color,
          fillOpacity = 0.85, 
          group = "Records", 
          pane = "records",
          radius = 20
        )
      }
    }
    }
    
  })
  
  # React to user selections from DATA table
  observeEvent(input$records_table_rows_selected, {

  if (!is.null(input$records_table_rows_selected)){
    
    if (length(input$records_table_rows_selected) < nrow(area_of_interest$metric_table)){
      
      updateCheckboxInput(session = session, "select_all", value = FALSE)
      
    } else {
      
      updateCheckboxInput(session = session, "select_all", value = TRUE)
      
    }
    
    if (input$metric_switch != "Locations"){
      
    # If metric is Records, highlight selected occurrences
    if (input$metric_switch == "Records"){
      target_records <- gsub("<a href='https://www.gbif.org/occurrence/", "", purrr::map(strsplit(area_of_interest$metric_table[input$records_table_rows_selected, ]$URL, "' target"), 1))
      area_of_interest$points_selected <- area_of_interest$gbif_data_filtered %>% 
        dplyr::filter(gbifid %in% target_records)
      # area_of_interest$points_selected <- area_of_interest$metric_table[input$records_table_rows_selected, ]
    }
    
    # If metric is Species, highlight all occurrences for the species selected
    if (input$metric_switch == "Species"){
      target_sp <- area_of_interest$metric_table[input$records_table_rows_selected, ]$`scientific name`
      area_of_interest$points_selected <- area_of_interest$gbif_data_filtered %>% 
        dplyr::filter(species %in% target_sp)
    } 
      
    # If metric is Observers, highlight all occurrences for the observer selected
    if (input$metric_switch == "Observers"){
      target_obs <- area_of_interest$metric_table[input$records_table_rows_selected, ]$`observer name`
      area_of_interest$points_selected <- area_of_interest$gbif_data_filtered %>% 
        dplyr::filter(recordedby %in% target_obs)
    }
      
    # If metric is Visits, highlight all occurrences for the visit selected
    if (input$metric_switch == "Visits"){
      target_vis <- area_of_interest$metric_table[input$records_table_rows_selected, ]$visitID
      area_of_interest$points_selected <- area_of_interest$gbif_data_filtered %>% 
        dplyr::filter(visitID %in% target_vis)
    } 

    area_of_interest$points_selected <- area_of_interest$points_selected %>%
        dplyr::mutate(
          gbifid = paste0("<a href='https://www.gbif.org/occurrence/", gbifid, "' target='_blank' onmousedown='event.stopPropagation();'>", gbifid, "</a>"),
          URL = paste0("<a href='https://www.gbif.org/occurrence/", gbifid, "' target='_blank' onmousedown='event.stopPropagation();'>", paste0("https://www.gbif.org/occurrence/", gbifid), "</a>")
        ) %>%
        dplyr::arrange(desc(eventdate), species)
      
    m <- leafletProxy("main_map") %>%
      clearGroup("Selected") %>%
      leaflet::addMapPane("records_selected", zIndex = 2000) %>%
      addMarkers(
        data = area_of_interest$points_selected,
        lng = ~decimallongitude, 
        lat = ~decimallatitude, 
        layerId = ~gbifid,
        options = pathOptions(pane = "records_selected"),
        group = "Selected",
        popup = leafpop::popupTable(area_of_interest$points_selected %>%
                                      st_set_geometry(NULL) %>%
                                      dplyr::mutate(
                                        URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")
                                      ), row.numbers = FALSE, feature.id = FALSE),
        popupOptions = popupOptions(maxWidth = 300, autoPan = FALSE, keepInView = TRUE)
      )

    m

    } else {
      
      selected_poly <- h3jsr::cell_to_polygon(input = as.character(area_of_interest$metric_table$hexagon[input$records_table_rows_selected])) %>% 
        sf::st_transform(4326)
          
      m <- leafletProxy("main_map") %>%
        clearGroup("Selected") %>%
        leaflet::addMapPane("hex_selected", zIndex = 2000) %>%
        addPolygons(data = selected_poly, 
                      color = "#111111", 
                      fillOpacity = 0,
                      popup = "metric", 
                      # pane = "hex_selected",
                      group = "Selected") 
      
      m
      
    }
    
  } else {

    m <- leafletProxy("main_map") %>%
      clearGroup("Selected")

    area_of_interest$points_selected <- area_of_interest$gbif_data[1, ][-1, ]

  }

}, ignoreNULL = FALSE)

  
  observeEvent(input$select_all, {
    
    if (input$select_all){
      DT::dataTableProxy("records_table") %>% selectRows(selected = c(input$records_table_rows_selected, input$records_table_rows_current))
      updateCheckboxInput(session = session, "deselect_all", value = FALSE)
    }
    
  })
  
  observeEvent(input$deselect_all, {
    
    if (input$deselect_all){
      DT::dataTableProxy("records_table") %>% selectRows(selected = NULL)
      updateCheckboxInput(session = session, "select_all", value = FALSE)
    }
    
  })
 
  output$time_plot <- dygraphs::renderDygraph({

    if (!is.null(area_of_interest$gbif_data_filtered)){

      if (isTRUE(input$redo_search)){
        dat <- area_of_interest$gbif_data_filtered %>%
          dplyr::filter(decimallatitude >= input$main_map_bounds$south & decimallatitude <= input$main_map_bounds$north & decimallongitude >= input$main_map_bounds$west & decimallongitude <= input$main_map_bounds$east)
      } else {
        dat <- area_of_interest$gbif_data_filtered
      }

      if (input$metric_switch == "Records"){
        dat <- dat %>%
          sf::st_set_geometry(NULL) %>%
          dplyr::filter(complete.cases(year)) %>%
          dplyr::group_by(year) %>%
          dplyr::summarise(number_records = n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(year = paste0(year, "-01-01") %>% as.Date())

        dat <- xts::xts(x = dat$number_records, order.by = dat$year)

      }

      if (input$metric_switch == "Species"){
        dat <- dat %>%
          sf::st_set_geometry(NULL) %>%
          dplyr::filter(complete.cases(year)) %>%
          dplyr::group_by(year) %>%
          dplyr::summarise(number_species = n_distinct(species)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(year = paste0(year, "-01-01") %>% as.Date())

        dat <- xts::xts(x = dat$number_species, order.by = dat$year)

      }

      if (input$metric_switch == "Observers"){
        dat <- dat %>%
          sf::st_set_geometry(NULL) %>%
          dplyr::filter(complete.cases(year)) %>%
          dplyr::group_by(year) %>%
          dplyr::summarise(number_observers = n_distinct(recordedby)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(year = paste0(year, "-01-01") %>% as.Date())

        dat <- xts::xts(x = dat$number_observers, order.by = dat$year)
      }

      if (input$metric_switch == "Visits"){
        dat <- dat %>%
          sf::st_set_geometry(NULL) %>%
          dplyr::filter(complete.cases(year)) %>%
          dplyr::group_by(year) %>%
          dplyr::summarise(number_visits = n_distinct(visitID)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(year = paste0(year, "-01-01") %>% as.Date())

        dat <- xts::xts(x = dat$number_visits, order.by = dat$year)
      }
      
      if (input$metric_switch == "Locations"){
        dat <- dat %>%
          sf::st_set_geometry(NULL) %>%
          dplyr::filter(complete.cases(year)) %>%
          group_by(species) %>%
          summarize(first_year = min(year)) %>%
          count(first_year) %>%
          arrange(first_year) %>%
          mutate(cumulative_species = cumsum(n)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(first_year = paste0(first_year, "-01-01") %>% as.Date())
        
        dat <- xts::xts(x = 100*(dat$cumulative_species/max(dat$cumulative_species, na.rm = TRUE)), order.by = dat$first_year)
      }

      if (input$metric_switch == "Locations"){
        plot_lab <- paste0("Species Accumulation (%)")
      } else {
        plot_lab <- paste0("Number of ", input$metric_switch)
      }
      
      dygraph(dat, ylab = "") %>%
        dyBarChart() %>%
        dySeries("V1", label = plot_lab, color = "#83353C") %>%
        dyAxis(
          "y",
          axisLabelWidth = 0
        ) %>%
        dyAxis(
          name="x",
          axisLabelFormatter = "function(d){ return d.getFullYear() }"
        ) %>%
        dygraphs::dyRangeSelector() %>%
        dyOptions(drawGrid = FALSE)

    }
    
  })

  output$taxa_donut <- plotly::renderPlotly({
    
    if (!is.null(area_of_interest$gbif_data)){

      if (isTRUE(input$redo_search)){
        dat <- filtered_data() %>%
          dplyr::filter(decimallatitude >= input$main_map_bounds$south & decimallatitude <= input$main_map_bounds$north & decimallongitude >= input$main_map_bounds$west & decimallongitude <= input$main_map_bounds$east)
      } else {
        dat <- filtered_data()
      }
      
    dat <- dat %>%
      # dplyr::filter(decimallatitude >= input$main_map_bounds$south & decimallatitude <= input$main_map_bounds$north & decimallongitude >= input$main_map_bounds$west & decimallongitude <= input$main_map_bounds$east) %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::filter(complete.cases(kingdom, phylum, class, order, family, genus)) %>% 
      dplyr::distinct(kingdom, phylum, class, order, family, genus, .keep_all = TRUE) %>% 
      dplyr::mutate(
        classification_path = paste0(paste("Life", kingdom, phylum, class, order, family, genus, sep = "|"), "|")
      ) 
    
    idees <- purrr::map(c("kingdom", "phylum", "class", "order", "family", "genus"), function(x) dat[[x]] %>% unique()) %>% unlist() %>% na.omit() %>% as.character()
    parentals <- purrr::map(1:length(idees), function(i){
      target_class_path <- dat$classification_path[grep(paste0("\\|", idees[i], "\\|"), dat$classification_path)[1]]
      target_class_path_names <- (target_class_path %>% strsplit("\\|"))[[1]]
      target_class_path_names[grep(paste0("^", idees[i], "$"), target_class_path_names) - 1]
    })
    parentals <- c("", purrr::map(parentals, function(x) ifelse(length(x) > 0, x, "Life")) %>% unlist())
    idees <- c("Life", idees)
    
    trace1 <- list(
      leaf = list(opacity = 1),
      meta = list(columnNames = list(
        ids = "data.0.ids",
        labels = "data.0.labels",
        parents = "data.0.parents"
      )),
      type = "sunburst",
      level = center_taxon$name,
      idssrc = "kirudang:0:8e4421",
      ids = idees,
      maxdepth = 3,
      rotation = -4,
      labelssrc = "kirudang:0:21e923",
      labels = idees,
      parentssrc = "kirudang:0:dc20c3",
      parents = parentals,
      hovertemplate = idees
    )
    data <- list(trace1)
    layout <- list(
      font = list(
        size = 12,
        color = "rgb(165, 25, 25)",
        family = "Roboto"
      ),
      xaxis = list(
        range = c(-1, 4),
        autorange = TRUE,
        fixedrange = TRUE
      ),
      yaxis = list(
        range = c(-1, 4),
        autorange = TRUE,
        fixedrange = TRUE
      ),
      height = "100%",
      width = "100%",
      margin = list(
        b = 0,
        r = 0,
        l = 0,
        t = 0,
        pad = 0
      ),
      metasrc = "kirudang:0:023680",
      meta = c("white", "#EF553B", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "rgb(252,195,195)", "rgb(252,195,195)", "#00CC96", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(141,211,199)", "rgb(141,211,199)", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#ffe600", "#faf693", "#faf693", "#faf693", "#ffd857", "#ffd857", "#ffd857", "#ffd857", "#fff16b", "#fff16b", "#fff16b", "#fff16b", "#ffb300", "#ffb300", "#ffb300", "#ffb300", "#ffb300", "#fcffc2", "#fcffc2", "#ff5ac3", "#ff5ac3", "#ff5ac3", "#ff5ac3", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#AB63FA", "#AB63FA", "#AB63FA"),
      modebar = list(orientation = "v"),
      autosize = TRUE,
      dragmode = "select",
      clickmode = "event",
      hovermode = "x",
      hoverlabel = list(
        font = list(
          size = 12,
          color = "#000",
          family = "Droid Sans"
        ),
        align = "auto",
        bgcolor = "rgb(255, 255, 255)"
      ),
      separators = ", ",
      uniformtext = list(mode = FALSE),
      selectdirection = "v",
      sunburstcolorway = c("#FF7F0F90","#2CA02C90","#D6272890","#1F77B490","#66666680")
    )
    p <- plot_ly(source = "taxa_plot", customdata = idees) %>%
      config(displayModeBar = FALSE)
    p <- add_trace(p, leaf=trace1$leaf, meta=trace1$meta, mode=trace1$mode, type=trace1$type, level=trace1$level, idssrc=trace1$idssrc, ids=trace1$ids, maxdepth=trace1$maxdepth, rotation=trace1$rotation, labelssrc=trace1$labelssrc, labels=trace1$labels, parentssrc=trace1$parentssrc, parents=trace1$parents)
    p <- layout(p, font=layout$font, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, margin=layout$margin, metasrc=layout$metasrc, meta=layout$meta, modebar=layout$modebar, autosize=layout$autosize, dragmode=layout$dragmode, template=layout$template, clickmode=layout$clickmode, hovermode=layout$hovermode, hoverlabel=layout$hoverlabel, separators=layout$separators, uniformtext=layout$uniformtext, selectdirection=layout$selectdirection, sunburstcolorway=layout$sunburstcolorway, extendsunburstcolors=layout$extendsunburstcolors)
    p <- p %>% event_register("plotly_sunburstclick")
    
    p
    
    }
  })


  observeEvent(event_data(event = "plotly_sunburstclick",
                          source = "taxa_plot",
                          priority = "event"), {

                            if (isTRUE(input$redo_search)){
                              dat <- area_of_interest$gbif_data_filtered %>%
                                dplyr::filter(decimallatitude >= input$main_map_bounds$south & decimallatitude <= input$main_map_bounds$north & decimallongitude >= input$main_map_bounds$west & decimallongitude <= input$main_map_bounds$east)
                            } else {
                              dat <- area_of_interest$gbif_data_filtered
                            }

                            dat <- dat %>%
                              sf::st_set_geometry(NULL) %>%
                              dplyr::mutate(
                                classification_path = paste("Life", kingdom, phylum, class, order, family, genus, sep = "|")
                              )
                            
                            clicked_taxa$taxon <- c(clicked_taxa$taxon, clickData()[["customdata"]])
                            
                            if (!identical(clicked_taxa$taxon[length(clicked_taxa$taxon)], "Life")){

                              if (length(clicked_taxa$taxon) == 1){

                                center_taxon$name <- clickData()[["customdata"]]
                              }
                              if (length(clicked_taxa$taxon) > 1){

                                if (identical(unique(clicked_taxa$taxon), "Life")){

                                  center_taxon$name <- "Life"

                                } else {

                                  last <- clicked_taxa$taxon[length(clicked_taxa$taxon)]
                                  last_taxon_path <- dat$classification_path[grep(last, dat$classification_path)[1]]
                                  last_taxon_path_names <- (last_taxon_path %>% strsplit("\\|"))[[1]]
                                  last_taxon_path_names <- last_taxon_path_names[-length(last_taxon_path_names)]
                                  beforelast <- clicked_taxa$taxon[(length(clicked_taxa$taxon)-1)]
                                  beforelast_taxon_path <- dat$classification_path[grep(beforelast, dat$classification_path)[1]]
                                  beforelast_taxon_path_names <- (beforelast_taxon_path %>% strsplit("\\|"))[[1]]
                                  beforelast_taxon_path_names <- beforelast_taxon_path_names[-length(beforelast_taxon_path_names)]
                                  
                                  if (last != beforelast){
                                    if (sum(grepl(last, last_taxon_path_names)) > 0){
                                    if (grep(last, last_taxon_path_names) > grep(beforelast, last_taxon_path_names)){
                                      center_taxon$name <- last
                                    } else {
                                      center_taxon$name <- last_taxon_path_names[grep(last, last_taxon_path_names)-1]
                                      clicked_taxa$taxon[(length(clicked_taxa$taxon))] <- center_taxon$name
                                    }
                                    }
                                  } else if (last == beforelast){
                                    center_taxon$name <- last_taxon_path_names[grep(last, last_taxon_path_names)-1]
                                    clicked_taxa$taxon[(length(clicked_taxa$taxon))] <- center_taxon$name
                                  }
                                
                                }
                              }

                            }

                          })

  output$records_table <- DT::renderDataTable({
    
    if (is.null(area_of_interest$records_table)){
      
      if (isTRUE(input$redo_search)){
        dat <- area_of_interest$gbif_data_filtered %>%
          dplyr::filter(decimallatitude >= input$main_map_bounds$south & decimallatitude <= input$main_map_bounds$north & decimallongitude >= input$main_map_bounds$west & decimallongitude <= input$main_map_bounds$east)
      } else {
        dat <- area_of_interest$gbif_data_filtered
      }
      
    if (input$metric_switch == "Records"){
      
      # if (!is.null(input$select_species)){
      #   dat <- dat %>% 
      #     dplyr::filter(species %in% input$select_species)
      # }
      
    dat <- dat %>% 
      dplyr::arrange(desc(eventdate), species) 
    
    dat <- dat %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::mutate(decimallongitude = round(decimallongitude, 3), decimallatitude = round(decimallatitude, 3),
                    URL = paste0("<a href='https://www.gbif.org/occurrence/", gbifid, "' target='_blank' onmousedown='event.stopPropagation();'>", gbifid, "</a>")
      ) %>%
      dplyr::select(species, eventdate, URL, basisofrecord, institutioncode, decimallongitude, decimallatitude, coordinateuncertaintyinmeters, kingdom, phylum, class, order, family, genus) %>%
      dplyr::rename("scientific name" = species,
                    "date" = eventdate,
                    "record type" = basisofrecord,
                    "longitude" = decimallongitude,
                    "latitude" = decimallatitude,
                    "uncertainty (m)" = coordinateuncertaintyinmeters,
                    "institution code" = institutioncode
      )
    area_of_interest$metric_table <- dat
    area_of_interest$records_table <- dat
    
    }
    
    if (input$metric_switch == "Species"){

      dat <- dat %>% 
        dplyr::group_by(species, kingdom, phylum, class, order, family, genus) %>%
        dplyr::summarise(number_records = n(),
                         number_years_recorded = n_distinct(year)
        ) %>% 
        dplyr::arrange(desc(number_records))
      
      dat <- dat %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::select(species, number_records, number_years_recorded, kingdom, phylum, class, order, family, genus) %>%
        dplyr::rename("scientific name" = species,
                      "number records" = number_records,
                      "number years recorded" = number_years_recorded
        ) 
      
      area_of_interest$metric_table <- dat
      area_of_interest$species_table <- dat
      
    }
    
    if (input$metric_switch == "Observers"){

      dat <- dat %>% 
        dplyr::group_by(recordedby) %>%
        dplyr::summarise(number_records = n(),
                         number_species_recorded = n_distinct(species),
                         proportion_species_recorded = n_distinct(species)/n_distinct(area_of_interest$gbif_data_filtered$species),
                         number_years_recorded = n_distinct(year)
        ) %>% 
        dplyr::arrange(desc(number_records)) 
      
      dat <- dat %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::select(recordedby, number_records, number_species_recorded, proportion_species_recorded, number_years_recorded) %>%
        dplyr::rename("observer name" = recordedby,
                      "number records" = number_records,
                      "number species recorded" = number_species_recorded,
                      "proportion species recorded" = proportion_species_recorded,
                      "number years recorded" = number_years_recorded
        )
      
      area_of_interest$metric_table <- dat
      area_of_interest$observers_table <- dat
      
    }
    
    if (input$metric_switch == "Visits"){

      dat <- dat %>% 
        dplyr::group_by(visitID, recordedby, eventdate) %>%
        dplyr::summarise(number_records = n(),
                         number_species_recorded = n_distinct(species),
                         proportion_species_recorded = n_distinct(species)/n_distinct(area_of_interest$gbif_data_filtered$species)
        ) %>% 
        dplyr::arrange(desc(number_records))
      
      dat <- dat %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::select(visitID, recordedby, eventdate, number_records, number_species_recorded, proportion_species_recorded) %>%
        dplyr::rename("observer name" = recordedby,
                      "date" = eventdate,
                      "number records" = number_records,
                      "number species recorded" = number_species_recorded,
                      "proportion species recorded" = proportion_species_recorded
        )
      
      area_of_interest$metric_table <- dat
      area_of_interest$visits_table <- dat

    }
      
      if (input$metric_switch == "Locations"){

          zoom <- input$main_map_zoom
          
          res <- case_when(
            zoom <= 10 ~ 5,
            zoom <= 12 ~ 6,
            zoom <= 13 ~ 7,
            zoom <= 15 ~ 8,
            TRUE ~ 9
          )
          
          resolution <- paste0("h", res)
          
        dat <- dat %>% 
          dplyr::group_by(.data[[resolution]]) %>%
          dplyr::summarise(number_records = n(),
                           number_species_recorded = n_distinct(species),
                           proportion_species_recorded = as.numeric(round((n_distinct(species)/n_distinct(area_of_interest$gbif_data_filtered$species)), 3)),
          ) %>% 
          dplyr::arrange(desc(proportion_species_recorded)) %>% 
          dplyr::mutate(
            proportion_species_recorded = paste0(100*proportion_species_recorded, "%")
          )
        
        dat <- dat %>%
          sf::st_set_geometry(NULL) %>%
          dplyr::select(all_of(c(resolution, "number_records", "number_species_recorded", "proportion_species_recorded"))) %>%
          dplyr::rename(
                        "number records" = number_records,
                        "number species recorded" = number_species_recorded,
                        "species inventory completeness" = proportion_species_recorded
          )
        
        area_of_interest$metric_table <- dat
        area_of_interest$completeness_table <- dat
        
      }
    } else {

      if (isTRUE(input$redo_search)){
        filtered_dat <- area_of_interest$gbif_data_filtered %>%
          dplyr::filter(decimallatitude >= input$main_map_bounds$south & decimallatitude <= input$main_map_bounds$north & decimallongitude >= input$main_map_bounds$west & decimallongitude <= input$main_map_bounds$east)
      } else {
        filtered_dat <- area_of_interest$gbif_data_filtered
      }
      
      if (input$metric_switch == "Records"){
        dat <- area_of_interest$records_table %>% 
          dplyr::filter(
            URL %in% paste0("<a href='https://www.gbif.org/occurrence/", filtered_dat$gbifid, "' target='_blank' onmousedown='event.stopPropagation();'>", filtered_dat$gbifid, "</a>")
          )
        area_of_interest$metric_table <- dat
        
      }
      if (input$metric_switch == "Species"){
        dat <- area_of_interest$species_table %>% 
          dplyr::filter(
            `scientific name` %in% filtered_dat$species
          )
        area_of_interest$metric_table <- dat

      }
      
      if (input$metric_switch == "Observers"){
        dat <- area_of_interest$observers_table %>% 
          dplyr::filter(
            `observer name` %in% filtered_dat$recordedby
          ) %>% 
          dplyr::mutate(
            proportion_species_recorded = as.numeric(round(`number species recorded`/nrow(area_of_interest$species_table), 3))
          ) %>% 
          dplyr::arrange(desc(proportion_species_recorded)) %>% 
          dplyr::mutate(
            `proportion species recorded` = paste0(100*proportion_species_recorded, "%")
          ) %>% 
          dplyr::select(-proportion_species_recorded)
        area_of_interest$metric_table <- dat

      }
      
      if (input$metric_switch == "Visits"){
        
        # if (nrow(area_of_interest$visits_table) != nrow(filtered_data())){
          dat <- area_of_interest$visits_table[which(area_of_interest$visits_table$visitID %in% (filtered_dat %>% dplyr::pull(visitID) %>% unique())), ] %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
              proportion_species_recorded = as.numeric(round(`number species recorded`/nrow(area_of_interest$species_table), 3))
            ) %>% 
            dplyr::arrange(desc(proportion_species_recorded)) %>% 
            dplyr::mutate(
              `proportion species recorded` = paste0(100*proportion_species_recorded, "%")
            ) %>% 
            dplyr::select(-proportion_species_recorded)
            
        # } else {
          # dat <- area_of_interest$visits_table
        # }

        area_of_interest$metric_table <- dat
        
      }

      if (input$metric_switch == "Locations"){
        
        dat <- area_of_interest$gbif_data_filtered %>%
          dplyr::filter(decimallatitude >= input$main_map_bounds$south & decimallatitude <= input$main_map_bounds$north & decimallongitude >= input$main_map_bounds$west & decimallongitude <= input$main_map_bounds$east)
        
        zoom <- input$main_map_zoom
        
        res <- case_when(
          zoom <= 10 ~ 5,
          zoom <= 12 ~ 6,
          zoom <= 13 ~ 7,
          zoom <= 15 ~ 8,
          TRUE ~ 9
        )
        
        resolution <- paste0("h", res)
        
        dat <- dat %>% 
          dplyr::group_by(.data[[resolution]]) %>%
          dplyr::summarise(number_records = n(),
                           number_species_recorded = n_distinct(species),
                           proportion_species_recorded = as.numeric(round((n_distinct(species)/n_distinct(area_of_interest$gbif_data_filtered$species)), 3)),
          ) %>% 
          dplyr::arrange(desc(proportion_species_recorded)) %>% 
          dplyr::mutate(
            proportion_species_recorded = paste0(100*proportion_species_recorded, "%")
          )
        
        dat <- dat %>%
          sf::st_set_geometry(NULL) %>%
          dplyr::select(all_of(c(resolution, "number_records", "number_species_recorded", "proportion_species_recorded"))) %>%
          dplyr::rename(
            "number records" = number_records,
            "number species recorded" = number_species_recorded,
            "species inventory completeness" = proportion_species_recorded
          )
        
        names(dat)[1] <- "hexagon"
        
        area_of_interest$metric_table <- dat

      }
    }
    
    if (input$metric_switch == "Trends"){
      dat <- area_of_interest$metric_table[1, ][-1, ] %>%
        sf::st_set_geometry(NULL)
    }
    
    dat %>% 
      dplyr::select(-any_of("visitID")) %>% 
      datatable(options = list(dom = 'tp',
                               pageLength = 10,
                               columnDefs = list(
                                 list(width = "17%", targets = 0)),
                               language = list(emptyTable = '')
      ),
      selection = list(mode = 'multiple', target = 'row', selected = NULL),
      escape = FALSE,
      rownames = FALSE
      )
    
  })

  observe({

    if (!is.null(area_of_interest$gbif_data) & input$pages == "TRENDS"){

      if (input$select_map_aoi %in% gsub("_data.rds", "", aoi_available_outputs)){ 
        
        aoi <- read_github_rds(
          owner = "elimia-analytics",
          repo  = "california-biodiversity-trends-engine",
          path  = "data/outputs",
          file  = gsub(" ", "%20", paste0(input$select_map_aoi, "_data.rds"))
        )

        area_of_interest$trends_table <- aoi$trends_table
        area_of_interest$species_trends_list <- aoi$species_trends_list
        area_of_interest$biggest_movers_table <- aoi$biggest_movers_table
        area_of_interest$focal_species_trends_table <- aoi$focal_species_trends_table

      } else {

      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())

      progress$set(message = "Calculating Species Trends", value = 0)

      area_of_interest$species_trends_list <- purrr::map(area_of_interest$trends_table$species, function(sp){

        # progress$inc(1/length(area_of_interest$trends_table$species), detail = paste0("Detecting trend for ", sp))

        out <- purrr::safely(get_species_trends)(analysis_records = area_of_interest$gbif_data,
                           focal_taxon = sp,
                           use_reference_taxon = area_of_interest$trends_table %>% dplyr::filter(species == sp) %>% dplyr::pull(full) %>% isFALSE(),
                           species_association_matrix = area_of_interest$species_association_matrix,
                           full = area_of_interest$trends_table %>% dplyr::filter(species == sp) %>% dplyr::pull(full),
                           resolution = "h5"
                           )
        out$result

      }) %>% purrr::set_names(area_of_interest$trends_table$species)

      species_metrics_df <- get_yearly_trend_metrics(area_of_interest$species_trends_list)

      area_of_interest$biggest_movers_table <- area_of_interest$trends_table %>%
        dplyr::ungroup() %>%
        dplyr::left_join(species_metrics_df, by = "species") %>%
        dplyr::mutate(trend = case_when(
          (reporting_rate_above_last_ten >= 5  & reporting_rate_below_last_ten < 3) | reporting_rate_trend_last_five >= 0.7 | reporting_rate_trend_last_ten >= 0.7 ~ "increasing",
          (reporting_rate_above_last_ten < 5  & reporting_rate_below_last_ten >= 3) | reporting_rate_trend_last_five <= -0.5 | reporting_rate_trend_last_ten <= -0.5 ~ "decreasing",
          is.na(reporting_rate_above_last_ten) ~ "needs more data",
          .default = "stable"
        ),
        trend_icon = case_when(
          trend == "increasing" ~ as.character(icon("arrow-up", "fa-2x", style = "color: #67a9cf;")),
          trend == "decreasing" ~ as.character(icon("arrow-down", "fa-2x", style = "color: #ef8a62;")),
          trend == "needs more data" ~ as.character(icon("question", "fa-2x", style = "color: #BEBEBE;")),
          trend == "stable" ~ as.character(icon("equals", "fa-2x", style = "color: #BEBEBE;"))
        ),
        reporting_rate_trend_last_five = round(reporting_rate_trend_last_five, 3),
        reporting_rate_trend_last_ten = round(reporting_rate_trend_last_ten, 3)
        ) %>%
        dplyr::select(species, trend, trend_icon, number_records, number_years_recorded, reporting_rate_above_last_ten, reporting_rate_below_last_ten, reporting_rate_trend_last_five, reporting_rate_trend_last_ten)

      }

      area_of_interest$focal_species_trends_table <- rbind(
        area_of_interest$biggest_movers_table %>%
          dplyr::filter(trend == "increasing") %>%
          dplyr::arrange(desc(reporting_rate_above_last_ten), desc(reporting_rate_trend_last_five), desc(reporting_rate_trend_last_ten)) %>%
          dplyr::slice_head(n = 5),
        area_of_interest$biggest_movers_table %>%
          dplyr::filter(trend == "decreasing") %>%
          dplyr::arrange(desc(reporting_rate_below_last_ten), reporting_rate_trend_last_five, reporting_rate_trend_last_ten) %>%
          dplyr::slice_head(n = 5)
      )

      output$trends_table <- DT::renderDataTable({

        out_tab <- area_of_interest$focal_species_trends_table %>%
          dplyr::select(-trend) %>%
          dplyr::left_join(area_of_interest$gbif_data %>% sf::st_set_geometry(NULL) %>% dplyr::filter(species %in% area_of_interest$focal_species_trends_table$species) %>% dplyr::select(species, specieskey) %>% dplyr::distinct(.keep_all = TRUE), by = "species") %>%
          dplyr::mutate(
            species = paste0("<a href='https://www.gbif.org/species/", specieskey, "' target='_blank' onmousedown='event.stopPropagation();'>", species, "</a>")
          ) %>%
          dplyr::select(-specieskey) %>%
          dplyr::rename(
            "Species" = species,
            "Trend" = trend_icon,
            "Number of records" = number_records,
            "Number of years recorded" = number_years_recorded,
            "High reporting rate instances" = reporting_rate_above_last_ten,
            "Low reporting rate instances" = reporting_rate_below_last_ten,
            "Reporting rate trend in last 5 years" = reporting_rate_trend_last_five,
            "Reporting rate trend in last 10 years" = reporting_rate_trend_last_ten
          )

        out_tab <- out_tab %>%
          datatable(options = list(dom = 'tp',
                                   pageLength = 10,
                                   columnDefs = list(list(width = "25%", targets = 0)),
                                   language = list(emptyTable = '')
          ),
          selection = list(mode = 'single', target = 'row', selected = 1),
          escape = FALSE,
          rownames = FALSE
          )

        if (sum(area_of_interest$focal_species_trends_table$trend == "decreasing") > 0) out_tab <- out_tab %>% formatStyle(columns = 1:ncol(area_of_interest$focal_species_trends_table), backgroundColor = styleRow(rows = which(area_of_interest$focal_species_trends_table$trend == "decreasing"), values = "#ef8a6250"))
        if (sum(area_of_interest$focal_species_trends_table$trend == "increasing") > 0) out_tab <- out_tab %>% formatStyle(columns = 1:ncol(area_of_interest$focal_species_trends_table), backgroundColor = styleRow(rows = which(area_of_interest$focal_species_trends_table$trend == "increasing"), values = "#67a9cf50"))

        out_tab

      })

    }

      updateSelectizeInput(session = session, inputId = "select_species_trend", choices = c("", area_of_interest$biggest_movers_table$species))

      # }

    })

  observeEvent({
    input$species_trends_tabs
    }, {

    if (!is.null(area_of_interest$gbif_data) & input$pages == "TRENDS"){

    # Save the object to the global environment
    if (input$species_trends_tabs == "At a Glance"){

      area_of_interest$focal_species_trends_table <- rbind(
        area_of_interest$biggest_movers_table %>%
          dplyr::filter(trend == "increasing") %>%
          dplyr::arrange(desc(reporting_rate_above_last_ten), desc(reporting_rate_trend_last_five), desc(reporting_rate_trend_last_ten)) %>%
          dplyr::slice_head(n = 5),
        area_of_interest$biggest_movers_table %>%
          dplyr::filter(trend == "decreasing") %>%
          dplyr::arrange(desc(reporting_rate_below_last_ten), reporting_rate_trend_last_five, reporting_rate_trend_last_ten) %>%
          dplyr::slice_head(n = 5)
      )

    }

    if (input$species_trends_tabs == "Decreasing Species"){
      area_of_interest$focal_species_trends_table <- area_of_interest$biggest_movers_table %>%
        dplyr::filter(trend == "decreasing") %>%
        dplyr::arrange(desc(reporting_rate_below_last_ten), reporting_rate_trend_last_five, reporting_rate_trend_last_ten)
    }

    if (input$species_trends_tabs == "Increasing Species"){
      area_of_interest$focal_species_trends_table <- area_of_interest$biggest_movers_table %>%
        dplyr::filter(trend == "increasing") %>%
        dplyr::arrange(desc(reporting_rate_above_last_ten), desc(reporting_rate_trend_last_five), desc(reporting_rate_trend_last_ten))
    }

    if (input$species_trends_tabs == "All Species"){
      area_of_interest$focal_species_trends_table <- area_of_interest$biggest_movers_table %>%
        dplyr::arrange(species)
    }

    output$trends_table <- DT::renderDataTable({

      out_tab <- area_of_interest$focal_species_trends_table %>%
        dplyr::select(-trend) %>%
        dplyr::left_join(area_of_interest$gbif_data %>% sf::st_set_geometry(NULL) %>% dplyr::filter(species %in% area_of_interest$focal_species_trends_table$species) %>% dplyr::select(species, specieskey) %>% dplyr::distinct(.keep_all = TRUE), by = "species") %>%
        dplyr::mutate(
          species = paste0("<a href='https://www.gbif.org/species/", specieskey, "' target='_blank' onmousedown='event.stopPropagation();'>", species, "</a>")
        ) %>%
        dplyr::select(-specieskey) %>%
        dplyr::rename(
          "Species" = species,
          "Trend" = trend_icon,
          "Number of records" = number_records,
          "Number of years recorded" = number_years_recorded,
          "High reporting rate instances" = reporting_rate_above_last_ten,
          "Low reporting rate instances" = reporting_rate_below_last_ten,
          "Reporting rate trend in last 5 years" = reporting_rate_trend_last_five,
          "Reporting rate trend in last 10 years" = reporting_rate_trend_last_ten
        )

      out_tab <- out_tab %>%
        datatable(options = list(dom = 'tp',
                                 pageLength = 10,
                                 columnDefs = list(list(width = "25%", targets = 0)),
                                 language = list(emptyTable = '')
        ),
        selection = list(mode = 'single', target = 'row', selected = 1),
        escape = FALSE,
        rownames = FALSE
        )

      if (sum(area_of_interest$focal_species_trends_table$trend == "decreasing") > 0) out_tab <- out_tab %>% formatStyle(columns = 1:ncol(area_of_interest$focal_species_trends_table), backgroundColor = styleRow(rows = which(area_of_interest$focal_species_trends_table$trend == "decreasing"), values = "#ef8a6250"))
      if (sum(area_of_interest$focal_species_trends_table$trend == "increasing") > 0) out_tab <- out_tab %>% formatStyle(columns = 1:ncol(area_of_interest$focal_species_trends_table), backgroundColor = styleRow(rows = which(area_of_interest$focal_species_trends_table$trend == "increasing"), values = "#67a9cf50"))

      out_tab

    })

    updateSelectizeInput(session = session, inputId = "select_species_trend", choices = c("", area_of_interest$focal_species_trends_table$species))

    }
  })
  observeEvent({
    input$select_species_trend
  }, {

  if (!is.null(input$select_species_trend)){
    if (input$select_species_trend != ""){
      area_of_interest$focal_species_trends_table <- area_of_interest$biggest_movers_table %>%
        dplyr::filter(species == input$select_species_trend)
    }
  }
  })

  observeEvent({
    input$trends_table_rows_selected
    }, {

      if (!is.null(input$trends_table_rows_selected)){

        if (input$species_trends_tabs == "At a Glance"){
          focal_species <- rbind(
            area_of_interest$biggest_movers_table %>%
              dplyr::filter(trend == "increasing") %>%
              dplyr::arrange(desc(reporting_rate_above_last_ten), desc(reporting_rate_trend_last_five), desc(reporting_rate_trend_last_ten)) %>%
              dplyr::slice_head(n = 5),
            area_of_interest$biggest_movers_table %>%
              dplyr::filter(trend == "decreasing") %>%
              dplyr::arrange(desc(reporting_rate_below_last_ten), reporting_rate_trend_last_five, reporting_rate_trend_last_ten) %>%
              dplyr::slice_head(n = 5)
          ) %>%
            dplyr::pull(species)
        }

        if (input$species_trends_tabs == "Decreasing Species"){
          focal_species <- area_of_interest$biggest_movers_table %>%
            dplyr::filter(trend == "decreasing") %>%
            dplyr::arrange(desc(reporting_rate_below_last_ten), reporting_rate_trend_last_five, reporting_rate_trend_last_ten) %>%
            dplyr::pull(species)
        }

        if (input$species_trends_tabs == "Increasing Species"){
          focal_species <- area_of_interest$biggest_movers_table %>%
            dplyr::filter(trend == "increasing") %>%
            dplyr::arrange(desc(reporting_rate_above_last_ten), desc(reporting_rate_trend_last_five), desc(reporting_rate_trend_last_ten)) %>%
            dplyr::pull(species)
        }

        if (input$species_trends_tabs == "All Species"){
          focal_species <- area_of_interest$biggest_movers_table %>%
            dplyr::arrange(species) %>%
            dplyr::pull(species)
        }

        if (!is.null(input$select_species_trend)){
          if (input$select_species_trend != ""){
            focal_species <- input$select_species_trend
          }
        }

        focal_taxon_name <- focal_species[input$trends_table_rows_selected]

        output$species_trends_output <- plotly::renderPlotly({

          assign("sd_dat", area_of_interest$species_trends_list[[focal_taxon_name]]$yearly_trend, pos = 1)
          
          yearly_plot <- plot_yearly_trends(area_of_interest$species_trends_list[[focal_taxon_name]]$yearly_trend, metric = "reporting_rate")

          yearly_plot

        })

        # Create web map and add basic elements and functionality
        output$trends_map <- leaflet::renderLeaflet({

          spatiotemporal_trends_data <- area_of_interest$species_trends_list[[focal_taxon_name]]$spatiotemporal_trend

          resolution <- names(spatiotemporal_trends_data)[grep("^h", names(spatiotemporal_trends_data))]
          spatiotemporal_trends_wide <- spatiotemporal_trends_data %>%
            dplyr::select(all_of(c("year", resolution, "reporting_rate_sd"))) %>%
            pivot_wider(names_from = year, values_from = reporting_rate_sd)
          base_hexes <- h3jsr::cell_to_polygon(spatiotemporal_trends_wide[[resolution]], simple = FALSE)
          names(base_hexes)[which(names(base_hexes) == "h3_address")] <- resolution
          spatiotemporal_trends_data <- base_hexes %>%
            dplyr::left_join(spatiotemporal_trends_wide, by = resolution)
          # hexes_data

          spatiotemporal_trends_data_pts <- spatiotemporal_trends_data %>% st_centroid()

          m <- leaflet::leaflet(options = leafletOptions(zoomDelta = 0.5, zoomSnap = 0, attributionControl = FALSE, worldCopyJump = FALSE, minZoom = 10)) %>% # Open new leaflet web map
            leaflet::flyToBounds(area_of_interest$baseline_bbox[[1]],
                                 area_of_interest$baseline_bbox[[2]],
                                 area_of_interest$baseline_bbox[[3]],
                                 area_of_interest$baseline_bbox[[4]],
                                 options = list(animate = TRUE, duration = 1, easeLinearity = 0.1, noMoveStart = TRUE)
            ) %>%
            leaflet::addMapPane("area_of_interest", zIndex = 200) %>%
            leaflet::addPolygons(
              data = area_of_interest$boundary,
              color = grey(.2),
              fillOpacity = 0.1,
              fill = TRUE,
              options = pathOptions(pane = "area_of_interest"),
              group = "Area of Interest"
            ) %>%
            leaflet::addMapPane("basemap1", zIndex = -100) %>% # Add basemap 1
            leaflet::addProviderTiles(providers$Esri.WorldTerrain, group = "Esri World Terrain", options = list(pathOptions(pane = "basemap1")),
                                      providerTileOptions(
                                        updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                                        updateWhenIdle = TRUE           # map won't load new tiles when panning
                                      )) %>%
            leaflet::addMapPane("basemap2", zIndex = -100) %>% # Add basemap 3
            leaflet::addProviderTiles(providers$OpenStreetMap, group = "Open Street Map", options = list(pathOptions(pane = "basemap2"))) %>%
            leaflet::addScaleBar(position = "bottomleft") %>% # Add scale bar
            leaflet.extras::addResetMapButton() %>% # Add button to reset map bounds
            leaflet::addLayersControl(baseGroups = c("Esri World Terrain", "Open Street Map"), # Add layers control widget
                                      options = layersControlOptions(collapsed = TRUE), position = "topleft")

          spatiotemporal_trends_data_pts_df <- spatiotemporal_trends_data_pts %>% dplyr::select(-all_of(names(spatiotemporal_trends_data_pts)[1])) %>% st_set_geometry(NULL)

          spatiotemporal_trends_data_list <- list(
            below = spatiotemporal_trends_data_pts_df %>% mutate(across(everything(), ~ ifelse(. < 0, ., NA))),
            above = spatiotemporal_trends_data_pts_df %>% mutate(across(everything(), ~ ifelse(. > 0, ., NA)))
          )

          # generate_tooltips <- function(dat) {
          #   dat_location <- dat %>%
          #     # st_set_geometry(NULL) %>%
          #     dplyr::select(where(~sum(!is.na(.)) > 0))
          #   dat_location <- dat_location[, -1]
          #   dat_location <- dat_location[sort(names(dat_location), decreasing = TRUE)]
          #   location_popup <- purrr::map_chr(names(dat_location), function(yr){
          #     paste0(yr, ": ", round(dat_location[[yr]], 3))
          #   }) %>% paste0(collapse = "<br>")
          #   # location_popup <- paste0("<div style='max-height: 100px; width: 100px; overflow-y: scroll;'>",
          #   #                          location_popup,
          #   #                          "</div>"
          #   # )
          #   return(location_popup)
          # }
          #
          # tips_below <- purrr::map(1:nrow(spatiotemporal_trends_data_list$below), function(i){
          #   generate_tooltips(spatiotemporal_trends_data_list$below[i, ])
          # })
          #
          # tips_above <- purrr::map(1:nrow(spatiotemporal_trends_data_list$above), function(i){
          #   generate_tooltips(spatiotemporal_trends_data_list$above[i, ])
          # })

          spatial_pattern <- area_of_interest$species_trends_list[[focal_taxon_name]]$spatial_pattern
          spatial_pattern <- spatial_pattern[match(spatiotemporal_trends_data_pts$h5, spatial_pattern$h5), ]
          count_pal <- colorNumeric("Blues", spatial_pattern$focal_species_count, na.color = "transparent")

          m <- m %>%
            addPolygons(
              data = spatiotemporal_trends_data,
              fillOpacity = 0.3,
              fillColor = ~count_pal(spatial_pattern$focal_species_count),
              color = grey(.2),
              weight = 0.5,
              opacity = 0.5,
              popup = leafpop::popupTable(spatial_pattern, row.numbers = FALSE, feature.id = FALSE),
            ) %>%
            addMinicharts(
              lng = st_coordinates(spatiotemporal_trends_data_pts)[, 1],
              lat = st_coordinates(spatiotemporal_trends_data_pts)[, 2]-0.0001,
              chartdata = spatiotemporal_trends_data_list$below %>% as.matrix() %>% round(3),
              type = "bar",
              width = 120,
              height = 60,
              maxValues = 4,
              colorPalette = "#ef8a62",
              legend = FALSE,
              #popup = popupArgs(html = tips_below)
            ) %>%
            addMinicharts(
              lng = st_coordinates(spatiotemporal_trends_data_pts)[, 1],
              lat = st_coordinates(spatiotemporal_trends_data_pts)[, 2],
              chartdata = spatiotemporal_trends_data_list$above %>% as.matrix() %>% round(3),
              type = "bar",
              width = 120,
              height = 60,
              maxValues = 4,
              colorPalette = "#67a9cf",
              legend = FALSE,
              #popup = popupArgs(html = tips_above)
            )

          m

        })

        output$association_table <- DT::renderDataTable({

        focal_associated_species <- get_associated_species(focal_species = focal_taxon_name, analysis_records = area_of_interest$gbif_data_filtered, num_species = 50)

        focal_associated_species_table <- area_of_interest$trends_table %>%
          dplyr::ungroup() %>%
          dplyr::filter(species %in% focal_associated_species$species) %>%
          dplyr::left_join(focal_associated_species, by = "species") %>%
          dplyr::select(species, kingdom, shared_visits, number_records, number_years_recorded) %>%
          dplyr::arrange(desc(shared_visits))

        focal_associated_species_table %>%
          dplyr::left_join(area_of_interest$gbif_data %>% sf::st_set_geometry(NULL) %>% dplyr::filter(species %in% focal_associated_species_table$species) %>% dplyr::select(species, specieskey) %>% dplyr::distinct(.keep_all = TRUE), by = "species") %>%
          dplyr::mutate(
            species = paste0("<a href='https://www.gbif.org/species/", specieskey, "' target='_blank' onmousedown='event.stopPropagation();'>", species, "</a>")
          ) %>%
          dplyr::select(-specieskey) %>%
          dplyr::rename(
            "Species" = "species",
            "Kingdom" = "kingdom",
            "Times observed with focal species" = "shared_visits",
            "Number of records" = "number_records",
            "Number of years recorded" = "number_years_recorded"
          ) %>%
        datatable(class="hover compact",
                  options = list(dom = 'tp',
                                 pageLength = 10,
                                 language = list(emptyTable = ''),
                                 pagingType = "simple",
                                 scrollX = TRUE,
                                 scrollY = TRUE,
                                 autoWidth = FALSE
        ),
        selection = "none",
        escape = FALSE,
        rownames = FALSE
        )

        #
        #   coords <- as.data.frame(area_of_interest$species_associations$loadings)
        #   rownames(coords) <- area_of_interest$species_associations$obj$fitted.values %>% colnames()
        #
        #
        #   # Bounding box for continental U.S.
        #   lon_range <- c(-125, -66)
        #   lat_range <- c(25, 49)
        #   # Normalize NMDS coordinates to [0, 1]
        #   coords$Factor1_norm <- (coords$Factor1 - min(coords$Factor1)) / (max(coords$Factor1) - min(coords$Factor1))
        #   coords$Factor2_norm <- (coords$Factor2 - min(coords$Factor2)) / (max(coords$Factor2) - min(coords$Factor2))
        #
        #   # Map to lon/lat
        #   coords$lon <- lon_range[1] + coords$Factor1_norm * diff(lon_range)
        #   coords$lat <- lat_range[1] + coords$Factor2_norm * diff(lat_range)
        #
        #   coords_sf <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
        #
        #   focal_species <- gsub(" ", "_", focal_taxon_name)
        #   focal_species_coords <- coords_sf[rownames(coords_sf) == focal_species, ]
        #
        #   focal_associations_coords <- coords_sf[rownames(coords_sf) %in% (area_of_interest$species_association_matrix[, focal_species] %>% sort(decreasing = TRUE) %>% head(30) %>% names()), ]
        #
        #
        #   focal_associations_bbox <- st_bbox(focal_associations_coords)
        #
        #   m <- leaflet(options = leafletOptions(minZoom = 4, maxZoom = 8, attributionControl = FALSE)) %>%
        #     htmlwidgets::onRender("
        #                            function(el, x) {
        #                              el.style.background = '#ffffff';
        #                                            }
        #                           ") %>%
        #     leaflet::setView(lng = st_coordinates(focal_species_coords)[1, 1] %>% as.numeric(),
        #                      lat = st_coordinates(focal_species_coords)[1, 2] %>% as.numeric(),
        #                      zoom = 4
        #     ) %>%
        #     addCircleMarkers(data = coords_sf,
        #                      radius = 5,
        #                      label = ~rownames(coords),
        #                      color = "#999999",
        #                      opacity = 0.5,
        #                      stroke = FALSE,
        #                      fillOpacity = 0.5) %>%
        #     addCircleMarkers(data = focal_associations_coords[!rownames(focal_associations_coords) == focal_species, ],
        #                      radius = 5,
        #                      label = ~rownames(focal_associations_coords[!rownames(focal_associations_coords) == focal_species, ]),
        #                      color = "#808080",
        #                      fillColor = "#808080",
        #                      opacity = 0.9,
        #                      stroke = TRUE,
        #                      weight = 2,
        #                      fillOpacity = 0.9) %>%
        #     addCircleMarkers(data = focal_species_coords,
        #                      radius = 8,
        #                      label = ~rownames(focal_species_coords),
        #                      color = "#a50f15",
        #                      fillColor = "#a50f15",
        #                      opacity = 0.9,
        #                      stroke = TRUE,
        #                      weight = 3,
        #                      fillOpacity = 0.9) %>%
        #     leaflet::addLegend(
        #       position = "bottomleft",
        #       colors = c("#a50f15", "#808080", "#999999"),
        #       opacity = c(0.9, 0.9, 0.5),
        #       labels = c("Focal taxon", "Reference taxa </br> (most frequently recorded with focal taxon)", "Other taxa")
        #     )
        #
        #   ttl <- tags$div(
        #     HTML('Taxa most frequently recorded with focal taxon')
        #   )
        #
        #   m <- m %>%
        #     addControl(ttl, position = "topright")
        #
        #   m
        #

        })

        # assign("area_of_interest",
        #        list(
        #          boundary = area_of_interest$boundary,
        #          baseline = area_of_interest$baseline,
        #          bbox = area_of_interest$bbox,
        #          baseline_bbox = area_of_interest$baseline_bbox,
        #          area = area_of_interest$area,
        #          hexes = area_of_interest$hexes,
        #          gbif_data = area_of_interest$gbif_data,
        #          gbif_data_filtered = area_of_interest$gbif_data_filtered,
        #          species_associations = area_of_interest$species_associations,
        #          species_associations_matrix = area_of_interest$species_associations_matrix,
        #          points_selected = area_of_interest$points_selected,
        #          metric_count_hexes = area_of_interest$metric_count_hexes,
        #          metric_table = area_of_interest$metric_table,
        #          records_table = area_of_interest$records_table,
        #          species_table = area_of_interest$species_table,
        #          observers_table = area_of_interest$observers_table,
        #          visits_table = area_of_interest$visits_table,
        #          trends_table = area_of_interest$trends_table,
        #          species_trends_list = area_of_interest$species_trends_list,
        #          biggest_movers_table = area_of_interest$biggest_movers_table,
        #          focal_species_trends_table = area_of_interest$focal_species_trends_table
        #        ),
        #        pos = 1
        # )
      }

  })

  output$metric_total <- renderUI({

    span(style = "display-inline: block; padding-top: 0;",
         p(strong(req(format(round(nrow(area_of_interest$metric_table), 1), big.mark=",", scientific = FALSE)), style = "font-size: 20px; color: #347AB7; padding-right: 20px; display-inline: block; float:left;")),
    )

  })
  
}

# OVERALL:

# DATA:

# TRENDS: 
# Improve taxon filtering
# Improve estimates of upward/downward trend
