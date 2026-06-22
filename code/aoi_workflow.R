#' ---
#' title: Place-based workflow to download and process data to be displayed by the California Biodiversity Trends Engine
#' ---
#'
#' Load packages
app_libraries <- c("tidyverse", ### data manipulation
                  "shiny", "shinyjs", "shinyWidgets", "shinydashboard", "shinycssloaders", "shinyBS", "shinybusy",  ### shiny
                 "sf", "terra", "leaflet", "leaflet.extras", "leaflet.minicharts", "leafpm", "h3jsr", "leafgl",   ### spatial
                 "plotly", "htmltools", "htmlwidgets", "sortable", "DT", "flexdashboard", "dygraphs", "bslib",   ### interactive
                 "natserv", "duckdbfs", "picante", "ecoCopula", "mvabund", ### data
                 "units", "memoise", "glue", "httr", "jsonlite", "future", "furrr"
)
# lapply(app_libraries, install.packages, character.only = TRUE)
# Create a connection to an in-memory database
# con <- dbConnect(duckdb::duckdb())
# # Run the SQL command to install and load the extension
# dbExecute(con, "INSTALL httpfs;")
# dbExecute(con, "LOAD httpfs;")
lapply(app_libraries, library, character.only = TRUE)
#' Set up data and functions to run workflow
#' # Data
ca_boundary <- readRDS("~/Dropbox/elimia/code/apps/biodiversity-trends-engine/data/boundaries/ca_boundary.rds")
# whr_raster <- terra::rast("data/boundaries/tiff/ds1327.tif") 
# terra::activeCat(whr_raster) <- 3
#' # Functions
source("biodiversity_trends_engine_functions.R")
#'
#' # AOI-based workflow
dir.create("data/outputs")
# AOI_outputs_current <- list.files("data/outputs-v1", full.names = TRUE)
AOI_boundaries <- readRDS("data/boundaries/aoi_polygons.rds")
AOI_boundaries <- AOI_boundaries %>% 
  dplyr::filter(
    aoi_name %in% c(
      "Jack and Laura Dangermond Preserve",
      "One Tam Area of Interest",
      "Pepperwood Preserve",
      "Presidio Land Trust",
      "Santa Monica Mountains Recreation Area",
      "Angelo Coast Range Reserve",
      "Fort Ord Natural Reserve",
      "Jasper Ridge Biological Preserve",
      "Carpinteria Salt Marsh Reserve",
      "Boyd Deep Canyon Desert Research Center",
      "Blue Oak Ranch Reserve_data",
      "San Joaquin Freshwater Marsh Reserve",
      "Dunn-Wildlake/Duff Ranch",
      "Mead Ranch",
      "Walt Ranch"
    )
    )
AOI_boundaries <- AOI_boundaries %>% 
  dplyr::filter(
    aoi_name %in% c(
      "Dunn-Wildlake/Duff Ranch",
  "Mead Ranch",
  "Walt Ranch"
  )
  )
AOI_boundaries$aoi_name[AOI_boundaries$aoi_name == "Dunn-Wildlake/Duff Ranch"] <- "Dunn_Wildlake_Duff_Ranch"

future::plan(multisession, workers = 10)

for (i in 1:nrow(AOI_boundaries)){
  # aoi <- aoi_polygons_left[i, ]
  # aoi <- aoi %>% 
  #   sf::st_make_valid()
  AOI <- AOI_boundaries[i, ] %>% # readRDS(AOI_outputs_current[i])$boundary
    sf::st_make_valid()
  cat(AOI$aoi_name)
  cat("\n")
# get_outputs_for_AOI <- memoise(function(AOI){
#'
## Create list object to store all outputs for area of interest
area_of_interest <- list(
  boundary = AOI,
  baseline = NULL,
  bbox = NULL,
  baseline_bbox = NULL,
  area = NULL,
  gbif_data = NULL,
  species_associations = NULL,
  species_associations_matrix = NULL,
  records_table = NULL,
  species_table = NULL,
  observers_table = NULL,
  visits_table = NULL,
  completeness_table = NULL,
  trends_table = NULL,
  species_trends_list = NULL,
  biggest_movers_table = NULL,
  focal_species_trends_table = NULL,
  major_taxon_trends = NULL,
  whr_trends = NULL,
  run_duration = NULL
)

## Analysis workflow for Area of Interest (AOI)
start_time <- Sys.time()
### STEP 1: Load AOI boundary
cat("STEP 1: Load AOI boundary")
cat("\n")
#### Load boundary from saved object or online source
# area_of_interest$boundary <- readRDS("places_data/places_boundaries/Jack and Laura Dangermond Preserve.rds")
#### Extract boundary bounding box
area_of_interest$bbox <- area_of_interest$boundary %>% sf::st_bbox()
#### Get boundary area in km2
area_of_interest$area <- area_of_interest$boundary %>% sf::st_area() %>% units::set_units(km^2) %>% as.numeric()

### STEP 2: Identify baseline place contingent on AOI area
cat("STEP 2: Identify baseline place contingent on AOI area")
cat("\n")
#### If AOI area is lower than 100 km2, baseline equals all watersheds overlapping AOI buffered by 1km
if (area_of_interest$area < 100){
  area_of_interest$baseline <- arcpullr::get_layer_by_poly("https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/NHD_WBD_HUC10_Watersheds/FeatureServer/0", area_of_interest$boundary %>% sf::st_buffer(1000), sp_rel = "intersects") %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    sf::st_transform(4326)
  sp_min_visits <- 10
  sp_min_records <- 30
  sp_num_years_recorded <- 10
}
#### If AOI area is higher or equal to 100km2 but lower than 200 km2, baseline equals all watersheds overlapping AOI
if (area_of_interest$area >= 100 & area_of_interest$area < 200){
  area_of_interest$baseline <- arcpullr::get_layer_by_poly("https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/NHD_WBD_HUC10_Watersheds/FeatureServer/0", area_of_interest$boundary, sp_rel = "intersects") %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    sf::st_transform(4326)
  sp_min_visits <- 15
  sp_min_records <- 40
  sp_num_years_recorded <- 10
}
#### If AOI area is higher than 200 km2, baseline equals AOI
if (area_of_interest$area >= 200){
  area_of_interest$baseline <- area_of_interest$boundary
  sp_min_visits <- 15
  sp_min_records <- 50
  sp_num_years_recorded <- 10
}
#### Limit baseline to land areas
# area_of_interest$baseline <- area_of_interest$baseline %>%
#    sf::st_intersection(ca_boundary %>% sf::st_transform(sf::st_crs(area_of_interest$baseline)))
# sf::st_geometry(area_of_interest$baseline) <- "geometry"

#### Extract baseline bounding box
area_of_interest$baseline_bbox <- area_of_interest$baseline %>% sf::st_bbox()

### STEP 3: Download gbif data
cat("STEP 3: Download gbif data")
cat("\n")
#### Download GBIF data for baseline area of interest
area_of_interest$gbif_data <- get_gbif_data(area_of_interest$baseline)

### STEP 4: Incorporate unobscured iNaturalist observations
cat("STEP 4: Incorporate unobscured iNaturalist observations")
cat("\n")
#### Load unobscured iNaturalist observations
unobscured_inat_observations <- read_csv("data/iNat_Sensitive_for_Trends_Engine_upto_WithSciName_2025-06-30.csv") %>% 
  dplyr::mutate(
    longitude = decimalLongitude,
    latitude = decimalLatitude
  ) %>% 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  )
#### Identify iNat observations intersecting the baseline area of interest
unobscured_inat_observations_intersecting <- unobscured_inat_observations[which(sf::st_intersects(unobscured_inat_observations, area_of_interest$boundary, sparse = FALSE) == TRUE), ]

if (nrow(unobscured_inat_observations_intersecting) > 0){
#### Remove iNat observations from dataset if they do not in reality intersect the boundary
area_of_interest$gbif_data <- area_of_interest$gbif_data %>% 
  dplyr::filter(!(occurrenceid %in% (area_of_interest$gbif_data %>% dplyr::filter((occurrenceid %in% unobscured_inat_observations$occurrenceID) & !(occurrenceid %in% unobscured_inat_observations_intersecting$occurrenceID)) %>% dplyr::pull(occurrenceid))))
#### Replace/add unobscured coordinates in dataset if they do in reality intersect the boundary
##### Create new data to be incorporated
unobscured_inat_new_data <- get_gbif_data(aoi = area_of_interest$baseline %>% sf::st_buffer(10000), occurrence_ids = unobscured_inat_observations_intersecting$occurrenceID)
unobscured_inat_observations_intersecting <- unobscured_inat_observations_intersecting %>% 
  dplyr::filter(occurrenceID %in% unobscured_inat_new_data$occurrenceid)
unobscured_inat_new_data$decimallatitude <- unobscured_inat_observations_intersecting$decimalLatitude
unobscured_inat_new_data$decimallongitude <- unobscured_inat_observations_intersecting$decimalLongitude
unobscured_inat_new_data$scientificname <- unobscured_inat_observations_intersecting$scientificName
unobscured_inat_new_data$coordinateuncertaintyinmeters <- unobscured_inat_observations_intersecting$coordinateUncertaintyInMeters
unobscured_inat_new_data$license <- unobscured_inat_observations_intersecting$license
unobscured_inat_new_data$h5 <- unobscured_inat_observations_intersecting$h5
unobscured_inat_new_data$h6 <- unobscured_inat_observations_intersecting$h6
unobscured_inat_new_data$h7 <- unobscured_inat_observations_intersecting$h7
unobscured_inat_new_data$h8 <- NA
unobscured_inat_new_data$h9 <- NA
unobscured_inat_new_data$h10 <- NA
##### Integrate new data and replace previous occurrence rows for overlapping occurrenceIDs
area_of_interest$gbif_data <- area_of_interest$gbif_data %>% 
  dplyr::filter(!(occurrenceid %in% unobscured_inat_new_data$occurrenceid)) %>% 
  rbind(unobscured_inat_new_data) 
}

##### Create display data where sensitive iNat records are obscured
area_of_interest$gbif_data_display <- area_of_interest$gbif_data %>%
  dplyr::mutate(obscure_from_map = occurrenceid %in% unobscured_inat_observations$occurrenceID)
area_of_interest$gbif_data_display$decimallatitude[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$gbif_data_display$decimallongitude[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$gbif_data_display$eventdate[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$gbif_data_display$observationdate[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$gbif_data_display$day[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$gbif_data_display$month[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$records_table <- area_of_interest$gbif_data_display %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::arrange(desc(eventdate), species) %>%
  dplyr::mutate(decimallongitude = round(decimallongitude, 3), decimallatitude = round(decimallatitude, 3),
                URL = paste0("<a href='https://www.gbif.org/occurrence/", gbifid, "' target='_blank' onmousedown='event.stopPropagation();'>", gbifid, "</a>")
  ) %>%
  dplyr::select(species, obscure_from_map, eventdate, URL, basisofrecord, institutioncode, decimallongitude, decimallatitude, coordinateuncertaintyinmeters, kingdom, phylum, class, order, family, genus) %>%
  dplyr::rename("scientific name" = species,
                "sensitive species" = obscure_from_map,
                "date" = eventdate,
                "record type" = basisofrecord,
                "longitude" = decimallongitude,
                "latitude" = decimallatitude,
                "uncertainty (m)" = coordinateuncertaintyinmeters,
                "institution code" = institutioncode
  )

### STEP 5: Identify individual visits
cat("STEP 5: Identify individual visits")
cat("\n")
#### Filter out observations with coordinate uncertainty higher than 1400 meters (i.e. the edge length of an h7 hexagon)
area_of_interest$gbif_data <- area_of_interest$gbif_data %>%
  dplyr::filter(
    coordinateuncertaintyinmeters <= 1400
  )
area_of_interest$gbif_data <- area_of_interest$gbif_data %>%
  dplyr::mutate(recordedby = ifelse(recordedby == "character(0)", "unknown", recordedby)) %>%
  dplyr::mutate(visitID = paste0(recordedby, "_", observationdate, "_", h7, "_", datasetkey))

### STEP 6: Identify species list
cat("STEP 6: Identify species list")
cat("\n")
# Extract list of species that satisfy minimum trends data requirements
area_of_interest$trends_table <- area_of_interest$gbif_data %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::group_by(species, kingdom, phylum, class, order, family, genus) %>%
  dplyr::summarise(number_records = n(),
                   number_visits = n_distinct(visitID),
                   number_years_recorded = n_distinct(year)
  ) %>%
  dplyr::mutate(
    full = ifelse(number_visits >= sp_min_visits & number_years_recorded >= sp_num_years_recorded, TRUE, FALSE)
    # full = ifelse(number_records >= sp_min_records & number_years_recorded >= sp_num_years_recorded, TRUE, FALSE)
  ) %>%
  dplyr::arrange(desc(number_visits)) %>%
  dplyr::ungroup()

### STEP 7: Generate geographical summaries
cat("STEP 7: Generate geographical summaries")
cat("\n")
#### Records
area_of_interest$records_table <- area_of_interest$gbif_data %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::arrange(desc(eventdate), species) %>%
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
#### Species
area_of_interest$species_table <- area_of_interest$gbif_data %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::group_by(species, kingdom, phylum, class, order, family, genus) %>%
  dplyr::summarise(number_records = n(),
                   number_years_recorded = n_distinct(year)
  ) %>%
  dplyr::arrange(desc(number_records)) %>%
  dplyr::select(species, number_records, number_years_recorded, kingdom, phylum, class, order, family, genus) %>%
  dplyr::rename("scientific name" = species,
                "number records" = number_records,
                "number years recorded" = number_years_recorded
  )

#### Observers
area_of_interest$observers_table <- area_of_interest$gbif_data %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::group_by(recordedby) %>%
  dplyr::summarise(number_records = n(),
                   number_species_recorded = n_distinct(species),
                   proportion_species_recorded = n_distinct(species)/n_distinct(area_of_interest$gbif_data$species),
                   number_years_recorded = n_distinct(year)
  ) %>%
  dplyr::arrange(desc(number_records)) %>%
  dplyr::select(recordedby, number_records, number_species_recorded, proportion_species_recorded, number_years_recorded) %>%
  dplyr::rename("observer name" = recordedby,
                "number records" = number_records,
                "number species recorded" = number_species_recorded,
                "proportion species recorded" = proportion_species_recorded,
                "number years recorded" = number_years_recorded
  )

#### Visits
area_of_interest$visits_table <- area_of_interest$gbif_data %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::group_by(visitID, recordedby, eventdate) %>%
  dplyr::summarise(number_records = n(),
                   number_species_recorded = n_distinct(species),
                   proportion_species_recorded = n_distinct(species)/n_distinct(area_of_interest$gbif_data$species)
  ) %>%
  dplyr::arrange(desc(number_records)) %>%
  dplyr::select(visitID, recordedby, eventdate, number_records, number_species_recorded, proportion_species_recorded) %>%
  dplyr::rename("observer name" = recordedby,
                "date" = eventdate,
                "number records" = number_records,
                "number species recorded" = number_species_recorded,
                "proportion species recorded" = proportion_species_recorded
  )

#### Completeness
area_of_interest$completeness_table <- area_of_interest$gbif_data %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::group_by(h6) %>%
  dplyr::summarise(number_records = n(),
                   number_species_recorded = n_distinct(species),
                   proportion_species_recorded = as.numeric(round((n_distinct(species)/n_distinct(area_of_interest$gbif_data$species)), 3)),
  ) %>%
  dplyr::arrange(desc(proportion_species_recorded)) %>%
  dplyr::mutate(
    proportion_species_recorded = paste0(100*proportion_species_recorded, "%")
    ) %>%
  dplyr::select(all_of(c("h6", "number_records", "number_species_recorded", "proportion_species_recorded"))) %>%
  dplyr::rename(
    "number records" = number_records,
    "number species recorded" = number_species_recorded,
    "species inventory completeness" = proportion_species_recorded
  )

saveRDS(area_of_interest, paste0("data/outputs/", gsub("-|/", "_", area_of_interest$boundary$aoi_name), "_data.rds"))

### STEP 8: Extract species trends
cat("STEP 8: Extract species trends")
cat("\n")
#### Calculate individual species' trends
##### Directory to create temporary species trends files
trends_temp_dir <- paste0("data/outputs/", area_of_interest$boundary$aoi_name, "_trends")
dir.create(trends_temp_dir)

gbif_data <- area_of_interest$gbif_data
trends_table <- area_of_interest$trends_table

sp_names <- setdiff(trends_table$species, gsub("_trend.rds", "", list.files(trends_temp_dir)))

furrr::future_map(sp_names, function(sp){

  sp_full <- trends_table %>% dplyr::filter(species == sp) %>% dplyr::pull(full)

  out <- purrr::safely(get_species_trends)(analysis_records = gbif_data,
                                           focal_taxon = sp,
                                           use_reference_taxon = isFALSE(sp_full),
                                           trends_table = trends_table,
                                           full = sp_full,
                                           resolution = "h5"
  )

  if (!is.null(out$result)) saveRDS(out$result, file = paste0(trends_temp_dir, "/", sp, "_trend.rds"))
  
  file.remove(list.files("/private/tmp/Rtmp-urban/", full.names = TRUE))
  
  NULL

},
.options = furrr::furrr_options(seed = TRUE,
                                packages = c("dplyr"),
                                globals = c(
                                  "trends_temp_dir",
                                  "gbif_data",
                                  "trends_table",
                                  "get_shared_visits",
                                  "get_associated_species",
                                  "get_count_data",
                                  "calculate_detection_data",
                                  "calculate_trends",
                                  "run_randomizations",
                                  "get_randomized_metric",
                                  "get_standardized_difference",
                                  "get_species_trends",
                                  "get_taxon_trends"
                                ),
                                scheduling = structure(TRUE, ordering = "random")
)
) # %>% purrr::set_names(area_of_interest$trends_table$species)

future::plan(sequential)
future::plan(multisession, workers = 10)


end_time <- Sys.time()
area_of_interest$run_duration <- end_time - start_time
cat("\n")
saveRDS(area_of_interest, paste0("data/outputs/", gsub("-|/", "_", area_of_interest$boundary$aoi_name), "_data.rds"))

}

#' # Update AOI trend outputs
AOI_outputs <- list.files("data/outputs", pattern = "data.rds", full.names = TRUE)
unobscured_inat_observations <- read_csv("data/iNat_Sensitive_for_Trends_Engine_upto_WithSciName_2025-06-30.csv") %>% 
  dplyr::mutate(
    longitude = decimalLongitude,
    latitude = decimalLatitude
  ) %>% 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  )

for (i in 1:length(AOI_outputs)){
area_of_interest <- readRDS(AOI_outputs[i])
trends_temp_dir <- paste0("data/outputs/", area_of_interest$boundary$aoi_name, "_trends")
area_of_interest$species_trends_list <- purrr::map(list.files(trends_temp_dir, full.names = TRUE), readRDS) %>% set_names(gsub("_trend.rds", "", list.files(trends_temp_dir)))
## Trawl individual species trends
species_metrics_df <- get_yearly_trend_metrics(trends = trends_temp_dir)

#### Identify increases/decreases
area_of_interest$biggest_movers_table <- area_of_interest$trends_table %>%
  dplyr::ungroup() %>%
  dplyr::left_join(species_metrics_df, by = "species") %>%
  dplyr::mutate(trend = case_when(
    (reporting_rate_above_0_last_ten >= 5  & reporting_rate_below_signif_last_ten < 3) & (reporting_rate_trend_last_five >= 0.5 | reporting_rate_trend_last_ten >= 0.5) ~ "increasing",
    (reporting_rate_above_signif_last_ten < 3  & reporting_rate_below_0_last_ten >= 5) & (reporting_rate_trend_last_five <= -0.5 | reporting_rate_trend_last_ten <= -0.5) ~ "decreasing",
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
  dplyr::select(species, trend, trend_icon, number_records, number_years_recorded, reporting_rate_above_0_last_ten, reporting_rate_below_0_last_ten, reporting_rate_trend_last_five, reporting_rate_trend_last_ten)
#### Extract top movers
area_of_interest$focal_species_trends_table <- rbind(
  area_of_interest$biggest_movers_table %>%
    dplyr::filter(trend == "increasing") %>%
    dplyr::arrange(desc(reporting_rate_above_0_last_ten * reporting_rate_trend_last_ten)) %>%
    dplyr::slice_head(n = 5),
  area_of_interest$biggest_movers_table %>%
    dplyr::filter(trend == "decreasing") %>%
    dplyr::arrange(desc(reporting_rate_below_0_last_ten * reporting_rate_trend_last_ten)) %>%
    dplyr::slice_head(n = 5)
)

area_of_interest$gbif_data_display <- area_of_interest$gbif_data %>%
  dplyr::mutate(obscure_from_map = occurrenceid %in% unobscured_inat_observations$occurrenceID)
area_of_interest$gbif_data_display$decimallatitude[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$gbif_data_display$decimallongitude[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$gbif_data_display$eventdate[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$gbif_data_display$observationdate[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$gbif_data_display$day[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$gbif_data_display$month[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
area_of_interest$records_table <- area_of_interest$gbif_data_display %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::arrange(desc(eventdate), species) %>%
  dplyr::mutate(decimallongitude = round(decimallongitude, 3), decimallatitude = round(decimallatitude, 3),
                URL = paste0("<a href='https://www.gbif.org/occurrence/", gbifid, "' target='_blank' onmousedown='event.stopPropagation();'>", gbifid, "</a>")
  ) %>%
  dplyr::select(species, obscure_from_map, eventdate, URL, basisofrecord, institutioncode, decimallongitude, decimallatitude, coordinateuncertaintyinmeters, kingdom, phylum, class, order, family, genus) %>%
  dplyr::rename("scientific name" = species,
                "sensitive species" = obscure_from_map,
                "date" = eventdate,
                "record type" = basisofrecord,
                "longitude" = decimallongitude,
                "latitude" = decimallatitude,
                "uncertainty (m)" = coordinateuncertaintyinmeters,
                "institution code" = institutioncode
  )

### STEP 9: Save output
cat("STEP 9: Save output")
saveRDS(area_of_interest, paste0("data/outputs/", gsub("-|/", "_", area_of_interest$boundary$aoi_name), "_data_full.rds"))
}

AOI_outputs <- list.files("data/outputs", pattern = "data_full.rds", full.names = TRUE)
unobscured_inat_observations <- read_csv("data/iNat_Sensitive_for_Trends_Engine_upto_WithSciName_2025-06-30.csv") %>% 
  dplyr::mutate(
    longitude = decimalLongitude,
    latitude = decimalLatitude
  ) %>% 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  )
whr_raster <- terra::rast("data/boundaries/tiff/ds1327.tif")
for (i in 1:length(AOI_outputs)){
  area_of_interest <- readRDS(AOI_outputs[i])
  # Extract major habitats across area of interest
  ### Baseline
  ## Crop raster to baseline area
  baseline_whr <- whr_raster %>%
    terra::crop(area_of_interest$baseline %>% sf::st_transform(crs(whr_raster))) %>% 
    terra::mask(area_of_interest$baseline %>% sf::st_transform(crs(whr_raster)))
  terra::activeCat(baseline_whr) <- 4
  
  area_of_interest$gbif_data <- area_of_interest$gbif_data %>%
    ### Remove highly uncertain observations
    dplyr::mutate(
      habitat = terra::extract(x = baseline_whr, y = area_of_interest$gbif_data)$WHRNAME %>% as.character()
    ) 
  
  area_of_interest$gbif_data_display <- area_of_interest$gbif_data %>%
    dplyr::mutate(obscure_from_map = occurrenceid %in% unobscured_inat_observations$occurrenceID)
  area_of_interest$gbif_data_display$decimallatitude[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
  area_of_interest$gbif_data_display$decimallongitude[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
  area_of_interest$gbif_data_display$eventdate[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
  area_of_interest$gbif_data_display$observationdate[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
  area_of_interest$gbif_data_display$day[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
  area_of_interest$gbif_data_display$month[area_of_interest$gbif_data_display$obscure_from_map == TRUE] <- NA
  area_of_interest$records_table <- area_of_interest$gbif_data_display %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::arrange(desc(eventdate), species) %>%
    dplyr::mutate(decimallongitude = round(decimallongitude, 3), decimallatitude = round(decimallatitude, 3),
                  URL = paste0("<a href='https://www.gbif.org/occurrence/", gbifid, "' target='_blank' onmousedown='event.stopPropagation();'>", gbifid, "</a>")
    ) %>%
    dplyr::select(species, obscure_from_map, eventdate, habitat, URL, basisofrecord, institutioncode, decimallongitude, decimallatitude, coordinateuncertaintyinmeters, kingdom, phylum, class, order, family, genus) %>%
    dplyr::rename("scientific name" = species,
                  "sensitive species" = obscure_from_map,
                  "date" = eventdate,
                  "record type" = basisofrecord,
                  "longitude" = decimallongitude,
                  "latitude" = decimallatitude,
                  "uncertainty (m)" = coordinateuncertaintyinmeters,
                  "institution code" = institutioncode
    )
  ### STEP 9: Save output
  cat("STEP 9: Save output")
  saveRDS(area_of_interest, paste0("data/outputs/", gsub("-|/", "_", area_of_interest$boundary$aoi_name), "_data_full.rds"))
}
