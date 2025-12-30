#' ---
#' title: Place-based workflow to download and process data to be displayed by the California Biodiversity Trends Engine
#' ---
#'
#' Set up data and function to run workflow
ca_boundary <- readRDS("~/Dropbox/elimia/code/apps/biodiversity-trends-engine/data/boundaries/ca_boundary.rds")
get_outputs_for_AOI <- memoise(function(AOI){
## Set things up
source("biodiversity_trends_engine_functions.R")
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
  focal_species_trends_table = NULL
)

## Analysis workflow for Area of Interest (AOI)

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
  sp_min_records <- 30
  sp_num_years_recorded <- 10
}
#### If AOI area is higher or equal to 100km2 but lower than 200 km2, baseline equals all watersheds overlapping AOI
if (area_of_interest$area >= 100 & area_of_interest$area < 200){
  area_of_interest$baseline <- arcpullr::get_layer_by_poly("https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/NHD_WBD_HUC10_Watersheds/FeatureServer/0", area_of_interest$boundary, sp_rel = "intersects") %>% 
    sf::st_union() %>% 
    sf::st_as_sf() %>% 
    sf::st_transform(4326)
  sp_min_records <- 50
  sp_num_years_recorded <- 10
}
#### If AOI area is higher than 200 km2, baseline equals AOI
if (area_of_interest$area >= 200){
  area_of_interest$baseline <- area_of_interest$boundary
  sp_min_records <- 100
  sp_num_years_recorded <- 10
}
#### Limit baseline to land areas
area_of_interest$baseline <- area_of_interest$baseline %>%
   sf::st_intersection(ca_boundary %>% sf::st_transform(sf::st_crs(area_of_interest$baseline)))
sf::st_geometry(area_of_interest$baseline) <- "geometry"

#### Extract baseline bounding box
area_of_interest$baseline_bbox <- area_of_interest$baseline %>% sf::st_bbox() 

### STEP 3: Download gbif data
cat("STEP 3: Download gbif data")
cat("\n")
area_of_interest$gbif_data <- get_gbif_data(area_of_interest$baseline)
#### Process and clean up gbif data
area_of_interest$gbif_data <- area_of_interest$gbif_data %>% 
  dplyr::mutate(
    recordedby = as.character(recordedby), # Turn recordedby to character vector
    observationdate = paste0(year, ifelse(nchar(month) == 1, paste0("0", month), month), ifelse(nchar(day) == 1, paste0("0", day), day))
  )
area_of_interest$gbif_data <- area_of_interest$gbif_data %>% 
  dplyr::mutate(observationdate = gsub("NA", "", observationdate))
## Identify individual visits
area_of_interest$gbif_data <- area_of_interest$gbif_data %>% 
  dplyr::mutate(recordedby = ifelse(recordedby == "character(0)", "unknown", recordedby)) %>% 
  dplyr::mutate(visitID = paste0(recordedby, "_", observationdate, "_", h7, "_", datasetkey))
## Select only necessary fields
area_of_interest$gbif_data <- area_of_interest$gbif_data %>% 
  dplyr::select(-catalognumber, -recordnumber, -license, -rightsholder, -typestatus, -establishmentmeans, -lastinterpreted, -mediatype, -issue)

### STEP 4: Identify species list
cat("STEP 4: Identify species list")
cat("\n")
# Extract list of species that satisfy minimum trends data requirements
area_of_interest$trends_table <- area_of_interest$gbif_data %>% 
  sf::st_set_geometry(NULL) %>% 
  dplyr::group_by(species, kingdom, phylum, class, order, family, genus) %>%
  dplyr::summarise(number_records = n(),
                   number_years_recorded = n_distinct(year)
  ) %>% 
  dplyr::mutate(
    full = ifelse(number_records >= sp_min_records & number_years_recorded >= sp_num_years_recorded, TRUE, FALSE)
  ) %>% 
  dplyr::arrange(desc(number_records))

### STEP 5: Identify species associations
# cat("STEP 5: Identify species associations")
# cat("\n")
# #### Identify species associations (i.e. frenquency with which pairs of species are recorded concurrently)
# area_of_interest$species_associations <- get_species_association_matrix(analysis_records = area_of_interest$gbif_data, species = area_of_interest$trends_table$species, minimum_number_records = sp_min_records)
# #### Extract species associations matrix
# area_of_interest$species_associations_matrix <- area_of_interest$species_associations$sigma

### STEP 6: Generate geographical summaries
cat("STEP 6: Generate geographical summaries")
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

### STEP 7: Extract species trends
cat("STEP 7: Extract species trends")
cat("\n")
#### Calculate individual species' trends
area_of_interest$species_trends_list <- purrr::map(area_of_interest$trends_table$species, function(sp){
  
  print(sp)
  
  out <- purrr::safely(get_species_trends)(analysis_records = area_of_interest$gbif_data, 
                                           focal_taxon = sp,
                                           use_reference_taxon = area_of_interest$trends_table %>% dplyr::filter(species == sp) %>% dplyr::pull(full) %>% isFALSE(),
                                           full = area_of_interest$trends_table %>% dplyr::filter(species == sp) %>% dplyr::pull(full),
                                           resolution = "h5"
  )
  
  file.remove(list.files("/private/tmp/Rtmp-urban/", full.names = TRUE))
  gc()
  
  out$result
  
}) %>% purrr::set_names(area_of_interest$trends_table$species)
#### Trawl individual species trends
species_metrics_df <- get_yearly_trend_metrics(area_of_interest$species_trends_list)
#### Identify increases/decreases
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
#### Extract top movers
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

### STEP 8: Save output
cat("STEP 8: Save output")
cat("\n")
saveRDS(area_of_interest, paste0("data/outputs/", gsub("-|/", "_", area_of_interest$boundary$aoi_name), "_data.rds"))

},
cache = cache_filesystem(CACHE)
)

### Run workflow across places
aoi_polygons <- readRDS(file = "data/boundaries/aoi_polygons.rds")
aoi_polygons_left <- aoi_polygons %>% 
  dplyr::filter(aoi_name %in% setdiff(gsub("-|/", "_", aoi_name), gsub("_data.rds", "", list.files("data/outputs/"))))
AOI_outputs <- purrr::map(1:nrow(aoi_polygons_left), function(i){
  aoi <- aoi_polygons_left[i, ]
  aoi <- aoi %>% 
    sf::st_make_valid()
  cat(aoi$aoi_name)
  cat("\n")
  purrr::safely(get_outputs_for_AOI)(aoi)
  NULL
  })
