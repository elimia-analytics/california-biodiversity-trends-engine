#' ---
#' title: Place-based workflow to download and process data to be displayed by the California Biodiversity Trends Engine
#' ---
#'
#' Load packages
app_libraries <- c("tidyverse", ### data manipulation
                   "shiny", "shinyjs", "shinyWidgets", "shinydashboard", "shinycssloaders", "shinyBS", "shinybusy",  ### shiny
                   "sf", "terra", "leaflet", "leaflet.extras", "leaflet.minicharts", "leafpm", "h3jsr", "leafgl",   ### spatial
                   "plotly", "htmltools", "htmlwidgets", "sortable", "DT", "flexdashboard", "dygraphs", "bslib",   ### interactive
                   "natserv", "duckdbfs", "picante", "ecoCopula", "mvabund", "ranger", ### data
                   "units", "memoise", "glue", "httr", "jsonlite", "future", "furrr"
)
lapply(app_libraries, library, character.only = TRUE)
#' Set up data and functions to run workflow
#' # Data
ca_boundary <- readRDS("~/Dropbox/elimia/code/apps/biodiversity-trends-engine/data/boundaries/ca_boundary.rds")
whr_raster <- terra::rast("data/boundaries/tiff/ds1327.tif") 
#' # Functions
source("biodiversity_trends_engine_functions.R")
#'
#' # AOI-based workflow
AOI_outputs <- list.files("data/outputs", pattern = "data_full.rds", full.names = TRUE)

for (i in 1:length(AOI_outputs)){
  area_of_interest <- readRDS(AOI_outputs[i])
  
  # Filter boundary gbif data
  gbif_data_boundary <- area_of_interest$gbif_data[which(sf::st_intersects(area_of_interest$gbif_data, area_of_interest$boundary, sparse = FALSE)), ]
  
  # Define major taxa of interest
  area_of_interest$major_taxon_names <- c(
    c("Aves", "Mammalia", "Squamata", "Amphibia", "Insecta"), # Animal classes
    area_of_interest$gbif_data %>% sf::st_set_geometry(NULL) %>% dplyr::filter(kingdom == "Plantae") %>% dplyr::count(order) %>% dplyr::filter(n >= 2000) %>% dplyr::arrange(desc(n)) %>% dplyr::pull(order)
  )
  
  # Extract major habitats across area of interest
  ### Baseline
  ## Crop raster to baseline area
  baseline_whr <- whr_raster %>%
    terra::crop(area_of_interest$baseline %>% sf::st_transform(crs(whr_raster))) %>% 
    terra::mask(area_of_interest$baseline %>% sf::st_transform(crs(whr_raster)))
  terra::activeCat(baseline_whr) <- 3
  ## Extract relative area proportions across all habitats
  whr_area_proportions <- terra::extract(x = baseline_whr,
                                         y = baseline_whr %>% as.data.frame(xy = TRUE) %>% dplyr::select(x, y)
  ) %>% 
    dplyr::pull(WHRNUM) %>% 
    table() %>% 
    stack() %>% 
    purrr::set_names(c("number_cells", "WHRNUM")) %>% 
    dplyr::mutate(WHRNUM = as.character(WHRNUM) %>% as.numeric(WHRNUM),
                  proportion_total_cells = round(number_cells/sum(number_cells, na.rm = TRUE), 3)
    ) %>% 
    dplyr::left_join(
      terra::cats(baseline_whr)[[1]] %>% as.data.frame() %>% dplyr::select(WHRNUM, WHRNAME) %>% dplyr::distinct(., .keep_all = TRUE),
      by = "WHRNUM"
    ) %>% 
    dplyr::distinct(., .keep_all = TRUE) %>% 
    dplyr::select(WHRNAME, WHRNUM, number_cells, proportion_total_cells) %>% 
    dplyr::arrange(desc(proportion_total_cells))
  
  ### Boundary
  ## Crop raster to baseline area
  boundary_whr <- whr_raster %>%
    terra::crop(area_of_interest$boundary %>% sf::st_transform(crs(whr_raster))) %>% 
    terra::mask(area_of_interest$boundary %>% sf::st_transform(crs(whr_raster)))
  terra::activeCat(boundary_whr) <- 3
  ## Extract relative area proportions across all habitats
  area_of_interest$whr_area_proportions_boundary <- terra::extract(x = boundary_whr,
                                                  y = boundary_whr %>% as.data.frame(xy = TRUE) %>% dplyr::select(x, y)
  ) %>% 
    dplyr::pull(WHRNUM) %>% 
    table() %>% 
    stack() %>% 
    purrr::set_names(c("number_cells", "WHRNUM")) %>% 
    dplyr::mutate(WHRNUM = as.character(WHRNUM) %>% as.numeric(WHRNUM),
                  proportion_total_cells = round(number_cells/sum(number_cells, na.rm = TRUE), 3)
    ) %>% 
    dplyr::left_join(
      terra::cats(boundary_whr)[[1]] %>% as.data.frame() %>% dplyr::select(WHRNUM, WHRNAME) %>% dplyr::distinct(., .keep_all = TRUE),
      by = "WHRNUM"
    ) %>% 
    dplyr::distinct(., .keep_all = TRUE) %>% 
    dplyr::select(WHRNAME, WHRNUM, number_cells, proportion_total_cells) %>% 
    dplyr::arrange(desc(proportion_total_cells))
  
  ## Identify major habitats (habitats found on at least 5% of baseline area)
  area_of_interest$major_habitats <- area_of_interest$whr_area_proportions_boundary %>% 
    dplyr::filter(proportion_total_cells >= 0.05) %>% 
    dplyr::pull(WHRNAME)

  # Extract species-habitat relationships across area of interest
  area_of_interest$species_whr_relationships <- area_of_interest$gbif_data %>%
    ### Remove highly uncertain observations
    dplyr::mutate(
      WHRNUM = terra::extract(x = baseline_whr, y = area_of_interest$gbif_data)$WHRNUM
    ) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::count(species, WHRNUM) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(species) %>% 
    mutate(proportion_occurrences = n / sum(n)) %>% 
    dplyr::left_join(
      terra::cats(baseline_whr)[[1]] %>% as.data.frame() %>% dplyr::select(WHRNUM, WHRNAME) %>% dplyr::distinct(., .keep_all = TRUE),
      by = "WHRNUM"
    ) %>% 
    dplyr::arrange(species, desc(proportion_occurrences))

  area_of_interest$species_whr_relationships_boundary <- gbif_data_boundary %>%
    ### Remove highly uncertain observations
    dplyr::mutate(
      WHRNUM = terra::extract(x = baseline_whr, y = gbif_data_boundary)$WHRNUM
    ) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::count(species, WHRNUM) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(species) %>% 
    mutate(proportion_occurrences = n / sum(n)) %>% 
    dplyr::left_join(
      terra::cats(baseline_whr)[[1]] %>% as.data.frame() %>% dplyr::select(WHRNUM, WHRNAME) %>% dplyr::distinct(., .keep_all = TRUE),
      by = "WHRNUM"
    ) %>% 
    dplyr::arrange(species, desc(proportion_occurrences))

  #### Which habitats are most undersampled and where (how complete is the sampling list for habitat X in hexagon Y)?
  area_of_interest$whr_representation <- area_of_interest$whr_area_proportions_boundary %>% 
    dplyr::left_join(
      area_of_interest$species_whr_relationships_boundary %>% 
        dplyr::group_by(WHRNAME) %>% 
        dplyr::summarise(proportion_occurrences = round(sum(n, na.rm = TRUE)/nrow(gbif_data_boundary), 3)),
      by = "WHRNAME"
    ) %>% 
    dplyr::mutate(proportion_occurrences = ifelse(is.na(proportion_occurrences), 0, proportion_occurrences),
                  representation = proportion_occurrences-proportion_total_cells
    ) %>% 
    dplyr::arrange(representation)
  
  
  
  underrepresented_habitats <- setdiff((area_of_interest$whr_representation %>% 
                                          dplyr::filter(representation < ifelse(quantile(representation, .1) <= -0.1, quantile(representation, .1), -0.1)) %>% 
                                          dplyr::pull(WHRNUM)), "53"
  )
  
  if (sum(underrepresented_habitats < 0) < 3) underrepresented_habitats <- setdiff(area_of_interest$whr_representation$WHRNUM[area_of_interest$whr_representation$representation < 0], "53")
  
  underrepresented_habitats_df <- area_of_interest$whr_representation %>% 
    dplyr::filter(WHRNUM %in% underrepresented_habitats) %>% 
    dplyr::select(WHRNAME, proportion_total_cells, proportion_occurrences, representation) %>% 
    dplyr::rename(`Habitat` = WHRNAME, 
                  `Proportion of total area` = proportion_total_cells,
                  `Proportion of total occurrences` = proportion_occurrences,
                  `Underrepresentation` = representation
    )
  area_of_interest$underrepresented_habitats_df <- underrepresented_habitats_df
  
  #### Where are the most underrepresented and undersampled habitats?
  underrepresented_habitats_raster <- boundary_whr %>% 
    as.data.frame(xy = TRUE) %>% 
    dplyr::mutate(
      underrepresented_habitats = WHRNUM %in% underrepresented_habitats
    ) %>% 
    dplyr::select(x, y, underrepresented_habitats) %>% 
    terra::rast(crs = sf::st_crs(boundary_whr)$wkt)
  terra::crs(underrepresented_habitats_raster) <- sf::st_crs(boundary_whr)$wkt
  
  record_count_hexes <- get_count_summary(
    records = gbif_data_boundary %>% 
      sf::st_set_geometry(NULL),
    base_hexes = generate_h3_cells(area_of_interest$boundary, 9),
    metric = "Records",
    resolution = "h9"
  )
  low_record_count_hexes <- record_count_hexes %>% 
    dplyr::filter(is.na(metric) | metric < quantile(metric, .10, na.rm = TRUE))
  
  area_of_interest$sampling_coldspots <- underrepresented_habitats_raster %>% 
    terra::mask(
      low_record_count_hexes %>% sf::st_transform(sf::st_crs(boundary_whr)) %>% terra::vect()
    ) %>% 
    terra::subst(from = 1, to = 1, others = NA) %>% 
    terra::trim()
  
  terra::writeRaster(area_of_interest$sampling_coldspots, paste0("data/outputs/", gsub("-|/", "_", area_of_interest$boundary$aoi_name), "_sampling_coldspots.tif"))
  
  ##### What species are associated with those habitats?
  underrepresented_habitats_species <- area_of_interest$biggest_movers_table %>% 
    dplyr::filter(
      species %in% (area_of_interest$species_whr_relationships %>% 
                      dplyr::filter(WHRNUM %in% underrepresented_habitats,
                                    proportion_occurrences >= 0.75,
                                    n >= 5
                      ) %>% 
                      dplyr::pull(species)),
      trend == "needs more data"
    ) %>% 
    dplyr::select(species, number_records, number_years_recorded) %>% 
    dplyr::left_join(
      area_of_interest$trends_table %>% dplyr::select(species, phylum, class, order, family, genus), by = "species"
    )
  area_of_interest$underrepresented_habitats_species <- underrepresented_habitats_species
  
  # Under-represented/over-represented taxa
  low_sample_size_species <- area_of_interest$trends_table %>% 
    dplyr::filter(species %in% (
      area_of_interest$biggest_movers_table %>% 
        dplyr::filter(
          trend == "needs more data"
        ) %>% 
        dplyr::pull(species)
    ))
  
  underrepresented_taxa <- rbind(
    area_of_interest$trends_table %>% 
      dplyr::ungroup() %>% 
      dplyr::count(class) %>% 
      dplyr::filter(class %in% area_of_interest$major_taxon_names) %>% 
      dplyr::rename(taxon = class, number_species = n),
    area_of_interest$trends_table %>% 
      dplyr::ungroup() %>% 
      dplyr::count(order) %>% 
      dplyr::filter(order %in% area_of_interest$major_taxon_names) %>% 
      dplyr::rename(taxon = order, number_species = n)
  ) %>% 
    dplyr::arrange(desc(number_species)) %>% 
    dplyr::left_join(
      rbind(
        area_of_interest$trends_table %>% 
          dplyr::ungroup() %>% 
          dplyr::filter(class %in% area_of_interest$major_taxon_names) %>% 
          dplyr::group_by(class) %>% 
          dplyr::summarise(total_records = sum(number_records)) %>% 
          dplyr::rename(taxon = class),
        area_of_interest$trends_table %>% 
          dplyr::ungroup() %>% 
          dplyr::filter(order %in% area_of_interest$major_taxon_names) %>% 
          dplyr::group_by(order) %>% 
          dplyr::summarise(total_records = sum(number_records)) %>% 
          dplyr::rename(taxon = order)
      ), by = "taxon"
    ) %>% 
    dplyr::left_join(
      rbind(
        low_sample_size_species %>% 
          dplyr::ungroup() %>% 
          dplyr::count(class) %>% 
          dplyr::filter(class %in% area_of_interest$major_taxon_names) %>% 
          dplyr::rename(taxon = class, number_low_sample_size_species = n),
        low_sample_size_species %>% 
          dplyr::ungroup() %>% 
          dplyr::count(order) %>% 
          dplyr::filter(order %in% area_of_interest$major_taxon_names) %>% 
          dplyr::rename(taxon = order, number_low_sample_size_species = n)
      ), by = "taxon"
    ) 
  
  underrepresented_taxa_prop_tests <- purrr::map(1:nrow(underrepresented_taxa), function(t){
    
    list(
      prop.test(x = c(underrepresented_taxa$number_species[t], underrepresented_taxa$total_records[t]), n = c(sum(underrepresented_taxa$number_species, na.rm = TRUE), sum(underrepresented_taxa$total_records, na.rm = TRUE))),
      prop.test(x = c(underrepresented_taxa$number_species[t], underrepresented_taxa$number_low_sample_size_species[t]), n = c(sum(underrepresented_taxa$number_species, na.rm = TRUE), sum(underrepresented_taxa$number_low_sample_size_species, na.rm = TRUE)))
    )
    
  })
  
  area_of_interest$underrepresented_taxa <- underrepresented_taxa %>% 
    dplyr::mutate(
      proportion_species = number_species/sum(number_species, na.rm = TRUE),
      proportion_records = total_records/sum(total_records, na.rm = TRUE),
      records_difference = round(proportion_records - proportion_species, 3),
      records_difference_signif = underrepresented_taxa_prop_tests %>% purrr::map(1) %>% purrr::map_dbl(3),
      records_representation = case_when(
        records_difference < 0 & records_difference_signif < 0.05 ~ "underrepresented",
        records_difference > 0 & records_difference_signif < 0.05 ~ "overrepresented",
        .default = "fair"
      ),
      proportion_low_sample_size_species = number_low_sample_size_species/sum(number_low_sample_size_species, na.rm = TRUE),
      low_sample_size_difference = round(proportion_species - proportion_low_sample_size_species, 3),
      low_sample_size_difference_signif = underrepresented_taxa_prop_tests %>% purrr::map(2) %>% purrr::map_dbl(3),
      low_sample_size_representation = case_when(
        low_sample_size_difference < 0 & low_sample_size_difference_signif < 0.05 ~ "underrepresented",
        low_sample_size_difference > 0 & low_sample_size_difference_signif < 0.05 ~ "overrepresented",
        .default = "fair"
      ),
    )
  
  # Average trends across taxa
  area_of_interest$major_taxon_trends_summed <- purrr::map(area_of_interest$major_taxon_names, function(tax){
    print(tax)
    out <- get_taxon_trends(aoi = area_of_interest, focal_taxon = tax, metric = "reporting_rate_sd", make_plot = FALSE)
    out
  }) %>% purrr::set_names(area_of_interest$major_taxon_names)
  
  # Relative trends among taxa
  area_of_interest$major_taxon_trends_relative <- purrr::map(area_of_interest$major_taxon_names[1:5], function(tax){
    print(tax)
    out <- get_major_taxon_trends(analysis_records = area_of_interest$gbif_data,
                                  focal_taxon = tax,
                                  use_reference_taxon = TRUE,
                                  use_reference_taxon_rank = "phylum", 
                                  use_taxonomic_resolution = "class",
                                  full = TRUE,
                                  resolution = "h5"
    )
    out
  }) %>% purrr::set_names(area_of_interest$major_taxon_names[1:5])

  area_of_interest$major_taxon_trends_relative <- purrr::map(1:length(area_of_interest$major_taxon_trends_relative), function(tax){
    area_of_interest$major_taxon_trends_relative[[tax]]$yearly_trend %>% dplyr::select(year, reporting_rate_sd) %>% 
      purrr::set_names(c("year", names(area_of_interest$major_taxon_trends_relative)[tax]))
  }) %>% 
    plyr::join_all(by = "year")
  
  # Average trends across habitats
  area_of_interest$major_habitat_trends <- purrr::map(area_of_interest$major_habitats, function(whr){
    
    whr_species <- area_of_interest$species_whr_relationships %>% dplyr::filter(WHRNAME == whr, proportion_occurrences >= 0.15) %>% dplyr::pull(species)
    
    if (length(whr_species) >= 1){
      
      out <- get_taxon_trends(aoi = area_of_interest, focal_taxon = whr_species, metric = "reporting_rate_sd", make_plot = FALSE)
      
    } else {
      NULL
    }
    
  }) %>% purrr::set_names(area_of_interest$major_habitats)
  
  # Prepare data for predictive model of trends
  ### Isolate species with estimated trends
  trends_df <- area_of_interest$biggest_movers_table %>% 
    dplyr::filter(trend != "needs more data") %>% 
    dplyr::left_join(
      area_of_interest$trends_table %>% dplyr::select(-setdiff(intersect(names(area_of_interest$trends_table), names(area_of_interest$biggest_movers_table)), "species")), by = "species"
    ) %>% 
    dplyr::filter(
      complete.cases(.)
    )
  ### Add binary major habitat variables
  trends_df <- trends_df %>% 
    cbind(
      data.frame(matrix(0, ncol = length(area_of_interest$major_habitats), nrow = nrow(trends_df))) %>% 
        purrr::set_names(area_of_interest$major_habitats)
    )
  
  for (whr in area_of_interest$major_habitats){
    
    whr_species <- area_of_interest$species_whr_relationships %>% 
      dplyr::filter(n >= 5, proportion_occurrences >= 0.20) %>% 
      dplyr::filter(WHRNAME == whr) 
    
    trends_df[which(trends_df$species %in% whr_species$species), whr] <- 1
    
  }
  
  ### Clean up model data
  names(trends_df) <- gsub(" ", "_", names(trends_df))
  predictor_names <- c(c("kingdom", "phylum", "class", "order", "family", "genus"), gsub(" ", "_", area_of_interest$major_habitats))
  
  ### Build random forest model
  X <- trends_df[, predictor_names]
  y <- trends_df$reporting_rate_trend_last_ten
  
  area_of_interest$rf <- ranger(
    x = X,
    y = y,
    importance = "permutation"
  )
  area_of_interest$rf_data <- trends_df
  
  file.remove(list.files("/private/tmp/Rtmp-urban/", full.names = TRUE))
  
  # Save output
  saveRDS(area_of_interest, paste0("data/outputs/", gsub("-|/", "_", area_of_interest$boundary$aoi_name), "_data_full.rds"))
  
}

