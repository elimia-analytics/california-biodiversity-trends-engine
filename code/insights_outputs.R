#' ---
#' title: California Biodiversity Trends Engine: Insights Report
#' ---
#'
#' Load packages
app_libraries <- c("tidyverse", ### data manipulation
                   "shiny", "shinyjs", "shinyWidgets", "shinydashboard", "shinycssloaders", "shinyBS", "shinybusy",  ### shiny
                   "sf", "terra", "leaflet", "leaflet.extras", "leaflet.minicharts", "leafpm", "h3jsr", "esri2sf", "leafgl",   ### spatial
                   "plotly", "htmltools", "htmlwidgets", "sortable", "DT", "flexdashboard", "dygraphs", "bslib",   ### interactive
                   "natserv", "duckdbfs", "picante", "ecoCopula", "mvabund", ### data
                   "units", "memoise", "glue", "httr", "jsonlite", "future", "furrr"
)
lapply(app_libraries, library, character.only = TRUE)
#' Set up data and functions to run workflow
#' # Data
ca_boundary <- readRDS("~/Dropbox/elimia/code/apps/biodiversity-trends-engine/data/boundaries/ca_boundary.rds")
# whr_raster <- terra::rast("data/boundaries/tiff/ds1327.tif") 
# terra::activeCat(whr_raster) <- 3
#' # Functions
source("biodiversity_trends_engine_functions.R")

### Define major focal taxa
major_taxon_names <- c(
  c("Aves", "Mammalia", "Squamata", "Amphibia", "Insecta"), # Animal classes
  area_of_interest$gbif_data %>% sf::st_set_geometry(NULL) %>% dplyr::filter(kingdom == "Plantae") %>% dplyr::count(order) %>% dplyr::filter(n >= 1000) %>% dplyr::arrange(desc(n)) %>% dplyr::pull(order)
)
### Major habitats
whr_raster <- terra::rast("data/boundaries/tiff/ds1327.tif") 
baseline_whr <- whr_raster %>%
  terra::crop(area_of_interest$baseline %>% sf::st_transform(crs(whr_raster))) %>% 
  terra::mask(area_of_interest$baseline %>% sf::st_transform(crs(whr_raster)))
terra::activeCat(baseline_whr) <- 3

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

### Species-habitat relationships
species_whr_relationships <- area_of_interest$gbif_data %>% 
  dplyr::mutate(
    WHRNUM = terra::extract(x = baseline_whr, y = area_of_interest$gbif_data)$WHRNUM
  ) %>% 
  sf::st_set_geometry(NULL) %>% 
  dplyr::count(species, WHRNUM) 

species_whr_relationships_full <- purrr::map(unique(species_whr_relationships$species), function(sp){
  species_whr_relationships %>% 
    dplyr::filter(species == sp) %>% 
    dplyr::mutate(
      proportion_occurrences = n/sum(n, na.rm = TRUE)
    ) %>% 
    dplyr::left_join(
      terra::cats(baseline_whr)[[1]] %>% as.data.frame() %>% dplyr::select(WHRNUM, WHRNAME) %>% dplyr::distinct(., .keep_all = TRUE),
      by = "WHRNUM"
    ) %>%
    dplyr::distinct(., .keep_all = TRUE) %>% 
    dplyr::filter(complete.cases(.)) 
}) %>% 
  bind_rows() %>% 
  dplyr::arrange(species, desc(proportion_occurrences))

### sampling coldspots
#### Which habitats are most undersampled and where (how complete is the sampling list for habitat X in hexagon Y)?
whr_representation <- whr_area_proportions %>% 
  dplyr::left_join(
    species_whr_relationships_full %>% 
      dplyr::group_by(WHRNAME) %>% 
      dplyr::summarise(proportion_occurrences = round(sum(n, na.rm = TRUE)/nrow(area_of_interest$gbif_data), 3)),
    by = "WHRNAME"
  ) %>% 
  dplyr::mutate(
    representation = proportion_occurrences-proportion_total_cells
  ) %>% 
  dplyr::arrange(representation)

underrepresented_habitats <- setdiff((whr_representation %>% 
                                        dplyr::filter(representation <= quantile(representation, .10, na.rm = TRUE)) %>% 
                                        dplyr::pull(WHRNUM)), "53"
)
baseline_whr %in% as.numeric(underrepresented_habitats)
##### What species are associated with those habitats?
area_of_interest$biggest_movers_table %>% 
  dplyr::filter(
    species %in% (species_whr_relationships_full %>% 
      dplyr::filter(WHRNAME %in%
                      setdiff((whr_representation %>% 
                                 dplyr::filter(representation <= quantile(representation, .10, na.rm = TRUE)) %>% 
                                 dplyr::pull(WHRNAME)), "Urban"
                      ),
                    proportion_occurrences >= 0.75
      ) %>% 
      dplyr::pull(species)),
    number_records <= 10,
    trend == "needs more data"
  )


#### Which taxa are least comprehensively sampled and where (how complete is the sampling list for taxon X in hexagon Y)?
#### Which species require more current and/or more historic data? Which species could not have trends calculated (what major taxa and/or habitats are they associated with?)
#### What habitats do those species occur in?

### summed trends
#### Major taxa -> extract "most interesting" trends
major_taxon_trends <- purrr::map(major_taxon_names, function(tax){
  out <- get_taxon_trends(aoi = area_of_interest, focal_taxon = tax, metric = "reporting_rate_sd", plot_label = tax)
  out
}) %>% purrr::set_names(major_taxon_names)
#### Major habitats -> extract "most interesting" trends
major_habitats <- whr_area_proportions %>% 
  dplyr::filter(proportion_total_cells >= 0.05) %>% 
  dplyr::pull(WHRNAME)
whr_trends <- purrr::map(major_habitats, function(whr){

  print(whr)
  whr_species <- species_whr_relationships_full %>% dplyr::filter(WHRNAME == whr, proportion_occurrences >= 0.15) %>% dplyr::pull(species)

  if (length(whr_species) >= 1){

    out <- get_taxon_trends(aoi = area_of_interest, focal_taxon = whr_species, metric = "reporting_rate_sd", plot_label = whr)

  } else {
    NULL
  }

}) %>% purrr::set_names(major_habitats)

### random forest model
#### across all trends, binarize major habitats and binarize major taxa
#### Extract variable importance
#### Extract partial effects of major habitat and major taxa
trends_df <- area_of_interest$biggest_movers_table %>% 
  dplyr::filter(trend != "needs more data") %>% 
  dplyr::left_join(
    area_of_interest$trends_table %>% dplyr::select(-setdiff(intersect(names(area_of_interest$trends_table), names(area_of_interest$biggest_movers_table)), "species")), by = "species"
  )

trend_species <- trends_df %>% dplyr::pull(species)

trend_species_whr <- purrr::map(1:length(trend_species), function(sp){
  
  whr_table <- terra::extract(x = baseline_whr,
                              y = area_of_interest$gbif_data %>% 
                                dplyr::filter(species == trend_species[sp])
  ) %>% 
    dplyr::pull(WHRNUM) %>% 
    table() %>% 
    stack() %>% 
    purrr::set_names(c("number_occurrences", "WHRNUM")) %>% 
    dplyr::mutate(WHRNUM = as.character(WHRNUM) %>% as.numeric(WHRNUM),
                  proportion_occurrences = number_occurrences/sum(number_occurrences, na.rm = TRUE),
                  species = trend_species[sp]
    ) %>% 
    dplyr::left_join(
      terra::cats(baseline_whr)[[1]] %>% as.data.frame() %>% dplyr::select(WHRNUM, WHRNAME),
      by = "WHRNUM"
    ) %>% 
    dplyr::distinct(., .keep_all = TRUE) 
  
}) %>% 
  bind_rows()

trend_species_whr <- trend_species_whr %>% dplyr::filter(proportion_occurrences >= 0.10)

trends_df <- trends_df %>% 
  cbind(
    data.frame(matrix(0, ncol = length(trend_species_whr$WHRNAME %>% unique()), nrow = nrow(trends_df))) %>% 
      purrr::set_names(trend_species_whr$WHRNAME %>% unique() %>% sort())
  )

for (whr in (trend_species_whr$WHRNAME %>% unique() %>% sort())){
  
  whr_species <- trend_species_whr %>% 
    dplyr::filter(WHRNAME == whr) 
  
  trends_df[which(trends_df$species %in% whr_species$species), whr] <- 1
  
}

names(trends_df) <- gsub(" ", "_", names(trends_df))

predictor_names <- c(c("kingdom", "phylum", "class", "order", "family", "genus"), names(trends_df)[17:ncol(trends_df)])

library(ranger)

X <- trends_df[, predictor_names]
y <- trends_df$reporting_rate_trend_last_ten

rf <- ranger(
  x = X,
  y = y,
  importance = "permutation"
)

# Importance
imp <- sort(importance(rf), decreasing = FALSE)

ggplot(imp %>% stack(), aes(x = ind, y = values)) +
  geom_col() +
  coord_flip() +
  labs(
    y = "Variable Importance",
    x = ""
  )

# Plot top variables
barplot(imp[1:15], las = 2)

# Partial dependence
library(pdp)
partial(
  rf,
  pred.var = "Annual_Grassland",
  train = trends_df, 
  plot = TRUE
)

# Phylum
phylum_plot <- partial(
  rf,
  pred.var = "phylum",
  train = trends_df, 
  plot = TRUE, 
)

get_partials_byTaxon <- function(taxonomic_scale = "phylum"){
  
  taxon_plot <- partial(
    rf,
    pred.var = taxonomic_scale,
    train = trends_df, 
    plot = TRUE, 
  )
  
  taxon_df <- data.frame(taxa = as.factor(taxon_plot$panel.args[[1]]$x),
                         trend = taxon_plot$panel.args[[1]]$y
  ) 
  
  freq <- table(trends_df[[taxonomic_scale]])
  taxon_df_extremes <- taxon_df %>% 
    mutate(n = freq[taxa]) %>%
    filter(n >= 3) %>%
    dplyr::filter(
      trend < quantile(trend, .25) | trend > quantile(trend, .75)
    )
  
  ggplot(taxon_df_extremes, aes(x = taxa, y = trend)) +
    geom_col() +
    coord_flip() 
  
}
get_partials_byTaxon(taxonomic_scale = "order")

get_partials_byHabitat <- function(habitats = gsub(" ", "_", major_habitats)){
  
  habitat_partials <- purrr::map(1:length(habitats), function(hab){
    
    habitat_plot <- pdp::partial(
      rf,
      pred.var = habitats[hab],
      train = trends_df, 
      plot = TRUE, 
    )
    
    freq <- table(trends_df[[habitats[hab]]])
    
    habitat_df <- data.frame(habitat = as.factor(habitat_plot$panel.args[[1]]$x),
                             trend = habitat_plot$panel.args[[1]]$y
    ) %>% 
      mutate(n = freq[habitat]) %>%
      dplyr::filter(habitat == 1) %>% 
      dplyr::mutate(habitat = gsub("_", " ", habitats[hab]))
    
    habitat_df
  }) %>% 
    bind_rows()
  
  return(habitat_partials)
  
}

habitat_partials <- get_partials_byHabitat()
