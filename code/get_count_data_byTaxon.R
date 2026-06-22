# Function to get count data from records
get_count_data_byTaxon <- function(
    records, 
    focal_taxon_name, 
    by_reference_taxon = FALSE,
    reference_taxon_rank = c("genus", "family", "order", "class", "phylum", "kingdom"), 
    taxonomic_resolution = c("species", "genus", "family", "order", "class", "phylum", "kingdom"),
    start_year = 1900,
    ...
){
  
  reference_taxon_rank <- match.arg(reference_taxon_rank)
  taxonomic_resolution <- match.arg(taxonomic_resolution)
  
  if (isFALSE(by_reference_taxon)){
    associated_species <- get_associated_species(
      focal_species = focal_taxon_name,
      analysis_records = records,
      num_species = 50
    )
    associated_species <- associated_species$species
  } else {
    associated_species <- NA
  }
  
  if (by_reference_taxon | (length(associated_species) < 10)){
    
    # Identify reference taxon name
    reference_taxon <- records %>% dplyr::filter_all(any_vars(. %in% focal_taxon)) %>% dplyr::pull(reference_taxon_rank) %>% unique()
    
    # Filter records from reference taxon across relevant years
    reference_records <- records %>% 
      sf::st_set_geometry(NULL) %>% 
      dplyr::filter(.data[[reference_taxon_rank]] == reference_taxon) %>% 
      dplyr::filter(year >= start_year & year < substr(Sys.Date()-365, 1, 4)) 
    
  } else {
    
    associated_species <- gsub("_", " ", associated_species)
    
    reference_records <- records %>% 
      sf::st_set_geometry(NULL) %>% 
      dplyr::filter(species %in% c(focal_taxon_name, associated_species)) %>% 
      dplyr::filter(year >= start_year & year < substr(Sys.Date()-365, 1, 4)) 
    
  }
  
  count_data <- reference_records %>%
    dplyr::group_by(visitID, h7, recordedby, observationdate, year) %>%
    dplyr::count(.data[[taxonomic_resolution]]) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = all_of(taxonomic_resolution), values_from = n, values_fill = 0)
    # tidyr::spread(key = species, value = n, fill = 0) 
  
  return(count_data)
  
}

area_of_interest <- readRDS("data/outputs/One Tam Area of Interest_data.rds")
get_major_taxon_trends <- memoise(function(
    analysis_records = area_of_interest$gbif_data, 
    focal_taxon, 
    use_reference_taxon = TRUE, 
    use_reference_taxon_rank = "phylum", 
    use_taxonomic_resolution = "class",
    full = TRUE,
    resolution = "h6"
){
  
  # Identify appropriate taxonomic resolution
  # Identify relevant taxonomic scale
  # tax_levels <- c("species", "genus", "family", "order", "class", "phylum", "kingdom")
  # taxonomic_scale_counts <- purrr::map(tax_levels, function(tax){
  #   focal_taxon_gbif_occurrences <- analysis_records %>%
  #     dplyr::filter(species == focal_taxon)
  #   taxonomic_name <- focal_taxon_gbif_occurrences[1, tax] %>%  
  #     sf::st_set_geometry(NULL) %>% 
  #     as.character()
  #   out <- data.frame(
  #     taxonomic_level = tax,
  #     taxonomic_name = taxonomic_name,
  #     n = analysis_records %>% 
  #       sf::st_set_geometry(NULL) %>% 
  #       dplyr::select(all_of(tax)) %>% 
  #       filter_all(any_vars(grepl(taxonomic_name, .))) %>% 
  #       nrow() 
  #   )
  # }) %>% 
  #   bind_rows()
  # taxonomic_scale_counts <- taxonomic_scale_counts %>% 
  #   dplyr::filter(
  #     n != taxonomic_scale_counts$n[1] | taxonomic_level == "species"
  #   ) %>% 
  #   dplyr::filter(complete.cases(.))
  # taxonomic_scale_counts %>% 
  #   dplyr::mutate(
  #     diff_from_species = abs((taxonomic_scale_counts$n[1]*10)-taxonomic_scale_counts$n)
  #   )
  # suggested_baseline_taxon <- taxonomic_scale_counts[which.min(abs((taxonomic_scale_counts$n[1]*10)-taxonomic_scale_counts$n)[-1])+1,"taxonomic_level"]
  # 
  # Identify reference taxon name
  reference_taxon <- analysis_records %>% dplyr::filter_all(any_vars(. %in% focal_taxon)) %>% dplyr::pull(all_of(use_reference_taxon_rank)) %>% unique()
  
  # if (taxonomic_scale_counts$n[taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon] < (1.5*taxonomic_scale_counts$n[1])) suggested_baseline_taxon <- taxonomic_scale_counts$taxonomic_level[which(taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon)+1]
  
  # Get observed trends
  ## Get observed detection history
  counts_observed <- get_count_data_byTaxon(
    records = analysis_records, 
    focal_taxon_name = focal_taxon, 
    by_reference_taxon = use_reference_taxon, 
    reference_taxon_rank = use_reference_taxon_rank, 
    taxonomic_resolution = use_taxonomic_resolution
  )
  
  # counts_observed <- counts_observed[-which(rowSums(counts_observed %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year)) == 1), ]
  
  detections_observed <- calculate_detection_data(counts = counts_observed, focal_taxon_name = focal_taxon)
  
  ## Get observed yearly trends
  yearly_trends_observed <- calculate_trends(detections = detections_observed, grp = "year")
  
  ## Get observed spatial trends
  spatial_trends_observed <- calculate_trends(detections = detections_observed, grp = resolution)
  
  ## Get observed spatiotemporal trends
  spatiotemporal_trends_observed <- calculate_trends(detections = detections_observed, grp = c("year", resolution))
  
  if (isTRUE(full)){
    
    # Get randomized (i.e., expected) trends
    ## Get randomized detection history
    detections_random <- run_randomizations(counts = counts_observed, iterations = 1000, focal_tax = focal_taxon)
    
    ## Get randomized yearly trends
    yearly_trends_random <- get_randomized_metric(randomized_detections = detections_random, grp = "year")
    
    # ## Get randomized spatiotemporal trends
    spatiotemporal_trends_random <- get_randomized_metric(randomized_detections = detections_random, grp = c("year", resolution))
    
    # Calculate standardized differences
    ## Yearly
    yearly_trends_difference <- get_standardized_difference(
      observed = yearly_trends_observed,
      randomized = yearly_trends_random,
      metric = "reporting_rate",
      grp = "year"
    ) %>%
      dplyr::mutate(focal_taxon = focal_taxon,
                    reference_taxon = reference_taxon
      )
    
    # difference_dat <- trends_metrics_list[[1]] %>% 
    #   cbind(trends_metrics_list[[2]] %>% 
    #           dplyr::select(ends_with(c("_mn", "_sdev", "_lower", "_upper", "_sd"))),
    #         trends_metrics_list[[3]] %>% 
    #           dplyr::select(ends_with(c("_mn", "_sdev", "_lower", "_upper", "_sd")))
    #   )
    
    ## Spatiotemporal
    spatiotemporal_trends_difference <- get_standardized_difference(
      observed = spatiotemporal_trends_observed,
      randomized = spatiotemporal_trends_random,
      metric = "reporting_rate",
      grp = c("year", resolution)
    ) %>% 
      dplyr::mutate(focal_taxon = focal_taxon,
                    reference_taxon = reference_taxon
      )
    
  } else {
    yearly_trends_difference <- yearly_trends_observed
    spatiotemporal_trends_difference <- NULL
  }
  
  out <- list(
    yearly_trends_observed = yearly_trends_observed,
    spatial_pattern = spatial_trends_observed,
    spatiotemporal_trends_observed = spatiotemporal_trends_observed,
    yearly_trend = yearly_trends_difference,
    spatiotemporal_trend = spatiotemporal_trends_difference 
  )
},
cache = cache_filesystem(CACHE)
)

taxon_trends <- vector("list", length = 4)
names(taxon_trends) <- c("Amphibia", "Aves", "Mammalia", "Squamata")
for (i in 1:length(taxon_trends)){
 
  taxon_trends[[i]] <- purrr::safely(get_major_taxon_trends)(analysis_records = area_of_interest$gbif_data,
                                           focal_taxon = names(taxon_trends)[i],
                                           use_reference_taxon = TRUE,
                                           use_reference_taxon_rank = "phylum", 
                                           use_taxonomic_resolution = "class",
                                           full = TRUE,
                                           resolution = "h5"
  )
  
}


traxon_trends_sd <- purrr::map(1:length(taxon_trends), function(tax){
  taxon_trends[[tax]]$result$yearly_trend %>% dplyr::select(year, reporting_rate_sd) %>% 
    purrr::set_names(c("year", names(taxon_trends)[tax]))
}) %>% 
  plyr::join_all(by = "year")

p <- traxon_trends_sd %>% 
  ggplot(aes(x=year)) +
  geom_ribbon(aes(ymin = -1.96, ymax = 1.96), fill = grey(0.5), alpha = 0.4) +
  geom_hline(yintercept = 0, colour = "black", size = 1) +
  geom_hline(yintercept = 1.96, colour = grey(.5), size = .5) +
  geom_hline(yintercept = -1.96, colour = grey(.5), size = .5) +
  geom_line(aes(y=Amphibia), colour = "#d7191c", size = 1.2, alpha = 1) +
  geom_line(aes(y=Aves), colour = "#fdae61", size = 1.2, alpha = 1) +
  geom_line(aes(y=Mammalia), colour = "#abd9e9", size = 1.2, alpha = 1) +
  geom_line(aes(y=Squamata), colour = "#2c7bb6", size = 1.2, alpha = 1) +
  ylab("Reporting rate \n anomaly") +
  xlab("") +
  theme_linedraw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)
  )
p


