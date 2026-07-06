## Set things up
CACHE <-  "/tmp/Rtmp-urban" # "cache/"
duckdbfs::load_h3()
duckdbfs::load_spatial()
con <- duckdbfs::cached_connection()
DBI::dbExecute(con, "SET threads = 60;")
aws_public_endpoint <- "minio.carlboettiger.info"
aws_s3_endpoint <- "minio.carlboettiger.info"
#'
## Read .rds object from GitHub repository
read_github_rds <- function(owner, repo, path, file, branch = "main") {
  url <- sprintf(
    "https://raw.githubusercontent.com/%s/%s/%s/%s/%s",
    owner, repo, branch, path, file
  )
  tmp <- tempfile(fileext = ".rds")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  readRDS(tmp)
}

## Read .rds object from GitHub repository
read_github_tif <- function(owner, repo, path, file, branch = "main") {
  url <- sprintf(
    "https://raw.githubusercontent.com/%s/%s/%s/%s/%s",
    owner, repo, branch, path, file
  )
  tmp <- tempfile(fileext = ".tif")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  terra::rast(tmp)
}
## Extract GBIF data
## Extract GBIF data
get_gbif_data <- memoise(function(
    aoi,
    occurrence_ids = NULL
) {
  
  library(DBI)
  library(duckdb)
  library(sf)
  library(h3jsr)
  library(dplyr)
  
  # ===============================
  # 1. Connect to DuckDB
  # ===============================
  con <- dbConnect(duckdb())
  
  dbExecute(con, "INSTALL spatial; LOAD spatial;")
  
  dbExecute(con, "SET s3_region='us-west-2';")
  dbExecute(con, "SET s3_url_style='path';")
  dbExecute(con, "SET s3_use_ssl=true;")
  
  # ===============================
  # 2. H3 indices
  # ===============================
  focal_h3_cells <- h3jsr::polygon_to_cells(aoi, res = 8) %>% unlist()
  
  h0 <- h3jsr::get_parent(focal_h3_cells, res = 0) %>%
    unique()
  
  # ===============================
  # 3. Bounding box
  # ===============================
  aoi_bbox <- aoi %>%
    st_transform(4326) %>%
    st_bbox()
  
  bbxmin <- as.numeric(aoi_bbox["xmin"])
  bbxmax <- as.numeric(aoi_bbox["xmax"])
  bbymin <- as.numeric(aoi_bbox["ymin"])
  bbymax <- as.numeric(aoi_bbox["ymax"])
  
  # ===============================
  # 4. H3 filter table
  # ===============================
  h8_df <- data.frame(
    h8 = paste0("0x", focal_h3_cells)
  )
  
  dbWriteTable(con, "h8_filter_raw", h8_df, overwrite = TRUE)
  
  dbExecute(con, "
    CREATE OR REPLACE TABLE h8_filter AS
    SELECT CAST(h8 AS UBIGINT) AS h8
    FROM h8_filter_raw
  ")
  
  # ===============================
  # 5. Build S3 paths
  # ===============================
  base_path <- "s3://us-west-2.opendata.source.coop/cboettig/gbif/2025-06/hex/"
  
  h0_paths <- paste0(
    base_path,
    "h0=",
    h0,
    "/part/*.parquet"
  )
  
  path_sql <- paste0(
    "['",
    paste(h0_paths, collapse = "','"),
    "']"
  )
  
  # ===============================
  # 6. Optional occurrenceid filter
  # ===============================
  occurrence_filter_sql <- ""
  
  if (!is.null(occurrence_ids)) {
    
    occurrence_ids <- unique(as.character(occurrence_ids))
    
    occurrence_sql <- paste0(
      "'",
      gsub("'", "''", occurrence_ids),
      "'",
      collapse = ","
    )
    
    occurrence_filter_sql <- paste0(
      "\n AND t.occurrenceid IN (",
      occurrence_sql,
      ")"
    )
  }
  
  # ===============================
  # 7. Query
  # ===============================
  query <- sprintf("
    SELECT
      t.*,
      UPPER(to_hex(t.h10)) AS h10_hex,
      UPPER(to_hex(t.h9)) AS h9_hex,
      UPPER(to_hex(t.h8)) AS h8_hex,
      UPPER(to_hex(t.h7)) AS h7_hex,
      UPPER(to_hex(t.h6)) AS h6_hex,
      UPPER(to_hex(t.h5)) AS h5_hex,
      ST_Point(t.decimallongitude, t.decimallatitude) AS geom
    FROM read_parquet(%s) t
    INNER JOIN h8_filter f
      ON t.h8 = f.h8
    WHERE t.decimallongitude BETWEEN %f AND %f
      AND t.decimallatitude BETWEEN %f AND %f
      AND t.year >= 1900
      AND t.species IS NOT NULL
      AND t.collectioncode != 'EBIRD'
      %s
  ",
                   path_sql,
                   bbxmin,
                   bbxmax,
                   bbymin,
                   bbymax,
                   occurrence_filter_sql
  )
  
  # ===============================
  # 8. Execute
  # ===============================
  gbif_data <- dbGetQuery(con, query)
  
  # ===============================
  # 9. Convert to sf
  # ===============================
  gbif_sf <- gbif_data %>% 
    mutate(
      longitude = decimallongitude,
      latitude = decimallatitude,
      h5 = h5_hex,
      h6 = h6_hex,
      h7 = h7_hex,
      h8 = h8_hex,
      h9 = h9_hex,
      h10 = h10_hex
    ) %>% 
    select(
      -h5_hex,
      -h6_hex,
      -h7_hex,
      -h8_hex,
      -h9_hex,
      -h10_hex
    ) %>% 
    st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326
    )
  
  # ===============================
  # 10. Exact AOI filter
  # ===============================
  gbif_sf <- gbif_sf[
    which(
      purrr::map_int(
        st_intersects(gbif_sf, aoi),
        length
      ) > 0
    ),
  ]
  
  # ===============================
  # 11. Process output
  # ===============================
  gbif_sf <- gbif_sf %>%
    dplyr::mutate(
      recordedby = as.character(recordedby), # Turn recordedby to character vector
      observationdate = paste0(year, ifelse(nchar(month) == 1, paste0("0", month), month), ifelse(nchar(day) == 1, paste0("0", day), day))
    )
  gbif_sf <- gbif_sf %>%
    dplyr::mutate(observationdate = gsub("NA", "", observationdate))
  
  gbif_sf <- gbif_sf %>%
    dplyr::select(-catalognumber, -recordnumber, -rightsholder, -typestatus, -establishmentmeans, -lastinterpreted, -mediatype, -issue)
  
  return(gbif_sf)
  
},
cache = cache_filesystem(CACHE)
)
# get_gbif_data <- memoise(function(aoi) {
#   
#   library(DBI)
#   library(duckdb)
#   library(sf)
#   library(h3jsr)
#   
#   # ===============================
#   # 1. Connect to DuckDB
#   # ===============================
#   con <- dbConnect(duckdb())
#   
#   dbExecute(con, "INSTALL spatial; LOAD spatial;")
#   
#   dbExecute(con, "SET s3_region='us-west-2';")
#   dbExecute(con, "SET s3_url_style='path';")
#   dbExecute(con, "SET s3_use_ssl=true;")
#   
#   # ===============================
#   # 2. H3 indices
#   # ===============================
#   focal_h3_cells <- h3jsr::polygon_to_cells(aoi, res = 8) %>% unlist()
#   
#   h0 <- h3jsr::get_parent(focal_h3_cells, res = 0) %>%
#     unique()
#   
#   # ===============================
#   # 3. Bounding box
#   # ===============================
#   aoi_bbox <- aoi %>%
#     st_transform(4326) %>%
#     st_bbox()
#   
#   bbxmin <- as.numeric(aoi_bbox["xmin"])
#   bbxmax <- as.numeric(aoi_bbox["xmax"])
#   bbymin <- as.numeric(aoi_bbox["ymin"])
#   bbymax <- as.numeric(aoi_bbox["ymax"])
#   
#   # ===============================
#   # 4. H3 filter table (hex → UBIGINT)
#   # ===============================
#   h8_df <- data.frame(
#     h8 = paste0('0x', focal_h3_cells)
#   )
#   
#   dbWriteTable(con, "h8_filter_raw", h8_df, overwrite = TRUE)
#   
#   dbExecute(con, "
#     CREATE OR REPLACE TABLE h8_filter AS
#     SELECT CAST(h8 AS UBIGINT) AS h8
#     FROM h8_filter_raw
#   ")
#   
#   # ===============================
#   # 5. Build S3 paths
#   # ===============================
#   base_path <- "s3://us-west-2.opendata.source.coop/cboettig/gbif/2025-06/hex/"
#   
#   h0_paths <- paste0(
#     base_path,
#     "h0=",
#     h0,
#     "/part/*.parquet"
#   )
#   
#   path_sql <- paste0("['", paste(h0_paths, collapse = "','"), "']")
#   
#   # ===============================
#   # 6. DuckDB query (UPDATED)
#   # ===============================
#   query <- sprintf("
#     SELECT
#       t.*,
#       UPPER(to_hex(t.h10)) AS h10_hex,
#       UPPER(to_hex(t.h9)) AS h9_hex,
#       UPPER(to_hex(t.h8)) AS h8_hex,
#       UPPER(to_hex(t.h7)) AS h7_hex,
#       UPPER(to_hex(t.h6)) AS h6_hex,
#       UPPER(to_hex(t.h5)) AS h5_hex,
#       ST_Point(t.decimallongitude, t.decimallatitude) AS geom
#     FROM read_parquet(%s) t
#     INNER JOIN h8_filter f
#       ON t.h8 = f.h8
#     WHERE t.decimallongitude BETWEEN %f AND %f
#       AND t.decimallatitude BETWEEN %f AND %f
#       AND t.year >= 1900
#       AND t.species IS NOT NULL
#       AND t.collectioncode != 'EBIRD'
#   ", path_sql, bbxmin, bbxmax, bbymin, bbymax)
#   
#   # ===============================
#   # 7. Execute
#   # ===============================
#   gbif_data <- dbGetQuery(con, query)
#   
#   # ===============================
#   # 8. Convert to sf
#   # ===============================
#   gbif_sf <- gbif_data %>% 
#     dplyr::mutate(
#       longitude = decimallongitude,
#       latitude = decimallatitude,
#       h5 = h5_hex,
#       h6 = h6_hex,
#       h7 = h7_hex,
#       h8 = h8_hex,
#       h9 = h9_hex,
#       h10 = h10_hex
#     ) %>% 
#     dplyr::select(-h5_hex, -h6_hex, -h7_hex, -h8_hex, , -h9_hex, -h10_hex) %>% 
#     st_as_sf(
#       coords = c("longitude", "latitude"),
#       crs = 4326
#     )
#   
#   # optional exact spatial filter
#   gbif_sf <- gbif_sf[
#     which(purrr::map_int(st_intersects(gbif_sf, aoi), length) > 0),
#   ]
#   
#   return(gbif_sf)
# },
# cache = cache_filesystem(CACHE)
# )

# get_gbif_data <- memoise(function(aoi) {
#   # identify h10 cells overlapping area of interest :
#   focal_h3_cells <- h3jsr::polygon_to_cells(geometry = aoi, res = 8) %>% unlist()
#   # identify parent h0 cell containing area of interest h10 cells
#   h0 <- h3jsr::get_parent(focal_h3_cells, res = 0) %>% unique() %>% toupper()
#   # Identify corresponding gbif h0 tile(s)
#   tiles <- paste0(glue("https://{aws_public_endpoint}/public-gbif/hex/h0="), h0, "/part0.parquet")
#   # Preload GBIF records for corresponding h0 tile(s)
#   tile_records <- open_dataset(tiles, recursive = FALSE)
#   aoi_bbox <- aoi %>% 
#     sf::st_transform(4326) %>% 
#     sf::st_bbox()
#   bbxmin <- as.numeric(aoi_bbox[["xmin"]])
#   bbxmax <- as.numeric(aoi_bbox[["xmax"]])
#   bbymin <- as.numeric(aoi_bbox[["ymin"]])
#   bbymax <- as.numeric(aoi_bbox[["ymax"]])
#   gbif_data <- tile_records %>% 
#     dplyr::filter(decimallongitude >= bbxmin & decimallongitude <= bbxmax & decimallatitude >= bbymin & decimallatitude <= bbymax,
#                   year >= 1900,
#                   !is.na(species),
#                   collectioncode != "EBIRD"
#     ) %>% 
#     dplyr::filter(h8 %in% toupper(as.character(focal_h3_cells))) %>% 
#     dplyr::mutate(geom = ST_Point(decimallongitude, decimallatitude)) %>% 
#     duckdbfs::to_sf(crs = 4326)
#   
#   gbif_data <- gbif_data[which(purrr::map_int(st_intersects(gbif_data, aoi), length) > 0), ]
#   
#   return(gbif_data)
# },
# cache = cache_filesystem(CACHE)
# )
## Function to generate hex summary metrics
#'
generate_h3_cells <- function(bbox_poly, res) {
  h3_indexes <- h3jsr::polygon_to_cells(bbox_poly, res)
  h3_cells <- h3jsr::cell_to_polygon(h3_indexes, simple = FALSE)
  h3_cells <- h3_cells %>% 
    dplyr::mutate(h3_address = toupper(h3_address))
  names(h3_cells)[1] <- paste0("h", res)
  h3_cells
}
#' 
## Function to generate hex summary metrics
get_count_summary <- memoise(function(records, base_hexes, metric = c("Records", "Species", "Habitats", "Observers", "Visits", "Locations"), resolution = "h7"){

  # Ensure metric is one of the three options
  metric <- match.arg(metric)
  
  # Create result raster
  metric_hexes <- base_hexes
  
  # Calculate "records" metric (i.e. number of records per cell)
  if (metric == "Records"){

    summary_counts <- records %>% 
      dplyr::count(.data[[resolution]], name = "metric")
    
    summary_counts_sf <- metric_hexes %>% 
      dplyr::left_join(summary_counts, by = resolution)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    
  }
  
  # Calculate "species" metric (i.e. number of taxa per cell)
  if (metric == "Species"){
    summary_counts <- records %>%
      dplyr::group_by(.data[[resolution]]) %>%
      dplyr::summarise(metric = n_distinct(species))
    # Update values of results raster with metric counts
    summary_counts_sf <- metric_hexes %>% 
      dplyr::left_join(summary_counts, by = resolution)   
  }
  
  # Calculate "observers" metric (i.e. number of observers per cell)
  if (metric == "Observers"){
    summary_counts <- records %>%
      dplyr::group_by(.data[[resolution]]) %>%
      dplyr::summarise(metric = n_distinct(recordedby))
    # Update values of results raster with metric counts
    summary_counts_sf <- metric_hexes %>% 
      dplyr::left_join(summary_counts, by = resolution)   
  }
  
  if (metric == "Visits"){
    summary_counts <- records %>%
      dplyr::group_by(.data[[resolution]]) %>%
      dplyr::summarise(metric = n_distinct(visitID))
    
    # Update values of results raster with metric counts
    summary_counts_sf <- metric_hexes %>% 
      dplyr::left_join(summary_counts, by = resolution)   
  }
  
  if (metric == "Locations"){
    
    summary_counts <- records %>%
      dplyr::group_by(.data[[resolution]]) %>%
      dplyr::summarise(metric = round(100*(n_distinct(species)/n_distinct(records$species)), 3))
    
    # Update values of results raster with metric counts
    summary_counts_sf <- metric_hexes %>% 
      dplyr::left_join(summary_counts, by = resolution)   
  }
  
  # Calculate "species" metric (i.e. number of taxa per cell)
  if (metric == "Habitats"){
    summary_counts <- records %>%
      dplyr::group_by(.data[[resolution]]) %>%
      dplyr::summarise(metric = n_distinct(habitat))
    # Update values of results raster with metric counts
    summary_counts_sf <- metric_hexes %>% 
      dplyr::left_join(summary_counts, by = resolution)   
  }
  
  # Return output object
  return(summary_counts_sf)
  
},
cache = cache_filesystem(CACHE)
)

# Function to get species association matrix
get_species_association_matrix <- memoise(function(analysis_records, species = NULL, minimum_number_records = 30){
  
  analysis_records <- analysis_records %>% 
    sf::st_set_geometry(NULL)
  
  if (is.null(species)){
  common_species <- analysis_records %>%
    dplyr::group_by(species) %>%
    dplyr::count(species) %>% 
    dplyr::filter(n >= 50) %>%
    pull(species)
  } else {
    common_species <- intersect(
      (analysis_records %>%
      dplyr::group_by(species) %>%
      dplyr::count(species) %>% 
      dplyr::filter(n >= minimum_number_records) %>%
      pull(species)), 
      species
    )
  }
  
  count_data <- analysis_records %>%
    dplyr::filter(species %in% common_species) %>%
    dplyr::group_by(visitID, species) %>%
    dplyr::count(species) %>%
    dplyr::mutate(species_name = gsub(" ", "_", species)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-species) %>%
    tidyr::spread(key = species_name, value = n, fill = 0) 
  
  #### Generate a reduced set that includes only visits with at least two species and species with at least 50 observations across those visits
  count_data_reduced <- count_data[, -1]
  count_data_reduced <- count_data_reduced[which(rowSums(count_data_reduced) >= 2), ]
  # count_data_reduced <- count_data_reduced[, which(colSums(count_data_reduced) >= 2)]
  # count_data_reduced <- count_data_reduced[which(rowSums(count_data_reduced) >= 2), ]
  #' #### Convert count data to presence absence
  presabs <- count_data_reduced %>% 
    dplyr::mutate_all(~as.numeric(. > 0))
  #' #### Keep only visits with at least two species observed
  presabs <- presabs[which(rowSums(presabs) >= 2), ]
  #' #### Flip site x species matrix to obtain species x sites matrix
  #' #### This is necessary to estimate associations among species, not sites, within vegdist
  presabs_flipped <- t(presabs) %>% as.data.frame() 
  
  # Calculate species associations
  species_association_matrix <- mvabund(count_data_reduced %>% as.matrix())
  species_association_matrix <- manyglm(species_association_matrix ~ 1)
  species_association_matrix <- ecoCopula::cord(species_association_matrix)
  # species_association_matrix <- species_association_matrix$sigma
  
  return(species_association_matrix)
  
},
cache = cache_filesystem(CACHE)
)

# Function to extract associated species
# get_associated_species <- function(focal_species, ### Select a focal species
#                                    association_matrix = species_associations, ### Point to the pairwise association matrix with all species
#                                    num_species = 100 ### Select the total number of associated species in output
# ){
#   
#   ### Update focal_species name
#   focal_species <- gsub(" ", "_", focal_species)
#   ### Extract the focal species' column from the association matrix 
#   association_species_ordered <- association_matrix[focal_species, ]
#   ### Order the associated species vector by decreasing association 
#   association_species_ordered <- association_species_ordered %>% 
#     stack() %>% 
#     purrr::set_names(c("association", "species")) %>% 
#     arrange(desc(association))
#   ### Extract the most associated species
#   #### Identify nonzero associations
#   # most_associated_species <- association_species_ordered %>% dplyr::filter(association >= 0.2) %>% dplyr::pull(species)
#   #### Extract the first n species associations
#   most_associated_species <- association_species_ordered %>% dplyr::pull(species) %>% head(num_species)
# 
#   ### Return vector of most associated species
#   return(most_associated_species)
#   
# }
get_shared_visits <- function(
    focal_species, ### Select a focal species
    analysis_records,
    trends_table
    ){
  
  association_species_full <- analysis_records %>% 
    dplyr::filter(visitID %in% (
      analysis_records %>% 
        dplyr::filter(species == focal_species) %>% 
        dplyr::pull(visitID)
    )
    ) %>% 
    dplyr::distinct(species, visitID, .keep_all = TRUE) %>% 
    dplyr::pull(species) %>% 
    base::table() %>% 
    utils::stack() %>% 
    purrr::set_names(c("shared_visits", "species")) %>% 
    dplyr::arrange(dplyr::desc(shared_visits))
  
  association_species_full <- association_species_full %>% 
    dplyr::left_join(
      trends_table %>% dplyr::select(species, number_visits), by = "species"
    )
  # association_species_full <- association_species_full %>% 
  #   dplyr::filter(
  #     species %in% (setdiff(association_species_full$species, focal_species) %>% head(num_species))
  #   )
  
  return(association_species_full)
  
}

get_associated_species <- function(focal_species, ### Select a focal species
                                   analysis_records,
                                   trends_table
                                   ){
  
  ### Extract the focal species' column from the association matrix 
  association_species_first <- get_shared_visits(
    focal_species = focal_species,
    analysis_records = analysis_records,
    trends_table = trends_table
  )
  
  focal_species_visits <- association_species_first$number_visits[1]
  total_visits_needed <- focal_species_visits*10
  total_associated_species <- NULL
  association_species_first <- association_species_first[-1, ]
  
  most_shared_visits_species <- association_species_first %>% 
    dplyr::filter(shared_visits > 1) 
  
  association_species_full <- association_species_first
  
  ### Extract the most associated species
  if (((nrow(association_species_first) < 100) & (nrow(most_shared_visits_species) < 10)) & sum(association_species_first$shared_visits) < total_visits_needed){
    
    if (nrow(most_shared_visits_species) >= 3){
      species_extended <- most_shared_visits_species$species[1:min(c(length(most_shared_visits_species$species), 100))]
    } else {
      species_extended <- association_species_first$species[1:min(c(length(association_species_first$species), 100))]
    }
    
    associated_species_extended <- purrr::map(species_extended, function(sp){
      get_shared_visits(focal_species = sp, 
                        analysis_records = analysis_records,
                        trends_table = trends_table
                        )
    }) %>% 
      dplyr::bind_rows() %>% 
      dplyr::group_by(species, number_visits) %>% 
      dplyr::summarize(shared_visits = sum(shared_visits, na.rm = TRUE)) %>% 
      dplyr::arrange(dplyr::desc(shared_visits)) %>% 
      dplyr::filter(species != focal_species) %>% 
      as.data.frame()
 
    association_species_full <- association_species_full %>% 
      base::rbind(associated_species_extended) %>% 
      dplyr::distinct(species, .keep_all = TRUE)

    # species_for_visits_needed <- which(cumsum(association_species_full$number_visits) > total_visits_needed)[1]
    # total_associated_species <- ifelse(species_for_visits_needed < 100, species_for_visits_needed, 100)
    
    total_associated_species <- ifelse(nrow(association_species_full) > 100, 100, 
                                       nrow(association_species_full)
    )
    
  } else {
    
    # association_species_full_class <- association_species_full %>% 
    #   dplyr::left_join(trends_table %>% dplyr::select(species, class), by = "species") %>% 
    #   dplyr::filter(class == (trends_table %>% dplyr::filter(species == focal_species) %>% dplyr::pull(class)))
    # 
    # if (sum(cumsum(association_species_full_class$number_visits) > total_visits_needed) > 0){
    #   total_associated_species <- which(cumsum(association_species_full_class$number_visits) > total_visits_needed)[1]
    # } else {
    #   total_associated_species <- which(cumsum(association_species_full$number_visits) > total_visits_needed)[1]
    # }
    total_associated_species <- 100
  }

  association_species_full <- association_species_full[1:total_associated_species, ]
  row.names(association_species_full) <- 1:nrow(association_species_full)
  ### Return vector of most associated species
  return(association_species_full)
  
}

# Function to get count data from records
get_count_data <- function(
    records, 
    focal_taxon_name, 
    trends_table,
    by_reference_taxon = FALSE,
    reference_taxon_rank = c("genus", "family", "order", "class", "phylum", "kingdom"), 
    start_year = 1900,
    ...
    ){
  
  reference_taxon_rank <- match.arg(reference_taxon_rank)
  
  if (isFALSE(by_reference_taxon)){
    associated_species <- get_associated_species(
      focal_species = focal_taxon_name,
      analysis_records = records,
      trends_table = trends_table
    )
    associated_species <- associated_species$species
  } else {
    associated_species <- NA
  }
  
  if (by_reference_taxon){
    
    # Identify reference taxon name
    reference_taxon <- records %>% dplyr::filter(species == focal_taxon_name) %>% dplyr::pull(reference_taxon_rank) %>% unique()
    
    # Filter records from reference taxon across relevant years
    reference_records <- records %>% 
      dplyr::filter(.data[[reference_taxon_rank]] == reference_taxon) %>% 
      dplyr::filter(year >= start_year & year < substr(Sys.Date()-365, 1, 4)) 
    
  } else {
    
    associated_species <- gsub("_", " ", associated_species)
    
    reference_records <- records %>% 
      dplyr::filter(species %in% c(focal_taxon_name, associated_species)) %>% 
      dplyr::filter(year >= start_year & year < substr(Sys.Date()-365, 1, 4)) 
    
  }
  
  count_data <- reference_records %>%
    dplyr::group_by(visitID, h7, recordedby, observationdate, year) %>%
    dplyr::count(species) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = species, value = n, fill = 0) 

  return(count_data)
  
}

# Function to get detection data from counts
calculate_detection_data <- function(counts = count_data, focal_taxon_name, ...){
  
  detection_data <- counts %>% 
    dplyr::mutate(dplyr::across(dplyr::all_of(base::setdiff(names(counts), c("visitID", "h5", "h6", "h7", "h8", "recordedby", "observationdate", "year"))), ~as.numeric(. > 0)))
  
  focal_species_detection_data <- detection_data %>% 
    dplyr::mutate(focal_species_count = counts[[focal_taxon_name]],
                  focal_species_detection = ifelse(focal_species_count > 0, 1, 0),
                  total_number_observations = counts %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year) %>% rowSums(),
                  species_list_length = detection_data %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year) %>% rowSums()
    )
  
  return(focal_species_detection_data)
  
}

# Function to calculate spatiotemporal trends from detection history
calculate_trends <- function(detections = focal_species_detection_data, grp = NULL){

  detections <- detections %>% 
    dplyr::mutate(
      h6 = h3jsr::get_parent(h7, res = 6),
      h5 = h3jsr::get_parent(h7, res = 5)
    )
  
  if (is.null(grp)) grp = c("year", "h7")
    
  # # Group detections
  # trend_data <- detections %>%
  #   dplyr::group_by(year, h7) 
  # 
  # } else {
  #   
  #   # Group detections
  #   trend_data <- detections %>%
  #     dplyr::group_by(.data[[grp]]) 
  #   
  # }
  
  trend_data <- detections %>%
    dplyr::group_by(across(all_of(grp)))
    
  trend_data <- trend_data %>%
    dplyr::summarise(
      focal_species_count = sum(focal_species_count, na.rm = TRUE),
      focal_species_detection = sum(focal_species_detection, na.rm = TRUE),
      total_number_observations = sum(total_number_observations, na.rm = TRUE),
      species_list_length = sum(species_list_length, na.rm = TRUE),
      n_visits = dplyr::n()
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      proportion_observations = focal_species_count/total_number_observations, # mean(proportion_observations),
      proportion_species = focal_species_detection/species_list_length,
      reporting_rate = focal_species_detection/n_visits
    )
  
  return(trend_data)
  
}

# Get randomized versions of observed detection history
run_randomizations <- memoise::memoise(function(counts = count_dat, iterations = 100, focal_tax){
  randomized_data <- purrr::map(1:iterations, function(i){
    count_matrix <- data.matrix(counts %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year))
    rownames(count_matrix) <- counts$visitID
    
    if (nrow(counts) < 10000){
      count_dat_random <- picante::randomizeMatrix(count_matrix, null.model="independentswap")
    } else {
      count_dat_random <- picante::randomizeMatrix(count_matrix, null.model="richness")
    }
    
    count_dat_random <- base::cbind(counts %>% dplyr::select(visitID, h7, recordedby, observationdate, year), count_dat_random %>% as.data.frame())
      rm(count_matrix)
  gc()
    detection_dat_random <- calculate_detection_data(counts = count_dat_random, focal_taxon_name = focal_tax)
  })

  return(randomized_data)
}, 
cache = cache_filesystem(CACHE)
)

# Get randomized versions of focal species trends
get_randomized_metric <- memoise::memoise(function(randomized_detections, grp = "year", ...){
  
  if (is.null(grp)) grp = c("year", "h7")
  
  randomized_trends <- purrr::map(1:length(randomized_detections), function(i){
    grp_dat_random <- calculate_trends(detections = randomized_detections[[i]], grp = grp)
    out <- data.frame(grp_dat_random[, grp], 
                      proportion_observations = grp_dat_random$proportion_observations, 
                      proportion_species = grp_dat_random$proportion_species, 
                      reporting_rate = grp_dat_random$reporting_rate, 
                      randomization = i)
    names(out)[1:length(grp)] <- grp
    
    out
    
  }) %>% dplyr::bind_rows()
  
  return(randomized_trends)
  
},
cache = cache_filesystem(CACHE)
)

# Calculate standardized difference
get_standardized_difference <- memoise(function(observed, randomized, metric = c("proportion_observations", "proportion_species", "reporting_rate"), grp = NULL){
  
  if (is.null(grp)) grp = c("year", "h7")
  
  metric <- match.arg(metric)
  
  randomized_values <- randomized %>% 
    dplyr::group_by(across(grp)) %>% 
    dplyr::summarise(
      mn = mean(.data[[metric]], na.rm = TRUE),
      lower = quantile(.data[[metric]], .025, na.rm = TRUE),
      upper = quantile(.data[[metric]], .975, na.rm = TRUE),
      sdev = sd(.data[[metric]], na.rm = TRUE)
    ) 
  
  sd_dat <- observed %>% 
    dplyr::select(all_of(grp), all_of(metric)) %>% 
    purrr::set_names(c(grp, "observed")) %>% 
    dplyr::left_join(observed, by = grp) %>% 
    dplyr::left_join(randomized_values, by = grp) %>% 
    dplyr::mutate(
      sd = (observed - mn)/sdev
    )
  names(sd_dat)[which(names(sd_dat) == "mn")] <- paste0(metric, "_mn")
  names(sd_dat)[which(names(sd_dat) == "sdev")] <- paste0(metric, "_sdev")
  names(sd_dat)[which(names(sd_dat) == "upper")] <- paste0(metric, "_upper")
  names(sd_dat)[which(names(sd_dat) == "lower")] <- paste0(metric, "_lower")  
  names(sd_dat)[which(names(sd_dat) == "sd")] <- paste0(metric, "_sd")
  
  return(sd_dat)
  
},
cache = cache_filesystem(CACHE)
)

get_species_trends <- memoise(function(
    analysis_records = area_of_interest$gbif_data, 
    focal_taxon, 
    use_reference_taxon = FALSE, 
    trends_table,
    full = TRUE,
    resolution = "h6"
    ){
  
  analysis_records <- analysis_records %>% 
    sf::st_set_geometry(NULL)
  
  # Identify appropriate taxonomic resolution
  # Identify relevant taxonomic scale
  tax_levels <- c("species", "genus", "family", "order", "class", "phylum", "kingdom")
  taxonomic_scale_counts <- purrr::map(tax_levels, function(tax){
    focal_taxon_gbif_occurrences <- analysis_records %>%
      dplyr::filter(species == focal_taxon)
    taxonomic_name <- focal_taxon_gbif_occurrences[1, tax] %>%  
      as.character()
    out <- data.frame(
      taxonomic_level = tax,
      taxonomic_name = taxonomic_name,
      n = analysis_records %>% 
        # sf::st_set_geometry(NULL) %>% 
        dplyr::select(all_of(tax)) %>% 
        dplyr::filter_all(dplyr::any_vars(grepl(taxonomic_name, .))) %>% 
        nrow() 
    )
  }) %>% 
    dplyr::bind_rows()
  taxonomic_scale_counts <- taxonomic_scale_counts %>% 
    dplyr::filter(
      n != taxonomic_scale_counts$n[1] | taxonomic_level == "species"
    ) %>% 
    dplyr::filter(complete.cases(.))
  taxonomic_scale_counts %>% 
    dplyr::mutate(
      diff_from_species = abs((taxonomic_scale_counts$n[1]*10)-taxonomic_scale_counts$n)
    )
  suggested_baseline_taxon <- taxonomic_scale_counts[which.min(abs((taxonomic_scale_counts$n[1]*10)-taxonomic_scale_counts$n)[-1])+1,"taxonomic_level"]
  
  # if (taxonomic_scale_counts$n[taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon] < (1.5*taxonomic_scale_counts$n[1])) suggested_baseline_taxon <- taxonomic_scale_counts$taxonomic_level[which(taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon)+1]
  
  # Get observed trends
  ## Get observed detection history
  counts_observed <- get_count_data(
    records = analysis_records, 
    focal_taxon_name = focal_taxon, 
    trends_table = trends_table,
    by_reference_taxon = use_reference_taxon, 
    reference_taxon_rank = suggested_baseline_taxon
    )
  
  if (nrow(counts_observed) > sum(counts_observed[[focal_taxon]] > 0)*10){
    counts_observed <- counts_observed[-which(rowSums(counts_observed %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year)) == 1), ]
  }
  
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
  # num_iterations <- 100
  # num_iterations <- ifelse(nrow(counts_observed) < 50, 500, 
  #                          ifelse(nrow(counts_observed) >= 50 & nrow(counts_observed) < 100, 200, 
  #                                 100
  #                          )
  # )
  num_iterations <- ifelse(nrow(counts_observed) >= 10000, 100,
                           ifelse(nrow(counts_observed) >= 1000 & nrow(counts_observed) < 10000, 200,
                                  300
                           )
  )

  # num_iterations <- ifelse(num_iterations < 100, 100, num_iterations)
  detections_random <- run_randomizations(counts = counts_observed, iterations = num_iterations, focal_tax = focal_taxon)

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
                  reference_taxon = paste(suggested_baseline_taxon, taxonomic_scale_counts$taxonomic_name[taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon])
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
                  reference_taxon = paste(suggested_baseline_taxon, taxonomic_scale_counts$taxonomic_name[taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon])
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

# get_yearly_trend_metrics <- function(trends_folder = "data/outputs/Año Nuevo Island Reserve_trends"){
#   
#   trends_files <- list.files(trends_folder, full.names = TRUE)
#   
#   # Calculate trend metrics
#   metrics_df <- purrr::map(1:length(trends_files), function(sp){
#     dat <- readRDS(trends_files[sp])$yearly_trend
#     # sp_name <- gsub(" ", "_", names(trends_list)[sp])
#     sp_name <- gsub(paste0(trends_folder, "/", "|_trend.rds"), "", trends_files[sp])
#     if ("reporting_rate_sd" %in% names(dat)){
#       out_metrics <- data.frame(
#         species = sp_name,
#         ## Calculate metrics
#         # years above expected in the last 5
#         reporting_rate_above_signif_last_five = ((dat %>% dplyr::pull(reporting_rate_sd)) > 1.96) %>% tail(5) %>% sum(),
#         # years above expected in the last 10
#         reporting_rate_above_signif_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) > 1.96) %>% tail(10) %>% sum(),
#         # years below expected in the last 10
#         reporting_rate_below_signif_last_five = ((dat %>% dplyr::pull(reporting_rate_sd)) < -1.96) %>% tail(5) %>% sum(),
#         # years below expected in the last 10
#         reporting_rate_below_signif_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) < -1.96) %>% tail(10) %>% sum(),
#         # years above 0 in the last 5
#         reporting_rate_above_0_last_five = ((dat %>% dplyr::pull(reporting_rate_sd)) > 0) %>% tail(5) %>% sum(),
#         # years below 0 in the last 10
#         reporting_rate_above_0_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) > 0) %>% tail(10) %>% sum(),
#         # years above 0 in the last 5
#         reporting_rate_below_0_last_five = ((dat %>% dplyr::pull(reporting_rate_sd)) < 0) %>% tail(5) %>% sum(),
#         # years below 0 in the last 10
#         reporting_rate_below_0_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) < 0) %>% tail(10) %>% sum(),
#         # 5 year correlation in the last 5
#         reporting_rate_trend_last_five = dat %>% dplyr::filter(year >= max(year, na.rm = TRUE)-5) %>% dplyr::mutate(new = cor(reporting_rate_sd, year, method = "spearman")) %>% dplyr::pull(new) %>% head(1),
#         # 5 year correlation in the last 10
#         reporting_rate_trend_last_ten = dat %>% dplyr::filter(year >= max(year, na.rm = TRUE)-10) %>% dplyr::mutate(new = cor(reporting_rate_sd, year, method = "spearman")) %>% dplyr::pull(new) %>% head(1)
#       )
#     } else {
#       out_metrics <- data.frame(
#         species = sp_name,
#         ## Calculate metrics
#         # years above expected in the last 10
#         reporting_rate_above_last_ten = NA,
#         # years below expected in the last 10
#         reporting_rate_below_last_ten = NA,
#         # 5 year correlation in the last 5
#         reporting_rate_trend_last_five = NA,
#         # 5 year correlation in the last 10
#         reporting_rate_trend_last_ten = NA
#       )
#     }
#     out_metrics
#   }) %>% dplyr::bind_rows()
#   
#   return(metrics_df)
# }

get_yearly_trend_metrics <- function(trends = "data/outputs/Año Nuevo Island Reserve_trends"){
  
  if (class(trends) == "character"){
    
    trends_list <- purrr::map(list.files(trends, full.names = TRUE), readRDS) %>%
      purrr::set_names(gsub("_trend.rds", "", list.files(trends)))
    
  } else {
    
    trends_list <- trends
  }

  metrics_df <- purrr::map(1:length(trends_list), function(t){
    
    dat <- trends_list[[t]]$yearly_trend
    
    if ("reporting_rate_sd" %in% names(dat)){
      out_metrics <- data.frame(
        species = names(trends_list)[t],
        ## Calculate metrics
        # years above expected in the last 5
        reporting_rate_above_signif_last_five = ((dat %>% dplyr::pull(reporting_rate_sd)) > 1.96) %>% tail(5) %>% sum(),
        # years above expected in the last 10
        reporting_rate_above_signif_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) > 1.96) %>% tail(10) %>% sum(),
        # years below expected in the last 10
        reporting_rate_below_signif_last_five = ((dat %>% dplyr::pull(reporting_rate_sd)) < -1.96) %>% tail(5) %>% sum(),
        # years below expected in the last 10
        reporting_rate_below_signif_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) < -1.96) %>% tail(10) %>% sum(),
        # years above 0 in the last 5
        reporting_rate_above_0_last_five = ((dat %>% dplyr::pull(reporting_rate_sd)) > 0) %>% tail(5) %>% sum(),
        # years below 0 in the last 10
        reporting_rate_above_0_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) > 0) %>% tail(10) %>% sum(),
        # years above 0 in the last 5
        reporting_rate_below_0_last_five = ((dat %>% dplyr::pull(reporting_rate_sd)) < 0) %>% tail(5) %>% sum(),
        # years below 0 in the last 10
        reporting_rate_below_0_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) < 0) %>% tail(10) %>% sum(),
        # 5 year correlation in the last 5
        reporting_rate_trend_last_five = dat %>% dplyr::filter(year >= max(year, na.rm = TRUE)-5) %>% dplyr::mutate(new = cor(reporting_rate_sd, year, method = "spearman")) %>% dplyr::pull(new) %>% head(1),
        # 5 year correlation in the last 10
        reporting_rate_trend_last_ten = dat %>% dplyr::filter(year >= max(year, na.rm = TRUE)-10) %>% dplyr::mutate(new = cor(reporting_rate_sd, year, method = "spearman")) %>% dplyr::pull(new) %>% head(1)
      )
    } else {
      out_metrics <- data.frame(
        species = names(trends_list)[t],
        ## Calculate metrics
        # years above expected in the last 10
        reporting_rate_above_last_ten = NA,
        # years below expected in the last 10
        reporting_rate_below_last_ten = NA,
        # 5 year correlation in the last 5
        reporting_rate_trend_last_five = NA,
        # 5 year correlation in the last 10
        reporting_rate_trend_last_ten = NA
      )
    }
    
    out_metrics
    
  }) %>% dplyr::bind_rows()

  return(metrics_df)
}

plot_yearly_trends <- memoise(function(sd_dat, metric = c("proportion_observations", "proportion_species", "reporting_rate"), ...){
  
  metric <- match.arg(metric)
  reference_taxon <- sd_dat$reference_taxon[1]
  lim_x <- c(min(c(min(sd_dat$year, na.rm = TRUE), 1950)), max(sd_dat$year, na.rm = TRUE))
  if (is.null(sd_dat$focal_taxon[1])) sd_dat$focal_taxon <- "focal taxon"
  
  scale_factor <-
    quantile(sd_dat$focal_species_count, .95, na.rm = TRUE) /
    quantile(sd_dat$total_number_observations, .95, na.rm = TRUE)
  
  offset <- 2

  legend_dat <- bind_rows(
    transmute(sd_dat,
              year,
              value = focal_species_count,
              Observations = paste0(sd_dat$focal_taxon[1], " observations")
    ),
    transmute(sd_dat,
              year,
              value = total_number_observations * scale_factor + offset,
              Observations = "Reference taxa observations"
    )
  ) %>% 
    dplyr::mutate(
      Observations = factor(Observations, levels = c("Reference taxa observations", paste0(sd_dat$focal_taxon[1], " observations")))
    )
  
  # Number of focal observations
  p1 <- ggplot(legend_dat, aes(year, value, fill = Observations)) +
    geom_col(
      alpha = 1,
      width = .9, 
      colour = "black", 
      linewidth = 0.1
    ) +
    scale_y_continuous(
      name = paste0("Observations of \n", sd_dat$focal_taxon[1]), # Primary Left Axis Title
      sec.axis = sec_axis(~ . / scale_factor, name = paste0("Observations of \n reference taxa")) # Secondary Right Axis
    ) +
    xlab("") +
    xlim(lim_x) +
    theme_linedraw() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(.02, .98),
      legend.justification = c(0, 1),
      legend.title = element_blank(), 
      legend.text = element_text(family = "Helvetica", size = 15), 
      legend.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(family = "Helvetica", size = 14),
          axis.text.y = element_text(family = "Helvetica", size = 15), 
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0)      
    ) +
    scale_fill_manual(
      values = c(
        setNames("tomato", paste0(sd_dat$focal_taxon[1], " observations")),
        "Reference taxa observations" = "#EEE"
      )
    )
  
  print(p1)

  metric_labs <- data.frame(metric_name = c("proportion_observations", "proportion_species", "reporting_rate"),
                            metric_lab = c("Proportion of \n observations", "Proportion of \n species", "Reporting rate")
  )
  
  if ("reporting_rate_mn" %in% names(sd_dat)){
    
    sd_dat <- sd_dat %>%
      arrange(year) %>%
      mutate(
        anomaly_sign =
          ifelse(reporting_rate_sd >= 0,
                 "Positive",
                 "Negative")
      )
    
    p2 <- sd_dat %>% 
      ggplot(aes(x=year, y=reporting_rate_sd)) +
      # geom_ribbon(
      #   aes(
      #     ymin = anomaly_lower_smooth,
      #     ymax = anomaly_upper_smooth
      #   ),
      #   fill = "grey90", 
      #   alpha = 0.3
      #   
      # ) +
      geom_col(
        aes(fill = anomaly_sign),
        width = .9,
        linewidth = .2,
        colour = "black"
      ) +
      geom_hline(
        yintercept = 0,
        linewidth = .5,
        colour = "black"
      ) +
      scale_fill_manual(
        values = c(
          Positive = "#2C7FB8",
          Negative = "#D95F0E"
        )
      ) +
      ylab(paste0(metric_labs %>% dplyr::filter(metric_name == metric) %>% dplyr::pull(metric_lab), "\n anomaly")) +
      xlab("") +
      xlim(lim_x) +
      guides(fill = "none") +
      theme_linedraw() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.title = element_text(family = "Helvetica", size = 14),
            axis.text = element_text(family = "Helvetica", size = 15),
            plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )  
    
  } else {
    p2 <- sd_dat %>% 
      ggplot(aes(x=year, y=reporting_rate, group = NULL)) +
      geom_bar(stat = "identity",
               fill = "tomato",
               width = .9,
               linewidth = .2,
               colour = "black"
               ) +
      ylab(metric_labs %>% dplyr::filter(metric_name == metric) %>% dplyr::pull(metric_lab)) +
      xlab("") +
      xlab("") +
      xlim(lim_x) +
      theme_linedraw() +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(family = "Helvetica", size = 14),
        axis.text = element_text(family = "Helvetica", size = 15),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
    
  }
  
  
  final_plot <-
    
    (p1 / p2) +
    
    patchwork::plot_layout(
      heights = c(
        0.4,
        0.6
      ), 
    )
  
  # gg <- plotly::plotly_build(final_plot) %>%
  #   plotly::config(displayModeBar = FALSE) %>%
  #   plotly::layout(# plot_bgcolor  = "rgba(0, 0, 0, 0)",
  #     paper_bgcolor = "rgba(0, 0, 0, 0)",
  #     font = list(family = "Roboto", size = 14),
  #     xaxis = list(titlefont = list(size = 13),
  #                  tickfont = list(size = 13)),
  #     yaxis = list(titlefont = list(size = 13),
  #                  tickfont = list(size = 13))
  #   )

  return(final_plot)
  
},
cache = cache_filesystem(CACHE)
)

# plot_yearly_trends <- memoise(function(sd_dat, metric = c("proportion_observations", "proportion_species", "reporting_rate"), ...){
#   
#   metric <- match.arg(metric)
#   reference_taxon <- sd_dat$reference_taxon[1]
#   lim_x <- c(min(c(min(sd_dat$year, na.rm = TRUE), 1950)), max(sd_dat$year, na.rm = TRUE))
#     
#   # Number of focal observations
#   p1 <- ggplot(data = sd_dat, aes(x = year, y = focal_species_count)) + 
#     geom_col(fill = "tomato", colour = "transparent", alpha = 0.8, width = 1) + 
#     ylab(paste0("Observations of \n", sd_dat$focal_taxon[1])) +
#     xlab("") +
#     xlim(lim_x) +
#     # annotate("text", x = quantile(temporal_trend_data$year, .26), y = max(temporal_trend_data$focal_species_count)+(0.08 *max(temporal_trend_data$focal_species_count)), label = paste0("Observations of ", taxon_data$info$scientificName, rep(" ", num_chars_missing[[1]])), hjust = 1) +
#     theme_linedraw() +
#     theme(legend.position = "none",
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           axis.title = element_text(size = 9),
#           axis.text.x = element_blank(),
#           axis.text = element_text(size = 8)
#     )
#   
#   # Total number of observations of reference taxon
#   p2 <- ggplot(data = sd_dat, aes(x = year, y = total_number_observations)) + 
#     geom_col(fill = grey(.5), colour = "transparent", alpha = 0.8, width = 1) + 
#     ylab(paste0("Observations of \n reference taxa")) +
#     xlab("") +
#     xlim(lim_x) +
#     # annotate("text", x = quantile(temporal_trend_data$year, .24), y = max(temporal_trend_data$total_number_observations)+(0.08 *max(temporal_trend_data$total_number_observations)), label = paste0("Observations of ", referenceTaxon, " ", reference_taxon_name, rep(" ", num_chars_missing[[2]])), hjust = 1) +
#     theme_linedraw() +
#     theme(legend.position = "none",
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           axis.title = element_text(size = 9),
#           axis.text.x = element_blank(),
#           axis.text = element_text(size = 8)        
#     )
#   
#   metric_labs <- data.frame(metric_name = c("proportion_observations", "proportion_species", "reporting_rate"),
#                             metric_lab = c("Proportion of \n observations", "Proportion of \n species", "Reporting rate")
#   )
#   
#  if ("reporting_rate_mn" %in% names(sd_dat)){
#     
#   p3 <- sd_dat %>% 
#     ggplot(aes(x=year, y=reporting_rate_mn)) +
#     geom_bar(stat = "identity", fill = grey(.5), width = 0.6, alpha = 0.8) +
#     geom_errorbar(aes(x=year, ymin=reporting_rate_lower, ymax=reporting_rate_upper), width=0.5, colour=grey(.5), alpha=0.8, size=0.5) +
#     geom_bar(data = sd_dat, mapping = aes(x=year, y=reporting_rate, group=NULL), stat = "identity", fill = "tomato", colour = "transparent", alpha = 0.5, width = 1) +
#     ylab(metric_labs %>% dplyr::filter(metric_name == metric) %>% dplyr::pull(metric_lab)) +
#     xlab("") +
#     xlim(lim_x) +
#     # annotate("text", x = quantile(temporal_trend_data$year, .21), y = max(temporal_trend_data$proportion_observations)+(0.08 *max(temporal_trend_data$proportion_observations)), label = paste0("Proportion of Observations"), hjust = 1) +
#     theme_linedraw() +
#     theme(legend.position = "none",
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           axis.title = element_text(size = 9),
#           axis.title.x = element_blank(),
#           axis.text.x = element_blank(),
#           axis.text = element_text(size = 8)
#     )
#   
#   p4 <- sd_dat %>% 
#     ggplot(aes(x=year, y=reporting_rate_sd)) +
#     geom_ribbon(aes(ymin = -1.96, ymax = 1.96), fill = grey(0.5), alpha = 0.1) +
#     geom_hline(yintercept = 0, colour = "black", size = 1) +
#     geom_hline(yintercept = 1.96, colour = grey(.5), size = .5) +
#     geom_hline(yintercept = -1.96, colour = grey(.5), size = .5) +
#     geom_line(colour = "tomato", size = 1.2, alpha = 1) +
#     geom_line(aes(x=year, y=(reporting_rate - reporting_rate_lower)/reporting_rate_sdev), colour = "tomato", size = 0.5, alpha = 0.5) +
#     geom_line(aes(x=year, y=(reporting_rate - reporting_rate_upper)/reporting_rate_sdev), colour = "tomato", size = 0.5, alpha = 0.5) +
#     ylab(paste0(metric_labs %>% dplyr::filter(metric_name == metric) %>% dplyr::pull(metric_lab), "\n anomaly")) +
#     xlab("") +
#     xlim(lim_x) +
#     # annotate("text", x = quantile(temporal_trend_data$year, .20), y = 1.08, label = paste0("Modeled probability of detection", rep(" ", num_chars_missing[[4]])), hjust = 1) +
#     theme_classic() +
#     theme(legend.position = "none",
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           axis.title = element_text(size = 9),
#           axis.text = element_text(size = 8)
#     )
#   
#   p <- subplot(p1, p2, p3, p4, nrows = 4, shareX = TRUE, titleX = TRUE, titleY = TRUE, margin = 0.01, which_layout = 1)
#   
#  } else {
#    p3 <- sd_dat %>% 
#      ggplot(aes(x=year, y=reporting_rate, group = NULL)) +
#      geom_bar(stat = "identity", fill = "tomato", colour = "transparent", alpha = 0.5, width = 1) +
#      ylab(metric_labs %>% dplyr::filter(metric_name == metric) %>% dplyr::pull(metric_lab)) +
#      xlab("") +
#      # annotate("text", x = quantile(temporal_trend_data$year, .21), y = max(temporal_trend_data$proportion_observations)+(0.08 *max(temporal_trend_data$proportion_observations)), label = paste0("Proportion of Observations"), hjust = 1) +
#      xlab("") +
#      xlim(lim_x) +
#      theme_linedraw() +
#      theme(legend.position = "none",
#            panel.grid.major = element_blank(), 
#            panel.grid.minor = element_blank(),
#            axis.title = element_text(size = 9),
#            axis.text = element_text(size = 8)
#      )
#    
#    p <- subplot(p1, p2, p3, nrows = 3, shareX = TRUE, titleX = TRUE, titleY = TRUE, margin = 0.01, which_layout = 1)
#    
#  }
#   
#   gg <- plotly::plotly_build(p) %>%
#     plotly::config(displayModeBar = FALSE) %>%
#     plotly::layout(# plot_bgcolor  = "rgba(0, 0, 0, 0)",
#       paper_bgcolor = "rgba(0, 0, 0, 0)",
#       font = list(family = "Helvetica", size = 14), 
#       xaxis = list(titlefont = list(size = 13),
#                    tickfont = list(size = 13)),
#       yaxis = list(titlefont = list(size = 13),
#                    tickfont = list(size = 13))
#     )
#   
#   return(gg)
#   
# },
# cache = cache_filesystem(CACHE)
# )

get_taxon_trends <- function(
    aoi = area_of_interest,
    focal_taxon,
    metric = "reporting_rate_sd",
    make_plot = FALSE,
    plot_label = NULL
){
  
  out <- NULL
  
  if (length(focal_taxon) == 1){
  ##### Identify focal taxon species
  taxon_table <- aoi$species_table %>% 
    # dplyr::filter(class == "Amphibia") %>%
    dplyr::filter_all(any_vars(. %in% focal_taxon))
  } else {
    taxon_table <- aoi$species_table %>% 
      # dplyr::filter(class == "Amphibia") %>%
      dplyr::filter(`scientific name` %in% focal_taxon)
  }
  
  ##### Isolate taxon trends
  taxon_trends <- aoi$species_trends_list[taxon_table$`scientific name`] %>% purrr::map("yearly_trend")
  taxon_trends <- purrr::map(1:length(taxon_trends), function(sp){
    dat <- taxon_trends[[sp]]
    if ("reporting_rate_sd" %in% names(dat)){
      out <- dat %>% 
        dplyr::select(all_of(c("year", metric))) %>% 
        purrr::set_names(c("year", names(taxon_trends)[sp]))
    } else {
      out <- NULL
    }
    out
  })
  taxon_trends <- taxon_trends[!purrr::map_lgl(taxon_trends, is.null)]
  
  if (!identical(taxon_trends, list())){
    
  ##### Combine amphibian trends in wide format
  # taxon_trends_df <- data.frame(
  #   year = taxon_trends %>% purrr::map("year") %>% unlist() %>% unique() %>% sort()
  # ) %>% 
  taxon_trends_df <- taxon_trends %>% 
    plyr::join_all(
      by = "year"
    )
  
  taxon_trends_df <- taxon_trends_df %>% 
    dplyr::mutate(
      mean_metric = taxon_trends_df %>% dplyr::select(-year) %>% rowMeans(na.rm = TRUE)
    )
  
  if (make_plot){
    
  p <- taxon_trends_df %>% 
    ggplot(aes(x=year)) +
    geom_ribbon(aes(ymin = -1.96, ymax = 1.96), fill = grey(0.5), alpha = 0.1) +
    geom_hline(yintercept = 0, colour = "black", size = 1) +
    geom_hline(yintercept = 1.96, colour = grey(.5), size = .5) +
    geom_hline(yintercept = -1.96, colour = grey(.5), size = .5) +
    geom_line(aes(y=mean_metric), colour = "#d7191c", size = 1.2, alpha = 1) +
    ylab("Reporting rate \n anomaly") +
    xlab("") +
    theme_linedraw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 8)
    )
  
  if (!is.null(plot_label)) p <- p + ggplot2::ggtitle(plot_label)

  out <- list(
    trends_data = taxon_trends_df,
    trends_plot = p
  )
  } else {
    out <- taxon_trends_df
  }
  
  }
  
  return(out)
}

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
    reference_taxon <- records %>% dplyr::filter_all(any_vars(. %in% focal_taxon_name)) %>% dplyr::pull(reference_taxon_rank) %>% unique()
    
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

get_major_taxon_trends <- function(
    analysis_records = area_of_interest$gbif_data, 
    focal_taxon, 
    use_reference_taxon = TRUE, 
    use_reference_taxon_rank = "phylum", 
    use_taxonomic_resolution = "class",
    full = TRUE,
    resolution = "h6"
){
  
  focal_taxon_local <- focal_taxon
  
  # Identify reference taxon name
  # reference_taxon <- analysis_records %>% dplyr::filter(dplyr::if_any(dplyr::everything(), ~ .x %in% focal_taxon_local)) %>% dplyr::pull(all_of(use_reference_taxon_rank)) %>% unique()
  reference_taxon <- analysis_records %>% dplyr::filter_all(any_vars(. %in% focal_taxon_local)) %>% dplyr::pull(use_reference_taxon_rank) %>% unique()
  
  # if (taxonomic_scale_counts$n[taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon] < (1.5*taxonomic_scale_counts$n[1])) suggested_baseline_taxon <- taxonomic_scale_counts$taxonomic_level[which(taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon)+1]
  
  # Get observed trends
  ## Get observed detection history
  counts_observed <- get_count_data_byTaxon(
    records = analysis_records, 
    focal_taxon_name = focal_taxon_local, 
    by_reference_taxon = use_reference_taxon, 
    reference_taxon_rank = use_reference_taxon_rank, 
    taxonomic_resolution = use_taxonomic_resolution
  )
  
  # counts_observed <- counts_observed[-which(rowSums(counts_observed %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year)) == 1), ]
  
  detections_observed <- calculate_detection_data(counts = counts_observed, focal_taxon_name = focal_taxon_local)
  
  ## Get observed yearly trends
  yearly_trends_observed <- calculate_trends(detections = detections_observed, grp = "year")
  
  ## Get observed spatial trends
  spatial_trends_observed <- calculate_trends(detections = detections_observed, grp = resolution)
  
  ## Get observed spatiotemporal trends
  spatiotemporal_trends_observed <- calculate_trends(detections = detections_observed, grp = c("year", resolution))
  
  if (isTRUE(full)){
    
    # Get randomized (i.e., expected) trends
    ## Get randomized detection history
    detections_random <- run_randomizations(counts = counts_observed, iterations = 1000, focal_tax = focal_taxon_local)
    
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
      dplyr::mutate(focal_taxon = focal_taxon_local,
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
      dplyr::mutate(focal_taxon = focal_taxon_local,
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
}


# get_yearly_trend_metrics <- function(trends_list){
#   
#   # Calculate trend metrics
#   metrics_df <- purrr::map(1:length(trends_list), function(sp){
#     # sp_name <- gsub(" ", "_", names(trends_list)[sp])
#     sp_name <- names(trends_list)[sp]
#     dat <- trends_list[[sp_name]]$yearly_trend
#     if ("reporting_rate_sd" %in% names(dat)){
#     out_metrics <- data.frame(
#       species = names(trends_list)[sp],
#       ## Calculate metrics
#       # years above expected in the last 10
#       reporting_rate_above_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) > 1.96) %>% tail(10) %>% sum(),
#       # years below expected in the last 10
#       reporting_rate_below_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) < -1.96) %>% tail(10) %>% sum(),
#       # 5 year correlation in the last 5
#       reporting_rate_trend_last_five = dat %>% dplyr::filter(year >= max(year, na.rm = TRUE)-5) %>% dplyr::mutate(new = cor(reporting_rate_sd, year, method = "spearman")) %>% dplyr::pull(new) %>% head(1),
#       # 5 year correlation in the last 10
#       reporting_rate_trend_last_ten = dat %>% dplyr::filter(year >= max(year, na.rm = TRUE)-10) %>% dplyr::mutate(new = cor(reporting_rate_sd, year, method = "spearman")) %>% dplyr::pull(new) %>% head(1)
#     )
#     } else {
#       out_metrics <- data.frame(
#         species = names(trends_list)[sp],
#         ## Calculate metrics
#         # years above expected in the last 10
#         reporting_rate_above_last_ten = NA,
#         # years below expected in the last 10
#         reporting_rate_below_last_ten = NA,
#         # 5 year correlation in the last 5
#         reporting_rate_trend_last_five = NA,
#         # 5 year correlation in the last 10
#         reporting_rate_trend_last_ten = NA
#       )
#     }
#     out_metrics
#   }) %>% dplyr::bind_rows()
#   
#   return(metrics_df)
# }

# Wrapper to generate yearly trends figure
# get_yearly_trend_wrapper <- memoise(function(
#     analysis_records = area_of_interest$gbif_data, 
#     focal_taxon = focal_taxon_name, 
#     ref_taxon_rank = "family",
#     random_iterations = 999,
#     ...
# ){
#   
#   count_dat <- get_count_data(records = analysis_records, focal_taxon_name = focal_taxon, reference_taxon_rank = ref_taxon_rank)
#   detection_dat <- calculate_detection_data(counts = count_dat[-which(rowSums(count_dat %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year)) == 1), ], focal_taxon_name = focal_taxon)
#   yearly_dat <- calculate_trends(detections = detection_dat, grp = "year")
#   yearly_dat_random <- get_randomized_metric(counts = count_dat, iterations = random_iterations, focal_tax = focal_taxon, grp = "year")
#   
#   out <- list(
#     observed = yearly_dat,
#     random = yearly_dat_random
#   )
#   
#   return(out)
# },
# cache = cache_filesystem(CACHE)
# )

# Function to extract temporal bins
# get_temporal_bins <- function(occurrences, number_groups = 3, first_year = 1950){
#   occurrence_years <- occurrences %>% 
#     sf::st_set_geometry(NULL) %>% 
#     dplyr::filter(
#       complete.cases(year),
#       year >= first_year
#     ) %>% 
#     dplyr::pull(year) 
#   years_df <- data.frame(
#     year = occurrence_years,
#     period = ntile(occurrence_years, n = number_groups)
#   )
#   periods_summary <- years_df %>% group_by(period) %>% dplyr::summarize(range = paste0(min(year), "-", max(year)))
#   
#   return(periods_summary)
# }
# 
# # Wrapper to generate spatiotemporal trends data
# get_spatiotemporal_trend_wrapper <- memoise(function(
#     analysis_records = area_of_interest$gbif_data, 
#     focal_taxon = focal_taxon_name, 
#     ref_taxon_rank = "family",
#     random_iterations = 999,
#     resolution = "h6",
#     ...
# ){
# 
#     count_dat <- get_count_data(records = analysis_records, focal_taxon_name = focal_taxon, reference_taxon_rank = ref_taxon_rank)
#     detection_dat <- calculate_detection_data(counts = count_dat, focal_taxon_name = focal_taxon)
#     spatial_dat <- calculate_trends(detections = detection_dat, grp = c("year", resolution))
#     spatial_dat_random <- get_randomized_metric(counts = count_dat, iterations = random_iterations, focal_tax = focal_taxon, grp = c("year", resolution))
# 
#     spatiotemporal_trends <- get_standardized_difference(
#       observed = spatial_dat,
#       randomized = spatial_dat_random,
#       metric = "reporting_rate", 
#       grp = c("year", resolution)
#     ) 
#     
#     spatiotemporal_trends_wide <- spatiotemporal_trends %>% 
#       dplyr::select(all_of(c("year", resolution, "reporting_rate_sd"))) %>% 
#       pivot_wider(names_from = year, values_from = reporting_rate_sd)
#     base_hexes <- h3jsr::cell_to_polygon(spatiotemporal_trends_wide[[resolution]], simple = FALSE)
#     names(base_hexes)[which(names(base_hexes) == "h3_address")] <- resolution
#     spatiotemporal_trends_data <- base_hexes %>%
#       dplyr::left_join(spatiotemporal_trends_wide, by = resolution)
#     # hexes_data
#   
#   return(spatiotemporal_trends_data)
# },
# cache = cache_filesystem(CACHE)
# )

calculate_temporal_trend_metrics <- function(trends_list){
  
  metrics_df <- purrr::map(1:length(trends_list), function(t){
    
    dat <- trends_list[[t]]$trends_data %>% 
      dplyr::select(year, mean_metric)

      out_metrics <- data.frame(
        taxon = names(trends_list)[t],
        ## Calculate metrics
        # years above expected in the last 10
        reporting_rate_above_last_ten = (dat$mean_metric > 1.96) %>% tail(10) %>% sum(),
        # years below expected in the last 10
        reporting_rate_below_last_ten = (dat$mean_metric < -1.96) %>% tail(10) %>% sum(),
        # 5 year correlation in the last 5
        reporting_rate_trend_last_five = dat %>% dplyr::filter(year >= max(year, na.rm = TRUE)-5) %>% dplyr::mutate(new = cor(mean_metric, year, method = "spearman")) %>% dplyr::pull(new) %>% head(1),
        # 5 year correlation in the last 10
        reporting_rate_trend_last_ten = dat %>% dplyr::filter(year >= max(year, na.rm = TRUE)-10) %>% dplyr::mutate(new = cor(mean_metric, year, method = "spearman")) %>% dplyr::pull(new) %>% head(1)
      )

  }) %>% dplyr::bind_rows()
  
  return(metrics_df)
  
}

safe_arcgis_raster <- function(
    service_url,
    bbox,
    out_crs = 3857,
    resolution = 10,
    format = "tiff",
    rast_crs = 3857,
    verbose = TRUE,
    extra_filters = ""
    ) {
  
  # ============================================================
  # REQUIREMENTS
  # ============================================================
  
  stopifnot(requireNamespace("sf", quietly = TRUE))
  stopifnot(requireNamespace("terra", quietly = TRUE))
  stopifnot(requireNamespace("httr", quietly = TRUE))
  
  # ============================================================
  # TRANSFORM TO OUTPUT CRS
  # ============================================================
  
  bbox_proj <- sf::st_transform(
    bbox,
    out_crs
  )
  
  bbox_proj_vals <- sf::st_bbox(bbox_proj)
  
  xmin <- bbox_proj_vals["xmin"]
  ymin <- bbox_proj_vals["ymin"]
  xmax <- bbox_proj_vals["xmax"]
  ymax <- bbox_proj_vals["ymax"]
  
  # ============================================================
  # COMPUTE IMAGE SIZE
  # maintain aspect ratio
  # ============================================================
  
  width_m  <- xmax - xmin
  height_m <- ymax - ymin
  
  ncol <- ceiling(width_m / resolution)
  nrow <- ceiling(height_m / resolution)
  
  # safety cap
  ncol <- min(ncol, 4000)
  nrow <- min(nrow, 4000)
  
  if (verbose) {
    
    message("Request dimensions:")
    message(paste("cols:", ncol))
    message(paste("rows:", nrow))
    
  }
  
  # ============================================================
  # BUILD EXPORT URL
  # ============================================================
  
  export_url <- paste0(
    service_url,
    "/export?",
    paste0(
      c(
        paste0(
          "bbox=",
          paste(c(xmin, ymin, xmax, ymax),
                collapse = ",")
        ),
        extra_filters,
        paste0("bboxSR=", out_crs),
        paste0("imageSR=", out_crs),
        paste0("size=", ncol, ",", nrow),
        paste0("format=", format),
        "transparent=false",
        "f=image"
      ),
      collapse = "&"
    )
  )
  
  if (verbose) {
    message(export_url)
  }
  
  # ============================================================
  # DOWNLOAD
  # ============================================================
  
  ext <- switch(
    format,
    png = ".png",
    jpg = ".jpg",
    jpeg = ".jpg",
    tiff = ".tif",
    tif = ".tif",
    ".img"
  )
  
  tf <- tempfile(fileext = ext)
  
  httr::GET(
    export_url,
    httr::write_disk(tf, overwrite = TRUE)
  )
  
  # ============================================================
  # READ RASTER
  # ============================================================
  
  r <- terra::rast(tf)
  
  # ============================================================
  # ASSIGN CRS
  # ============================================================
  
  terra::crs(r) <- paste0("EPSG:", rast_crs)
  
  # ============================================================
  # FIX EXTENT
  # ArcGIS exports can slightly drift
  # ============================================================
  
  terra::ext(r) <- terra::ext(
    xmin,
    xmax,
    ymin,
    ymax
  )
  
  # ============================================================
  # OPTIONAL:
  # fix upside-down exports
  # ============================================================
  
  # if (format %in% c("png", "jpg", "jpeg")) {
  
  r <- terra::flip(
    r,
    direction = "vertical"
  )
  
  out <- r
  
  return(out)
  
}
