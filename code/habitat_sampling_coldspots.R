whr_table_records <- terra::extract(x = baseline_whr,
                            y = area_of_interest$gbif_data
)%>% 
  dplyr::pull(WHRNUM) %>% 
  table() %>% 
  stack() %>% 
  purrr::set_names(c("number_occurrences", "WHRNUM")) %>% 
  dplyr::mutate(WHRNUM = as.character(WHRNUM) %>% as.numeric(WHRNUM),
                proportion_occurrences = round(number_occurrences/sum(number_occurrences, na.rm = TRUE), 3)
  ) %>% 
  dplyr::left_join(
    terra::cats(baseline_whr)[[1]] %>% as.data.frame() %>% dplyr::select(WHRNUM, WHRNAME) %>% dplyr::distinct(., .keep_all = TRUE),
    by = "WHRNUM"
  ) %>% 
  dplyr::distinct(., .keep_all = TRUE) %>% 
  dplyr::select(WHRNAME, WHRNUM, number_occurrences, proportion_occurrences) %>% 
  dplyr::arrange(desc(proportion_occurrences))

whr_table_baseline <- terra::extract(x = baseline_whr,
                            y = baseline_whr %>% as.data.frame(xy = TRUE) %>% dplyr::select(x, y)
)%>% 
  dplyr::pull(WHRNUM) %>% 
  table() %>% 
  stack() %>% 
  purrr::set_names(c("number_occurrences", "WHRNUM")) %>% 
  dplyr::mutate(WHRNUM = as.character(WHRNUM) %>% as.numeric(WHRNUM),
                proportion_occurrences = round(number_occurrences/sum(number_occurrences, na.rm = TRUE), 3)
  ) %>% 
  dplyr::left_join(
    terra::cats(baseline_whr)[[1]] %>% as.data.frame() %>% dplyr::select(WHRNUM, WHRNAME) %>% dplyr::distinct(., .keep_all = TRUE),
    by = "WHRNUM"
  ) %>% 
  dplyr::distinct(., .keep_all = TRUE) %>% 
  dplyr::select(WHRNAME, WHRNUM, number_occurrences, proportion_occurrences) %>% 
  dplyr::arrange(desc(proportion_occurrences))


whr_table_records <- whr_table_records %>% 
  dplyr::left_join(whr_table_baseline %>% dplyr::select(WHRNAME, proportion_occurrences) %>% dplyr::rename(proportion_total_area = proportion_occurrences), by = "WHRNAME") %>% 
  dplyr::mutate(
    representation = proportion_occurrences-proportion_total_area
  ) %>% 
  dplyr::arrange(representation)


trend_species <- area_of_interest$biggest_movers_table %>% 
  dplyr::filter(trend == "needs more data") %>% 
  dplyr::pull(species)

trend_species_whr <- purrr::map(1:length(trend_species), function(sp){
  
  whr_table <- terra::extract(x = baseline_whr,
                              y = area_of_interest$gbif_data %>% 
                                dplyr::filter(species == trend_species[sp])
  ) %>% 
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::pull(WHRNUM) 
  
  if (length(whr_table) > 0){
    
    whr_table <- whr_table %>% 
      table() %>% 
      stack() %>% 
      purrr::set_names(c("number_occurrences", "WHRNUM")) %>% 
      dplyr::mutate(WHRNUM = as.character(WHRNUM) %>% as.numeric(WHRNUM),
                    proportion_occurrences = number_occurrences/sum(number_occurrences, na.rm = TRUE),
                    species = trend_species[sp]
      ) %>% 
      dplyr::left_join(
        terra::cats(baseline_whr)[[1]] %>% as.data.frame() %>% dplyr::select(WHRNUM, WHRNAME) %>% dplyr::distinct(., .keep_all = TRUE),
        by = "WHRNUM"
      ) %>% 
      dplyr::distinct(., .keep_all = TRUE) 
    
  }
  
}) %>% 
  bind_rows()
