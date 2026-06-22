library(duckdbfs)
inat_unobscured <- duckdbfs::open_dataset("data/Gio_Test_Obs_for_2025-07-23.parquet/part.0.parquet") %>% 
  as.data.frame()
area_of_interest$gbif_data %>% 
  dplyr::filter(institutioncode == "iNaturalist") %>% 
  dplyr::pull(occurrenceid)

intersect(
  area_of_interest$gbif_data %>% 
    dplyr::filter(institutioncode == "iNaturalist") %>% 
    dplyr::pull(occurrenceid), 
  inat_unobscured$occurrenceID
  )

library(gmp)
hex_vals <- c(
  "88283084B7FFFFF",
  "882830A265FFFFF",
  "88283085D3FFFFF",
  "882830B14DFFFFF",
  "8828308495FFFFF"
)

# Use arbitrary precision integers
dec_vals <- as.bigz(paste0("0x", hex_vals))
# reverse:
hex_vals <- toupper(as.character(as.hexmode(dec_vals)))
