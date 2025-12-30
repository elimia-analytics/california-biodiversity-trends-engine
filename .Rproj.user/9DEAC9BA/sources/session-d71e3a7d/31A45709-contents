# Create custom management areas layer
## List of URLs for each areas' shapefiles
areas_urls <- list(
  "Jack and Laura Dangermond Preserve" = "https://services.arcgis.com/F7DSX1DSNSiWmOqh/arcgis/rest/services/jldp_boundary/FeatureServer/2", # Jack and Laura Dangermond Preserve
  "One Tam Area of Interest" = "https://services9.arcgis.com/D49vnq4lqWmKCLj0/arcgis/rest/services/One_Tam_Area_of_Interest/FeatureServer/0", # One Tam Area of Interest
  "Pepperwood Preserve" = "https://services1.arcgis.com/tIgNEsvmQ5Qg6njs/ArcGIS/rest/services/AVIRISNG_Pepperwood_Species_WFL/FeatureServer/0",
  "Presidio Land Trust" = "https://services5.arcgis.com/KS6joEEoa0G4EO56/arcgis/rest/services/Presidio_Trust_Boundary/FeatureServer/0"
)

## Download shapefiles one by one using esri2sf package
areas_polygons <- purrr::map(1:length(areas_urls), function(p){
  
  poly <- esri2sf::esri2sf(areas_urls[[p]])
  
  poly <- poly %>% 
    dplyr::mutate(aoi_id = p,
                  aoi_name = names(areas_urls)[p]
                  ) %>% 
    dplyr::select(aoi_id, aoi_name)
}) %>% 
  purrr::set_names(names(areas_urls)[1:length(areas_urls)])
## Combine shapefiles into single sf object
aoi_polygons <- do.call("rbind", areas_polygons)

# Add Santa Monica Mountains Recreation Area downloaded from "https://services2.arcgis.com/AhxrK3F6WM8ECvDi/arcgis/rest/services/TMP_StudyAreaMask/FeatureServer/0", # "https://services1.arcgis.com/fBc8EJBxQRMcHlei/ArcGIS/rest/services/Santa_Monica_Mountains_National_Recreation_Area_Boundary_(public_view)/FeatureServer/0", # Santa Monica Mountains Recreation Area
aoi_polygons <- rbind(
  aoi_polygons,
  readRDS("data/boundaries/Santa Monica Mountains Recreation Area.rds") %>% 
    dplyr::mutate(aoi_id = max(aoi_polygons$aoi_id) + 1)
)
# UCNRS downloaded from https://ucnrs.org/research/research-resources/gis-database/
ucnrs_boundaries <- sf::st_read("data/boundaries/UC_NRS.gdb")
ucnrs_boundaries <- ucnrs_boundaries %>% 
  sf::st_transform(st_crs(aoi_polygons)) %>% 
  sf::st_set_geometry(value = "geoms") %>% 
  sf::st_make_valid() %>% 
  group_by(Name) %>% 
  summarise(geoms = st_union(geoms)) 
ucnrs_boundaries <- ucnrs_boundaries %>% 
  dplyr::mutate(
    aoi_id = (max(aoi_polygons$aoi_id) + 1):(max(aoi_polygons$aoi_id) + nrow(ucnrs_boundaries)),
    aoi_name = Name
  ) %>% 
  dplyr::select(aoi_id, aoi_name)
row.names(ucnrs_boundaries) <- ucnrs_boundaries$aoi_name
aoi_polygons <- rbind(
  aoi_polygons,
  ucnrs_boundaries
)
# Jasper Ridge downloaded from https://earthworks.stanford.edu/catalog/stanford-mf819md0352
jasper_ridge <- sf::st_read("data/boundaries/stanford-mf819md0352-shapefile/mf819md0352.shp")
jasper_ridge <- jasper_ridge %>% 
  dplyr::mutate(
    aoi_name = "Jasper Ridge Biological Preserve",
    aoi_id = max(aoi_polygons$aoi_id) + 1
  ) %>% 
  dplyr::select(aoi_id, aoi_name) %>% 
  sf::st_set_geometry("geoms")
row.names(jasper_ridge) <- "Jasper Ridge Biological Preserve"
aoi_polygons <- rbind(
  aoi_polygons,
  jasper_ridge
)

# "California Marine Protected Areas" = "https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/biosds582_fpu/FeatureServer/0"
mpas <- esri2sf::esri2sf("https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/biosds582_fpu/FeatureServer/0")
mpas <- mpas %>% 
  dplyr::mutate(aoi_id = max(aoi_polygons$aoi_id) + OBJECTID,
                aoi_name = FULLNAME
  ) %>% 
  dplyr::select(aoi_id, aoi_name)
aoi_polygons <- rbind(
  aoi_polygons,
  mpas
)

## Write out areas of interest polygons
saveRDS(aoi_polygons, "data/boundaries/aoi_polygons.rds")
