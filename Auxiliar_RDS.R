# 1. Leer GeoJSON pesados
rutas <- st_read("Assets/GeoJSON/09_Ruta_sabanas_id_hexagonoV2.geojson") %>% 
  st_transform(4326) %>% 
  filter(!st_is_empty(.)) %>%
  subset(st_geometry_type(.) %in% c("LINESTRING","MULTILINESTRING"))

poligonosV2 <- st_read("Assets/GeoJSON/10_Hexagonos_clusteres_filtrados.geojson") %>% 
  st_transform(4326)

# 2. Guardar versión RDS en una carpeta Assets/RDS
dir.create("Assets/RDS", showWarnings = FALSE)
saveRDS(rutas, "Assets/RDS/rutas.rds")
saveRDS(poligonosV2, "Assets/RDS/poligonosV2.rds")