# 1. Leer GeoJSON pesados
rutas <- st_read("Assets/GeoJSON/09_Ruta_sabanas_id_hexagonoV2.geojson") %>% 
  st_transform(4326) %>% 
  filter(!st_is_empty(.)) %>%
  subset(st_geometry_type(.) %in% c("LINESTRING","MULTILINESTRING"))

poligonosV2 <- st_read("Assets/GeoJSON/10_Hexagonos_clusteres_filtrados.geojson") %>% 
  st_transform(4326)

patios_ele_buff <- st_read("Assets/GeoJSON/11_Buffer_3km_Patios_electricos.geojson")%>%
  st_transform(4326)
patios_ele_punt <-st_read("Assets/GeoJSON/12_Patios_electricos_TM.geojson")%>%
  st_transform(4326)
punt_ad_buff <-st_read("Assets/GeoJSON/13_Buffer_3km_Estaciones_adicionales.geojson")%>%
  st_transform(4326) 
punt_ad_punt <-st_read("Assets/GeoJSON/14_Cargadores_adicionales.geojson")%>%
  st_transform(4326)
colegios_punt <-st_read("Assets/GeoJSON/Colegios_sabana_rutas.geojson")%>%
  st_transform(4326)

# 2. Guardar versión RDS en una carpeta Assets/RDS
dir.create("Assets/RDS", showWarnings = FALSE)
saveRDS(rutas, "Assets/RDS/rutas.rds")
saveRDS(poligonosV2, "Assets/RDS/poligonosV2.rds")

saveRDS(patios_ele_buff, "Assets/RDS/P_Elec_buff.rds")
saveRDS(patios_ele_punt, "Assets/RDS/P_Elec_punt.rds")
saveRDS(punt_ad_buff, "Assets/RDS/P_Ad_buff.rds")
saveRDS(punt_ad_punt, "Assets/RDS/P_Ad_punt.rds")
saveRDS(colegios_punt, "Assets/RDS/colegios.rds")