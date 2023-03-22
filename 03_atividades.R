

# mapbiomas7 <- rast("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/MapBiomas/MapBiomas7/brasil_coverage_2021.tif")
mapbiomas7_df <- read_xlsx("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/MapBiomas/MapBiomas7/Codigos_Classes_Legenda_Colecao_7_-__PT__.docx__1_.xlsx") %>%
  janitor::clean_names()
# mapbiomas7_se <- crop(mapbiomas7, vect(ufs_se))
# mapbiomas7_se <- mask(mapbiomas7_se, vect(ufs_se))
# plot(mapbiomas7_se)
# 
# terraOptions(datatype = "INT8U")
# terraOptions()
# writeRaster(mapbiomas7_se, "../rasters/mapbiomas7_se.tif", overwrite = T)

mapbiomas7_se <- rast("../rasters/mapbiomas7_se.tif")


ids <- terra::unique(mapbiomas7_se)


freq <- terra::freq(mapbiomas7_se)
freq_area <- freq %>% 
  dplyr::select(value, count) %>% 
  mutate(area_m2 = count * (30*30))

freq_area2 <- mapbiomas7_df %>% 
  dplyr::select(new_id, colecao_7_classes, color_number) %>% 
  right_join(freq_area, by = c("new_id" = "value"))

freq_area3 <- freq_area2 %>% 
  dplyr::mutate(grupo = dplyr::case_when(
    str_starts(colecao_7_classes, "1")   ~ "Floresta",
    str_starts(colecao_7_classes, "2")   ~ "Formação Natural não Florestal",
    str_starts(colecao_7_classes, "3.1") ~ "Pastagem",
    str_starts(colecao_7_classes, "3.2") ~ "Agricultura",
    str_starts(colecao_7_classes, "3.3") ~ "Silvicultura",
    str_starts(colecao_7_classes, "3.4") ~ "Mosaico de usos",
    str_starts(colecao_7_classes, "4")   ~ "Área não Vegetada",
    str_starts(colecao_7_classes, "5")   ~ "Corpo D'água"
  ))

# mapbiomas ---------------------------------------------------------------

# areas urbanas
# rcl_areaurb <- freq_area3 %>% 
#   mutate(reclass = 
#            case_when(
#              new_id == 24 ~ new_id
#            )) %>% 
#   select(new_id, reclass) %>% 
#   as.matrix
# 
# terraOptions(datatype = "INT4U")
# terraOptions()
# areas_urb <- classify(mapbiomas7_se, rcl_areaurb)
# writeRaster(areas_urb, "../rasters/areas_urb.tif", overwrite = T)

areas_urb <- st_read("../rasters/areas_urb.shp") %>% 
  janitor::clean_names()
# sort(unique(areas_urb$gridcode))

areas_urb$gridcode <- factor(areas_urb$gridcode, 
                             levels = sort(unique(areas_urb$gridcode))) 

## silvicultura

# rcl_silvic <- freq_area3 %>%
#   mutate(reclass =
#            case_when(
#              grupo == "Silvicultura" ~ new_id
#            )) %>%
#   select(new_id, reclass) %>%
#   as.matrix
# 
# terraOptions(datatype = "INT4U")
# terraOptions()
# silvicultura <- classify(mapbiomas7_se, rcl_silvic)
# writeRaster(silvicultura, "../rasters/silvicultura.tif", overwrite = T)

silvicultura <- st_read("../rasters/silvicultura.shp") %>% 
  janitor::clean_names()


silvicultura$gridcode <- factor(silvicultura$gridcode,
                                levels = sort(unique(silvicultura$gridcode)))

# ## mineracao (barragem de residuos inclusive)
# rcl_mineracao <- freq_area3 %>% 
#   mutate(reclass = 
#            case_when(
#              new_id == 30 ~ new_id
#            )) %>% 
#   select(new_id, reclass) %>% 
#   as.matrix
# 
# terraOptions(datatype = "INT4U")
# terraOptions()
# mineracao <- classify(mapbiomas7_se, rcl_mineracao)
# writeRaster(mineracao, "../rasters/mineracao.tif", overwrite = T)
mineracao <- st_read("../rasters/mineracao.shp") %>% 
  janitor::clean_names()

mineracao$gridcode <- factor(mineracao$gridcode,
                             levels = sort(unique(mineracao$gridcode)))

# # agricultura
# 
# # rcl_agric <- freq_area3 %>% 
# #   mutate(reclass = 
# #            case_when(
# #              grupo == "Agricultura" ~ new_id
# #            )) %>% 
# #   select(new_id, reclass) %>% 
# #   as.matrix
# # 
# # terraOptions(datatype = "INT4U")
# # terraOptions()
# # agric <- classify(mapbiomas7_se, rcl_agric)
# # writeRaster(agric, "../rasters/agric.tif", overwrite = T)
# 
# agric <- st_read("../rasters/agric.shp") %>% 
#   janitor::clean_names()
# sort(unique(agric$gridcode))
# 
# agric$gridcode <- factor(agric$gridcode, levels = sort(unique(agric$gridcode))) 
# cores_agric <- freq_area3 %>% 
#   dplyr::filter(new_id %in% levels(agric$gridcode)) %>% 
#   dplyr::select(new_id, color_number) %>% 
#   arrange(new_id) %>% 
#   pull(color_number)
# 
# 
# 
# pal_agric <- colorFactor(cores_agric, domain = agric$gridcode)
# 
# 
# leaflet() %>% 
#   addTiles() %>% 
#   addPolygons(
#     # group = "pas",
#     data = agric,
#     color = "black",
#     fillColor = ~ pal_agric(gridcode),
#     fillOpacity = 0.4,
#     # fill = FALSE,
#     weight = 2,
#     smoothFactor = 1,
#     opacity = 0.5,
#     # label = ~ htmlEscape(nome_abrev),
#     # options = pathOptions(pane = "labels") 
#     
#   ) %>% 
#   addLegend(
#     data = agric,
#     values = ~ levels(gridcode),
#     pal = pal_agric,
#     title = "Agricultura",
#     position = "topright"
#   ) 
# 
# 
# 
# 
# 
# 

## barragens mineracao anm

barragens_anm <- st_read("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/ANM/barragens_anm/barragens_anm_sf_lls2k.shp")

barragens_anm <- barragens_anm %>% 
  st_transform(4326) %>% 
  st_filter(ufs_se) %>% 
  st_simplify()

barragens_anm$cat_rsc <- factor(barragens_anm$cat_rsc,
                                levels = c("Baixo", "Médio", "Alto", "N/A"))




# 
# # AHE

ahe <- st_read("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/ANEEL/Aproveitamento_Hidrelétricos_AHE.shp") %>% 
  janitor::clean_names()
ahe <- ahe %>%
  mutate(long = unlist(map(.$geometry,1)),
         lat = unlist(map(.$geometry,2))) %>%
  st_transform(4326) %>% 
  st_filter(ufs_se) %>% 
  st_simplify()

ahe$tipo_ahe <- factor(ahe$tipo_ahe,
                       levels = c("CGH", "PCH", "UHE"))



## ferrovia
# # 
# # # install.packages('osmdata')
# library(osmdata)
# 
# available_features()
# 
# bbox_rj <- as(ufs[ufs$SIGLA=="RJ",], "Spatial") %>%
#   sp::bbox(.)
# 
# # available_tags("railway")
# 
# rj_rail <- bbox_rj %>%
#   opq() %>%
#   add_osm_feature(key = "railway") %>%
#   osmdata_sf()
# 
# 
# 
# 
# ## ES
# bbox_es <- as(ufs[ufs$SIGLA=="ES",], "Spatial") %>%
#   sp::bbox(.)
# 
# es_rail <- bbox_es %>%
#   opq() %>%
#   add_osm_feature(key = "railway") %>%
#   osmdata_sf()
# 
# ## MG
# bbox_mg <- as(ufs[ufs$SIGLA=="MG",], "Spatial") %>%
#   sp::bbox(.)
# 
# mg_rail <- bbox_mg %>%
#   opq() %>%
#   add_osm_feature(key = "railway") %>%
#   osmdata_sf()
# 
# ## SP
# bbox_sp <- as(ufs[ufs$SIGLA=="SP",], "Spatial") %>%
#   sp::bbox(.)
# 
# sp_rail <- bbox_sp %>%
#   opq() %>%
#   add_osm_feature(key = "railway") %>%
#   osmdata_sf()
# 
# ferrovia <- bind_rows(
#   dplyr::mutate(sp_rail$osm_lines, uf = "SP"),
#   dplyr::mutate(rj_rail$osm_lines, uf = "RJ"),
#   dplyr::mutate(mg_rail$osm_lines, uf = "MG"),
#   dplyr::mutate(es_rail$osm_lines, uf = "ES")
#   ) %>%
#   janitor::clean_names() 
# 
# st_write(ferrovia, "ferrovia_todas.gpkg", delete_layer = TRUE)
# 
# ferrovia <- st_read("ferrovia_todas.shp")

ferrovia_br <- st_read("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/MapBiomas/MapBiomas7/1.4_Ferroviario/Ferrovias/ferrovias.shp") %>%
  janitor::clean_names()

ferrovia_br <- ferrovia_br %>% 
  st_transform(4326) %>% 
  st_filter(ufs_se)

ferrovia_df <- ferrovia_br %>% 
  tibble() %>% 
  dplyr::select(-geometry) %>% 
  lapply(unique)

ferrovia_br$tip_situac <- factor(ferrovia_br$tip_situac, 
                                 levels = c("Em Obra", "Em Operação"))



# # represa pra uso da água
# 
# # estradas
# 
# 
# 
# 
# 
# 
# 
