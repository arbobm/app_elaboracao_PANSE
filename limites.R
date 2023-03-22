ufs <- sf::st_read("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/BR_UF_2021/BR_UF_2021.shp")
ufs <- st_simplify(st_transform(ufs, crs = 4326))

ufs_se <- st_as_sf(st_union(dplyr::filter(ufs, NM_REGIAO == "Sudeste")))



limite_PANs <- read_sf("C:/SIG/RAN_homeoffice/PANs/shapes/limites_pans.shp")
limite_PANs <- st_make_valid(limite_PANs)
limite_PANs <- st_transform(limite_PANs, crs = 4326) %>% 
  st_simplify()

biomas <- read_sf("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/Biomas/lm_bioma_250.shp")
biomas <- st_transform(biomas, crs = 4326) %>% 
  dplyr::filter(Bioma != "Amazônia") %>% 
  st_filter(ufs_se) %>% 
  st_simplify()


## paleta de cores dos biomas

biomas$Bioma <- factor(biomas$Bioma)

cores_biomas <- c(
  "#B3964F", # Caatinga
  "#B7DB6E", # Cerrado
  "#86A686" # Mata Atlântica
)

pal_bioams <- colorFactor(cores_biomas, domain = biomas$Bioma)




ilhas <- sf::st_read("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/BR_UF_2021/ilhas.shp") %>% 
  janitor::clean_names()
ilhas_se <- ilhas %>% 
  filter(nm_regiao == "Sudeste") %>% 
  st_transform(crs = 4326) 

ilhas_se <- ilhas_se %>% 
  st_transform(crs = proj_albrs2k) %>% 
  mutate(area_m2 = as.numeric(st_area(ilhas_se)),
         area_km2 = as.numeric(st_area(ilhas_se))/1000000,
         area_ha = as.numeric(st_area(ilhas_se))/10000,
  ) %>%
  st_transform(crs = 4326) 

