


# UCs ---------------------------------------------------------------------


ucs <- sf::st_read("C:/Users/bruna/ICMBio/NGEO - General/PAN/Projeto_indicadores/shapes/UCsFedIcmb_EstMunicMMA_wgs84.shp")
head(ucs)
ucs <- ucs %>%  
  rename(siglagrupo = grupo) %>% 
  dplyr::mutate(grupo =
                  dplyr::case_when(
                    siglagrupo == "US" ~ "Uso sustentável",
                    siglagrupo == "PI" ~ "Proteção integral")) %>% 
  st_filter(st_as_sf(st_union(dplyr::filter(ufs, NM_REGIAO == "Sudeste")))) %>% 
  st_simplify()


ucs$grupo <- factor(ucs$grupo, levels = c("Uso sustentável", 
                                          "Proteção integral"))
ucs$categoria <- factor(ucs$categoria, levels = c("APA", "FLONA", 
                                                  "Floresta Estadual", "RDS",
                                                  "ARIE", "RESEX", "RPPN", 
                                                  "PARNA", "Parque Estadual",
                                                  "REVIS", "REBIO", "MONA",
                                                  "ESEC"))
# levels(ucs$categoria)
# 
# ucs %>% 
#   tibble %>% 
#   select(grupo, categoria) %>% 
#   distinct() %>% 
#   arrange(grupo)


cores_ucs <- c("#ffa237", # US
               "#9dce45"  #PI
               )

pal_ucs <- colorFactor(cores_ucs, domain = ucs$grupo)

# cores_ucs <- c(
#   
#   # Uso sustentavel
#   "#FFC685", # APA
#   "#FFA237", # FLONA
#   "#CC822D", # Floresta Estadual
#   "#806342", # RDS
#   "#BF7A2A", # ARIE
#   "#80511C", # RESEX
#   "#40290E", # RPPN  
#   
#   # Proteção Integral
#   "#3D4F1A", # PARNA
#   "#454F32", # Parque Estadual
#   "#9DCE45", # REVIS
#   "#BBD787", # REBIO
#   "#779C33", # MONA
#   "#6D8F2F"  # ESEC
#   )




# pal_ucs <- colorFactor(cores_ucs, domain = ucs$categoria)


# Terras Indigenas --------------------------------------------------------

tis <- st_read("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/funai/GEOFT_TERRA_INDIGENA/GEOFT_TERRA_INDIGENA_valid.shp") %>% 
  janitor::clean_names()

tis <- tis %>% 
  st_transform(crs = 4326) %>% 
  st_filter(st_as_sf(st_union(dplyr::filter(ufs, NM_REGIAO == "Sudeste")))) %>% 
  st_simplify()


