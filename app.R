#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(dplyr)
library(leaflet)
library(stringr)
library(RColorBrewer)
library(htmltools)
library(sf)
library(shinybusy)
library(leaflet.extras)
library(terra)
library(raster)
library(purrr)


n <- 90
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual', ]
sp_pal <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1, n), col = sample(col_vector, n, replace = TRUE))

proj_albrs2k <- st_read("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/BR_UF_2020/BR_UF_2020_AlbBRs2k.shp") %>% 
  st_crs()


## load data

source("01_limites.R")
source("02_oportunidades.R")
source("03_atividades.R")


# setup -------------------------------------------------------------------

legend_opacity <- 0.6


# species data ------------------------------------------------------------

occ <- read_xlsx("../../dados_ocorrencia/planilhas/pts_sppalvo_SE_sdupli.xlsx")
occ <- occ %>% 
  rename(taxon = especie)
benef <- read_xlsx("../../Espécies Alvo e Beneficiadas_PAN SE_23jan23_atualizada.xlsx",
                   sheet = "Beneficiadas_tudo_mapa") %>% 
  janitor::clean_names()
unique(benef$taxon)
beneficiadas_occ <- list.files(path = "../../dados_ocorrencia/originais_salve/beneficiadas/",    
                               pattern = "*.xlsx",
                               full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows() %>% 
  janitor::clean_names()




beneficiadas_occ <- beneficiadas_occ %>% 
  dplyr::select(taxon = nome_cientifico, lat = latitude, 
                long = longitude) 

beneficiadas_occ$taxon <- str_trim(beneficiadas_occ$taxon)
benef$taxon <- str_trim(benef$taxon)
benef$taxon %>% unique %>% sort

beneficiadas_occ$lat <- as.numeric(gsub(x = beneficiadas_occ$lat, 
                                        pattern = ",", 
                                        replacement = "."))
beneficiadas_occ$long <- as.numeric(gsub(x = beneficiadas_occ$long, 
                                        pattern = ",", 
                                        replacement = "."))

beneficiadas_occ <- beneficiadas_occ %>% 
  distinct(taxon, lat, long)

benef2 <- benef %>% 
  left_join(beneficiadas_occ, by = "taxon")


beneficiadas_sem_pts <- benef2 %>% 
  filter(is.na(lat)) %>% 
  pull(taxon) %>% 
  paste0(collapse = ", ")

benef3 <- benef2 %>% 
  filter(!is.na(lat))


# info
occ_sf <- st_as_sf(occ, coords = c("long", "lat"), crs = 4674) %>%
  mutate(lon = unlist(purrr::map(.$geometry,1)),
         lat = unlist(purrr::map(.$geometry,2))) %>% 
  dplyr::select(-geometry, geometry) %>% 
  st_transform(crs = 4326)

combined_distribution_benef <- st_as_sf(benef3, coords = c("long", "lat"), crs = 4674) %>%
  mutate(lon = unlist(purrr::map(.$geometry,1)),
         lat = unlist(purrr::map(.$geometry,2))) %>% 
  dplyr::select(-geometry, geometry) %>% 
  st_transform(crs = 4326)


combined_distribution <- occ_sf %>% 
  # dplyr::select(taxon, lat, lon) %>% 
  bind_rows(combined_distribution_benef)

species_list <- sort(unique(benef3$taxon))



# comeca app --------------------------------------------------------------




# Define UI for application that draws a histogram
ui <- navbarPage("Oficina de Elaboração do PAN Herpetofauna do Sudeste", 
                 id = "nav", 
                 
                 # aba 1 - mapa ------------------------------------------------------------
                 
                 tabPanel("Base de dados",
                          
                          tags$head(includeHTML(("google-analytics.html"))),
                          div(class="outer",
                              # Include custom styles and javascript
                              tags$head(includeCSS("styles.css"), 
                                        includeScript("gomap.js")),
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class="panel panel-default", 
                                            fixed = TRUE, draggable = TRUE, top = 60, 
                                            left = 20, right = "auto", bottom = 50,
                                            width = 400, style = "overflow-y: scroll;",
                                            
                                            # tags$div(HTML("<br><center><p><a target=\"_blank\" href=\"https://github.com/RhettRautsaw/VenomMaps\"><img src=\"https://img.shields.io/badge/User%20Guide-GitHub-blue\"></a></p></center>")),
                                            # tags$div(HTML("<center><p><a target=\"_blank\" href=\"https://doi.org/10.1038/s41597-022-01323-4\"><img src=\"https://img.shields.io/badge/Citation-Scientific%20Data-blue\"></a></p></center>")),
                                            # tags$div(HTML("<center><p><a target=\"_blank\" href=\"https://doi.org/10.5281/zenodo.5637094\"><img src=\"https://img.shields.io/badge/Archive-10.5281/zenodo.5637094-blue\"></a></p></center>")),
                                            # tags$div(HTML("<center><p><a target=\"_blank\" href=\"https://creativecommons.org/licenses/by/4.0/\"><img src=\"https://img.shields.io/badge/License-CC%20BY-blue\"></a></p></center>")),
                                            
                                            ## Selecionar espécies alvo
                                            
                                            h4("Selecionar espécies"),
                                            checkboxInput(inputId = "especiesalvo", 
                                                          label = h5("Espécies alvo"),
                                                          value = FALSE),
                                            
                                            
                                            
                                            ## selecionar especies beneficiadas
                                            selectizeInput(inputId = "especiesbe",
                                                           label = h5("Espécies potencialmente beneficiadas:"), 
                                                           choices = species_list, 
                                                           multiple = TRUE,
                                            ),
                                            
                                            
                                            actionButton("update", "Atualizar"),
                                            
                                            h4("Áreas protegidas:"),
                                            checkboxInput(inputId = "ucs", 
                                                          label = h5("Unidades de Conservação"),
                                                          value = FALSE),
                                            checkboxInput(inputId = "tis", 
                                                          label = h5("Terras Indígenas"),
                                                          value = FALSE),
                                            
                                            br(),
                                            
                                            h4("Ameaças potenciais:"),
                                            checkboxInput(inputId = "areas_urbanas", 
                                                          label = h5("Áreas urbanas"),
                                                          value = FALSE),
                                            checkboxInput(inputId = "silvicultura", 
                                                          label = h5("Silvicultura"),
                                                          value = FALSE),
                                            
                                            checkboxInput(inputId = "mineracao", 
                                                          label = h5("Mineração"),
                                                          value = FALSE),
                                            checkboxInput(inputId = "barragens_anm", 
                                                          label = h5("Barragens ANM"),
                                                          value = FALSE),
                                            checkboxInput(inputId = "ahe", 
                                                          label = h5("AHEs"),
                                                          value = FALSE),
                                            checkboxInput(inputId = "ferrovias", 
                                                          label = h5("Ferrovias"),
                                                          value = FALSE),

                                            
                                            
                              )
                          )
                 ),
                 
                 
                 # aba 2 - créditos --------------------------------------------------------
                 
                 
                 tabPanel("Créditos",
                          
                 )
                 
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # aba 1 - mapa ------------------------------------------------------------
  
  
  
  
  output$map <- renderLeaflet({
  
    cores_biomas <- c(
      "#B3964F", # Caatinga
      "#B7DB6E", # Cerrado
      "#86A686"  # Mata Atlântica
    )
    
    pal_bioams <- colorFactor(cores_biomas, domain = biomas$Bioma)
    
    
      
    leaflet() %>% 
      addMapPane("background", zIndex = 0) %>%        # Level 1: bottom
      addMapPane("points", zIndex = 1) %>%          # Level 2: middle
      # addMapPane("rasters", zIndex = 100000) %>%      # Level 3: middle
      addMapPane("polygons", zIndex = 440) %>%          # Level 4: middle
      addMapPane("labels", zIndex = 450) %>%          # Level 5: top
      # addWMSTiles('http://ows.mundialis.de/services/service?', layers='TOPO-WMS', group="Topography", options = pathOptions(pane = "background")) %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Open Street Map", 
                       options = pathOptions(pane = "background")
      ) %>% 
      # addProviderTiles(providers$Esri.WorldTerrain, group="Terrain", options = pathOptions(pane = "background")) %>%
      addProviderTiles(providers$Esri.WorldImagery, group="Satellite", options = pathOptions(pane = "background")) %>%
      # addProviderTiles(providers$Stamen.TonerLines, group="Boundaries", options = pathOptions(pane = "labels")) %>%
      addProviderTiles(providers$Stamen.TonerLabels, group="Labels", options = pathOptions(pane = "labels")) %>%
      
      addPolygons(group = "Ilhas do Sudeste",
                  data = ilhas_se,
                  color = c("#191970"),
                  fillOpacity = 0.01,
                  weight = 2,
                  smoothFactor = 1,
                  opacity = 0.5,
                  label = ~ htmlEscape(ilhas),
                  popup = paste0(
                    "<strong>", ilhas_se$ilhas, "</strong>",
                    "</br>",
                    "<strong>Estado: </strong>",
                    ilhas_se$sigla,
                    "<strong>Área (m²): </strong>",
                    format(round(ilhas_se$area_m2,digits = 2), decimal.mark = ","),
                    "</br>", 
                    "<strong>Área (ha): </strong>",
                    format(round(ilhas_se$area_ha,digits = 2), decimal.mark = ","),
                    "</br>",
                    "<strong>Área (km²): </strong>",
                    format(round(ilhas_se$area_km2,digits = 2), decimal.mark = ",")
               
                  ),
                  options = pathOptions(pane = "labels")
      ) %>%
      addPolygons(group = "PAN Herpeto do Espinhaço MG",
                  data = filter(limite_PANs, ID == 4),
                  color = "black",
                  fillOpacity = 0.01,
                  weight = 2,
                  smoothFactor = 1,
                  opacity = 0.5,
                  label = ~ htmlEscape(pan),
                  options = pathOptions(pane = "labels")
      ) %>%
      addPolygons(group = "Estados",
                  data = ufs,
                  color = "black",
                  fillOpacity = 0.01,
                  weight = 2,
                  smoothFactor = 1,
                  opacity = 0.5,
                  label = ~ htmlEscape(NM_UF),
                  options = pathOptions(pane = "labels")
      ) %>%
      addPolygons(group = "Estados do SE",
                  data = st_as_sf(st_union(dplyr::filter(ufs, NM_REGIAO == "Sudeste"))),
                  color = "black",
                  # fillOpacity = 0.01,
                  fill = FALSE,
                  weight = 2,
                  smoothFactor = 1,
                  opacity = 0.5,
                  # label = ~ htmlEscape(NM_UF),
                  options = pathOptions(pane = "labels")
      ) %>%
      addPolygons(group = "Biomas",
                  data = biomas,
                  color = "black",
                  fillOpacity = 0.5,
                  fillColor = ~ pal_bioams(Bioma),
                  weight = 2,
                  smoothFactor = 1,
                  opacity = 0.5,
                  label = ~ htmlEscape(Bioma),
                  options = pathOptions(pane = "labels")
      ) %>%
      addPolygons(group = "Bacias",
                  data = bacias_ana,
                  fill = TRUE,
                  stroke = TRUE,
                  fillColor = "lightblue",
                  color = "lightblue",
                  weight = 2,
                  smoothFactor = 1,
                  fillOpacity = 0.0000005,
                  label = ~ htmlEscape(name),
                  options = pathOptions(pane = "labels")
      ) %>%
      addLayersControl(
        baseGroups = c(
          # "Topography", 
          "Satellite",
          "Open Street Map" 
          # "Terrain", 
          
          ), 
        overlayGroups = c(
          "PAN Herpeto do Espinhaço MG",
          "Ilhas do Sudeste",
          "Estados do SE",
          "Estados",
          "Bacias",
          "Biomas",
          "Labels"
          ),
        options = layersControlOptions(collapsed = T, position = "bottomright")
      ) %>%
      hideGroup(c("PAN Herpeto do Espinhaço MG",
                # "Estados do SE",
                "Ilhas do Sudeste",
                "Estados",
                "Labels",
                "Biomas",
                "Bacias")) %>%
      addScaleBar(position = c("bottomleft"), options = scaleBarOptions()) 
  })
  
  # update species list
  
  new_species_list <- reactive({
    sp_list <- benef3 %>%
      dplyr::select(taxon) %>%
      arrange(taxon) %>%
      distinct() %>% 
      pull(taxon)
    
  })
  
  observe({
    updateSelectizeInput(session,
      "especiesbe", choices = new_species_list() 
      # selected = head(new_species_list(), 1)
    )
  })
  
  
  # Filter distributions
  distribution <- reactive({
    
    if (is.null(input$especiesbe)) {

     # as(combined_distribution, "Spatial")
     as(combined_distribution %>% 
          filter(taxon == "Scinax pinimus"), 
        "Spatial")

    } else {
    
      as(combined_distribution %>%
           filter(taxon %in% input$especiesbe),
         "Spatial")
    }
  })


  # Update map with distribution/points
  
  observeEvent(
    input$update,
    
 
      

    {
      show_modal_spinner()
      distribution <- distribution()
      bbox <- st_bbox(as(distribution, "sf")) %>%
        as.vector()
      sp_factpal <- colorFactor(sp_pal, domain = distribution$taxon)
      
      
      leafletProxy("map", data = distribution) %>%
        clearGroup("distribution") %>%
        clearGroup("occpoints") %>%
        clearGroup("pas") %>%
        clearGroup("atividade") %>%
        clearHeatmap() %>%
        clearMarkerClusters() %>%
        removeControl("distribution") %>%
        clearImages() %>%
        clearControls() %>% 
        addCircleMarkers(data = distribution, group = "occpoints",
                         lng =  ~ as.numeric(lon),
                         lat =  ~ as.numeric(lat),
                         radius = 4,
                         fill = TRUE,
                         # fillColor = "black",
                         fillColor = ~ sp_factpal(taxon),
                         fillOpacity = 1, weight = 1,
                         stroke = TRUE, color = "black", opacity = 1,
                         options = pathOptions(pane = "points"),
                         popup = paste0(
                           distribution$grupo,
                           "</br>",
                           "<em>", distribution$taxon, "</em>",
                           "</br>",
                           "<strong> Lista estadual:</strong> ",
                           distribution$lista_estadual,
                           "</br>",
                           "<strong> Categoria estadual de ameaça:</strong> ",
                           distribution$categoria_estadual_de_ameaca,
                           "</br>",
                           "<strong> Categoria nacional de ameaça:</strong> ",
                           distribution$categoria_nacional_de_ameaca_avaliacao_validacao_2022,
                           "</br>",
                           "<strong> Recomendações:</strong> ",
                           distribution$recomendacoes_oficina_preparatoria,
                           "</br>",
                           "<strong> Observações:</strong> ",
                           distribution$observacoes)
        ) %>%
        addLegend(data = distribution,
                  position = "topright",
                  pal = sp_factpal,
                  values = ~ taxon,
                  layerId = "distribution",
                  opacity = legend_opacity,
                  title = "Espécie beneficiada")
        
        # fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
    

      
      ### começa mapa espécie alvo ------------
      
      if (input$especiesalvo) {
        # Filter points
        
        
        colors <- c("#cce226", # Quase Ameaçada (NT)
                    "#fae813",  # Vulnerável (VU)
                    "#fc7f40", # Em Perigo (EN)
                    "#d81e06", # Criticamente em Perigo (CR)
                    "#000000" # Extinta (EX)
                    
                    
        )
        
        occ$cat_validada <- factor(occ$cat_validada,
                                   levels = c("Quase Ameaçada (NT)",
                                              "Vulnerável (VU)",
                                              "Em Perigo (EN)",
                                              "Criticamente em Perigo (CR)",
                                              "Extinta (EX)"))
        # factpal <- colorFactor(topo.colors(5), pointsData$cat_validada)
        factpal <- colorFactor(colors, domain = occ$cat_validada)
        
        bbox2 <- st_bbox(occ_sf) %>%
          as.vector()

          leafletProxy("map", data = distribution) %>%
            addCircleMarkers(
              data = occ,
              group = "occpoints",
              lng =  ~ as.numeric(long),
              lat =  ~ as.numeric(lat),
              radius = 4,
              fill = TRUE,
              fillColor = ~ factpal(cat_validada),
              fillOpacity = 1,
              weight = 1,
              stroke = TRUE,
              color = "black",
              opacity = 1,
              options = pathOptions(pane = "points"),
              popup = paste(
                "<strong><em>",
                occ$taxon,
                "</em></strong>",
                "</br>
                <strong>Grupo:</strong>",
                occ$grupo,
                "</br>
                <strong>Classe:</strong>",
                occ$classe,
                "</br>
                <strong>Ordem:</strong>",
                occ$ordem,
                "</br>
                <strong>Família:</strong>",
                occ$familia,
                "</br>
                <strong>Categoria validada:</strong>",
                occ$cat_validada
              ) 
              #clusterOptions = markerClusterOptions(),
              # popup = lapply(labs, htmltools::HTML)) #%>%
            ) %>% 
            addLegend(position = "topright",
                      data = occ,
                      values = ~cat_validada,
                      colors = colors,
                      labels = levels(occ$cat_validada),
                      opacity = legend_opacity,
                      title = "Categorias de ameaça") %>%
            fitBounds(bbox2[1], bbox2[2], bbox2[3], bbox2[4])
        
      }
      
      ### termina mapa espécie alvo ------------
      
      ### começa mapa de ucs ------------------------------------------------------
      
      if (input$ucs) {
        
        
        leafletProxy("map", data = distribution) %>%
          addPolygons(group = "pas",
                      data = ucs,
                      color = "black",
                      fillColor = ~ pal_ucs(grupo),
                      fillOpacity = 0.4,
                      # fill = FALSE,
                      weight = 2,
                      smoothFactor = 1,
                      opacity = 0.5,
                      label = ~ htmlEscape(nome_abrev),
                      options = pathOptions(pane = "labels") 
                      
          ) %>% 
          addLegend(
            data = ucs,
            values = ~levels(grupo),
            pal = pal_ucs,
            opacity = legend_opacity,
            title = "Unidades de Conservação",
            position = "topright"
          ) 
      }
      
      ### termina mapa de ucs ------------------------------------------------------
      
      ### começa mapa de tis ------------------------------------------------------
      
      if (input$tis) {
        
        leafletProxy("map", data = distribution) %>%
        # leaflet() %>% 
        #   addTiles() %>% 
          addPolygons(data = tis,
                      group = "pas",
                      color = "black",
                      fillColor = "yellow",
                      fillOpacity = 0.4,
                      weight = 2,
                      smoothFactor = 1,
                      opacity = 0.5,
                      label = ~ htmlEscape(terrai_nom),
                      options = pathOptions(pane = "labels")
                      )%>% 
          addLegend(
            data = tis,
            colors = "yellow",
            opacity = legend_opacity,
            labels = "Terras indígenas",
            title = "Áreas protegidas",
            position = "topright"
          ) 
      }
      ### termina mapa de tis ------------------------------------------------------
      
      ### começa mapa de areas urbanas ------------------------------------------------------
      
      if (input$areas_urbanas) {
        
        cores_areas_urb <- freq_area3 %>% 
          dplyr::filter(new_id %in% levels(areas_urb$gridcode)) %>% 
          dplyr::select(new_id, color_number) %>% 
          arrange(new_id) %>% 
          pull(color_number)
        
        
        
        leafletProxy("map", data = distribution) %>%
          addPolygons(
            group = "atividade",
            data = areas_urb,
            # color = "black",
            stroke = FALSE,
            fillColor = cores_areas_urb,
            fillOpacity = 0.4,
            # fill = FALSE,
            weight = 2,
            smoothFactor = 1,
            opacity = 0.5,
            options = pathOptions(pane = "labels")
          ) %>% 
          addLegend(
            data = areas_urb,
            opacity = legend_opacity,
            labels = "Áreas urbanas",
            colors = cores_areas_urb,
            title = "Atividade",
            position = "topright"
          ) 
        
      }  
      
      ### termina mapa de areas urbanas ------------------------------------------------------
      
      ### começa mapa de silvicultura ------------------------------------------------------
      if (input$silvicultura) {
        
        
        
        
        cores_silvicultura <- freq_area3 %>% 
          dplyr::filter(new_id %in% levels(silvicultura$gridcode)) %>% 
          dplyr::select(new_id, color_number) %>% 
          arrange(new_id) %>% 
          pull(color_number)
        
        
        
        leafletProxy("map", data = distribution) %>%
          addPolygons(
            group = "atividade",
            data = silvicultura,
            # color = "black",
            stroke = FALSE,
            fillColor = cores_silvicultura,
            fillOpacity = 0.4,
            # fill = FALSE,
            weight = 2,
            smoothFactor = 1,
            opacity = 0.5,
            options = pathOptions(pane = "labels")
          ) %>% 
          addLegend(
            data = silvicultura,
            opacity = legend_opacity,
            labels = "Silvicultura",
            colors = cores_silvicultura,
            title = "Atividade",
            position = "topright"
          ) 
        
      }  
      
      
      ### termina mapa de silvicultura ------------------------------------------------------
      
      ### começa mapa de mineracao ------------------------------------------------------
      
      if (input$mineracao) {
        
        cores_mineracao <- freq_area3 %>% 
          dplyr::filter(new_id %in% levels(mineracao$gridcode)) %>% 
          dplyr::select(new_id, color_number) %>% 
          arrange(new_id) %>% 
          pull(color_number)
        
        
        
        leafletProxy("map", data = distribution) %>%
          addPolygons(
            group = "atividade",
            data = mineracao,
            # color = "black",
            stroke = FALSE,
            fillColor = cores_mineracao,
            fillOpacity = 0.4,
            # fill = FALSE,
            weight = 2,
            smoothFactor = 1,
            opacity = 0.5,
            options = pathOptions(pane = "labels")
          ) %>% 
          addLegend(
            data = mineracao,
            opacity = legend_opacity,
            labels = "Mineração",
            colors = cores_mineracao,
            title = "Atividade",
            position = "topright"
          ) 
        
      }  
      
      
      ### termina mapa de mineracao ------------------------------------------------------
      
      ### começa mapa barragens ANM ------------------------------------------------------
      
      if (input$barragens_anm) {
        
        
        cores_barragensanm <- c(
          "#67dddd", # Baixo
          "#ff9900", # Médio
          "#e661ac", # Alto
          "gray"     # N/A
        )
        
        pal_barraganm <- colorFactor(cores_barragensanm, 
                                     domain = barragens_anm$cat_rsc)
        
        leafletProxy("map", data = distribution) %>%
          addCircleMarkers(
            group = "atividade",
            data = barragens_anm,
            lng = ~long,
            lat = ~lat,
            radius = 4,
            fillOpacity = 1, weight = 1,
            stroke = TRUE, color = "black", opacity = 1,
            fillColor = ~pal_barraganm(barragens_anm$cat_rsc),
            options = pathOptions(pane = "labels"),
            popup = paste0(
              "<strong>",
              barragens_anm$nome,
              "</strong>",
              "</br> Município: ",
              barragens_anm$municip,
              "</br>Estado: ",
              barragens_anm$uf,
              "</br>Minério principal: ",
              barragens_anm$mnr_prn,
              "</br>Método construtivo: ", 
              barragens_anm$mtd_cnt,
              "</br>Categoria de risco: ",
              barragens_anm$cat_rsc,
              "</br>Dano Potencial Associado: ",
              barragens_anm$dano_pt,
              "</br>Nível de emergência: ",
              barragens_anm$nvl_mrg,
              "</br>Status DCE atual: ",
              barragens_anm$status_dce,
              "</br>Status DCO atual: ",
              barragens_anm$status_dco
            )) %>%  
          addLegend(position = "topright",
                    group = "atividade",
                    data = barragens_anm,
                    values = ~ cat_rsc,
                    colors = cores_barragensanm,
                    labels = levels(barragens_anm$cat_rsc),
                    opacity = legend_opacity,
                    title = "Categorias de risco")
      }
      
      ### termina mapa barragens ANM ------------------------------------------------------
      
      
      ### começa mapa ahe ------------------------------------------------------
      
      cores_ahe <- c(
        "#009ACD", # CGH
        "#00698C", # PCH
        "#00394D" # UHE
      )
      
      
      pal_ahe <- colorFactor(cores_ahe, 
                             domain = ahe$tipo_ahe)
      
      if (input$ahe) {
        
        leafletProxy("map", data = distribution) %>%
        # 
        # leaflet() %>%
        #   addTiles() %>%
          addCircleMarkers(data = ahe, 
                           group = "atividade", 
                           lng =  ~ long, 
                           lat =  ~ lat, 
                           radius = 4,
                           fill = TRUE, 
                           fillColor = ~ pal_ahe(tipo_ahe),
                           fillOpacity = 1, weight = 1, 
                           stroke = TRUE, color = "black", opacity = 1, 
                           options = pathOptions(pane = "points"),
                           popup = paste(
                             "<strong>",
                             ahe$nome,
                             "</strong>",
                             "</br>Tipo de AHE:",
                             ahe$tipo_ahe,
                             "</br> Fase:",
                             ahe$fase,
                             "</br> Última atualização:",
                             ahe$data_atual
                           )
          ) %>% 
          addLegend(
            data = ahe,
            title = "Atividade",
            values = ~ tipo_ahe,
            pal = pal_ahe,
            opacity = legend_opacity
          )
      }
      
      ### termina mapa  ahe ------------------------------------------------------
      
      ### começa mapa ferrovias ------------------------------------------------------
      
      cores_ferrovias <- c(
        "#71392A", # em obra
        "#631147" # em operacao
      )
      
      pal_ferrov <- colorFactor(
        cores_ferrovias, domain = ferrovia_br$tip_situac
      )
      
      if (input$ferrovias) {
        
        leafletProxy("map", data = distribution) %>%
          addPolylines(
            data = ferrovia_br,
            group = "atividade",
            color = ~pal_ferrov(tip_situac), 
            weight = 2,
            opacity = 0.5,
            label = ~ htmlEscape(sigla),
            options = pathOptions(pane = "points"),
            popup = paste0(
              "<strong>", ferrovia_br$sigla, "</strong>",
              "</br>", "Situação: ",
              ferrovia_br$tip_situac,
              "</br>", "Comprimento: ",
              ferrovia_br$length,
              "</br>", "Linha: ",
              ferrovia_br$linha_fe_4,
              "</br>", "Produtos: ",
              ferrovia_br$produtos_5,
              "</br>", "Tráfego: ",
              ferrovia_br$trafego,
              "</br>", "Ano: ",
              ferrovia_br$ano,
              "</br>", "Fonte: ",
              ferrovia_br$fonte
            )
          ) %>% 
          addLegend(
            data = ferrovia_br,
            title = "Atividade",
            values = ~ tip_situac,
            pal = pal_ferrov,
            opacity = legend_opacity
          )
      }
      
      ### termina mapa ferrovia ------------------------------------------------------
      
      
      
      
      
      
      remove_modal_spinner()
    })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
