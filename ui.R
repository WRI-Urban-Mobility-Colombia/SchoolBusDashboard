##----------------------------------------------------------------------------##
##DashboardTest
##----------------------------------------------------------------------------##
# Este es un piloto general de la herramienta para la elección de frentes de trabajo de rutas escolares
# 

##----------------------------------------------------------------------------##
##Instalar/iniciar librerías
##----------------------------------------------------------------------------##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(sf)
library(leaflet)
library(dplyr)
library(here)
library(shinyEffects)
library(plotly)

##----------------------------------------------------------------------------##
##Abrir archivos necesarios
##----------------------------------------------------------------------------##
## WD
setwd(here())
## Polígonos
#poligonos <-st_read("Assets/GeoJSON/09_hexagonos_Patios_Cargadores.geojson")%>%
#  st_transform(crs =4326)
## PoligonosV02
poligonosV2 <-st_read("Assets/GeoJSON/10_Hexagonos_clusteres_filtrados.geojson")%>%
  st_transform(crs =4326)
## Base de datos
## Rutas
#rutas <-st_read("Assets/GeoJSON/09_Ruta_sabanas_id_hexagonoV2.geojson")%>%
#  st_transform(crs = 4326) %>% 
#  filter(!st_is_empty(.)) %>%
#  subset(st_geometry_type(.)%in%c("LINESTRING","MULTILINESTRING"))
## Rutas V2
rutasv2 <-st_read("Assets/GeoJSON/09_Ruta_sabanas_id_hexagonoV2.geojson")%>%
  st_transform(crs = 4326) %>% 
  filter(!st_is_empty(.)) %>%
  subset(st_geometry_type(.)%in%c("LINESTRING","MULTILINESTRING"))


  
## Datos_Rutas
rutasDB <- read.csv2("Assets/CSV/Rutas_EX.csv",encoding = "UTF-8", sep = ";")

##----------------------------------------------------------------------------##
##Variables adicionales
##----------------------------------------------------------------------------##
## Paleta de colores para 'nom_patio' en mapa_clusteres
factpal_patios <- colorFactor(
  palette = "Set3", 
  domain  = poligonosV2$nom_patio
)
##----------------------------------------------------------------------------##
##Preprocesamiento
##----------------------------------------------------------------------------##
## poligonos V2, filtrar poligonos sin data
## Unir rutas para saber poligonos sin data
rutasv2R1 <- rutasv2%>%filter(Recorrido_ == "R1")


poligonosV2$NoRutas <- sapply(poligonosV2$id, function(x) sum(rutasv2R1$id_2 == x, na.rm = TRUE))

poligonosV3 <-poligonosV2 %>%filter(NoRutas >0)

##----------------------------------------------------------------------------##
##UI
##----------------------------------------------------------------------------##


ui <- dashboardPage(

  title = "Geoselector de rutas escolares",
  
  ## Header
  header = dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Geoselector"),
      span(class = "logo-mini", "WRI GS")
    ),
    rightUi = userOutput("skin_dropdown")
  ),
  
  ## Sidebar
  sidebar = dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "tab_seleccionada",
      
      menuItem("Introducción", tabName = "tab_intro", icon = icon("info-circle")),
      menuItem("Explore la red", tabName = "tab_mapa", icon = icon("binoculars")),
      menuItem("Clústeres identificados", tabName = "tab_mapa2", icon = icon("map")),
      menuItem("Documentación", tabName = "documentacion", icon = icon("users")),
      menuItem("Créditos", tabName = "creditos", icon = icon("users"))
    )
  ),
  ## ControlBar
  controlbar = dashboardControlbar(
    id = "Controlbar",
    skinSelector()
  ),
  ## Body
  body = dashboardBody(
    # CSS enfocado únicamente en eliminar scrolls sobrantes
    tags$head(
      tags$style(HTML("
        /* Forzar alto completo al mapa */
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        #mapa_interactivo, #mapa_clusteres {
          height: calc(100vh - 200px) !important;
        }

      "))
    ),
    
    tabItems(
      ## Pestaña 1: Introducción
      tabItem(
        tabName = "tab_intro",
        box(
          title = "Herramienta de análisis de rutas",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          p("Esta herramienta permite seleccionar frentes de trabajo y analizar rutas escolares en Bogotá.")          
        )
      ),
      ## Pestaña 2: Mapa
      tabItem(
        tabName = "tab_mapa",
        
        # 1. Mantener la caja KPI superior (ancho completo)
        fluidRow(
          valueBoxOutput("box_beneficiarios", width = 12)
        ),
        
        # Fila principal dividida en 2/3 (ancho 8) y 1/3 (ancho 4)
        fluidRow(
          
          # 2. Mapa ocupando 2/3 de la pantalla (costado izquierdo)
          column(
            width = 8,
            box(
              title = "Análisis por sector",
              status = "primary",
              solidHeader = TRUE,
              width = NULL, # Al estar dentro de column(), el ancho se ajusta al contenedor
              collapsible = FALSE,
              leafletOutput("mapa_interactivo", height = "600px") 
            )
          ),
          
          # Costado derecho (1/3 de la pantalla)
          column(
            width = 4,
            
            # 3a. Espacio para controles
            box(
              title = "Filtros y Controles",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              collapsible = TRUE,
              # Aquí puedes agregar inputs como selectizeInput, sliderInput, etc.
              p("Espacio reservado para controles futuros.")
            ),
            
            # 3b. Espacio posterior para gráficas
            box(
              title = "Visualizaciones",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              collapsible = TRUE,
              # Aquí puedes incluir outputs como plotlyOutput, plotOutput, etc.
              plotlyOutput("grafica_secundaria", height = "300px")
            )
          )
        )
      ),
      
      ## Pestaña 3: Clústeres identificados
      tabItem(
        tabName = "tab_mapa2",
        fluidRow(
          column(
            width = 4,
            valueBoxOutput("box_beneficiarios1",width =4)
          ),
          column(
            width = 4,
            valueBoxOutput("box_beneficiarios2",width =4)
          ),
          column(
            width = 4,
            valueBoxOutput("box_beneficiarios3",width =4)
          ),
        ),
        box(
          title = "Explorar clústeres",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          leafletOutput("mapa_clusteres")
        )
      ),
      
      ## Pestaña 4: Documentación
      tabItem(
        tabName = "documentacion",
        box(
          title = "Descripción",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          p("Créditos, entidades, fuentes de información, otros")         
        )
      ),
      ## Pestaña 5: Créditos
      tabItem(
        tabName = "creditos",
        box(
          title = "Créditos y Desarrollo",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          p("Desarrollado para el análisis espacial de rutas escolares.")
        )
      )
    )
  )
)

##----------------------------------------------------------------------------##
## Servidor
##----------------------------------------------------------------------------##

server <- function(input, output, session) {
  
  ## Mapa 1. Explorar
  ## 1.1. Contenedor reactivo 
  seleccionados <- reactiveVal(character(0)) # Para mapa_interactivo
  ## 1.2. Render del mapa interactivo principal
  output$mapa_interactivo <- renderLeaflet({
    leaflet(poligonosV2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId     = ~id,
        fillColor   = "#3498db",
        fillOpacity = 0.4,
        color       = "#2c3e50",
        weight      = 1.5,
        label       = ~paste("Polígono:", id, " | Cluster:", Cluster)
        )
  })
  # 1.3. Capturar click en mapa_interactivo
  observeEvent(input$mapa_interactivo_shape_click, {
    click <- input$mapa_interactivo_shape_click
    id_cliqueado <- as.character(click$id)
    
    vector_actual <- seleccionados()
    
    if (id_cliqueado %in% vector_actual) {
      nuevo_vector <- setdiff(vector_actual, id_cliqueado)
    } else {
      nuevo_vector <- c(vector_actual, id_cliqueado)
    }
    
    seleccionados(nuevo_vector)
  })
  
  # 1.4. Re-dibujar capa dinámicas en mapa_interactivo
  observe({
    vector_actual <- seleccionados()
    
    leafletProxy("mapa_interactivo") %>%
      clearGroup("seleccion_roja")
    
    if (length(vector_actual) > 0) {
      poly_seleccionados <- poligonosV2 %>% filter(id %in% vector_actual)
      
      leafletProxy("mapa_interactivo") %>%
        addPolygons(
          data        = poly_seleccionados,
          layerId     = ~id,
          group       = "seleccion_roja",
          fillColor   = "orange",
          fillOpacity = 0.75,
          color       = "purple",
          weight      = 2.5,
          label       = ~paste("Zona:", id, " | Zona:", Cluster)
        )
    }
  })
  ##1.4.1. Valuebox dinámico original
  output$box_beneficiarios <- renderValueBox({
    lista_ids <- seleccionados()
    
    if (length(lista_ids) == 0) {
      poligonos_filtrados <- poligonosV2
      rutas_filtradas     <- rutasv2R1
      subtitulo_caja      <- "Consolidado Total (Toda la Ciudad)"
      color_caja          <- "navy"
    } else {
      poligonos_filtrados <- poligonosV2 %>% filter(id %in% lista_ids)
      rutas_filtradas     <- rutasv2R1 %>% filter(Id_Hexagono %in% lista_ids)
      subtitulo_caja      <- paste("Acumulado en", length(lista_ids), "hexágonos seleccionados")
      color_caja          <- "orange"
    }
    
    total_ben <- if (!is.null(rutas_filtradas$SR_TotalEst)) {
      sum(rutas_filtradas$SR_TotalEst, na.rm = TRUE)
    } else { 0 }
    
    total_rutas <- nrow(rutas_filtradas)
    
    total_km <- if (!is.null(rutas_filtradas$Dis_ruta_m)) {
      sum(rutas_filtradas$Dis_ruta_m, na.rm = TRUE)
    } else { 0 }
    
    ben_txt   <- format(total_ben, big.mark = ",")
    rutas_txt <- format(total_rutas, big.mark = ",")
    km_txt    <- format(round(total_km/1000,0), big.mark = ",")
    print("Calculos exitosos")
    print(ben_txt)
    print(rutas_txt)
    print(km_txt)
    valor_resumen <- paste(ben_txt, " Beneficiarios |", rutas_txt, " Rutas| ", km_txt, " km")
    
    valueBox(
      value    = valor_resumen,
      subtitle = subtitulo_caja,
      icon     = icon("chart-line"),
      color    = color_caja
    )
  })
  
  
  
  
  
  
  # 1. Contenedores reactivos
  
  seleccionados_cluster <- reactiveVal(character(0)) # Para mapa_clusteres
  
  
  
  
  # 3. Render base de mapa_clusteres (Coloreado por 'nom_patio')
  output$mapa_clusteres <- renderLeaflet({
    leaflet(poligonosV3) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId     = ~id,
        fillColor   = ~factpal_patios(nom_patio),
        fillOpacity = 0.2,
        color       = "#2c3e50",
        weight      = 1.5,
        group       = "Zonas hexagonales",
        label       = ~paste("Zona:", id, "| Zona:", Cluster)
      ) %>%
      addLegend(
        pal      = factpal_patios,
        values   = ~nom_patio,
        title    = "Nombre del Patio",
        position = "bottomright"
      ) %>%
      addLayersControl(
        overlayGroups = c("Zonas hexagonales", "Rutas escolares"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # 4. Capturar clic en mapa_clusteres y alternar estado (Toggle ON/OFF de forma segura)
  observeEvent(input$mapa_clusteres_shape_click, {
    click <- input$mapa_clusteres_shape_click
    
    # Validar que el objeto cliqueado tenga un id válido
    req(click$id)
    
    # Extraer y limpiar el ID (remover 'sel_' si se hace clic sobre la capa destacada)
    raw_id <- as.character(click$id)
    
    if (length(raw_id) > 0 && nzchar(raw_id)) {
      id_cliqueado <- gsub("^sel_", "", raw_id)
      vector_actual <- seleccionados_cluster()
      
      # Alternar selección
      if (id_cliqueado %in% vector_actual) {
        nuevo_vector <- setdiff(vector_actual, id_cliqueado)
      } else {
        nuevo_vector <- c(vector_actual, id_cliqueado)
      }
      
      seleccionados_cluster(nuevo_vector)
    }
  })
  
  # 5. Actualizar dinámicamente resalte y rutas activas en mapa_clusteres
  observe({
    vector_actual <- seleccionados_cluster()
    proxy <- leafletProxy("mapa_clusteres")
    
    # A) SIEMPRE LIMPIAR CAPAS DINÁMICAS PREVIAS
    # Al borrar el grupo "seleccion_cluster", se retira el color rojo y 
    # vuelve a quedar visible el polígono original con el color de su 'nom_patio'.
    proxy %>%
      clearGroup("seleccion_cluster") %>%
      clearGroup("Rutas escolares")
    
    # B) DIBUJAR ÚNICAMENTE SI HAY ELEMENTOS EN EL VECTOR
    if (length(vector_actual) > 0) {
      
      # 1. Resaltar polígonos seleccionados (Capa roja superpuesta)
      poly_sel <- poligonos %>% filter(id %in% vector_actual)
      
      proxy %>%
        addPolygons(
          data        = poly_sel,
          layerId     = ~paste0("sel_", id), # Mantiene el ID rastreable
          group       = "seleccion_cluster",
          fillColor   = "#e74c3c",           # Rojo de selección
          fillOpacity = 0.85,
          color       = "#900C3F",
          weight      = 3,
          label       = ~paste("SELECCIONADO - Zona:", id, "| Patio:", nom_patio)
        )
      
      # 2. Filtrar y dibujar ÚNICAMENTE las rutas correspondientes a los polígonos activos
      rutas_activas <- rutas %>% filter(as.character(Vertices_g) %in% vector_actual)
      
      if (nrow(rutas_activas) > 0) {
        proxy %>%
          addPolylines(
            data        = rutas_activas,
            color       = "#2ecc71",         # Verde para las rutas
            weight      = 3,
            opacity     = 0.9,
            group       = "Rutas escolares",
            label       = ~paste("Ruta:", ifelse(is.na(Ruta_fix), "Sin nombre", Ruta_fix))
          )
      }
    }
  })
  
  
  

}

##----------------------------------------------------------------------------##
## Ejecutar App
##----------------------------------------------------------------------------##

shinyApp(ui = ui, server = server)
