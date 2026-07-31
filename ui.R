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
library(tidyr)
library(here)
library(shinyEffects)
library(plotly)
library(DT)

##----------------------------------------------------------------------------##
##Abrir archivos necesarios
##----------------------------------------------------------------------------##
## WD
setwd(here())

# Archivos
poligonosV2 <- readRDS("Assets/RDS/poligonosV2.rds")
rutasv2 <- readRDS("Assets/RDS/rutas.rds")

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
## Filtrar solo recorrido R1

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
          status = "primary",
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
          
          # Costado derecho (1/3 de pantalla)
          column(
            width = 4,
            
            # Filtros
            box(
              title = "Filtros y Controles",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              collapsible = TRUE,
              ## Control mapa
              
              tags$h4(
                style = "font-weight: bold; color: #2c3e50; margin-top: 5px; margin-bottom: 8px;",
                "Variable de rutas en el mapa"
              ),
              radioButtons(
                inputId = "var_color_ruta",
                label = "Colorear Rutas por:",
                choices = c(
                "Tipo de Ruta" = "SR_Tip_Ruta",
                "Tipo de Vehículo" = "SR_Veh_Aj_2"
              ),
              selected = "SR_Tip_Ruta"
            ),
              tags$h4(
                style = "font-weight: bold; color: #2c3e50; margin-top: 5px; margin-bottom: 8px;",
                "Filtrar rutas"
              ),
              
              # Tipo de ruta
              selectizeInput(
                inputId  = "filtro_tipo_ruta",
                label    = "Seleccionar Tipo de Ruta:",
                choices  = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Todas las rutas", plugins = list("remove_button"))
              ),
              
              tags$hr(style = "border-top: 1px solid #e0e0e0; margin: 10px 0;"),
              
              # Tipo de vehículo
              selectizeInput(
                inputId  = "filtro_tipo_vehiculo",
                label    = "Seleccionar Tipo de Vehículo:",
                choices  = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Todos los vehículos", plugins = list("remove_button"))
              )
            ),
            
            # Visualizaciones / Tablas
            box(
              title = "Visualizaciones",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              collapsible = TRUE,
              
              tags$h4(
                style = "font-weight: bold; color: #2c3e50; margin-top: 5px; margin-bottom: 8px;",
                "Distancia acumulada (Km) por Tipo y Vehículo"
              ),
              
              DT::dataTableOutput("tabla_resumen_rutas")
            )
          )
        )
      ),
      
      ## Pestaña 3: Clústeres identificados
      tabItem(
        tabName = "tab_mapa2",
        fluidRow(
          column(width = 4, valueBoxOutput("box_beneficiarios1", width = 12)),
          column(width = 4, valueBoxOutput("box_beneficiarios2", width = 12)),
          column(width = 4, valueBoxOutput("box_beneficiarios3", width = 12))
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
  
  ##--------------------------------------------------------------------------##
  ## 1. Mapa Interactivo y Selección
  ##--------------------------------------------------------------------------##
  seleccionados <- reactiveVal(character(0)) # Almacena IDs de polígonos seleccionados
  
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
      ) %>%
      addLayersControl(
        overlayGroups = c("Polígonos", "Polígonos Seleccionados", "Rutas Filtradas"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$mapa_interactivo_shape_click, {
    click <- input$mapa_interactivo_shape_click
    req(click$id)
    
    # Manejar IDs directos o con prefijo de selección
    raw_id <- as.character(click$id)
    id_cliqueado <- gsub("^sel_", "", raw_id)
    
    vector_actual <- seleccionados()
    
    if (id_cliqueado %in% vector_actual) {
      nuevo_vector <- setdiff(vector_actual, id_cliqueado)
    } else {
      nuevo_vector <- c(vector_actual, id_cliqueado)
    }
    
    seleccionados(nuevo_vector)
  })
  
  # Resaltar Polígonos Seleccionados
  observe({
    vector_actual <- seleccionados()
    
    proxy <- leafletProxy("mapa_interactivo")
    proxy %>% clearGroup("seleccion_roja")
    
    if (length(vector_actual) > 0) {
      poly_seleccionados <- poligonosV2 %>% filter(id %in% vector_actual)
      
      proxy %>%
        addPolygons(
          data        = poly_seleccionados,
          layerId     = ~paste0("sel_", id),
          group       = "seleccion_roja",
          fillColor   = "orange",
          fillOpacity = 0.6,
          color       = "purple",
          weight      = 2.5,
          label       = ~paste("Zona:", id, " | Cluster:", Cluster)
        )
    }
  })
  
  ##--------------------------------------------------------------------------##
  ## 2. Cargar Opciones de Filtros
  ##--------------------------------------------------------------------------##
  observe({
    req(rutasv2R1)
    
    opciones_rutas     <- sort(unique(na.omit(rutasv2R1$SR_Tip_Ruta)))
    opciones_vehiculos <- sort(unique(na.omit(rutasv2R1$SR_Veh_Aj_2)))
    
    updateSelectizeInput(
      session, 
      "filtro_tipo_ruta", 
      choices  = opciones_rutas, 
      selected = NULL, 
      server   = TRUE
    )
    
    updateSelectizeInput(
      session, 
      "filtro_tipo_vehiculo", 
      choices  = opciones_vehiculos, 
      selected = NULL, 
      server   = TRUE
    )
  })
  
  ##--------------------------------------------------------------------------##
  ## 3. Reactivo de Filtrado Conjunto (Espacial + Controles UI)
  ##--------------------------------------------------------------------------##
  rutas_filtradas_reactivas <- reactive({
    req(rutasv2R1)
    datos <- rutasv2R1
    
    # 1. Filtro Espacial (Polígonos seleccionados por el campo Id_Hexagono)
    lista_ids <- seleccionados()
    if (length(lista_ids) > 0) {
      datos <- datos %>% filter(as.character(Id_Hexagono) %in% lista_ids)
    }
    
    # 2. Filtro por Tipo de Ruta
    if (!is.null(input$filtro_tipo_ruta) && length(input$filtro_tipo_ruta) > 0) {
      datos <- datos %>% filter(SR_Tip_Ruta %in% input$filtro_tipo_ruta)
    }
    
    # 3. Filtro por Tipo de Vehículo
    if (!is.null(input$filtro_tipo_vehiculo) && length(input$filtro_tipo_vehiculo) > 0) {
      datos <- datos %>% filter(SR_Veh_Aj_2 %in% input$filtro_tipo_vehiculo)
    }
    
    # 4. LIMPIEZA CRÍTICA: Filtrar geometrías vacías o no válidas que rompen addPolylines
    if (inherits(datos, "sf") && nrow(datos) > 0) {
      datos <- datos %>% 
        filter(!st_is_empty(.)) %>%    # Remueve geometrías vacías
        sf::st_make_valid()           # Corrige posibles geometrías corruptas
    }
    
    return(datos)
  })

  ##--------------------------------------------------------------------------##
  ## 3.1 Dibuja y Colorea las Rutas en el Mapa Interactivo Principal
  ##--------------------------------------------------------------------------##
  observe({
    # Validar que los datos y los seleccionados existan
    ids <- seleccionados()
    if (length(ids) == 0) {
      leafletProxy("mapa_interactivo") %>% 
        clearGroup("Rutas Filtradas") %>% 
        clearControls()
      return()
    }
    
    rutas_sub <- rutas_filtradas_reactivas()
    proxy     <- leafletProxy("mapa_interactivo")
    
    proxy %>% 
      clearGroup("Rutas Filtradas") %>% 
      clearControls()
    
    # Asegurar que hay filas válidas para graficar
    if (!is.null(rutas_sub) && nrow(rutas_sub) > 0) {
      
      # Determinar variable de color de forma segura
      var_color <- "SR_Tip_Ruta"
      if (!is.null(input$var_color_ruta) && is.character(input$var_color_ruta) && nzchar(input$var_color_ruta)) {
        var_color <- input$var_color_ruta
      }
      
      # Validar si la columna existe en el dataset
      if (var_color %in% names(rutas_sub)) {
        
        # Omitir valores NA de la paleta
        valores_unicos <- sort(unique(na.omit(rutas_sub[[var_color]])))
        
        if (length(valores_unicos) > 0) {
          paleta <- colorFactor(palette = "Set1", domain = valores_unicos)
          
          titulo_leyenda <- if (identical(var_color, "SR_Tip_Ruta")) "Tipo de Ruta" else "Tipo de Vehículo"
          
          proxy %>%
            addPolylines(
              data        = rutas_sub,
              group       = "Rutas Filtradas",
              color       = ~paleta(get(var_color)),
              weight      = 3.5,
              opacity     = 0.85,
              popup       = ~paste0(
                "<b>Tipo de Ruta: </b>", ifelse(is.na(SR_Tip_Ruta), "N/A", SR_Tip_Ruta), "<br>",
                "<b>Tipo Vehículo: </b>", ifelse(is.na(SR_Veh_Aj_2), "N/A", SR_Veh_Aj_2), "<br>",
                "<b>Hexágono ID: </b>", ifelse(is.na(Id_Hexagono), "N/A", Id_Hexagono), "<br>",
                "<b>Distancia: </b>", round(Dis_ruta_m / 1000, 2), " Km"
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal      = paleta,
              values   = rutas_sub[[var_color]],
              title    = titulo_leyenda,
              opacity  = 0.9
            )
        }
      }
    }
  })
  
  ##--------------------------------------------------------------------------##
  ## 4. ValueBox Informativo Superior
  ##--------------------------------------------------------------------------##
  output$box_beneficiarios <- renderValueBox({
    rutas_filtradas <- rutas_filtradas_reactivas()
    lista_ids       <- seleccionados()
    
    subtitulo_caja <- if (length(lista_ids) == 0) {
      "Consolidado Total (Toda la Ciudad)"
    } else {
      paste("Acumulado en", length(lista_ids), "hexágonos seleccionados")
    }
    
    color_caja <- if (length(lista_ids) == 0) "navy" else "orange"
    
    total_ben <- if (!is.null(rutas_filtradas$SR_TotalEst)) {
      sum(rutas_filtradas$SR_TotalEst, na.rm = TRUE)
    } else { 0 }
    
    total_rutas <- nrow(rutas_filtradas)
    
    total_km <- if (!is.null(rutas_filtradas$Dis_ruta_m)) {
      sum(rutas_filtradas$Dis_ruta_m, na.rm = TRUE)
    } else { 0 }
    
    ben_txt   <- format(total_ben, big.mark = ",")
    rutas_txt <- format(total_rutas, big.mark = ",")
    km_txt    <- format(round(total_km / 1000, 0), big.mark = ",")
    
    valor_resumen <- paste(ben_txt, " Beneficiarios |", rutas_txt, " Rutas |", km_txt, " Km")
    
    valueBox(
      value    = valor_resumen,
      subtitle = subtitulo_caja,
      icon     = icon("chart-line"),
      color    = color_caja
    )
  })
  
  ##--------------------------------------------------------------------------##
  ## 5. Tabla Cruzada (Filas: Tipos de Vehículo | Columnas: Tipos de Ruta)
  ##--------------------------------------------------------------------------##
  output$tabla_resumen_rutas <- DT::renderDataTable({
    datos <- rutas_filtradas_reactivas()
    
    # Validar que existan datos tras aplicar los filtros
    if (nrow(datos) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada."),
        rownames = FALSE,
        options = list(dom = 't')
      ))
    }
    
    # 1. Agrupar y pivotear los datos base
    resumen_crosstab <- datos %>% 
      st_drop_geometry() %>% 
      filter(!is.na(SR_Veh_Aj_2), !is.na(SR_Tip_Ruta)) %>% 
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>% 
      summarise(Distancia_Total_Km = sum(Dis_ruta_m, na.rm = TRUE) / 1000, .groups = "drop") %>% 
      tidyr::pivot_wider(
        names_from  = SR_Tip_Ruta, 
        values_from = Distancia_Total_Km,
        values_fill = 0
      ) %>% 
      rename(`Tipo Vehículo` = SR_Veh_Aj_2)
    
    # 2. Agregar Columna "Total" (Suma horizontal por fila)
    resumen_crosstab <- resumen_crosstab %>% 
      mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE))
    
    # 3. Redondear valores a 2 decimales
    resumen_crosstab <- resumen_crosstab %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2)))
    
    # 4. Crear Fila "Total" (Suma vertical por columna)
    fila_total <- resumen_crosstab %>% 
      summarise(across(where(is.numeric), ~ round(sum(.x, na.rm = TRUE), 2))) %>% 
      mutate(`Tipo Vehículo` = "Total")
    
    # 5. Unir la fila de Totales al final del data.frame
    resumen_crosstab <- bind_rows(resumen_crosstab, fila_total)
    
    # 6. Renderizar tabla con DT y resaltar totales
    DT::datatable(
      resumen_crosstab,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"
        ),
        dom = "t" # Tabla limpia sin controles redundantes
      ),
      class = "cell-border stripe hover compact"
    ) %>% 
      # Resaltar la fila "Total"
      DT::formatStyle(
        'Tipo Vehículo',
        target = 'row',
        fontWeight = DT::styleEqual('Total', 'bold'),
        backgroundColor = DT::styleEqual('Total', '#f0f0f0')
      ) %>% 
      # Resaltar la columna "Total"
      DT::formatStyle(
        'Total',
        fontWeight = 'bold',
        backgroundColor = '#f9f9f9'
      )
  })
  
  ##--------------------------------------------------------------------------##
  ## 6. Lógica de Pestaña: Mapa de Clústeres
  ##--------------------------------------------------------------------------##
  seleccionados_cluster <- reactiveVal(character(0))
  
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
        label       = ~paste("Zona:", id, "| Cluster:", Cluster)
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
  
  observeEvent(input$mapa_clusteres_shape_click, {
    click  <- input$mapa_clusteres_shape_click
    req(click$id)
    raw_id <- as.character(click$id)
    
    if (length(raw_id) > 0 && nzchar(raw_id)) {
      id_cliqueado  <- gsub("^sel_", "", raw_id)
      vector_actual <- seleccionados_cluster()
      
      if (id_cliqueado %in% vector_actual) {
        nuevo_vector <- setdiff(vector_actual, id_cliqueado)
      } else {
        nuevo_vector <- c(vector_actual, id_cliqueado)
      }
      
      seleccionados_cluster(nuevo_vector)
    }
  })
  
  observe({
    vector_actual <- seleccionados_cluster()
    proxy <- leafletProxy("mapa_clusteres")
    
    proxy %>%
      clearGroup("seleccion_cluster") %>%
      clearGroup("Rutas escolares")
    
    if (length(vector_actual) > 0) {
      poly_sel <- poligonosV3 %>% filter(id %in% vector_actual)
      
      proxy %>%
        addPolygons(
          data        = poly_sel,
          layerId     = ~paste0("sel_", id),
          group       = "seleccion_cluster",
          fillColor   = "#e74c3c",
          fillOpacity = 0.85,
          color       = "#900C3F",
          weight      = 3,
          label       = ~paste("SELECCIONADO - Zona:", id, "| Patio:", nom_patio)
        )
      
      rutas_activas <- rutasv2 %>% filter(as.character(id_2) %in% vector_actual)
      
      if (nrow(rutas_activas) > 0) {
        proxy %>%
          addPolylines(
            data        = rutas_activas,
            color       = "#2ecc71",
            weight      = 3,
            opacity     = 0.9,
            group       = "Rutas escolares",
            label       = ~paste("Ruta:", ifelse(is.na(CodigoRuta), "Sin nombre", CodigoRuta))
          )
      }
    }
  })
}
##----------------------------------------------------------------------------##
## Ejecutar App
##----------------------------------------------------------------------------##
shinyApp(ui = ui, server = server)