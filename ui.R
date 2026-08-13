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
library(janitor)

##----------------------------------------------------------------------------##
##Abrir archivos necesarios
##----------------------------------------------------------------------------##
## WD
setwd(here())

# Archivos
poligonosV2 <- readRDS("Assets/RDS/poligonosV2.rds")
rutasv2 <- readRDS("Assets/RDS/rutas.rds")

##----------------------------------------------------------------------------##
##Variables adicionales
##----------------------------------------------------------------------------##
## Paleta de colores para 'nom_patio' en mapa_clusteres
factpal_patios <- colorFactor(
  palette = "Accent", 
  domain  = poligonosV2$Cluster
)
##----------------------------------------------------------------------------##
##Preprocesamiento
##----------------------------------------------------------------------------##
## Filtrar solo recorrido R1



## Seleccionar R1 si existen duplicados para el mismo CodigoRuta; de lo contrario, conservar el registro disponible (R1 o R2)
rutasv2R1 <- rutasv2 %>%
  group_by(CodigoRuta) %>%
  arrange(Recorrido_ == "R2") %>% # Ordena poniendo "R1" primero (FALSE < TRUE)
  slice(1) %>%
  ungroup()
print(rutasv2R1)

## Calcular variables semanales (distancia semanal y horas semanales)

rutasv2R1 <- rutasv2R1 %>% mutate(disRutaSem = Dis_ruta_m * SR_Tot_Dias)
rutasv2R1 <- rutasv2R1 %>% mutate(disHorasSem = SR_Toal_H * SR_Tot_Dias)


poligonosV2$NoRutas <- sapply(poligonosV2$id, function(x) sum(rutasv2R1$id_2 == x, na.rm = TRUE))

poligonosV3 <-poligonosV2 %>%filter(NoRutas >0)

##----------------------------------------------------------------------------##
##UI
##----------------------------------------------------------------------------##


ui <- dashboardPage(

  title = "Visor de rutas escolares",
  
  ## Header
  header = dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Rutas Escolares"),
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
      menuItem("Documentación", tabName = "documentacion", icon = icon("users")),
      menuItem("Cree sus zonas", tabName = "tab_mapa", icon = icon("binoculars")),
      menuItem("Explore zonas creadas", tabName = "tab_mapa2", icon = icon("map")),
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
          height: calc(90vh - 200px) !important;
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
      ## Pestaña 2: Mapa de zonas creadas
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
      
      ## Pestaña 3: Explore sus zonas
      tabItem(
        tabName = "tab_mapa2",
        fluidRow(
          column(width = 4, valueBoxOutput("box_beneficiarios1", width = 12)),
          column(width = 4, valueBoxOutput("box_beneficiarios2", width = 12)),
          column(width = 4, valueBoxOutput("box_beneficiarios3", width = 12))
        ),
        column(
          width = 7,
          box(
            title = "Mapa de exploracion de clústeres",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            leafletOutput("mapa_clusteres")
          )
        ),
        column(
          width = 5,
          # caja de controles del mapa
          box(
            width = 12,
            title = "Filtros de clústeres",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            solidHeader = TRUE,
            selectInput(
              inputId = "filtro_cluster",
              label = "Seleccione la zona",
              choices = c(sort(unique(poligonosV2$Cluster))),
              selected = "Todos",
              multiple = TRUE
            ),
            selectInput(
              inputId = "filtro_tipo_ruta_clus",
              label = "Seleccione los tipos de ruta",
              choices = c("Todos",sort(unique(rutasv2R1$SR_Tip_Ruta))),
              selected = "Todos",
              multiple = TRUE
            ),
            selectInput(
              inputId = "filtro_tipo_veh_clus",
              label = "Seleccione el tipo de vehículo",
              choices = c("Todos", sort(unique(rutasv2R1$SR_Veh_Aj_2))),
              selected = "Todos",
              multiple = TRUE
            )
          ),
          ## Cuadro de rutas
          box(
            width = 12,
            title = "Cantidad de rutas",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            DTOutput("CL_tabla_rutas")
          ),
          ## Cuadro de resultados_ Km semanales
          box(
            width = 12,
            title = "Distancias (Km) semanales por tipo de vehiculo y tipo de ruta",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            DTOutput("tabla_resumen_km")
          ),
          ## Cuadro de vehículos
          box(
            width = 12,
            title = "Cantidad de vehículos (Aproximado al siguiente entero)",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            h5("Parte de los factores de utilización promedio"),
            DTOutput("CL_tabla_veh")
          ),
          box(
            width = 12,
            title = "Horas contratadas a la semana",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            h5("Total de horas en una semana"),
            DTOutput("CL_tabla_horas")
          )
        ),
        ### Sección de demanda energética
        box(
          width = 12,
          title = "Demanda energética e infraestructura",
          status = 'primary',
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          h5("Demanda energética semanal"),
          radioButtons(
            inputId  = "demanda_tipo_calculo",
            label    = "Método de cálculo:",
            choices  = c("Promedio" = "promedio", "Factor de utilización diaria" = "factor"),
            selected = "promedio",
            inline   = TRUE
          ),
          conditionalPanel(
            condition = "input.demanda_tipo_calculo == 'factor'",
            column(
              width = 4,
              sliderInput(
                inputId = "demanda_factor_slider",
                label   = "Factor de ajuste/eficiencia: 1: Carga total semanal en 1 día a la semana, 0,2 carga promedio diaria (5 días a la semana)",
                min     = 0.2,
                max     = 1.0,
                value   = 0.6,
                step    = 0.2
              )
            )
          ),
          column(
            width = 4,
            sliderInput(
              inputId = "horas_vent_carga",
              label = "Elija las horas de la ventana de carga, suele ser similar a la duración de la jornada educativa",
              min = 2,
              max = 6,
              value = 3,
              step = 0.5
            )
          ),
          plotlyOutput("CL_demanda_energetica", height = "350px"),
          DTOutput("CL_demanda_energetica")
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
        fillColor   = "#ffffbf",
        fillOpacity = 0.5,
        color       = "#fc8d59",
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
    print(nuevo_vector)
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
                "<b>Distancia: </b>", round(disRutaSem/ 1000, 2), " Km"
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
      sum(rutas_filtradas$disRutaSem, na.rm = TRUE)/1000
    } else { 0 }
    
    ben_txt   <- format(total_ben, big.mark = ",")
    rutas_txt <- format(total_rutas, big.mark = ",")
    km_txt    <- format(round(total_km, 0), big.mark = ",")
    
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
  output$tabla_resumen_rutas <- renderDT({
    req(rutas_filtradas_reactivas())
    print(rutas_filtradas_reactivas())
    # Si el dataset resultante no tiene filas, muestra una tabla vacía sin error
    if (nrow(rutas_filtradas_reactivas()) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    print(rutas_filtradas_reactivas)
    tabla_resumen <- rutas_filtradas_reactivas() %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Total_KM = sum(disRutaSem, na.rm = TRUE)/1000, .groups = "drop") %>%
      pivot_wider(
        names_from  = SR_Tip_Ruta,
        values_from = Total_KM,
        values_fill = 0
      ) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2)%>% 
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
    
    datatable(
      tabla_resumen,
      extensions = 'Buttons', ## Activa botones
      options = list(
        pageLength = 10,
        #dom = 't',
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'csv',
            filename = '01_dist_km_semanal',
            text = 'Descargar CSV',
            fieldSeparator = ";"
          )
        )
      ),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 2:ncol(tabla_resumen), digits = 2)
  })
##--------------------------------------------------------------------------##
## 1. Lógica de pestaña: Escenarios propios
##--------------------------------------------------------------------------##
## 2.1. Ló  
##--------------------------------------------------------------------------##
## 2. Lógica de Pestaña: Mapa de Clústeres
##--------------------------------------------------------------------------##
  seleccionados_cluster <- reactiveVal(character(0))
  
  output$mapa_clusteres <- renderLeaflet({
    datos <- poligonos_filtrados()
    leaflet(datos) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId     = ~id,
        fillColor   = ~factpal_patios(Cluster),
        fillOpacity = 0.5,
        color       = "#f7f7f7",
        weight      = 1.5,
        group       = "Zonas hexagonales",
        label       = ~paste("Zona:", id, "| Cluster:", Cluster)
      ) %>%
      addLegend(
        pal      = factpal_patios,
        values   = ~Cluster,
        title    = "Grupo de rutas",
        position = "bottomright"
      )
  })
## 2.1. Lógica de filtrar zonas
  poligonos_filtrados <- reactive({
    # Si se selecciona "Todos" o no hay nada seleccionado, retorna todo el dataset
    if (is.null(input$filtro_cluster) || "Todos" %in% input$filtro_cluster) {
      return(poligonosV2)
    } else {
      return(poligonosV2[poligonosV2$Cluster %in% input$filtro_cluster, ])
    }
  })

## 2.2. Filtrar rutas por poligonos

rutasSubDataset <- reactive({
  # Activa reactividad previa 2.1.
  req(poligonos_filtrados())
  ids_presentes <- poligonos_filtrados()$id
  ## Filtra en rutas y almacena en rutasSubDataset
  datos_filtrados <-rutasv2R1 %>%filter(Id_Hexagono %in% ids_presentes)
  # Filtro por tipo de ruta
  print("Entrando al filtro de la base de datos de las rutas")
  if (!is.null(input$filtro_tipo_ruta_clus) && !"Todos" %in% input$filtro_tipo_ruta_clus) {
    datos_filtrados <- datos_filtrados %>% 
      filter(SR_Tip_Ruta %in% input$filtro_tipo_ruta_clus)
  }
  
  # Filtro por tipo de vehículo
  if (!is.null(input$filtro_tipo_veh_clus) && !"Todos" %in% input$filtro_tipo_veh_clus) {
    datos_filtrados <- datos_filtrados %>% 
      filter(SR_Veh_Aj_2 %in% input$filtro_tipo_veh_clus)
  }
  
  return(datos_filtrados)
})
## 2.3.Generación de tablas y gráficas
## 2.3.1. Km
## 2.3.1.1. Pivot table Km
output$tabla_resumen_km <- renderDT({
  req(rutasSubDataset())
  print(rutasSubDataset)
  # Si el dataset resultante no tiene filas, muestra una tabla vacía sin error
  if (nrow(rutasSubDataset()) == 0) {
    return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
  }
  print(rutasSubDataset)
  tabla_resumen <- rutasSubDataset() %>%
    sf::st_drop_geometry() %>%
    group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
    summarise(Total_KM = sum(disRutaSem, na.rm = TRUE)/1000, .groups = "drop") %>%
    pivot_wider(
      names_from  = SR_Tip_Ruta,
      values_from = Total_KM,
      values_fill = 0
    ) %>% 
    rename("Tipo de Vehículo" = SR_Veh_Aj_2)%>% 
  janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
  
  datatable(
    tabla_resumen,
    extensions = 'Buttons', ## Activa botones
    options = list(
      pageLength = 10,
      #dom = 't',
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'csv',
          filename = '01_dist_km_semanal',
          text = 'Descargar CSV',
          fieldSeparator = ";"
        )
      )
    ),
    rownames = FALSE
  ) %>% 
    formatRound(columns = 2:ncol(tabla_resumen), digits = 2)
})
## 2.3.1.2. Pivot table Cantidad de rutas
output$CL_tabla_rutas <- renderDT({
  req(rutasSubDataset())
  
  # Si el dataset resultante no tiene filas, muestra una tabla vacía sin error
  if (nrow(rutasSubDataset()) == 0) {
    return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
  }
  
  tabla_resumen <- rutasSubDataset() %>%
    sf::st_drop_geometry() %>%
    group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
    summarise(Cantidad = n(), .groups = "drop") %>%
    pivot_wider(
      names_from  = SR_Tip_Ruta,
      values_from = Cantidad,
      values_fill = 0
    ) %>% 
    rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
    janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
  
  datatable(
    tabla_resumen,
    extensions = 'Buttons',
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'csv',
          filename = '01_conteo_rutas',
          text = 'Descargar CSV',
          fieldSeparator = ";"
        )
      )
    ),
    rownames = FALSE
  )
})
## 2.3.1.3. Pivot table Cantidad de vehículos
output$CL_tabla_veh <- renderDT({
  req(rutasSubDataset())
  
  # Si el dataset resultante no tiene filas, muestra una tabla vacía sin error
  if (nrow(rutasSubDataset()) == 0) {
    return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
  }
  
  tabla_resumen <- rutasSubDataset() %>%
    sf::st_drop_geometry() %>%
    group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
    summarise(Cantidad = n(), .groups = "drop") %>%
    pivot_wider(
      names_from  = SR_Tip_Ruta,
      values_from = Cantidad,
      values_fill = 0
    ) %>% 
    rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
    # Modificación: Asignar factor por vehículo, dividir y redondear al siguiente entero (Mover a zona de variables pronto)
    mutate(
      factor = case_when(
        toupper(`Tipo de Vehículo`) == "BUS"       ~ 1.5,
        toupper(`Tipo de Vehículo`) == "BUSETA"    ~ 2.5,
        toupper(`Tipo de Vehículo`) %in% c("MICROBÚS", "MICROBUS") ~ 1.5,
        toupper(`Tipo de Vehículo`) == "VAN"       ~ 1.0,
        toupper(`Tipo de Vehículo`) == "CAMIONETA" ~ 2.5,
        TRUE ~ 1.0
      ),
      across(where(is.numeric) & !c(factor), ~ ceiling(.x / factor))
    ) %>% 
    select(-factor) %>% 
    janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
  
  datatable(
    tabla_resumen,
    extensions = 'Buttons',
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'csv',
          filename = '02_calculo_vehiculos_factor',
          text = 'Descargar CSV',
          fieldSeparator = ";"
        )
      )
    ),
    rownames = FALSE
  )
})
## 2.3.1.4. Pivot table Cantidad de horas a la semana
output$CL_tabla_horas <- renderDT({
  req(rutasSubDataset())
  print(rutasSubDataset)
  # Si el dataset resultante no tiene filas, muestra una tabla vacía sin error
  if (nrow(rutasSubDataset()) == 0) {
    return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
  }
  print(rutasSubDataset)
  tabla_resumen <- rutasSubDataset() %>%
    sf::st_drop_geometry() %>%
    group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
    summarise(TotalHoras = sum(disHorasSem, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from  = SR_Tip_Ruta,
      values_from = TotalHoras,
      values_fill = 0
    ) %>% 
    rename("Tipo de Vehículo" = SR_Veh_Aj_2)%>% 
    janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
  
  datatable(
    tabla_resumen,
    extensions = 'Buttons', ## Activa botones
    options = list(
      pageLength = 10,
      #dom = 't',
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'csv',
          filename = '04_Horas_contratadas',
          text = 'Descargar CSV',
          fieldSeparator = ";"
        )
      )
    ),
    rownames = FALSE
  ) %>% 
    formatRound(columns = 2:ncol(tabla_resumen), digits = 2)
})
## 2.3.2. Reactividad consumo
## 2.3.2.1. Grafica consumo

CL_demanda_energetica_datos <- reactive({
  # Requerir el dataset de rutas filtradas
  req(rutasSubDataset())
  
  datos <- rutasSubDataset()
  
  # Si es objeto espacial sf, remover geometría para acelerar procesamiento
  if (inherits(datos, "sf")) {
    datos <- sf::st_drop_geometry(datos)
  }
  
  if (nrow(datos) == 0) return(NULL)
  
  # Determinar el divisor o factor según el radio button
  factor_calculo <- if (input$demanda_tipo_calculo == "factor") {
    req(input$demanda_factor_slider)
    as.numeric(input$demanda_factor_slider)
  } else {
    5 # Si es promedio, dividir entre 5
  }
  
  # Evitar división por cero si el slider se sitúa en 0
  if (is.na(factor_calculo) || factor_calculo == 0) return(NULL)
  
  # Filtrar explícitamente por selección de inputs si existen
  if (!is.null(input$filtro_tipo_veh_clus) && length(input$filtro_tipo_veh_clus) > 0) {
    datos <- datos %>% filter(SR_Veh_Aj_2 %in% input$filtro_tipo_veh_clus)
  }
  
  if (!is.null(input$filtro_tipo_ruta_clus) && length(input$filtro_tipo_ruta_clus) > 0) {
    datos <- datos %>% filter(SR_Tip_Ruta %in% input$filtro_tipo_ruta_clus)
  }
  
  if (nrow(datos) == 0) return(NULL)
  
  # Agrupar por Tipo de Vehículo y Tipo de Ruta, y aplicar el factor a los km semanales
  df_agrupado <- datos %>%
    group_by(
      TipoVehiculo = SR_Veh_Aj_2,
      TipoRuta     = SR_Tip_Ruta
    ) %>%
    summarise(
      Km_Semanales = sum(disRutaSem / 1000, na.rm = TRUE),
      # Cálculo: suma de km / factor
      Resultado    = sum(disRutaSem / 1000, na.rm = TRUE) / factor_calculo,
      .groups      = "drop"
    )
  print(df_agrupado)
  return(df_agrupado)
})

# 2. Renderizar Gráfica Plotly en Barras Apiladas
output$CL_demanda_energetica <- renderPlotly({
  df <- CL_demanda_energetica_datos()
  print(df)
  
  # Control cuando no existen registros para los filtros seleccionados
  if (is.null(df) || nrow(df) == 0) {
    return(
      plotly_empty() %>% 
        layout(
          title = list(
            text = "No hay datos disponibles para la combinación de filtros seleccionada",
            font = list(size = 13, color = "#7f8c8d")
          )
        )
    )
  }
  
  # Etiqueta dinámica para el método aplicado
  metodo_lbl <- if (input$demanda_tipo_calculo == "factor") {
    paste0("Factor ajuste (÷ ", input$demanda_factor_slider, ")")
  } else {
    "Promedio (÷ 5)"
  }
  
  # Paleta de colores para los tipos de ruta
  paleta_rutas <- c(
    "#2c3e50", "#e74c3c", "#3498db", "#2ecc71", 
    "#f39c12", "#9b59b6", "#1abc9c", "#34495e"
  )
  
  # Construcción del gráfico apilado por tipo de ruta
  plot_ly(
    data = df,
    x = ~TipoVehiculo,
    y = ~Resultado,
    split = ~TipoRuta,
    color = ~TipoRuta,
    colors = paleta_rutas,
    type = "bar",
    hovertemplate = paste(
      "<b>Vehículo:</b> %{x}<br>",
      "<b>Tipo de Ruta:</b> %{fullData.name}<br>",
      "<b>Resultado:</b> %{y:,.2f}<br>",
      "<b>Km Semanales:</b> %{customdata:,.2f} km<extra></extra>"
    ),
    customdata = ~Km_Semanales
  ) %>%
    layout(
      barmode = "stack", # Apila los tipos de ruta dentro de cada vehículo
      title = list(
        text = paste0("<b>Demanda Energética por Vehículo y Tipo de Ruta</b><br><sup>Método: ", metodo_lbl, "</sup>"),
        font = list(size = 14)
      ),
      xaxis = list(
        title = "Tipo de Vehículo",
        tickangle = -15
      ),
      yaxis = list(
        title = ifelse(input$demanda_tipo_calculo == "factor", "Resultado (Km / Factor)", "Resultado (Km / 5)"),
        zeroline = TRUE
      ),
      legend = list(
        title = list(text = "<b>Tipo de Ruta</b>"),
        orientation = "h",
        x = 0,
        y = -0.25
      ),
      margin = list(t = 60, b = 80, l = 50, r = 20),
      hoverlabel = list(bgcolor = "white")
    ) %>%
    config(displayModeBar = FALSE)
})

}


##----------------------------------------------------------------------------##
## Ejecutar App
##----------------------------------------------------------------------------##
shinyApp(ui = ui, server = server)