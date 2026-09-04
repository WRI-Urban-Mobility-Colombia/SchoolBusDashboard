##----------------------------------------------------------------------------##
## Visor de Rutas Escolares - UI Moderna & Clara (Light Theme Immersivo)
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
library(config)
library(lubridate)

##----------------------------------------------------------------------------##
## Configuración y Carga de Datos
##----------------------------------------------------------------------------##
setwd(here())
Sys.setlocale("LC_ALL", "en_US.UTF-8")

CFG <- config::get(file = "config.yml")

poligonosV2 <- readRDS("Assets/RDS/poligonosV2.rds")
rutasv2     <- readRDS("Assets/RDS/rutas.rds")

pat_ele_buff <- readRDS("Assets/RDS/P_Elec_buff.rds")
pat_ele_punt <- readRDS("Assets/RDS/P_Elec_punt.rds")
punt_ad_buff <- readRDS("Assets/RDS/P_Ad_buff.rds")
punt_ad_punt <- readRDS("Assets/RDS/P_Ad_punt.rds")
colegios_pun <- readRDS("Assets/RDS/colegios.rds")
llaves_rutas <- read.csv2("Assets/csv/260901_Rutas_colegio3.csv")

## Preprocesamiento de datos
rutasv2R1 <- rutasv2 %>%
  group_by(CodigoRuta) %>%
  arrange(Recorrido_ == "R2") %>% 
  slice(1) %>%
  ungroup() %>%
  mutate(
    disRutaSem  = Dis_ruta_m * SR_Tot_Dias,
    disHorasSem = SR_Toal_H * SR_Tot_Dias,
    SR_DaneIED  = gsub("\\.", "", SR_DaneIED)
  )

poligonosV2$NoRutas <- sapply(poligonosV2$id, function(x) sum(rutasv2R1$id_2 == x, na.rm = TRUE))

##----------------------------------------------------------------------------##
## Definición de Interfaz de Usuario (UI)
##----------------------------------------------------------------------------##
ui <- dashboardPage(
  title = "Dashboard para la electrificación de rutas escolares de Bogotá D.C.",
  
  ## A. HEADER ---------------------------------------------------------------##
  header = dashboardHeader(
    title = tagList(
      span(class = "logo-lg", style = "font-weight: 800; letter-spacing: 0.5px; color: #ffffff;", "WRI | MOBILITY"),
      span(class = "logo-mini", style = "color: #ffffff; font-weight: 800;", "W")
    ),
    rightUi = userOutput("skin_dropdown")
  ),
  
  ## B. SIDEBAR AZUL OSCURO Y COLAPSABLE ------------------------------------##
  sidebar = dashboardSidebar(
    width = 240,
    minified = TRUE,
    collapsed = FALSE,
    sidebarMenu(
      id = "tab_seleccionada",
      menuItem("Diseño de Zonas", tabName = "tab_mapa", icon = icon("drafting-compass")),
      menuItem("Exploración & Impacto", tabName = "tab_mapa2", icon = icon("globe-americas")),
      menuItem("Documentación", tabName = "documentacion", icon = icon("book-open")),
      menuItem("Créditos", tabName = "creditos", icon = icon("award"))
    )
  ),
  
  ## C. CONTROLBAR -----------------------------------------------------------##
  controlbar = dashboardControlbar(id = "Controlbar", skinSelector()),
  
  ## D. CUERPO DE LA APLICACIÓN ----------------------------------------------##
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Estilos Globales - Fuente y Paletas */
        @import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700;800&display=swap');

        body, .content-wrapper, .right-side {
          font-family: 'Plus Jakarta Sans', sans-serif !important;
          background-color: #f8fafc !important;
          color: #0f172a;
        }

        /* Header / Navbar */
        .main-header .navbar, .main-header .logo {
          background-color: #0f172a !important;
          color: #ffffff !important;
        }

        /* Sidebar Azul Oscuro Avanzada */
        .main-sidebar, .left-side {
          background-color: #0f172a !important;
          box-shadow: 2px 0 10px rgba(0,0,0,0.1);
        }
        .sidebar-menu > li > a {
          color: #94a3b8 !important;
          border-left: 3px solid transparent;
          transition: all 0.2s ease;
        }
        .sidebar-menu > li.active > a, .sidebar-menu > li:hover > a {
          background-color: #1e293b !important;
          color: #38bdf8 !important;
          border-left-color: #38bdf8 !important;
        }

        /* Tooltips en Sidebar Colapsada */
        .sidebar-collapse .sidebar-menu > li:hover > a > span {
          display: block !important;
          position: absolute;
          left: 50px;
          top: 0;
          width: 180px;
          margin-left: 5px;
          padding: 10px 15px;
          background-color: #0f172a;
          color: #ffffff;
          border-radius: 0 6px 6px 0;
          box-shadow: 4px 4px 10px rgba(0,0,0,0.2);
          z-index: 9999;
          font-size: 13px;
        }

        /* Contenedor Immersivo del Mapa Canvas */
        .map-canvas-container {
          position: relative;
          width: 100%;
          height: calc(100vh - 130px);
          min-height: 580px;
          border-radius: 16px;
          overflow: hidden;
          border: 1px solid #e2e8f0;
          box-shadow: 0 10px 25px -5px rgba(15, 23, 42, 0.08);
        }

        #mapa_clusteres, #mapa_interactivo {
          width: 100% !important;
          height: 100% !important;
        }

        /* Evitar solapamiento de leyendas Leaflet con controles flotantes */
        .leaflet-bottom.leaflet-right {
          margin-bottom: 25px !important;
        }

        /* Paneles Traslúcidos (Frosted Light Glass) */
        .glass-panel-light {
          background: rgba(255, 255, 255, 0.92) !important;
          backdrop-filter: blur(12px);
          -webkit-backdrop-filter: blur(12px);
          border: 1px solid rgba(226, 232, 240, 0.9) !important;
          border-radius: 14px !important;
          padding: 16px;
          color: #0f172a;
          box-shadow: 0 10px 20px rgba(15, 23, 42, 0.06);
        }

        .glass-floating-controls {
          position: absolute;
          top: 160px;
          right: 16px;
          z-index: 1000;
          width: 320px;
        }

        .glass-floating-kpi {
          position: absolute;
          top: 16px;
          left: 16px;
          z-index: 1000;
          width: 330px;
        }

        .kpi-metric-card {
          background: #ffffff;
          border-radius: 8px;
          padding: 10px 12px;
          margin-bottom: 8px;
          border: 1px solid #e2e8f0;
          border-left: 4px solid #0284c7;
        }

        .kpi-title {
          font-size: 0.70rem;
          font-weight: 700;
          text-transform: uppercase;
          letter-spacing: 0.05em;
          color: #64748b;
          margin-bottom: 2px;
        }

        .kpi-value {
          font-size: 1.25rem;
          font-weight: 800;
          color: #0f172a;
        }

        /* Hoja Analítica Inferior */
        .analytics-sheet-light {
          margin-top: 24px;
          background: #ffffff;
          border-radius: 16px;
          padding: 24px;
          border: 1px solid #e2e8f0;
          box-shadow: 0 4px 12px rgba(0,0,0,0.03);
        }

        /* Botón Exportación PDF */
        .btn-export-light {
          background: linear-gradient(135deg, #0284c7 0%, #0369a1 100%) !important;
          color: #ffffff !important;
          font-weight: 700;
          border: none;
          border-radius: 8px;
          padding: 9px 22px;
          box-shadow: 0 4px 12px rgba(2, 132, 199, 0.25);
          transition: all 0.2s ease;
        }

        .btn-export-light:hover {
          transform: translateY(-1px);
          box-shadow: 0 6px 16px rgba(2, 132, 199, 0.35);
        }

        /* Estilos de Impresión */
        @media print {
          body, .content-wrapper {
            background: #ffffff !important;
            color: #000000 !important;
          }
          .no-print, .glass-floating-controls, .main-header, .main-sidebar, .controlbar {
            display: none !important;
          }
          .map-canvas-container {
            height: 450px !important;
            border: 1px solid #cbd5e1;
            page-break-inside: avoid;
          }
          .glass-floating-kpi {
            position: relative !important;
            top: 0 !important;
            left: 0 !important;
            width: 100% !important;
            background: #ffffff !important;
            border: 1px solid #e2e8f0 !important;
          }
          .analytics-sheet-light {
            border: none !important;
            box-shadow: none !important;
            page-break-before: always;
          }
        }
      "))
    ),
    
    tabItems(
      ##----------------------------------------------------------------------##
      ## PESTAÑA 1: DISEÑO DE ZONAS (Cree sus Zonas)
      ##----------------------------------------------------------------------##
      tabItem(
        tabName = "tab_mapa",
        fluidRow(
          style = "margin-bottom: 12px;",
          column(width = 12, valueBoxOutput("box_beneficiarios", width = 12))
        ),
        
        fluidRow(
          column(
            width = 8,
            div(
              class = "map-canvas-container",
              leafletOutput("mapa_interactivo")
            )
          ),
          column(
            width = 4,
            div(
              class = "glass-panel-light",
              style = "margin-bottom: 16px;",
              h4("Filtros y Controles de Diseño", style = "font-weight: 800; color: #0284c7; margin-top:0;"),
              radioButtons(
                inputId = "var_color_ruta",
                label = "Colorear Rutas por:",
                choices = c("Tipo de Ruta" = "SR_Tip_Ruta", "Tipo de Vehículo" = "SR_Veh_Aj_2"),
                selected = "SR_Tip_Ruta"
              ),
              selectizeInput(
                inputId  = "filtro_tipo_ruta",
                label    = "Seleccionar Tipo de Ruta:",
                choices  = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Todas las rutas", plugins = list("remove_button"))
              ),
              selectizeInput(
                inputId  = "filtro_tipo_vehiculo",
                label    = "Seleccionar Tipo de Vehículo:",
                choices  = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Todos los vehículos", plugins = list("remove_button"))
              )
            ),
            div(
              class = "glass-panel-light",
              h4("Distancia Acumulada (Km)", style = "font-weight: 800; color: #0f172a; margin-top:0;"),
              DT::dataTableOutput("tabla_resumen_rutas")
            )
          )
        )
      ),
      
      ##----------------------------------------------------------------------##
      ## PESTAÑA 2: EXPLORACIÓN & IMPACTO (Light Theme)
      ##----------------------------------------------------------------------##
      tabItem(
        tabName = "tab_mapa2",
        
        # Header y Exportación
        fluidRow(
          style = "margin-bottom: 12px; display: flex; align-items: center;",
          column(
            width = 8,
            h2("Exploración de Zonas e Impacto", style = "margin: 0; font-weight: 800; letter-spacing: -0.5px; color: #0f172a;")
          ),
          column(
            width = 4,
            div(
              class = "no-print",
              style = "text-align: right;",
              actionButton(
                inputId = "btn_imprimir",
                label   = " Exportar Reporte PDF",
                icon    = icon("file-pdf"),
                class   = "btn-export-light",
                onclick = "window.print();"
              )
            )
          )
        ),
        
        # Canvas de Mapa Integrado
        div(
          class = "map-canvas-container",
          
          leafletOutput("mapa_clusteres"),
          
          # Panel Flotante Izquierdo: Métricas PCBE
          div(
            class = "glass-panel-light glass-floating-kpi no-print",
            h5("CUMPLIMIENTO PCBE", style = "margin-top:30; font-weight:800; color:#0284c7; letter-spacing: 0.5px;"),
            div(
              class = "kpi-metric-card",
              div(class = "kpi-title", "Beneficiarios Atendidos"),
              div(class = "kpi-value", uiOutput("ben_atendidos"))
            ),
            div(
              class = "kpi-metric-card", style = "border-left-color: #16a34a;",
              div(class = "kpi-title", "Meta Política Pública"),
              div(class = "kpi-value", uiOutput("metaPCBE"))
            ),
            div(
              style = "height: 180px; margin-top: 50px;",
              plotlyOutput("plotly_gauge", height = "160px")
            )
          ),
          
          # Panel Flotante Derecho: Filtros
          div(
            class = "glass-panel-light glass-floating-controls no-print",
            h5("FILTROS DE MAPA", style = "margin-top:20; font-weight:800; color:#0284c7; letter-spacing: 0.5px;"),
            selectInput(
              inputId  = "filtro_cluster",
              label    = "Seleccione zona:",
              choices  = c(sort(unique(poligonosV2$Cluster))),
              selected = NULL,
              multiple = TRUE
            ),
            radioButtons(
              inputId  = "var_color_ruta_clus",
              label    = "Simbología de Rutas:",
              choices  = c("Tipo de Ruta" = "SR_Tip_Ruta", "Tipo de Vehículo" = "SR_Veh_Aj_2"),
              selected = "SR_Tip_Ruta"
            ),
            selectInput(
              inputId  = "filtro_tipo_ruta_clus",
              label    = "Tipo de Ruta:",
              choices  = c("Todos", sort(unique(rutasv2R1$SR_Tip_Ruta))),
              selected = "Todos",
              multiple = TRUE
            ),
            selectInput(
              inputId  = "filtro_tipo_veh_clus",
              label    = "Tipo de Vehículo:",
              choices  = c("Todos", sort(unique(rutasv2R1$SR_Veh_Aj_2))),
              selected = "Todos",
              multiple = TRUE
            )
          )
        ),
        
        # Módulos Analíticos Inferiores
        div(
          class = "analytics-sheet-light",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Beneficiarios",
              br(),
              fluidRow(
                box(
                  width = 12,
                  title = "Rutas y colegios",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  h4(em("Beneficiarios atendidos por colegio", style = "font-weight:700; color:#0f172a;")),
                  br(),
                  DTOutput("CL_tabla_beneficiarios_colegio"),
                  hr(),
                  h4(em("Beneficiarios atendidos por tipo de ruta y vehículo", style = "font-weight:700; color:#0f172a;")),
                  br(),
                  DTOutput("CL_tabla_beneficiarios_ruta_veh")
                )
              )
            ),
          
            # Sub-Pestaña 2: Reporte Operacional
            tabPanel(
              title = " Operación",
              br(),
              fluidRow(
                box(
                  width = 12,
                  title = "Rutas y colegios",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  h4(em("Reporte por colegio", style = "font-weight:700; color:#0f172a;")),
                  DTOutput("CL_tabla_colegios_detalle"),
                  hr(),
                  h4(em("Listado de rutas a impactar", style = "font-weight:700; color:#0f172a;")),
                  DTOutput("CL_tabla_rutas_detalle")
                ),
                box(
                  width = 12,
                  title = "Horas y kilómetros",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  h4(em("Horas contratadas", style = "font-weight:700; color:#0f172a;")),
                  DTOutput("CL_tabla_horas"),
                  hr(),
                  h4(em("Kilómetros semanales", style = "font-weight:700; color:#0f172a;")),
                  DTOutput("tabla_resumen_km")
                ),
                box(
                  width = 12,
                  title = "Rutas y vehículos",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  h4(em("Rutas", style = "font-weight:700; color:#0f172a;")),
                  DTOutput("CL_tabla_rutas"),
                  hr(),
                  h4(em("Vehículos", style = "font-weight:700; color:#0f172a;")),
                  DTOutput("CL_tabla_veh")
                ),
              )
            ),
            
            # Sub-Pestaña 2: Electro-Movilidad
            tabPanel(
              title = " Energía y recarga",
              br(),
              fluidRow(
                column(
                  width = 3,
                  div(
                    class = "glass-panel-light",
                    h4("Parámetros operativos", style = "color:#0284c7; font-weight:700; margin-top:0;"),
                    radioButtons(
                      inputId  = "EscenarioKm",
                      label    = "Escenario de Operación:",
                      choices  = c("Operación base (+26% vacío)" = "base", "Base más servicios adicionales" = "extras"),
                      selected = "base"
                    ),
                    conditionalPanel(
                      condition = 'input.EscenarioKm == "extras"',
                      numericInput("Kms_ad", "Km Adicionales / Semana:", value = 140, min = 0, max = 2100, step = 20)
                    ),
                    hr(),
                    radioButtons(
                      inputId  = "demanda_tipo_calculo",
                      label    = "Método de Cálculo:",
                      choices  = c("Promedio diario" = "promedio", "Factor de carga" = "factor"),
                      selected = "promedio"
                    ),
                    conditionalPanel(
                      condition = "input.demanda_tipo_calculo == 'factor'",
                      sliderInput("demanda_factor_slider", "Días de recarga semanal:", min = 1, max = 7, value = 3, step = 1)
                    ),
                    sliderInput("VentanaCarga", "Ventana de recarga (Horas):", min = 1.5, max = 12, value = 3, step = 0.5)
                  )
                ),
                column(
                  width = 9,
                  h4(em("Consumo de energía proyectado para los parámetros elegidos (kWh)",style = "font-weight:700; color:#0f172a;")),
                  plotlyOutput("CL_demanda_energetica_plot", height = "280px"),
                  br(),
                  DTOutput("CL_demanda_energetica"),
                  hr(),
                  h4(em("Patrones de Actividad de Entrada / Salida", style = "font-weight:700; color:#0f172a;")),
                  plotlyOutput("CL_horas_act", height = "240px"),
                  br(),
                  fluidRow(
                    column(4, div(class = "glass-panel-light", style="text-align:center;", h5("Energía Diaria"), uiOutput("CL_cons_diario"))),
                    column(4, div(class = "glass-panel-light", style="text-align:center;", h5("Potencia Total"), uiOutput("CL_pot_req"))),
                    column(4, div(class = "glass-panel-light", style="text-align:center;", h5("Cargadores Req."), uiOutput("CL_cargadores_req")))
                  )
                )
              )
            ),
            
            # Sub-Pestaña 3: Impacto Ambiental
            tabPanel(
              title = " Impacto Ambiental",
              br(),
              h4(em("Kilómetros Anuales Proyectados", style = "font-weight:700; color:#0f172a;")),
              DTOutput("CL_Km_ano_table"),
              hr(),
              fluidRow(
                column(
                  width = 7,
                  h4("Emisiones Evitadas por Contaminante (Ton/año)", style = "color:#16a34a; font-weight:700;"),
                  plotlyOutput("CL_emisiones_plot", height = "280px"),
                  selectInput(
                    inputId  = 'filtro_t_emision',
                    label    = 'Filtrar Contaminantes:',
                    choices  = c("CO", "VOC", "NOX", "SOX", "PM25", "PM10"),
                    selected = c("CO", "VOC", "NOX", "SOX", "PM25", "PM10"),
                    multiple = TRUE
                  )
                ),
                column(
                  width = 5,
                  h4("Descarbonización CO2eq", style = "color:#16a34a; font-weight:700;"),
                  plotlyOutput("CL_emisionesCO2eq", height = "280px")
                )
              ),
              br(),
              DTOutput("CL_emisiones_table")
            )
          )
        )
      ),
      
      ##----------------------------------------------------------------------##
      ## OTRAS PESTAÑAS
      ##----------------------------------------------------------------------##
      tabItem(
        tabName = "documentacion",
        div(
          class = "analytics-sheet-light",
          h3("Documentación del Sistema", style = "font-weight:800; color:#0f172a; margin-top:0;"),
          p("Esta sección reúne las especificaciones metodológicas, fuentes cartográficas e índices de evaluación de la flota escolar e infraestructura eléctrica de recarga.")
        )
      ),
      tabItem(
        tabName = "creditos",
        div(
          class = "analytics-sheet-light",
          h3("Créditos & Desarrollo", style = "font-weight:800; color:#0f172a; margin-top:0;"),
          p("Plataforma diseñada para la evaluación espacial, energética y ambiental de proyectos de electrificación de transporte de estudiantes.")
        )
      )
    )
  )
)

##----------------------------------------------------------------------------##
## Servidor
##----------------------------------------------------------------------------##
server <- function(input, output, session) {
  
  ## 2. Mapa creación de proyectos -------------------------------------------##
  observeEvent(input$mapa_interactivo_shape_click, {
    click <- input$mapa_interactivo_shape_click
    req(click$id)
    
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
  
  output$mapa_interactivo <- renderLeaflet({
    bus_icon <- makeAwesomeIcon(
      icon        = "bus",
      iconColor   = "white",
      markerColor = "green",
      library     = "fa"
    )
    cargador_icon <- makeAwesomeIcon(
      icon        = "bolt",
      iconColor   = "white",           
      markerColor = "blue",             
      library     = "fa"                
    )
    
    leaflet(poligonosV2) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addPolygons(
        data = pat_ele_buff,
        fillColor = "#4db608",
        fillOpacity = 0.5,
        weight = 2,
        dashArray = "4,4",
        group = "Buffer "
      ) %>%
      addPolygons(
        data = punt_ad_buff,
        fillColor = "#1335f3",
        fillOpacity = 0.5,
        weight = 2,
        dashArray = "4,4",
        group = "Buffer "
      ) %>%
      addPolygons(
        layerId     = ~id,
        fillColor   = "#ffffbf",
        fillOpacity = 0.5,
        color       = "#fc8d59",
        weight      = 1.5,
        label       = ~paste("Polígono:", id, " | Cluster:", Cluster)
      ) %>%
      addAwesomeMarkers(
        data = pat_ele_punt,
        icon = bus_icon, 
        group = "Patios eléctricos SITP"
      ) %>%
      addAwesomeMarkers(
        data = punt_ad_punt,
        icon = cargador_icon, 
        group = "Patios eléctricos SITP"
      ) %>%
      addLayersControl(
        overlayGroups = c("Polígonos", "Polígonos Seleccionados", "Rutas Filtradas"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })
  
  seleccionados <- reactiveVal(character(0))
  
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
  
  rutas_filtradas_reactivas <- reactive({
    req(rutasv2R1)
    datos <- rutasv2R1
    
    lista_ids <- seleccionados()
    if (length(lista_ids) > 0) {
      datos <- datos %>% filter(as.character(Id_Hexagono) %in% lista_ids)
    }
    
    if (!is.null(input$filtro_tipo_ruta) && length(input$filtro_tipo_ruta) > 0) {
      datos <- datos %>% filter(SR_Tip_Ruta %in% input$filtro_tipo_ruta)
    }
    
    if (!is.null(input$filtro_tipo_vehiculo) && length(input$filtro_tipo_vehiculo) > 0) {
      datos <- datos %>% filter(SR_Veh_Aj_2 %in% input$filtro_tipo_vehiculo)
    }
    
    if (inherits(datos, "sf") && nrow(datos) > 0) {
      datos <- datos %>% 
        filter(!st_is_empty(.)) %>%
        sf::st_make_valid()
    }
    
    return(datos)
  })
  
  observe({
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
    
    if (!is.null(rutas_sub) && nrow(rutas_sub) > 0) {
      var_color <- "SR_Tip_Ruta"
      if (!is.null(input$var_color_ruta) && is.character(input$var_color_ruta) && nzchar(input$var_color_ruta)) {
        var_color <- input$var_color_ruta
      }
      
      if (var_color %in% names(rutas_sub)) {
        vec_color <- rutas_sub[[var_color]]
        valores_unicos <- sort(unique(na.omit(vec_color)))
        
        if (length(valores_unicos) > 0) {
          paleta <- if (var_color == "SR_Tip_Ruta") paleta_tipo_ruta else paleta_tipo_vehiculo
          titulo_leyenda <- if (identical(var_color, "SR_Tip_Ruta")) "Tipo de Ruta" else "Tipo de Vehículo"
          
          dist_km <- ifelse(
            is.na(rutas_sub$disRutaSem), 
            "N/A", 
            paste0(round(rutas_sub$disRutaSem / 1000, 2), " Km")
          )
          
          proxy %>%
            addPolylines(
              data        = rutas_sub,
              group       = "Rutas Filtradas",
              color       = paleta(vec_color),
              weight      = 3.5,
              opacity     = 0.85,
              popup       = ~paste0(
                "<b>Tipo de Ruta: </b>", ifelse(is.na(SR_Tip_Ruta), "N/A", SR_Tip_Ruta), "<br>",
                "<b>Tipo Vehículo: </b>", ifelse(is.na(SR_Veh_Aj_2), "N/A", SR_Veh_Aj_2), "<br>",
                "<b>Hexágono ID: </b>", ifelse(is.na(Id_Hexagono), "N/A", Id_Hexagono), "<br>",
                "<b>Distancia: </b>", dist_km
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal      = paleta,
              values   = vec_color,
              title    = titulo_leyenda,
              opacity  = 0.9
            )
        }
      }
    }
  })
  
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
    
    ben_txt   <- format(total_ben, big.mark = ".")
    rutas_txt <- format(total_rutas, big.mark = ".")
    km_txt    <- format(round(total_km, 0), big.mark = ".")
    
    valor_resumen <- paste(ben_txt, " Beneficiarios |", rutas_txt, " Rutas |", km_txt, " Km")
    
    valueBox(
      value    = valor_resumen,
      subtitle = subtitulo_caja,
      icon     = icon("chart-line"),
      color    = color_caja
    )
  })
  
  output$tabla_resumen_rutas <- renderDT({
    df_rutas <- rutas_filtradas_reactivas()
    
    if (is.null(df_rutas) || nrow(df_rutas) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- df_rutas %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Total_KM = sum(disRutaSem, na.rm = TRUE)/1000, .groups = "drop") %>%
      pivot_wider(
        names_from  = SR_Tip_Ruta,
        values_from = Total_KM,
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
            filename = '01_dist_km_semanal',
            text = 'Descargar CSV',
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            fieldSeparator = ";",exportOptions = list(
              modifier = list(page = 'all', search = 'none')
            )
          )
        )
      ),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 2:ncol(tabla_resumen), digits = 2)
  })
  
  ## 3. Lógica de Pestaña: Mapa de Clústeres ----------------------------------##
  output$mapa_clusteres <- renderLeaflet({
    datos <- poligonos_filtrados()
    bbox  <- sf::st_bbox(poligonosV2)
    
    leaflet(datos,
            options = leafletOptions(
              minZoom = 10,
              maxZoom = 18
            )) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setMaxBounds(
        lng1 = as.numeric(bbox["xmin"]),
        lat1 = as.numeric(bbox["ymin"]),
        lng2 = as.numeric(bbox["xmax"]),
        lat2 = as.numeric(bbox["ymax"])
      ) %>%
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
      ) %>%
      addLayersControl(
        overlayGroups = c("Zonas hexagonales", "Rutas Clúster", "Colegios (DANE)"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    proxy <- leafletProxy("mapa_clusteres")
    poligonos_sub <- poligonos_filtrados()
    colegios <- colegios_filtrados_dane
    rutas_clus <- rutasSubDataset()
    
    proxy %>% 
      clearGroup("Zonas hexagonales") %>% 
      clearGroup("Rutas Clúster") %>% 
      clearGroup("Colegios beneficiarios") %>% 
      clearControls()
    
    if (nrow(poligonos_sub) > 0) {
      proxy %>%
        addPolygons(
          data        = poligonos_sub,
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
          values   = poligonos_sub$Cluster,
          title    = "Grupo de rutas",
          position = "bottomright"
        )
    }
    
    if (!is.null(rutas_clus) && nrow(rutas_clus) > 0 && !is.null(colegios)) {
      cods_dane_rutas <- unique(na.omit(rutas_clus$Dane_IED2))
      colegios_sub <- colegios[colegios$COD_DANE %in% cods_dane_rutas, ]
      
      if (nrow(colegios_sub) > 0) {
        proxy %>%
          addAwesomeMarkers(
            data  = colegios_sub,
            icon  = colegio_icon,
            group = "Colegios (DANE)",
            popup = ~paste0("<b>Colegio: </b>", NOMBRE_INS, "<br><b>Código DANE: </b>", COD_DANE)
          )
      }
    }
    
    if (!is.null(rutas_clus) && nrow(rutas_clus) > 0) {
      var_color <- ifelse(!is.null(input$var_color_ruta_clus) && nzchar(input$var_color_ruta_clus), 
                          input$var_color_ruta_clus, "SR_Tip_Ruta")
      
      if (var_color %in% names(rutas_clus)) {
        vec_color <- rutas_clus[[var_color]]
        valores_unicos <- sort(unique(na.omit(vec_color)))
        
        if (length(valores_unicos) > 0) {
          paleta <- if (var_color == "SR_Tip_Ruta") paleta_tipo_ruta else paleta_tipo_vehiculo
          titulo_leyenda <- if (identical(var_color, "SR_Tip_Ruta")) "Tipo de Ruta" else "Tipo de Vehículo"
          
          dist_km <- ifelse(
            is.na(rutas_clus$disRutaSem), 
            "N/A", 
            paste0(round(rutas_clus$disRutaSem / 1000, 2), " Km")
          )
          
          proxy %>%
            addPolylines(
              data        = rutas_clus,
              group       = "Rutas Clúster",
              color       = paleta(vec_color),
              weight      = 3.5,
              opacity     = 0.85,
              popup       = ~paste0(
                "<b>Código de la ruta: </b>", ifelse(is.na(CodigoRuta), "N/A", CodigoRuta), "<br>",
                "<b>Tipo de Ruta: </b>", ifelse(is.na(SR_Tip_Ruta), "N/A", SR_Tip_Ruta), "<br>",
                "<b>Tipo Vehículo: </b>", ifelse(is.na(SR_Veh_Aj_2), "N/A", SR_Veh_Aj_2), "<br>",
                "<b>Distancia semanal: </b>", dist_km
              )
            ) %>%
            addLegend(
              position = "bottomleft",
              pal      = paleta,
              values   = vec_color,
              title    = titulo_leyenda,
              opacity  = 0.9
            )
        }
      }
    }
  })
  
  poligonos_filtrados <- reactive({
    if (is.null(input$filtro_cluster) || length(input$filtro_cluster) == 0) {
      return(poligonosV2[0, ])
    }
    
    if ("Todos" %in% input$filtro_cluster) {
      return(poligonosV2)
    }
    
    return(poligonosV2[poligonosV2$Cluster %in% input$filtro_cluster, ])
  })
  
  rutasSubDataset <- reactive({
    req(poligonos_filtrados())
    
    if (nrow(poligonos_filtrados()) == 0) {
      return(rutasv2R1[0, ])
    }
    
    ids_presentes <- poligonos_filtrados()$id
    datos_filtrados <- rutasv2R1 %>% filter(Id_Hexagono %in% ids_presentes)
    
    if (!is.null(input$filtro_tipo_ruta_clus) && !"Todos" %in% input$filtro_tipo_ruta_clus) {
      datos_filtrados <- datos_filtrados %>% 
        filter(SR_Tip_Ruta %in% input$filtro_tipo_ruta_clus)
    }
    
    if (!is.null(input$filtro_tipo_veh_clus) && !"Todos" %in% input$filtro_tipo_veh_clus) {
      datos_filtrados <- datos_filtrados %>% 
        filter(SR_Veh_Aj_2 %in% input$filtro_tipo_veh_clus)
    }
    
    if (inherits(datos_filtrados, "sf") && nrow(datos_filtrados) > 0) {
      datos_filtrados <- datos_filtrados %>% 
        filter(!st_is_empty(.)) %>% 
        sf::st_make_valid()
    }
    
    return(datos_filtrados)
  })
  
  output$tabla_resumen_km <- renderDT({
    datos_clus <- rutasSubDataset()
    
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Total_KM = sum(disRutaSem, na.rm = TRUE)/1000, .groups = "drop") %>%
      pivot_wider(
        names_from  = SR_Tip_Ruta,
        values_from = Total_KM,
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
            filename = '01_dist_km_semanal',
            text = 'Descargar CSV',
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none')
            )
          )
        )
      ),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 2:ncol(tabla_resumen), digits = 2, interval = 3, mark = ".", dec.mark = ",")
  })
  
  output$CL_tabla_rutas <- renderDT({
    datos_clus <- rutasSubDataset()
    
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
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
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none')
            )
          )
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(tabla_resumen), digits = 0, interval = 3, mark = ".")
  })
  
  output$CL_tabla_veh <- renderDT({
    datos_clus <- rutasSubDataset()
    
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      pivot_wider(
        names_from  = SR_Tip_Ruta,
        values_from = Cantidad,
        values_fill = 0
      ) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      mutate(
        factor = case_when(
          toupper(`Tipo de Vehículo`) == "BUS"                       ~ 1.5,
          toupper(`Tipo de Vehículo`) == "BUSETA"                    ~ 2.5,
          toupper(`Tipo de Vehículo`) %in% c("MICROBÚS", "MICROBUS") ~ 1.5,
          toupper(`Tipo de Vehículo`) == "VAN"                       ~ 1.0,
          toupper(`Tipo de Vehículo`) == "CAMIONETA"                 ~ 2.5,
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
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none')
            )
          )
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(tabla_resumen), interval = 3, mark = ".", dec.mark = ",")
  })
  
  CL_tabla_veh_data <- reactive({
    datos <- rutasSubDataset()
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(data.frame())
    }
    
    tabla_resumen <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      pivot_wider(
        names_from  = SR_Tip_Ruta,
        values_from = Cantidad,
        values_fill = 0
      ) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      mutate(
        factor = case_when(
          toupper(`Tipo de Vehículo`) == "BUS"                       ~ 1.5,
          toupper(`Tipo de Vehículo`) == "BUSETA"                    ~ 2.5,
          toupper(`Tipo de Vehículo`) %in% c("MICROBÚS", "MICROBUS") ~ 1.5,
          toupper(`Tipo de Vehículo`) == "VAN"                       ~ 1.0,
          toupper(`Tipo de Vehículo`) == "CAMIONETA"                 ~ 2.5,
          TRUE ~ 1.0
        ),
        across(where(is.numeric) & !c(factor), ~ ceiling(.x / factor))
      ) %>% 
      select(-factor) %>% 
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
    
    return(tabla_resumen)
  })
  
  output$CL_tabla_rutas_detalle <- renderDT({
    data <- rutasSubDataset()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos disponibles")))
    }
    
    datos <- as.data.frame(data) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        CodigoRuta,
        'Segmento' = SR_Segmento_Geografico,
        'Tipo Ruta' = 17,
        'Segmento_op' = SR_Segmento_Op,
        'Contrato' = SR_No_Contrato,
        'Vehículo' = 21,
        'Colegio' =  SR_IED,
        'ColegioDane' = SR_DaneIED
      )
    datatable(
      datos,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'csv',
            filename = 'Listado_rutas',
            text = 'Descargar CSV',
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none')
            )
          )
        )
      ),
      rownames = FALSE
    ) 
  })
  
  output$CL_tabla_colegios_detalle <- renderDT({
    data <- rutasSubDataset()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos disponibles")))
    }
    
    datos <- data %>%
      sf::st_drop_geometry() %>%
      mutate(
        SR_Veh_Aj_2_clean = toupper(trimws(SR_Veh_Aj_2)),
        factor_veh = case_when(
          SR_Veh_Aj_2_clean == "BUS"                    ~ 1.5,
          SR_Veh_Aj_2_clean == "BUSETA"                 ~ 2.5,
          SR_Veh_Aj_2_clean %in% c("MICROBÚS", "MICROBUS") ~ 1.5,
          SR_Veh_Aj_2_clean == "VAN"                    ~ 1.0,
          SR_Veh_Aj_2_clean == "CAMIONETA"              ~ 2.5,
          TRUE ~ 1.0
        )
      ) %>%
      group_by(SR_IED, SR_DaneIED) %>%
      summarise(
        `Horas Semanales` = sum(disHorasSem, na.rm = TRUE),
        `Cantidad Rutas`  = n(),
        `Beneficiarios`   = sum(SR_TotalEst, na.rm = TRUE),
        `Vehículos`       = sum(
          tapply(factor_veh, SR_Veh_Aj_2_clean, function(f) ceiling(length(f) / f[1])),
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      rename(
        `Colegio`     = SR_IED,
        `Código DANE` = SR_DaneIED
      ) %>%
      mutate(
        `Código DANE` = as.character(`Código DANE`)
      )
    
    datatable(
      datos,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'csv',
            filename = 'Listado_rutas_agrupado_colegio',
            text = 'Descargar CSV',
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none')
            )
          )
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Horas Semanales"), digits = 2, interval = 3, mark = ".", dec.mark = ",") %>%
      formatRound(columns = c("Cantidad Rutas", "Beneficiarios", "Vehículos"), digits = 0, interval = 3, mark = ".")
  })
  
  output$CL_tabla_horas <- renderDT({
    datos_clus <- rutasSubDataset()
    
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(TotalHoras = sum(disHorasSem, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from  = SR_Tip_Ruta,
        values_from = TotalHoras,
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
            filename = '04_Horas_contratadas',
            text = 'Descargar CSV',
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none')
            )
          )
        )
      ),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 2:ncol(tabla_resumen), digits = 2, interval = 3, mark = ".", dec.mark = ",")
  })
  
  beneficiarios_atendidos <- reactive({
    data <- rutasSubDataset()
    if (is.null(data) || nrow(data) == 0) return(0)
    sum(data$SR_TotalEst, na.rm = TRUE)
  })
  
  output$ben_atendidos <- renderValueBox({
    total_atendidos <- beneficiarios_atendidos()
    
    valueBox(
      value = format(total_atendidos, big.mark = "."),
      subtitle = "Beneficiarios Atendidos",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$metaPCBE <- renderValueBox({
    meta_val <- CFG$meta_pcbe$beneficiarios
    
    valueBox(
      value = format(meta_val, big.mark = "."),
      subtitle = "Meta de Beneficiarios",
      icon = icon("bullseye"),
      color = "purple"
    )
  })
  
  output$plotly_gauge <- renderPlotly({
    data <- rutasSubDataset()
    meta <- CFG$meta_pcbe$beneficiarios
    
    if (is.null(data) || nrow(data) == 0) {
      cumplimiento <- 0
    } else {
      beneficiarios <- sum(data$SR_TotalEst, na.rm = TRUE)
      cumplimiento <- beneficiarios / meta * 100
    }
    
    fig <- plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = cumplimiento,
      number = list(suffix = "%", valueFormat = ".2f"),
      title = list(text = "Nivel de Avance", font = list(size = 16)),
      gauge = list(
        axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "gray"),
        bar = list(color = "#AD0909"),
        bgcolor = "white",
        borderwidth = 1,
        bordercolor = "gray",
        steps = list(
          list(range = c(0, 30), color = "#f8d7da"),
          list(range = c(30, 65), color = "#fff3cd"),
          list(range = c(65, 100), color = "#d1e7dd")
        )
      )
    ) %>%
      layout(
        separators = ",.",
        margin = list(l = 20, r = 20, t = 40, b = 20),
        font = list(family = "Arial")
      )
    fig
  })
  
  CLConsSubDataset <- reactive({
    datos <- rutasSubDataset()
    
    factor_perdidas <- 1 + dplyr::coalesce(CFG$modif_consumo$perdidas, 0)
    factor_kmvac    <- 1 + dplyr::coalesce(CFG$modif_km_vacio$km_vacio_perc, 0)
    
    req(datos, nrow(datos) > 0)
    
    tabla_Km <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(
        Total_disRutaSem = sum(disRutaSem/1000, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = SR_Tip_Ruta,
        values_from = Total_disRutaSem,
        values_fill = 0
      ) %>%
      rename(`Tipo de Vehículo` = SR_Veh_Aj_2) %>%
      mutate(across(where(is.numeric), ~ .x * factor_kmvac))
    
    cols_rutas <- setdiff(names(tabla_Km), "Tipo de Vehículo")
    
    if (isTruthy(input$EscenarioKm) && input$EscenarioKm == "extras") {
      req(input$Kms_ad, CL_tabla_veh_data())
      tabla_adicional <- CL_tabla_veh_data() %>%
        mutate(
          factor_veh = purrr::map_dbl(`Tipo de Vehículo`, ~ dplyr::coalesce(CFG$factor_consumo[[as.character(.x)]], 1.0)),
          across(
            where(is.numeric) & !matches("factor_veh"), 
            ~ .x * (input$Kms_ad * factor_perdidas * factor_veh)
          )
        )
      tabla_Km <- tabla_Km %>%
        left_join(tabla_adicional, by = "Tipo de Vehículo", suffix = c("", "_extra")) %>%
        mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
    }
    
    tabla_demanda <- tabla_Km %>%
      rowwise() %>%
      mutate(
        clave_vehiculo = tolower(gsub("[ -]", "_", `Tipo de Vehículo`)),
        factor_veh = purrr::map_dbl(`Tipo de Vehículo`, ~ dplyr::coalesce(CFG$factor_consumo[[as.character(.x)]], 1.0)),
      ) %>%
      mutate(across(all_of(cols_rutas), ~ .x * factor_veh * (factor_perdidas))) %>%
      ungroup() %>%
      select(-clave_vehiculo, -factor_veh)
    
    return(tabla_demanda)
  })
  
  output$CL_demanda_energetica <- renderDT({
    df_demanda <- req(CLConsSubDataset())
    df_demanda <- df_demanda %>% select(-any_of(c("Total")))
    
    if (nrow(df_demanda) == 0) {
      return(
        datatable(
          data.frame("Estado" = "No hay datos para la combinación de filtros seleccionada."),
          rownames = FALSE,
          options = list(dom = 't', ordering = FALSE)
        )
      )
    }
    
    tabla_con_totales <- df_demanda %>%
      janitor::adorn_totals(
        where = c("row", "col"), 
        fill  = "-", 
        na.rm = TRUE, 
        name  = "Total"
      ) %>%
      dplyr::rename("Consumo total (kWh)" = Total)
    
    datatable(
      tabla_con_totales,
      extensions = 'Buttons',
      rownames   = FALSE,
      class      = 'cell-border stripe hover compact',
      options    = list(
        pageLength = 10,
        scrollX    = TRUE,
        autoWidth  = FALSE,
        dom        = 'Bfrtip',
        columnDefs = list(
          list(width = '140px', className = 'dt-center', targets = '_all')
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({'text-align': 'center'});",
          "  this.api().columns.adjust();",
          "}"
        ),
        language   = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        buttons    = list(
          list(
            extend         = 'csv',
            filename       = paste0('02_demanda_energetica_', Sys.Date()),
            text           = 'Descargar CSV',
            fieldSeparator = ';',
            bom            = TRUE
          )
        )
      )
    ) %>% formatCurrency(
      columns  = setdiff(names(tabla_con_totales), "Tipo de Vehículo"),
      currency = "",
      interval = 3,
      mark     = ".",
      dec.mark = ",",
      digits   = 0
    )
  })
  
  output$CL_demanda_energetica_plot <- renderPlotly({
    df_demanda <- req(CLConsSubDataset())
    
    if (nrow(df_demanda) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "text") %>%
          layout(
            title = list(
              text = "No hay datos para la combinación de filtros seleccionada.",
              font = list(size = 14, color = "gray")
            )
          )
      )
    }
    
    df_long <- df_demanda %>%
      tidyr::pivot_longer(
        cols      = setdiff(names(df_demanda), "Tipo de Vehículo"),
        names_to  = "Tipo_Ruta",
        values_to = "Consumo_kWh"
      ) %>%
      dplyr::filter(Consumo_kWh > 0) %>%
      dplyr::mutate(
        Consumo_fmt = format(round(Consumo_kWh), big.mark = ".", decimal.mark = ",")
      )
    
    plot_ly(
      data      = df_long,
      x         = ~`Tipo de Vehículo`,
      y         = ~Consumo_kWh,
      color     = ~Tipo_Ruta,
      type      = "bar",
      text      = ~paste0(
        "<b>Vehículo:</b> ", `Tipo de Vehículo`, "<br>",
        "<b>Tipo de Ruta:</b> ", Tipo_Ruta, "<br>",
        "<b>Consumo:</b> ", Consumo_fmt, " kWh"
      ),
      hoverinfo = "text",
      textposition = "none"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "", tickangle = 0),
        yaxis = list(title = "Consumo (kWh)", zeroline = TRUE),
        legend = list(orientation = "h", x = 0, y = 1.15, title = list(text = "")),
        margin = list(l = 50, r = 20, t = 40, b = 40),
        hoverlabel = list(bgcolor = "purple")
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo    = FALSE,
        modeBarButtonsToRemove = list(
          "zoom2d", "pan2d", "select2d", "lasso2d", 
          "zoomIn2d", "zoomOut2d", "autoScale2d"
        )
      )
  })
  
  output$CL_horas_act <- renderPlotly({
    data <- req(rutasSubDataset())
    
    cols <- c("SR_H_Ini_R1", "SR_H_Ini_Jornada", "SR_H_Fin_Jornada", "SR_H_Ini_R2")
    
    fecha_ref <- Sys.Date()
    grid_intervalos <- data.frame(
      intervalo = seq(
        from = as.POSIXct(paste(fecha_ref, "04:00:00")),
        to   = as.POSIXct(paste(fecha_ref, "20:00:00")),
        by   = "15 mins"
      )
    ) %>% 
      mutate(intervalo_texto = format(intervalo, "%H:%M"))
    
    data_proc <- data %>%
      mutate(across(all_of(cols), ~ ifelse(.x == "N/A" | is.na(.x), NA, .x))) %>%
      mutate(across(all_of(cols), ~ {
        time_obj <- lubridate::parse_date_time(.x, orders = c("HM", "HMS"))
        lubridate::floor_date(time_obj, "15 mins")
      }))
    
    resumen <- data_proc %>%
      pivot_longer(
        cols = all_of(cols),
        names_to = "Variable",
        values_to = "Hora_Redondeada"
      ) %>%
      filter(!is.na(Hora_Redondeada)) %>%
      mutate(intervalo_texto = format(Hora_Redondeada, "%H:%M")) %>%
      group_by(intervalo_texto, Variable) %>%
      summarise(Conteo = n(), .groups = "drop")
    
    datos_grafica <- grid_intervalos %>%
      select(Hora = intervalo_texto) %>%
      left_join(resumen, by = c("Hora" = "intervalo_texto")) %>%
      tidyr::complete(Hora, Variable = cols, fill = list(Conteo = 0)) %>%
      filter(!is.na(Variable)) %>%
      mutate(Variable = factor(
        Variable,
        levels = c("SR_H_Ini_R1", "SR_H_Ini_Jornada", "SR_H_Fin_Jornada", "SR_H_Ini_R2"),
        labels = c(
          "Hora inicio del recorrido de ida",
          "Hora de inicio de clases",
          "Hora de fin de clases",
          "Hora de inicio del recorrido de regreso"
        )
      ))
    
    plot_ly(
      data = datos_grafica,
      x = ~Hora,
      y = ~Conteo,
      color = ~Variable,
      type = "bar"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Intervalo de Tiempo (15 min)", tickangle = -45, type = "category"),
        yaxis = list(title = "Cantidad de servicios"),
        legend = list(orientation = "h", x = 0, y = 1.15),
        margin = list(b = 80)
      )
  })
  
  output$CL_cons_diario <- renderUI({
    consData <- req(CLConsSubDataset(), input$demanda_tipo_calculo)
    
    total_energia <- consData %>%
      dplyr::select(-dplyr::any_of("Total")) %>%
      dplyr::select(where(is.numeric)) %>%
      as.matrix() %>%
      sum(na.rm = TRUE)
    
    divisor <- if (input$demanda_tipo_calculo == "promedio") {
      5
    } else {
      req(input$demanda_factor_slider)
      input$demanda_factor_slider
    }
    
    resultado <- total_energia / divisor
    total_fmt <- format(round(resultado, 2), big.mark = ".", decimal.mark = ",")
    
    return(valueBox(
      width = NULL,
      value = format(paste0(total_fmt, " kWh"), big.mark = "."),
      subtitle = "Energía diaria a utilizar",
      icon = icon("bolt"),
      color = "blue"
    ))
  })
  
  output$CL_pot_req <- renderUI({
    consData <- req(CLConsSubDataset(), input$demanda_tipo_calculo, input$VentanaCarga)
    
    total_energia <- consData %>%
      dplyr::select(-dplyr::any_of("Total")) %>%
      dplyr::select(where(is.numeric)) %>%
      as.matrix() %>%
      sum(na.rm = TRUE)
    
    divisor <- if (input$demanda_tipo_calculo == "promedio") {
      5
    } else {
      req(input$demanda_factor_slider)
      input$demanda_factor_slider
    }
    
    resultado <- total_energia / divisor / input$VentanaCarga
    total_fmt <- format(round(resultado, 2), big.mark = ".", decimal.mark = ",")
    
    return(valueBox(
      width = NULL,
      value = paste0(total_fmt, " kW"),
      subtitle = "Potencia diaria requerida",
      icon = icon("plug"),
      color = "green"
    ))
  })
  
  output$CL_cargadores_req <- renderUI({
    consData <- req(CLConsSubDataset(), input$demanda_tipo_calculo)
    total_energia <- consData %>%
      dplyr::select(-dplyr::any_of("Total")) %>%
      dplyr::select(where(is.numeric)) %>%
      as.matrix() %>%
      sum(na.rm = TRUE)
    
    divisor <- if (input$demanda_tipo_calculo == "promedio") {
      5
    } else {
      req(input$demanda_factor_slider)
      input$demanda_factor_slider
    }
    
    resultado <- total_energia / divisor / input$VentanaCarga
    potencia <- round(resultado, 2)
    cargadores <- ceiling(potencia / 150)
    
    return(valueBox(
      width = NULL,
      value = paste0(cargadores),
      subtitle = "Con cargadores de 150 kW",
      icon = icon("charging-station"),
      color = "purple"
    ))
  })
  
  CL_KmSubdataset <- reactive({
    datos <- req(rutasSubDataset())
    req(datos, nrow(datos) > 0)
    
    fac_esco <- dplyr::coalesce(CFG$factor_exp$semana_esco, 40)
    fac_gral <- dplyr::coalesce(CFG$factor_exp$semana_gral, 52)
    
    tabla_Km <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(
        Total_disRutaSem = sum(disRutaSem / 1000, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = SR_Tip_Ruta,
        values_from = Total_disRutaSem,
        values_fill = 0
      ) %>%
      rename(`Tipo de Vehículo` = SR_Veh_Aj_2)
    
    if (isTruthy(input$EscenarioKm) && input$EscenarioKm == "extras") {
      req(input$Kms_ad, CL_tabla_veh_data())
      
      tabla_adicional <- CL_tabla_veh_data() %>%
        mutate(across(where(is.numeric), ~ .x * input$Kms_ad)) %>%
        select(-matches("^total$", ignore.case = TRUE))
      
      tabla_Km <- tabla_Km %>%
        left_join(tabla_adicional, by = "Tipo de Vehículo", suffix = c("", "_extra")) %>%
        mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
    }
    
    tabla_Km <- tabla_Km %>%
      rowwise() %>%
      mutate(
        Km_escolares = sum(c_across(where(is.numeric) & !ends_with("_extra")), na.rm = TRUE),
        Km_Extras = if (any(endsWith(names(.), "_extra"))) {
          sum(c_across(ends_with("_extra")), na.rm = TRUE)
        } else { 0 }
      ) %>%
      ungroup() %>%
      select(`Tipo de Vehículo`, Km_escolares, Km_Extras) %>%
      mutate(across(where(is.numeric), ~ round(.x, 0)))
    
    tabla_Km <- tabla_Km %>%
      mutate(
        Km_escolares = Km_escolares * fac_esco,
        Km_Extras    = Km_Extras * fac_gral,
        Km_totales   = Km_escolares + Km_Extras
      )
    return(tabla_Km)
  })
  
  emisionesSubDataset <- reactive({
    tabla_Km <- req(CL_KmSubdataset())
    emisiones_yaml <- CFG$emisiones
    
    tabla_emisiones <- emisiones_yaml %>%
      dplyr::bind_rows(.id = "Tipo de Vehículo") %>%
      rename_with(~ toupper(.x), -`Tipo de Vehículo`) %>%
      pivot_longer(
        cols = -`Tipo de Vehículo`, 
        names_to = "Contaminante", 
        values_to = "Factor"
      ) %>%
      inner_join(tabla_Km, by = "Tipo de Vehículo") %>%
      mutate(
        Emisiones_Escolar = (Km_escolares * Factor) / 1000000,
        Emisiones_Extra   = (Km_Extras * Factor) / 1000000,
        Emisiones_Total   = Emisiones_Escolar + Emisiones_Extra
      ) %>%
      group_by(Contaminante) %>%
      summarise(
        Emisiones_Escolar = sum(Emisiones_Escolar, na.rm = TRUE),
        Emisiones_Extra   = sum(Emisiones_Extra, na.rm = TRUE),
        Emisiones_Total   = sum(Emisiones_Total, na.rm = TRUE),
        .groups = "drop"
      )
    return(tabla_emisiones)
  })
  
  output$CL_emisionesCO2eq <- renderPlotly({
    data <- req(emisionesSubDataset())
    contaminantes_sel <- "CO2EQ"
    
    data_filtrada <- data %>%
      mutate(
        Contaminante = toupper(as.character(Contaminante)),
        Emisiones_Escolar = ifelse(is.na(Emisiones_Escolar), 0, Emisiones_Escolar),
        Emisiones_Extra   = ifelse(is.na(Emisiones_Extra), 0, Emisiones_Extra)
      ) %>%
      filter(Contaminante %in% toupper(contaminantes_sel)) %>%
      mutate(Contaminante = factor(Contaminante, levels = toupper(contaminantes_sel)))
    
    req(nrow(data_filtrada) > 0)
    
    plot_ly(
      data = data_filtrada, 
      x = ~Contaminante, 
      y = ~Emisiones_Escolar, 
      name = 'Escolar', 
      type = 'bar',
      marker = list(color = '#1f77b4')
    ) %>%
      add_trace(
        y = ~Emisiones_Extra, 
        name = 'Extra', 
        marker = list(color = '#ff7f0e')
      ) %>%
      layout(
        separators = ",.",
        barmode = 'stack',
        xaxis = list(title = 'Contaminante', type = 'category'),
        yaxis = list(title = 'Emisiones evitadas (Toneladas)', tickformat = ',.1f'),
        legend = list(title = list(text = 'Tipo de Emisión')),
        hovermode = 'x unified'
      )
  })
  
  output$CL_emisiones_plot <- renderPlotly({
    data <- req(emisionesSubDataset())
    contaminantes_sel <- req(input$filtro_t_emision)
    
    data_filtrada <- data %>%
      mutate(
        Contaminante = toupper(as.character(Contaminante)),
        Emisiones_Escolar = ifelse(is.na(Emisiones_Escolar), 0, Emisiones_Escolar),
        Emisiones_Extra   = ifelse(is.na(Emisiones_Extra), 0, Emisiones_Extra)
      ) %>%
      filter(Contaminante %in% toupper(contaminantes_sel)) %>%
      mutate(Contaminante = factor(Contaminante, levels = toupper(contaminantes_sel)))
    
    req(nrow(data_filtrada) > 0)
    
    plot_ly(
      data = data_filtrada, 
      x = ~Contaminante, 
      y = ~Emisiones_Escolar, 
      name = 'Escolar', 
      type = 'bar',
      marker = list(color = '#1f77b4')
    ) %>%
      add_trace(
        y = ~Emisiones_Extra, 
        name = 'Extra', 
        marker = list(color = '#ff7f0e')
      ) %>%
      layout(
        separators = ",.",
        barmode = 'stack',
        xaxis = list(title = 'Contaminante', type = 'category'),
        yaxis = list(title = 'Emisiones evitadas (Toneladas)', tickformat = ',.1f'),
        legend = list(title = list(text = 'Tipo de Emisión')),
        hovermode = 'x unified'
      )
  })
  
  output$CL_Km_ano_table <- renderDT({
    data <- req(CL_KmSubdataset())
    datatable(data,
              extensions = 'Buttons',
              options = list(
                pageLength = 7,
                dom = 'Bfrtip',
                buttons = c('csv', 'excel')
              ),
              rownames = FALSE)
  })
  
  output$CL_emisiones_table <- renderDT({
    data <- req(emisionesSubDataset())
    datatable(data,
              extensions = 'Buttons',
              options = list(
                pageLength = 7,
                dom = 'Bfrtip',
                buttons = c('csv', 'excel')
              ),
              rownames = FALSE) %>%
      formatRound(columns = c("Emisiones_Escolar", "Emisiones_Extra", "Emisiones_Total"), digits = 2)
  })
  
}

shinyApp(ui = ui, server = server)
##----------------------------------------------------------------------------##
## Servidor
##----------------------------------------------------------------------------##
server <- function(input, output, session) {

##--------------------------------------------------------------------------##
## 2. Mapa creación de proyectos
##--------------------------------------------------------------------------##

  ## 2.1. Observer del mapa interactivo (Identificar zonas seleccionadas)-----##
  observeEvent(input$mapa_interactivo_shape_click, {
    click <- input$mapa_interactivo_shape_click
    req(click$id)
    
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

##--------------------------------------------------------------------------##  
## 2.2. Mapa interactivo base-----------------------------------------------##
  output$mapa_interactivo <- renderLeaflet({
    bus_icon <- makeAwesomeIcon(
      icon        = "bus",
      iconColor   = "white",
      markerColor = "green",
      library     = "fa"
    )
    cargador_icon <- makeAwesomeIcon(
      icon        = "bolt",
      iconColor   = "white",           
      markerColor = "blue",             
      library     = "fa"                
    )
    
    leaflet(poligonosV2) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addPolygons(
        data = pat_ele_buff,
        fillColor = "#4db608",
        fillOpacity = 0.5,
        weight = 2,
        dashArray = "4,4",
        group = "Buffer "
      ) %>%
      addPolygons(
        data = punt_ad_buff,
        fillColor = "#1335f3",
        fillOpacity = 0.5,
        weight = 2,
        dashArray = "4,4",
        group = "Buffer "
      ) %>%
      addPolygons(
        layerId     = ~id,
        fillColor   = "#ffffbf",
        fillOpacity = 0.5,
        color       = "#fc8d59",
        weight      = 1.5,
        label       = ~paste("Polígono:", id, " | Cluster:", Cluster)
      ) %>%
      addAwesomeMarkers(
        data = pat_ele_punt,
        icon = bus_icon, 
        group = "Patios eléctricos SITP"
      ) %>%
      addAwesomeMarkers(
        data = punt_ad_punt,
        icon = cargador_icon, 
        group = "Patios eléctricos SITP"
      ) %>%
      addLayersControl(
        overlayGroups = c("Polígonos", "Polígonos Seleccionados", "Rutas Filtradas"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })

##--------------------------------------------------------------------------##  
## 2.3. Vector zonas seleccionadas------------------------------------------##
  seleccionados <- reactiveVal(character(0))

##--------------------------------------------------------------------------##
## 2.4. Resaltar polígonos seleccionados-------------------------------------##
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
## 2.5. Cargar Opciones de Filtros------------------------------------------##
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
## 2.6. Reactivo de Filtrado Conjunto (Espacial + Controles UI)--------------##
  rutas_filtradas_reactivas <- reactive({
    req(rutasv2R1)
    datos <- rutasv2R1
    
    lista_ids <- seleccionados()
    if (length(lista_ids) > 0) {
      datos <- datos %>% filter(as.character(Id_Hexagono) %in% lista_ids)
    }
    
    if (!is.null(input$filtro_tipo_ruta) && length(input$filtro_tipo_ruta) > 0) {
      datos <- datos %>% filter(SR_Tip_Ruta %in% input$filtro_tipo_ruta)
    }
    
    if (!is.null(input$filtro_tipo_vehiculo) && length(input$filtro_tipo_vehiculo) > 0) {
      datos <- datos %>% filter(SR_Veh_Aj_2 %in% input$filtro_tipo_vehiculo)
    }
    
    if (inherits(datos, "sf") && nrow(datos) > 0) {
      datos <- datos %>% 
        filter(!st_is_empty(.)) %>%
        sf::st_make_valid()
    }
    
    return(datos)
  })

##--------------------------------------------------------------------------##
## 2.7. Dibuja y Colorea las Rutas en el Mapa Interactivo Principal---------##
  observe({
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
    
    if (!is.null(rutas_sub) && nrow(rutas_sub) > 0) {
      var_color <- "SR_Tip_Ruta"
      if (!is.null(input$var_color_ruta) && is.character(input$var_color_ruta) && nzchar(input$var_color_ruta)) {
        var_color <- input$var_color_ruta
      }
      
      if (var_color %in% names(rutas_sub)) {
        vec_color <- rutas_sub[[var_color]]
        valores_unicos <- sort(unique(na.omit(vec_color)))
        
        if (length(valores_unicos) > 0) {
          paleta <- if (var_color == "SR_Tip_Ruta") paleta_tipo_ruta else paleta_tipo_vehiculo
          titulo_leyenda <- if (identical(var_color, "SR_Tip_Ruta")) "Tipo de Ruta" else "Tipo de Vehículo"
          
          dist_km <- ifelse(
            is.na(rutas_sub$disRutaSem), 
            "N/A", 
            paste0(round(rutas_sub$disRutaSem / 1000, 2), " Km")
          )
          
          proxy %>%
            addPolylines(
              data        = rutas_sub,
              group       = "Rutas Filtradas",
              color       = paleta(vec_color),
              weight      = 3.5,
              opacity     = 0.85,
              popup       = ~paste0(
                "<b>Tipo de Ruta: </b>", ifelse(is.na(SR_Tip_Ruta), "N/A", SR_Tip_Ruta), "<br>",
                "<b>Tipo Vehículo: </b>", ifelse(is.na(SR_Veh_Aj_2), "N/A", SR_Veh_Aj_2), "<br>",
                "<b>Hexágono ID: </b>", ifelse(is.na(Id_Hexagono), "N/A", Id_Hexagono), "<br>",
                "<b>Distancia: </b>", dist_km
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal      = paleta,
              values   = vec_color,
              title    = titulo_leyenda,
              opacity  = 0.9
            )
        }
      }
    }
  })

##--------------------------------------------------------------------------##
## 2.8. Value box superior--------------------------------------------------##  
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
    
    ben_txt   <- format(total_ben, big.mark = ".")
    rutas_txt <- format(total_rutas, big.mark = ".")
    km_txt    <- format(round(total_km, 0), big.mark = ".")
    
    valor_resumen <- paste(ben_txt, " Beneficiarios |", rutas_txt, " Rutas |", km_txt, " Km")
    
    valueBox(
      value    = valor_resumen,
      subtitle = subtitulo_caja,
      icon     = icon("chart-line"),
      color    = color_caja
    )
  })

##--------------------------------------------------------------------------##
## 2.9. Pivot table Km------------------------------------------------------##   
  output$tabla_resumen_rutas <- renderDT({
    df_rutas <- rutas_filtradas_reactivas()
    
    if (is.null(df_rutas) || nrow(df_rutas) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- df_rutas %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Total_KM = sum(disRutaSem, na.rm = TRUE)/1000, .groups = "drop") %>%
      pivot_wider(
        names_from  = SR_Tip_Ruta,
        values_from = Total_KM,
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
            filename = '01_dist_km_semanal',
            text = 'Descargar CSV',
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            fieldSeparator = ";",exportOptions = list(
              modifier = list(page = 'all', search = 'none') # Captura todas las páginas e ignora el filtro si se requiere
            )
          )
        )
      ),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 2:ncol(tabla_resumen), digits = 2)
  })

##--------------------------------------------------------------------------##
## 3. Lógica de Pestaña: Mapa de Clústeres----------------------------------##
##--------------------------------------------------------------------------##
## 3.1. Render mapa base de zonas-------------------------------------------##
  output$mapa_clusteres <- renderLeaflet({
    datos <- poligonos_filtrados()
    
    bbox <- sf::st_bbox(poligonosV2)
    
    leaflet(datos,
            options = leafletOptions(
              minZoom = 10,
              maxZoom = 18
            )) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setMaxBounds(
        lng1 = as.numeric(bbox["xmin"]),
        lat1 = as.numeric(bbox["ymin"]),
        lng2 = as.numeric(bbox["xmax"]),
        lat2 = as.numeric(bbox["ymax"])
      ) %>%
      addPolygons(
        layerId     = ~id,
        fillColor   = ~factpal_patios(Cluster),
        fillOpacity = 0.5,
        color       = "#f7f7f7",
        weight      = 1.5,
        group       = "Zonas hexagonales",
        label       = ~paste("Zona:", id, "| Cluster:", Cluster)
      ) %>%
      #addAwesomeMarkers(
      #  data  = colegios_filtrados_dane,
      #  icon  = colegio_icon,
      #  group = "Colegios (DANE)",
      #  popup = ~paste0("<b>Colegio: </b>", NOMBRE_INS)
      #) %>%
      addLegend(
        pal      = factpal_patios,
        values   = ~Cluster,
        title    = "Grupo de rutas",
        position = "bottomright"
      ) %>%
      addLayersControl(
        overlayGroups = c("Zonas hexagonales", "Rutas Clúster", "Colegios (DANE)"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })
## 3.1.a. Reactive colegios
  
## 3.1.b Observer para actualizar Polígonos y Rutas en Mapa de Clústeres ----##
  observe({
    proxy <- leafletProxy("mapa_clusteres")
    poligonos_sub <- poligonos_filtrados()
    colegios <- colegios_filtrados_dane
    rutas_clus <- rutasSubDataset()
    
    proxy %>% 
      clearGroup("Zonas hexagonales") %>% 
      clearGroup("Rutas Clúster") %>% 
      clearGroup("Colegios beneficiarios") %>% 
      clearControls()
    
    if (nrow(poligonos_sub) > 0) {
      proxy %>%
        addPolygons(
          data        = poligonos_sub,
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
          values   = poligonos_sub$Cluster,
          title    = "Grupo de rutas",
          position = "bottomright"
        )
    }
    
    ## Colegios filtrados por SR_DaneIED de las rutas
    if (!is.null(rutas_clus) && nrow(rutas_clus) > 0 && !is.null(colegios)) {
      cods_dane_rutas <- unique(na.omit(rutas_clus$Dane_IED2))
      print("Códigos dane de colegios cuyas rutas fueron seleccionadas")
      print(cods_dane_rutas)
      colegios_sub <- colegios[colegios$COD_DANE %in% cods_dane_rutas, ]
      print("Colegios filtrados")
      print(colegios_sub)
      print(colegios$COD_DANE)
      
      if (nrow(colegios_sub) > 0) {
        proxy %>%
          addAwesomeMarkers(
            data  = colegios_sub,
            icon  = colegio_icon,
            group = "Colegios (DANE)",
            popup = ~paste0("<b>Colegio: </b>", NOMBRE_INS, "<br><b>Código DANE: </b>", COD_DANE)
          )
      }
    }
    
    if (!is.null(rutas_clus) && nrow(rutas_clus) > 0) {
      var_color <- ifelse(!is.null(input$var_color_ruta_clus) && nzchar(input$var_color_ruta_clus), 
                          input$var_color_ruta_clus, "SR_Tip_Ruta")
      
      if (var_color %in% names(rutas_clus)) {
        vec_color <- rutas_clus[[var_color]]
        valores_unicos <- sort(unique(na.omit(vec_color)))
        
        if (length(valores_unicos) > 0) {
          paleta <- if (var_color == "SR_Tip_Ruta") paleta_tipo_ruta else paleta_tipo_vehiculo
          titulo_leyenda <- if (identical(var_color, "SR_Tip_Ruta")) "Tipo de Ruta" else "Tipo de Vehículo"
          
          dist_km <- ifelse(
            is.na(rutas_clus$disRutaSem), 
            "N/A", 
            paste0(round(rutas_clus$disRutaSem / 1000, 2), " Km")
          )
          
          proxy %>%
            addPolylines(
              data        = rutas_clus,
              group       = "Rutas Clúster",
              color       = paleta(vec_color),
              weight      = 3.5,
              opacity     = 0.85,
              popup       = ~paste0(
                "<b>Código de la ruta: </b>", ifelse(is.na(CodigoRuta), "N/A", CodigoRuta), "<br>",
                "<b>Tipo de Ruta: </b>", ifelse(is.na(SR_Tip_Ruta), "N/A", SR_Tip_Ruta), "<br>",
                "<b>Tipo Vehículo: </b>", ifelse(is.na(SR_Veh_Aj_2), "N/A", SR_Veh_Aj_2), "<br>",
                "<b>Distancia semanal: </b>", dist_km
              )
            ) %>%
            addLegend(
              position = "bottomleft",
              pal      = paleta,
              values   = vec_color,
              title    = titulo_leyenda,
              opacity  = 0.9
            )
        }
      }
    }
  })

##--------------------------------------------------------------------------##
## 3.2. Lógica de filtrar zonas---------------------------------------------##
  poligonos_filtrados <- reactive({
    if (is.null(input$filtro_cluster) || length(input$filtro_cluster) == 0) {
      return(poligonosV2[0, ])
    }
    
    if ("Todos" %in% input$filtro_cluster) {
      return(poligonosV2)
    }
    
    return(poligonosV2[poligonosV2$Cluster %in% input$filtro_cluster, ])
  })

##--------------------------------------------------------------------------##
## 3.3. Filtrar rutas por poligonos-----------------------------------------##
  rutasSubDataset <- reactive({
    req(poligonos_filtrados())
    
    if (nrow(poligonos_filtrados()) == 0) {
      return(rutasv2R1[0, ])
    }
    
    ids_presentes <- poligonos_filtrados()$id
    datos_filtrados <- rutasv2R1 %>% filter(Id_Hexagono %in% ids_presentes)
    
    if (!is.null(input$filtro_tipo_ruta_clus) && !"Todos" %in% input$filtro_tipo_ruta_clus) {
      datos_filtrados <- datos_filtrados %>% 
        filter(SR_Tip_Ruta %in% input$filtro_tipo_ruta_clus)
    }
    
    if (!is.null(input$filtro_tipo_veh_clus) && !"Todos" %in% input$filtro_tipo_veh_clus) {
      datos_filtrados <- datos_filtrados %>% 
        filter(SR_Veh_Aj_2 %in% input$filtro_tipo_veh_clus)
    }
    
    if (inherits(datos_filtrados, "sf") && nrow(datos_filtrados) > 0) {
      datos_filtrados <- datos_filtrados %>% 
        filter(!st_is_empty(.)) %>% 
        sf::st_make_valid()
    }
    
    return(datos_filtrados)
  })

##--------------------------------------------------------------------------##
## 3.3. Tablas de datos por categoría---------------------------------------##

  ## 2.3.1.1. Pivot table Km
  output$tabla_resumen_km <- renderDT({
    datos_clus <- rutasSubDataset()
    
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Total_KM = sum(disRutaSem, na.rm = TRUE)/1000, .groups = "drop") %>%
      pivot_wider(
        names_from  = SR_Tip_Ruta,
        values_from = Total_KM,
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
            filename = '01_dist_km_semanal',
            text = 'Descargar CSV',
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none') # Captura todas las páginas e ignora el filtro si se requiere
            )
          )
        )
      ),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 2:ncol(tabla_resumen), digits = 2, interval = 3, mark = ".", dec.mark = ",")
  })

  ## 2.3.1.2. Pivot table Cantidad de rutas
  output$CL_tabla_rutas <- renderDT({
    datos_clus <- rutasSubDataset()
    
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
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
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none') # Captura todas las páginas e ignora el filtro si se requiere
            )
          )
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(tabla_resumen), digits = 0, interval = 3, mark = ".")
  })

  ## 2.3.1.3. Pivot table Cantidad de vehículos
  output$CL_tabla_veh <- renderDT({
    datos_clus <- rutasSubDataset()
    
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      pivot_wider(
        names_from  = SR_Tip_Ruta,
        values_from = Cantidad,
        values_fill = 0
      ) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      mutate(
        factor = case_when(
          toupper(`Tipo de Vehículo`) == "BUS"                       ~ 1.5,
          toupper(`Tipo de Vehículo`) == "BUSETA"                    ~ 2.5,
          toupper(`Tipo de Vehículo`) %in% c("MICROBÚS", "MICROBUS") ~ 1.5,
          toupper(`Tipo de Vehículo`) == "VAN"                       ~ 1.0,
          toupper(`Tipo de Vehículo`) == "CAMIONETA"                 ~ 2.5,
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
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none') # Captura todas las páginas e ignora el filtro si se requiere
            )
          )
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(tabla_resumen), interval = 3, mark = ".", dec.mark = ",")
  })

  ## 2.3.1.3. Pivot table Cantidad de vehículos (Solo datos)
  CL_tabla_veh_data <- reactive({
    datos <- rutasSubDataset()
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(data.frame())
    }
    
    tabla_resumen <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      pivot_wider(
        names_from  = SR_Tip_Ruta,
        values_from = Cantidad,
        values_fill = 0
      ) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      mutate(
        factor = case_when(
          toupper(`Tipo de Vehículo`) == "BUS"                       ~ 1.5,
          toupper(`Tipo de Vehículo`) == "BUSETA"                    ~ 2.5,
          toupper(`Tipo de Vehículo`) %in% c("MICROBÚS", "MICROBUS") ~ 1.5,
          toupper(`Tipo de Vehículo`) == "VAN"                       ~ 1.0,
          toupper(`Tipo de Vehículo`) == "CAMIONETA"                 ~ 2.5,
          TRUE ~ 1.0
        ),
        across(where(is.numeric) & !c(factor), ~ ceiling(.x / factor))
      ) %>% 
      select(-factor) %>% 
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
    
    return(tabla_resumen)
  })

  ## 2.3.1.3.9. Listado reporte de rutas
  output$CL_tabla_rutas_detalle <- renderDT({
    data <- rutasSubDataset()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos disponibles")))
    }
    
    datos <- as.data.frame(data) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        CodigoRuta,
        'Segmento' = SR_Segmento_Geografico,
        'Tipo Ruta' = 17,
        'Segmento_op' = SR_Segmento_Op,
        'Contrato' = SR_No_Contrato,
        'Vehículo' = 21,
        'Colegio' =  SR_IED,
        'ColegioDane' = SR_DaneIED
      )
    datatable(
      datos,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'csv',
            filename = 'Listado_rutas',
            text = 'Descargar CSV',
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none') # Captura todas las páginas e ignora el filtro si se requiere
            )
          )
        )
      ),
      rownames = FALSE
    ) 
  })
  ## 2.3.1.3.9. Listado reporte de rutas
  
  output$CL_tabla_colegios_detalle <- renderDT({
    data <- rutasSubDataset()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos disponibles")))
    }
    
    datos <- data %>%
      sf::st_drop_geometry() %>%
      mutate(
        SR_Veh_Aj_2_clean = toupper(trimws(SR_Veh_Aj_2)),
        factor_veh = case_when(
          SR_Veh_Aj_2_clean == "BUS"                    ~ 1.5,
          SR_Veh_Aj_2_clean == "BUSETA"                 ~ 2.5,
          SR_Veh_Aj_2_clean %in% c("MICROBÚS", "MICROBUS") ~ 1.5,
          SR_Veh_Aj_2_clean == "VAN"                    ~ 1.0,
          SR_Veh_Aj_2_clean == "CAMIONETA"              ~ 2.5,
          TRUE ~ 1.0
        )
      ) %>%
      group_by(SR_IED, SR_DaneIED) %>%
      summarise(
        `Horas Semanales` = sum(disHorasSem, na.rm = TRUE),
        `Cantidad Rutas`  = n(), # Se mantiene el cálculo directo original
        `Beneficiarios`   = sum(SR_TotalEst, na.rm = TRUE),
        
        # Cálculo exclusivo de vehículos por tipo de vehículo dentro del mismo colegio:
        `Vehículos`       = sum(
          tapply(factor_veh, SR_Veh_Aj_2_clean, function(f) ceiling(length(f) / f[1])),
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      rename(
        `Colegio`     = SR_IED,
        `Código DANE` = SR_DaneIED
      ) %>%
      mutate(
        `Código DANE` = as.character(`Código DANE`)
      )
    
    datatable(
      datos,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'csv',
            filename = 'Listado_rutas_agrupado_colegio',
            text = 'Descargar CSV',
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none')
            )
          )
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Horas Semanales"), digits = 2, interval = 3, mark = ".", dec.mark = ",") %>%
      formatRound(columns = c("Cantidad Rutas", "Beneficiarios", "Vehículos"), digits = 0, interval = 3, mark = ".")
  })

  ## 2.3.1.4. Pivot table Cantidad de horas a la semana
  output$CL_tabla_horas <- renderDT({
    datos_clus <- rutasSubDataset()
    
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(TotalHoras = sum(disHorasSem, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from  = SR_Tip_Ruta,
        values_from = TotalHoras,
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
            filename = '04_Horas_contratadas',
            text = 'Descargar CSV',
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none') # Captura todas las páginas e ignora el filtro si se requiere
            )
          )
        )
      ),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 2:ncol(tabla_resumen), digits = 2, interval = 3, mark = ".", dec.mark = ",")
  })

  ## 2.3.1.1. Calculo beneficiarios
  beneficiarios_atendidos <- reactive({
    data <- rutasSubDataset()
    if (is.null(data) || nrow(data) == 0) return(0)
    sum(data$SR_TotalEst, na.rm = TRUE)
  })

  ## 2.3.1.2. value box beneficiarios y pcbe
  output$ben_atendidos <- renderValueBox({
    total_atendidos <- beneficiarios_atendidos()
    
    valueBox(
      value = format(total_atendidos, big.mark = "."),
      subtitle = "Beneficiarios Atendidos",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$metaPCBE <- renderValueBox({
    meta_val <- CFG$meta_pcbe$beneficiarios
    
    valueBox(
      value = format(meta_val, big.mark = "."),
      subtitle = "Meta de Beneficiarios",
      icon = icon("bullseye"),
      color = "purple"
    )
  })

  ## 2.3.1.3. Velocimetro
  output$plotly_gauge <- renderPlotly({
    data <- rutasSubDataset()
    meta <- CFG$meta_pcbe$beneficiarios
    
    if (is.null(data) || nrow(data) == 0) {
      cumplimiento <- 0
    } else {
      beneficiarios <- sum(data$SR_TotalEst, na.rm = TRUE)
      cumplimiento <- beneficiarios / meta * 100
    }
    
    fig <- plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = cumplimiento,
      number = list(suffix = "%",
                    valueFormat = ".2f"),
      title = list(text = "Nivel de Avance", font = list(size = 16)),
      gauge = list(
        axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "gray"),
        bar = list(color = "#AD0909"),
        bgcolor = "white",
        borderwidth = 1,
        bordercolor = "gray",
        steps = list(
          list(range = c(0, 30), color = "#f8d7da"),
          list(range = c(30, 65), color = "#fff3cd"),
          list(range = c(65, 100), color = "#d1e7dd")
        )
      )
    ) %>%
      layout(
        separators = ",.",
        margin = list(l = 20, r = 20, t = 40, b = 20),
        font = list(family = "Arial")
      )
    fig
  })

  ## 2.3.2. Reactividad consumo (Dataset)
  CLConsSubDataset <- reactive({
    datos <- rutasSubDataset()
    
    factor_perdidas <- 1 + dplyr::coalesce(CFG$modif_consumo$perdidas, 0)
    factor_kmvac <- 1 + dplyr::coalesce(CFG$modif_km_vacio$km_vacio_perc, 0)
    
    req(datos, nrow(datos) > 0)
    
    tabla_Km <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(
        Total_disRutaSem = sum(disRutaSem/1000, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = SR_Tip_Ruta,
        values_from = Total_disRutaSem,
        values_fill = 0
      ) %>%
      rename(`Tipo de Vehículo` = SR_Veh_Aj_2) %>%
      mutate(across(where(is.numeric), ~ .x * factor_kmvac))
    
    cols_rutas <- setdiff(names(tabla_Km), "Tipo de Vehículo")
    
    if (isTruthy(input$EscenarioKm) && input$EscenarioKm == "extras") {
      req(input$Kms_ad, CL_tabla_veh_data())
      tabla_adicional <- CL_tabla_veh_data() %>%
        mutate(
          factor_veh = purrr::map_dbl(`Tipo de Vehículo`, ~ dplyr::coalesce(CFG$factor_consumo[[as.character(.x)]], 1.0)),
          across(
            where(is.numeric) & !matches("factor_veh"), 
            ~ .x * (input$Kms_ad * factor_perdidas * factor_veh)
          )
        )
      tabla_Km <- tabla_Km %>%
        left_join(tabla_adicional, by = "Tipo de Vehículo", suffix = c("", "_extra")) %>%
        mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
    }
    
    tabla_demanda <- tabla_Km %>%
      rowwise() %>%
      mutate(
        clave_vehiculo = tolower(gsub("[ -]", "_", `Tipo de Vehículo`)),
        factor_veh = purrr::map_dbl(`Tipo de Vehículo`, ~ dplyr::coalesce(CFG$factor_consumo[[as.character(.x)]], 1.0)),
      ) %>%
      mutate(across(all_of(cols_rutas), ~ .x * factor_veh * (factor_perdidas))) %>%
      ungroup() %>%
      select(-clave_vehiculo, -factor_veh)

    return(tabla_demanda)
  })

  ### 2.3.3. Render tabla
  output$CL_demanda_energetica <- renderDT({
    df_demanda <- req(CLConsSubDataset())
    df_demanda <- df_demanda %>% select(-any_of(c("Total")))
    
    if (nrow(df_demanda) == 0) {
      return(
        datatable(
          data.frame("Estado" = "No hay datos para la combinación de filtros seleccionada."),
          rownames = FALSE,
          options = list(dom = 't', ordering = FALSE)
        )
      )
    }
    
    tabla_con_totales <- df_demanda %>%
      janitor::adorn_totals(
        where = c("row", "col"), 
        fill  = "-", 
        na.rm = TRUE, 
        name  = "Total"
      ) %>%
      dplyr::rename("Consumo total (kWh)" = Total)

    datatable(
      tabla_con_totales,
      extensions = 'Buttons',
      rownames   = FALSE,
      class      = 'cell-border stripe hover compact',
      options    = list(
        pageLength = 10,
        scrollX    = TRUE,
        autoWidth  = FALSE,
        dom        = 'Bfrtip',
        columnDefs = list(
          list(width = '140px', className = 'dt-center', targets = '_all')
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({'text-align': 'center'});",
          "  this.api().columns.adjust();",
          "}"
        ),
        language   = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        buttons    = list(
          list(
            extend         = 'csv',
            filename       = paste0('02_demanda_energetica_', Sys.Date()),
            text           = 'Descargar CSV',
            fieldSeparator = ';',
            bom            = TRUE
          )
        )
      )
    ) %>% formatCurrency(
      columns  = setdiff(names(tabla_con_totales), "Tipo de Vehículo"),
      currency = "",
      interval = 3,
      mark     = ".",
      dec.mark = ",",
      digits   = 0
    )
  })

  ## 2.3.4. Render gráfica consumos
  output$CL_demanda_energetica_plot <- renderPlotly({
    df_demanda <- req(CLConsSubDataset())
    
    if (nrow(df_demanda) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "text") %>%
          layout(
            title = list(
              text = "No hay datos para la combinación de filtros seleccionada.",
              font = list(size = 14, color = "gray")
            )
          )
      )
    }
    
    df_long <- df_demanda %>%
      tidyr::pivot_longer(
        cols      = setdiff(names(df_demanda), "Tipo de Vehículo"),
        names_to  = "Tipo_Ruta",
        values_to = "Consumo_kWh"
      ) %>%
      dplyr::filter(Consumo_kWh > 0) %>%
      dplyr::mutate(
        Consumo_fmt = format(round(Consumo_kWh), big.mark = ".", decimal.mark = ",")
      )
    
    plot_ly(
      data      = df_long,
      x         = ~`Tipo de Vehículo`,
      y         = ~Consumo_kWh,
      color     = ~Tipo_Ruta,
      type      = "bar",
      text      = ~paste0(
        "<b>Vehículo:</b> ", `Tipo de Vehículo`, "<br>",
        "<b>Tipo de Ruta:</b> ", Tipo_Ruta, "<br>",
        "<b>Consumo:</b> ", Consumo_fmt, " kWh"
      ),
      hoverinfo = "text",
      textposition = "none"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "", tickangle = 0),
        yaxis = list(title = "Consumo (kWh)", zeroline = TRUE),
        legend = list(orientation = "h", x = 0, y = 1.15, title = list(text = "")),
        margin = list(l = 50, r = 20, t = 40, b = 40),
        hoverlabel = list(bgcolor = "purple")
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo    = FALSE,
        modeBarButtonsToRemove = list(
          "zoom2d", "pan2d", "select2d", "lasso2d", 
          "zoomIn2d", "zoomOut2d", "autoScale2d"
        )
      )
  })

  ## 2.3.2.2. Dinámica de la zona
  output$CL_horas_act <- renderPlotly({
    data <- req(rutasSubDataset())
    
    cols <- c("SR_H_Ini_R1", "SR_H_Ini_Jornada", "SR_H_Fin_Jornada", "SR_H_Ini_R2")
    
    fecha_ref <- Sys.Date()
    grid_intervalos <- data.frame(
      intervalo = seq(
        from = as.POSIXct(paste(fecha_ref, "04:00:00")),
        to   = as.POSIXct(paste(fecha_ref, "20:00:00")),
        by   = "15 mins"
      )
    ) %>% 
      mutate(intervalo_texto = format(intervalo, "%H:%M"))
    
    data_proc <- data %>%
      mutate(across(all_of(cols), ~ ifelse(.x == "N/A" | is.na(.x), NA, .x))) %>%
      mutate(across(all_of(cols), ~ {
        time_obj <- lubridate::parse_date_time(.x, orders = c("HM", "HMS"))
        lubridate::floor_date(time_obj, "15 mins")
      }))
    
    resumen <- data_proc %>%
      pivot_longer(
        cols = all_of(cols),
        names_to = "Variable",
        values_to = "Hora_Redondeada"
      ) %>%
      filter(!is.na(Hora_Redondeada)) %>%
      mutate(intervalo_texto = format(Hora_Redondeada, "%H:%M")) %>%
      group_by(intervalo_texto, Variable) %>%
      summarise(Conteo = n(), .groups = "drop")
    
    datos_grafica <- grid_intervalos %>%
      select(Hora = intervalo_texto) %>%
      left_join(resumen, by = c("Hora" = "intervalo_texto")) %>%
      tidyr::complete(Hora, Variable = cols, fill = list(Conteo = 0)) %>%
      filter(!is.na(Variable)) %>%
      mutate(Variable = factor(
        Variable,
        levels = c("SR_H_Ini_R1", "SR_H_Ini_Jornada", "SR_H_Fin_Jornada", "SR_H_Ini_R2"),
        labels = c(
          "Hora inicio del recorrido de ida",
          "Hora de inicio de clases",
          "Hora de fin de clases",
          "Hora de inicio del recorrido de regreso"
        )
      ))
    
    plot_ly(
      data = datos_grafica,
      x = ~Hora,
      y = ~Conteo,
      color = ~Variable,
      type = "bar"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Intervalo de Tiempo (15 min)", tickangle = -45, type = "category"),
        yaxis = list(title = "Cantidad de servicios"),
        legend = list(orientation = "h", x = 0, y = 1.15),
        margin = list(b = 80)
      )
  })

  ## 2.3.2.3. Valores de recarga
  output$CL_cons_diario <- renderUI({
    consData <- req(CLConsSubDataset(), input$demanda_tipo_calculo)
    
    total_energia <- consData %>%
      dplyr::select(-dplyr::any_of("Total")) %>%
      dplyr::select(where(is.numeric)) %>%
      as.matrix() %>%
      sum(na.rm = TRUE)
    
    divisor <- if (input$demanda_tipo_calculo == "promedio") {
      5
    } else {
      req(input$demanda_factor_slider)
      input$demanda_factor_slider
    }
    
    resultado <- total_energia / divisor
    total_fmt <- format(round(resultado, 2), big.mark = ".", decimal.mark = ",")
    
    return(valueBox(
      width = NULL,
      value = format(paste0(total_fmt, " kWh"), big.mark = "."),
      subtitle = "Energía diaria a utilizar",
      icon = icon("bolt"),
      color = "blue"
    ))
  })

  output$CL_pot_req <- renderUI({
    consData <- req(CLConsSubDataset(), input$demanda_tipo_calculo, input$VentanaCarga)
    
    total_energia <- consData %>%
      dplyr::select(-dplyr::any_of("Total")) %>%
      dplyr::select(where(is.numeric)) %>%
      as.matrix() %>%
      sum(na.rm = TRUE)
    
    divisor <- if (input$demanda_tipo_calculo == "promedio") {
      5
    } else {
      req(input$demanda_factor_slider)
      input$demanda_factor_slider
    }
    
    resultado <- total_energia / divisor / input$VentanaCarga
    total_fmt <- format(round(resultado, 2), big.mark = ".", decimal.mark = ",")
    
    return(valueBox(
      width = NULL,
      value = paste0(total_fmt, " kW"),
      subtitle = "Potencia diaria requerida",
      icon = icon("plug"),
      color = "green"
    ))
  })

  output$CL_cargadores_req <- renderUI({
    consData <- req(CLConsSubDataset(), input$demanda_tipo_calculo)
    total_energia <- consData %>%
      dplyr::select(-dplyr::any_of("Total")) %>%
      dplyr::select(where(is.numeric)) %>%
      as.matrix() %>%
      sum(na.rm = TRUE)
    
    divisor <- if (input$demanda_tipo_calculo == "promedio") {
      5
    } else {
      req(input$demanda_factor_slider)
      input$demanda_factor_slider
    }
    
    resultado <- total_energia / divisor / input$VentanaCarga
    potencia <- round(resultado, 2)
    cargadores <- ceiling(potencia / 150)
    
    return(valueBox(
      width = NULL,
      value = paste0(cargadores),
      subtitle = "Con cargadores de 150 kW",
      icon = icon("charging-station"),
      color = "purple"
    ))
  })

  ## 2.4. Emisiones
  CL_KmSubdataset <- reactive({
    datos <- req(rutasSubDataset())
    req(datos, nrow(datos) > 0)
    
    fac_esco <- dplyr::coalesce(CFG$factor_exp$semana_esco, 40)
    fac_gral <- dplyr::coalesce(CFG$factor_exp$semana_gral, 52)
    
    tabla_Km <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(
        Total_disRutaSem = sum(disRutaSem / 1000, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = SR_Tip_Ruta,
        values_from = Total_disRutaSem,
        values_fill = 0
      ) %>%
      rename(`Tipo de Vehículo` = SR_Veh_Aj_2)
    
    if (isTruthy(input$EscenarioKm) && input$EscenarioKm == "extras") {
      req(input$Kms_ad, CL_tabla_veh_data())
      
      tabla_adicional <- CL_tabla_veh_data() %>%
        mutate(across(where(is.numeric), ~ .x * input$Kms_ad)) %>%
        select(-matches("^total$", ignore.case = TRUE))
        
      tabla_Km <- tabla_Km %>%
        left_join(tabla_adicional, by = "Tipo de Vehículo", suffix = c("", "_extra")) %>%
        mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
    }

    tabla_Km <- tabla_Km %>%
      rowwise() %>%
      mutate(
        Km_escolares = sum(c_across(where(is.numeric) & !ends_with("_extra")), na.rm = TRUE),
        Km_Extras = if (any(endsWith(names(.), "_extra"))) {
          sum(c_across(ends_with("_extra")), na.rm = TRUE)
        } else { 0 }
      ) %>%
      ungroup() %>%
      select(`Tipo de Vehículo`, Km_escolares, Km_Extras) %>%
      mutate(across(where(is.numeric), ~ round(.x, 0)))
      
    tabla_Km <- tabla_Km %>%
      mutate(
        Km_escolares = Km_escolares * fac_esco,
        Km_Extras    = Km_Extras * fac_gral,
        Km_totales   = Km_escolares + Km_Extras
      )
    return(tabla_Km)
  })

  emisionesSubDataset <- reactive({
    tabla_Km <- req(CL_KmSubdataset())
    emisiones_yaml <- CFG$emisiones
    
    tabla_emisiones <- emisiones_yaml %>%
      dplyr::bind_rows(.id = "Tipo de Vehículo") %>%
      rename_with(~ toupper(.x), -`Tipo de Vehículo`) %>%
      pivot_longer(
        cols = -`Tipo de Vehículo`, 
        names_to = "Contaminante", 
        values_to = "Factor"
      ) %>%
      inner_join(tabla_Km, by = "Tipo de Vehículo") %>%
      mutate(
        Emisiones_Escolar = (Km_escolares * Factor) / 1000000,
        Emisiones_Extra   = (Km_Extras * Factor) / 1000000,
        Emisiones_Total   = Emisiones_Escolar + Emisiones_Extra
      ) %>%
      group_by(Contaminante) %>%
      summarise(
        Emisiones_Escolar = sum(Emisiones_Escolar, na.rm = TRUE),
        Emisiones_Extra   = sum(Emisiones_Extra, na.rm = TRUE),
        Emisiones_Total   = sum(Emisiones_Total, na.rm = TRUE),
        .groups = "drop"
      )
    return(tabla_emisiones)
  })

  output$CL_emisiones_plotOLD <- renderPlotly({
    data <- req(emisionesSubDataset())
    contaminantes_sel <- req(input$filtro_t_emision)
    
    data_filtrada <- data %>%
      mutate(
        Contaminante = toupper(as.character(Contaminante)),
        Emisiones_Escolar = ifelse(is.na(Emisiones_Escolar), 0, Emisiones_Escolar),
        Emisiones_Extra   = ifelse(is.na(Emisiones_Extra), 0, Emisiones_Extra)
      ) %>%
      filter(Contaminante %in% toupper(contaminantes_sel)) %>%
      mutate(Contaminante = factor(Contaminante, levels = toupper(contaminantes_sel)))
    
    req(nrow(data_filtrada) > 0)
    
    plot_ly(
      data = data_filtrada, 
      x = ~Contaminante, 
      y = ~Emisiones_Escolar, 
      name = 'Escolar', 
      type = 'bar',
      marker = list(color = '#1f77b4')
    ) %>%
      add_trace(
        y = ~Emisiones_Extra, 
        name = 'Extra', 
        marker = list(color = '#ff7f0e')
      ) %>%
      layout(
        separators = ",.",
        barmode = 'stack',
        xaxis = list(title = 'Contaminante', type = 'category'),
        yaxis = list(title = 'Emisiones evitadas (Toneladas)', tickformat = ',.1f'),
        legend = list(title = list(text = 'Tipo de Emisión')),
        hovermode = 'x unified'
      )
  })
  
  output$CL_emisionesCO2eq <- renderPlotly({
    data <- req(emisionesSubDataset())
    contaminantes_sel <- "CO2EQ"
    
    data_filtrada <- data %>%
      mutate(
        Contaminante = toupper(as.character(Contaminante)),
        Emisiones_Escolar = ifelse(is.na(Emisiones_Escolar), 0, Emisiones_Escolar),
        Emisiones_Extra   = ifelse(is.na(Emisiones_Extra), 0, Emisiones_Extra)
      ) %>%
      filter(Contaminante %in% toupper(contaminantes_sel)) %>%
      mutate(Contaminante = factor(Contaminante, levels = toupper(contaminantes_sel)))
    
    req(nrow(data_filtrada) > 0)
    
    plot_ly(
      data = data_filtrada, 
      x = ~Contaminante, 
      y = ~Emisiones_Escolar, 
      name = 'Escolar', 
      type = 'bar',
      marker = list(color = '#1f77b4')
    ) %>%
      add_trace(
        y = ~Emisiones_Extra, 
        name = 'Extra', 
        marker = list(color = '#ff7f0e')
      ) %>%
      layout(
        separators = ",.",
        barmode = 'stack',
        xaxis = list(title = 'Contaminante', type = 'category'),
        yaxis = list(title = 'Emisiones evitadas (Toneladas)', tickformat = ',.1f'),
        legend = list(title = list(text = 'Tipo de Emisión')),
        hovermode = 'x unified'
      )
  })
  
  output$CL_emisiones_plot <- renderPlotly({
    data <- req(emisionesSubDataset())
    contaminantes_sel <- req(input$filtro_t_emision)
    
    data_filtrada <- data %>%
      mutate(
        Contaminante = toupper(as.character(Contaminante)),
        Emisiones_Escolar = ifelse(is.na(Emisiones_Escolar), 0, Emisiones_Escolar),
        Emisiones_Extra   = ifelse(is.na(Emisiones_Extra), 0, Emisiones_Extra)
      ) %>%
      filter(Contaminante %in% toupper(contaminantes_sel)) %>%
      mutate(Contaminante = factor(Contaminante, levels = toupper(contaminantes_sel)))
    
    req(nrow(data_filtrada) > 0)
    
    plot_ly(
      data = data_filtrada, 
      x = ~Contaminante, 
      y = ~Emisiones_Escolar, 
      name = 'Escolar', 
      type = 'bar',
      marker = list(color = '#1f77b4')
    ) %>%
      add_trace(
        y = ~Emisiones_Extra, 
        name = 'Extra', 
        marker = list(color = '#ff7f0e')
      ) %>%
      layout(
        separators = ",.",
        barmode = 'stack',
        xaxis = list(title = 'Contaminante', type = 'category'),
        yaxis = list(title = 'Emisiones evitadas (Toneladas)', tickformat = ',.1f'),
        legend = list(title = list(text = 'Tipo de Emisión')),
        hovermode = 'x unified'
      )
  })
  

  output$CL_Km_ano_table <- renderDT({
    data <- req(CL_KmSubdataset())
    datatable(data,
              extensions = 'Buttons',
              options = list(
                pageLength = 7,
                dom = 'Bfrtip',
                buttons = list(
                  list(
                    extend = 'csv',
                    filename = 'km_anuales',
                    text = 'Descargar CSV',
                    fieldSeparator = ';'
                  )
                )
              ),
              rownames = FALSE
    ) %>%
      formatRound(
        columns = 2:ncol(data), 
        digits = 2, 
        interval = 3, 
        mark = ".", 
        dec.mark = ","
      )
  })

  output$CL_emisiones_table <- renderDT({
    data <- req(emisionesSubDataset())
    datatable(data,
              extensions = 'Buttons',
              options = list(
                pageLength = 7,
                dom = 'Bfrtip',
                buttons = list(
                  list(
                    extend = 'csv',
                    filename = 'Emisiones_evitadas_año',
                    text = 'Descargar CSV',
                    fieldSeparator = ';'
                  )
                )
              ),
              rownames = FALSE
    ) %>%
      formatRound(
        columns = 2:ncol(data), 
        digits = 2, 
        interval = 3, 
        mark = ".", 
        dec.mark = ","
      )
  })
  ## 3. Beneficiarios
  output$CL_tabla_beneficiarios_colegio <- DT::renderDT({
    data <- rutasSubDataset()
    
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(data.frame(Mensaje = "No hay datos disponibles")))
    }
    
    # Procesamiento de datos
    datos <- data %>%
      sf::st_drop_geometry() %>%
      filter(!is.na(SR_IED) & SR_IED != "") %>%
      group_by(SR_IED) %>%
      summarise(
        Beneficiarios = sum(SR_TotalEst, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Beneficiarios)) %>%
      rename(`Colegio` = SR_IED)
    
    # Fila de Total General
    fila_total <- tibble(
      `Colegio` = "TOTAL",
      Beneficiarios = sum(datos$Beneficiarios, na.rm = TRUE)
    )
    
    datos_final <- bind_rows(datos, fila_total)
    
    # Renderizado
    DT::datatable(
      datos_final,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'csv',
            filename = 'Beneficiarios_por_Colegio',
            text = 'Descargar CSV',
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none')
            )
          )
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = "Beneficiarios", digits = 0, interval = 3, mark = ".", dec.mark = ",") %>%
      DT::formatStyle(
        'Colegio',
        target = 'row',
        fontWeight = DT::styleEqual('TOTAL', 'bold'),
        backgroundColor = DT::styleEqual('TOTAL', '#f8fafc')
      )
  })
  # 3.2. Beneficiarios por tipo de ruta
  output$CL_tabla_beneficiarios_ruta_veh <- DT::renderDT({
    data <- rutasSubDataset()
    
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(data.frame(Mensaje = "No hay datos disponibles")))
    }
    
    # Limpieza y pivoteado de la matriz
    df_base <- data %>%
      sf::st_drop_geometry() %>%
      mutate(
        SR_Veh_Aj_2 = ifelse(is.na(SR_Veh_Aj_2) | trimws(SR_Veh_Aj_2) == "", "Sin Especificar", as.character(SR_Veh_Aj_2)),
        SR_Tip_Ruta = ifelse(is.na(SR_Tip_Ruta) | trimws(SR_Tip_Ruta) == "", "Sin Tipo", as.character(SR_Tip_Ruta))
      )
    
    df_pivot <- df_base %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(TotalEst = sum(SR_TotalEst, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = SR_Tip_Ruta, 
        values_from = TotalEst, 
        values_fill = 0
      )
    
    cols_rutas <- setdiff(names(df_pivot), "SR_Veh_Aj_2")
    
    # Totales Horizontales (Fila por fila)
    df_pivot <- df_pivot %>%
      mutate(TOTAL = rowSums(across(all_of(cols_rutas))))
    
    # Totales Verticales (Columna por columna)
    totales_verticales <- df_pivot %>%
      summarise(across(where(is.numeric), sum)) %>%
      mutate(SR_Veh_Aj_2 = "TOTAL")
    
    # Consolidación final
    datos_final <- bind_rows(df_pivot, totales_verticales) %>%
      rename(`Tipo de Vehículo` = SR_Veh_Aj_2)
    
    cols_numericas <- setdiff(names(datos_final), "Tipo de Vehículo")
    
    # Renderizado
    DT::datatable(
      datos_final,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'csv',
            filename = 'Beneficiarios_por_Vehiculo_y_Ruta',
            text = 'Descargar CSV',
            fieldSeparator = ";",
            action = DT::JS(
              "function (e, dt, node, config) {",
              "  var self = this;",
              "  var oldStart = dt.settings()[0]._iDisplayStart;",
              "  dt.one('preXhr', function (e, s, data) {",
              "    data.start = 0;",
              "    data.length = -1;",
              "  });",
              "  dt.one('draw', function (e, settings) {",
              "    $.fn.dataTable.ext.buttons.csvHtml5.action.call(self, e, dt, node, config);",
              "    dt.one('preXhr', function (e, s, data) {",
              "      data.start = oldStart;",
              "    });",
              "    dt.draw(false);",
              "  });",
              "  dt.draw();",
              "}"
            ),
            exportOptions = list(
              modifier = list(page = 'all', search = 'none')
            )
          )
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = cols_numericas, digits = 0, interval = 3, mark = ".", dec.mark = ",") %>%
      DT::formatStyle(
        'Tipo de Vehículo',
        target = 'row',
        fontWeight = DT::styleEqual('TOTAL', 'bold'),
        backgroundColor = DT::styleEqual('TOTAL', '#f8fafc')
      )
  })
}

##----------------------------------------------------------------------------##
## Ejecutar App
##----------------------------------------------------------------------------##
shinyApp(ui = ui, server = server)
