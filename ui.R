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
library(purrr)

##----------------------------------------------------------------------------##
## Configuración y Carga de Datos
##----------------------------------------------------------------------------##
setwd(here())
Sys.setlocale("LC_ALL", "en_US.UTF-8")

CFG <- config::get(file = "config.yml")

poligonosV2  <- readRDS("Assets/RDS/poligonosV2.rds")
rutasv2      <- readRDS("Assets/RDS/rutas.rds")

pat_ele_buff <- readRDS("Assets/RDS/P_Elec_buff.rds")
pat_ele_punt <- readRDS("Assets/RDS/P_Elec_punt.rds")
punt_ad_buff <- readRDS("Assets/RDS/P_Ad_buff.rds")
punt_ad_punt <- readRDS("Assets/RDS/P_Ad_punt.rds")
colegios_pun <- readRDS("Assets/RDS/colegios.rds")
#llaves_rutas <- read.csv2("Assets/csv/260901_Rutas_colegio3.csv")

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

# Paletas de color
factpal_patios <- colorFactor("Set3", domain = poligonosV2$Cluster)
paleta_tipo_ruta <- colorFactor("Set1", domain = rutasv2R1$SR_Tip_Ruta)
paleta_tipo_vehiculo <- colorFactor("Dark2", domain = rutasv2R1$SR_Veh_Aj_2)

# Iconos
colegio_icon <- makeAwesomeIcon(
  icon        = "graduation-cap",
  iconColor   = "white",
  markerColor = "red",
  library     = "fa"
)

## Script JS para exportación completa a CSV
js_csv_btn <- function(filename) {
  list(
    extend = 'csv',
    filename = filename,
    text = 'Descargar CSV',
    fieldSeparator = ";",
    bom = TRUE,
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
}

##----------------------------------------------------------------------------##
## Definición de Interfaz de Usuario (UI)
##----------------------------------------------------------------------------##
ui <- dashboardPage(
  title = "Dashboard para la electrificación de rutas escolares de Bogotá D.C.",
  
  header = dashboardHeader(
    title = tagList(
      span(class = "logo-lg", style = "font-weight: 800; letter-spacing: 0.5px; color: #ffffff;", "Electrificación de rutas escolares"),
      span(class = "logo-mini", style = "color: #ffffff; font-weight: 800;", "W")
    ),
    rightUi = userOutput("skin_dropdown")
  ),
  
  sidebar = dashboardSidebar(
    width = 240,
    minified = TRUE,
    collapsed = FALSE,
    sidebarMenu(
      id = "tab_seleccionada",
      menuItem("Diseñe el proyecto", tabName = "tab_mapa", icon = icon("drafting-compass")),
      menuItem("Explore las propuestas", tabName = "tab_mapa2", icon = icon("globe-americas")),
      menuItem("FAQ", tabName = "documentacion", icon = icon("book-open")),
      menuItem("Créditos", tabName = "creditos", icon = icon("award"))
    )
  ),
  
  controlbar = dashboardControlbar(id = "Controlbar", skinSelector()),
  
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700;800&display=swap');

        body, .content-wrapper, .right-side {
          font-family: 'Plus Jakarta Sans', sans-serif !important;
          background-color: #f8fafc !important;
          color: #0f172a;
        }

        .main-header .navbar, .main-header .logo {
          background-color: #0f172a !important;
          color: #ffffff !important;
        }

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

        .leaflet-bottom.leaflet-right {
          margin-bottom: 25px !important;
        }

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

        .analytics-sheet-light {
          margin-top: 24px;
          background: #ffffff;
          border-radius: 16px;
          padding: 24px;
          border: 1px solid #e2e8f0;
          box-shadow: 0 4px 12px rgba(0,0,0,0.03);
        }

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
      ## PESTAÑA 1: DISEÑO DE ZONAS (Réplica exacta de tab_mapa2)
      ##----------------------------------------------------------------------##
      tabItem(
        tabName = "tab_mapa",
        
        # Header y Exportación
        fluidRow(
          style = "margin-bottom: 12px; display: flex; align-items: center;",
          column(
            width = 8,
            h2("Plantee su proyecto", style = "margin: 0; font-weight: 800; letter-spacing: -0.5px; color: #0f172a;"),
            h6(em("Elija los hexágonos para crear su proyecto, abajo verá los resultados agregados para los hexágonos seleccionados",
                  style = "margin-top:30; font-weight:800; color:#0284c7; letter-spacing: 0.5px;"))
          ),
          column(
            width = 4,
            div(
              class = "no-print",
              style = "text-align: right;",
              actionButton(
                inputId = "btn_imprimir_t1",
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
          
          leafletOutput("mapa_interactivo"),
          
          # Panel Flotante Izquierdo: Métricas PCBE
          div(
            class = "glass-panel-light glass-floating-kpi no-print",
            h5("CUMPLIMIENTO PCBE", style = "margin-top:30; font-weight:800; color:#0284c7; letter-spacing: 0.5px;"),
            div(
              class = "kpi-metric-card",
              div(class = "kpi-title", "Beneficiarios Atendidos"),
              div(class = "kpi-value", uiOutput("ben_atendidos_t1"))
            ),
            div(
              class = "kpi-metric-card", style = "border-left-color: #16a34a;",
              div(class = "kpi-title", "Meta Política Pública"),
              div(class = "kpi-value", uiOutput("metaPCBE_t1"))
            ),
            div(
              style = "height: 180px; margin-top: 50px;",
              plotlyOutput("plotly_gauge_t1", height = "160px")
            )
          ),
          
          # Panel Flotante Derecho: Filtros
          div(
            class = "glass-panel-light glass-floating-controls no-print",
            h5("FILTROS DE MAPA", style = "margin-top:20; font-weight:800; color:#0284c7; letter-spacing: 0.5px;"),
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
                  DTOutput("T1_tabla_beneficiarios_colegio"),
                  hr(),
                  h4(em("Beneficiarios atendidos por tipo de ruta y vehículo", style = "font-weight:700; color:#0f172a;")),
                  br(),
                  DTOutput("T1_tabla_beneficiarios_ruta_veh")
                )
              )
            ),
          
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
                  DTOutput("T1_tabla_colegios_detalle"),
                  hr(),
                  h4(em("Listado de rutas a impactar", style = "font-weight:700; color:#0f172a;")),
                  DTOutput("T1_tabla_rutas_detalle")
                ),
                box(
                  width = 12,
                  title = "Horas y kilómetros",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  h4(em("Horas contratadas", style = "font-weight:700; color:#0f172a;")),
                  DTOutput("T1_tabla_horas"),
                  hr(),
                  h4(em("Kilómetros semanales", style = "font-weight:700; color:#0f172a;")),
                  DTOutput("T1_tabla_resumen_km")
                ),
                box(
                  width = 12,
                  title = "Rutas y vehículos",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  h4(em("Rutas", style = "font-weight:700; color:#0f172a;")),
                  DTOutput("T1_tabla_rutas"),
                  hr(),
                  h4(em("Vehículos", style = "font-weight:700; color:#0f172a;")),
                  DTOutput("T1_tabla_veh")
                )
              )
            ),
            
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
                      inputId  = "T1_EscenarioKm",
                      label    = "Escenario de Operación:",
                      choices  = c("Operación base (+26% vacío)" = "base", "Base más servicios adicionales" = "extras"),
                      selected = "base"
                    ),
                    conditionalPanel(
                      condition = 'input.T1_EscenarioKm == "extras"',
                      numericInput("T1_Kms_ad", "Km Adicionales / Semana:", value = 140, min = 0, max = 2100, step = 20)
                    ),
                    hr(),
                    radioButtons(
                      inputId  = "T1_demanda_tipo_calculo",
                      label    = "Método de Cálculo:",
                      choices  = c("Promedio diario" = "promedio", "Factor de carga" = "factor"),
                      selected = "promedio"
                    ),
                    conditionalPanel(
                      condition = "input.T1_demanda_tipo_calculo == 'factor'",
                      sliderInput("T1_demanda_factor_slider", "Días de recarga semanal:", min = 1, max = 7, value = 3, step = 1)
                    ),
                    sliderInput("T1_VentanaCarga", "Ventana de recarga (Horas):", min = 1.5, max = 12, value = 3, step = 0.5)
                  )
                ),
                column(
                  width = 9,
                  h4(em("Consumo de energía proyectado para los parámetros elegidos (kWh)", style = "font-weight:700; color:#0f172a;")),
                  plotlyOutput("T1_demanda_energetica_plot", height = "280px"),
                  br(),
                  DTOutput("T1_demanda_energetica"),
                  hr(),
                  h4(em("Patrones de Actividad de Entrada / Salida", style = "font-weight:700; color:#0f172a;")),
                  plotlyOutput("T1_horas_act", height = "240px"),
                  br(),
                  fluidRow(
                    column(4, div(class = "glass-panel-light", style="text-align:center;", h5("Energía Diaria"), uiOutput("T1_cons_diario"))),
                    column(4, div(class = "glass-panel-light", style="text-align:center;", h5("Potencia Total"), uiOutput("T1_pot_req"))),
                    column(4, div(class = "glass-panel-light", style="text-align:center;", h5("Cargadores Req."), uiOutput("T1_cargadores_req")))
                  )
                )
              )
            ),
            
            tabPanel(
              title = " Impacto Ambiental",
              br(),
              h4(em("Kilómetros Anuales Proyectados", style = "font-weight:700; color:#0f172a;")),
              DTOutput("T1_Km_ano_table"),
              hr(),
              fluidRow(
                column(
                  width = 7,
                  h4("Emisiones Evitadas por Contaminante (Ton/año)", style = "color:#16a34a; font-weight:700;"),
                  plotlyOutput("T1_emisiones_plot", height = "280px"),
                  selectInput(
                    inputId  = 'T1_filtro_t_emision',
                    label    = 'Filtrar Contaminantes:',
                    choices  = c("CO", "VOC", "NOX", "SOX", "PM25", "PM10"),
                    selected = c("CO", "VOC", "NOX", "SOX", "PM25", "PM10"),
                    multiple = TRUE
                  )
                ),
                column(
                  width = 5,
                  h4("Descarbonización CO2eq", style = "color:#16a34a; font-weight:700;"),
                  plotlyOutput("T1_emisionesCO2eq", height = "280px")
                )
              ),
              br(),
              DTOutput("T1_emisiones_table")
            )
          )
        )
      ),
      
      ##----------------------------------------------------------------------##
      ## PESTAÑA 2: EXPLORACIÓN & IMPACTO (Original - Intacta)
      ##----------------------------------------------------------------------##
      tabItem(
        tabName = "tab_mapa2",
        
        fluidRow(
          style = "margin-bottom: 12px; display: flex; align-items: center;",
          column(
            width = 8,
            h2("Exploración de propuestas", style = "margin: 0; font-weight: 800; letter-spacing: -0.5px; color: #0f172a;"),
            h6(em("Explore las zonas creadas para ver sus características",
                  style = "margin-top:30; font-weight:800; color:#0284c7; letter-spacing: 0.5px;"))
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
        
        div(
          class = "map-canvas-container",
          
          leafletOutput("mapa_clusteres"),
          
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
                )
              )
            ),
            
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
                  h4(em("Consumo de energía proyectado para los parámetros elegidos (kWh)", style = "font-weight:700; color:#0f172a;")),
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
  
  ##--------------------------------------------------------------------------##
  ## SECCIÓN 1: PESTAÑA TAB_MAPA (Diseño de Zonas)
  ##--------------------------------------------------------------------------##
  
  seleccionados <- reactiveVal(character(0))
  
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
        overlayGroups = c("Polígonos", "Polígonos Seleccionados", "Rutas Filtradas", "Colegios (DANE)"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })
  
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
  
  # Reactive Principal para Tab 1 (rutas_filtradas_reactivas)
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
    proxy <- leafletProxy("mapa_interactivo")
    proxy %>% 
      clearGroup("Rutas Filtradas") %>% 
      clearGroup("Colegios (DANE)") %>%
      clearControls()
    
    rutas_sub <- rutas_filtradas_reactivas()
    
    if (!is.null(rutas_sub) && nrow(rutas_sub) > 0) {
      cods_dane_rutas <- unique(na.omit(rutas_sub$Dane_IED2))
      colegios_sub <- colegios_pun[colegios_pun$COD_DANE %in% cods_dane_rutas, ]
      
      if (nrow(colegios_sub) > 0) {
        proxy %>%
          addAwesomeMarkers(
            data  = colegios_sub,
            icon  = colegio_icon,
            group = "Colegios (DANE)",
            popup = ~paste0("<b>Colegio: </b>", NOMBRE_INS, "<br><b>Código DANE: </b>", COD_DANE)
          )
      }
      
      var_color <- ifelse(!is.null(input$var_color_ruta) && nzchar(input$var_color_ruta), input$var_color_ruta, "SR_Tip_Ruta")
      
      if (var_color %in% names(rutas_sub)) {
        vec_color <- rutas_sub[[var_color]]
        valores_unicos <- sort(unique(na.omit(vec_color)))
        
        if (length(valores_unicos) > 0) {
          paleta <- if (var_color == "SR_Tip_Ruta") paleta_tipo_ruta else paleta_tipo_vehiculo
          titulo_leyenda <- if (identical(var_color, "SR_Tip_Ruta")) "Tipo de Ruta" else "Tipo de Vehículo"
          
          dist_km <- ifelse(
            is.na(rutas_sub$disRutaSem), 
            "N/A", 
            paste0(format(round(rutas_sub$disRutaSem / 1000, 2), big.mark = ".", decimal.mark = ","), " Km")
          )
          
          proxy %>%
            addPolylines(
              data        = rutas_sub,
              group       = "Rutas Filtradas",
              color       = paleta(vec_color),
              weight      = 3.5,
              opacity     = 0.85,
              popup       = ~paste0(
                "<b>Código de la ruta: </b>", ifelse(is.na(CodigoRuta), "N/A", CodigoRuta), "<br>",
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
  
  # KPI y Gauge Tab 1
  beneficiarios_atendidos_t1 <- reactive({
    data <- rutas_filtradas_reactivas()
    if (is.null(data) || nrow(data) == 0) return(0)
    sum(data$SR_TotalEst, na.rm = TRUE)
  })
  
  output$ben_atendidos_t1 <- renderUI({
    total_atendidos <- beneficiarios_atendidos_t1()
    span(format(total_atendidos, big.mark = ".", decimal.mark = ","))
  })
  
  output$metaPCBE_t1 <- renderUI({
    meta_val <- CFG$meta_pcbe$beneficiarios
    span(format(meta_val, big.mark = ".", decimal.mark = ","))
  })
  
  output$plotly_gauge_t1 <- renderPlotly({
    data <- rutas_filtradas_reactivas()
    meta <- CFG$meta_pcbe$beneficiarios
    
    if (is.null(data) || nrow(data) == 0) {
      cumplimiento <- 0
    } else {
      beneficiarios <- sum(data$SR_TotalEst, na.rm = TRUE)
      cumplimiento <- beneficiarios / meta * 100
    }
    
    plot_ly(
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
  })
  
  # Sub-pestaña Beneficiarios Tab 1
  output$T1_tabla_beneficiarios_colegio <- renderDT({
    data <- rutas_filtradas_reactivas()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    datos <- data %>%
      sf::st_drop_geometry() %>%
      group_by(SR_IED, SR_DaneIED) %>%
      summarise(Beneficiarios = sum(SR_TotalEst, na.rm = TRUE), .groups = "drop") %>%
      rename("Colegio" = SR_IED, "Código DANE" = SR_DaneIED) %>%
      janitor::adorn_totals(where = "row", fill = "-", na.rm = TRUE, name = "Total")
    
    datatable(
      datos,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('01_beneficiarios_colegio'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = "Beneficiarios", digits = 0, interval = 3, mark = ".", dec.mark = ",")
  })
  
  output$T1_tabla_beneficiarios_ruta_veh <- renderDT({
    datos_clus <- rutas_filtradas_reactivas()
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(TotalEst = sum(SR_TotalEst, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = SR_Tip_Ruta, values_from = TotalEst, values_fill = 0) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
    
    datatable(
      tabla_resumen,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('02_beneficiarios_ruta_vehiculo'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(tabla_resumen), digits = 0, interval = 3, mark = ".", dec.mark = ",")
  })
  
  # Sub-pestaña Operación Tab 1
  output$T1_tabla_colegios_detalle <- renderDT({
    data <- rutas_filtradas_reactivas()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos disponibles")))
    }
    
    datos <- data %>%
      sf::st_drop_geometry() %>%
      mutate(
        SR_Veh_Aj_2_clean = toupper(trimws(SR_Veh_Aj_2)),
        factor_veh = case_when(
          SR_Veh_Aj_2_clean == "BUS"                       ~ 1.5,
          SR_Veh_Aj_2_clean == "BUSETA"                    ~ 2.5,
          SR_Veh_Aj_2_clean %in% c("MICROBÚS", "MICROBUS") ~ 1.5,
          SR_Veh_Aj_2_clean == "VAN"                       ~ 1.0,
          SR_Veh_Aj_2_clean == "CAMIONETA"                 ~ 2.5,
          TRUE ~ 1.0
        )
      ) %>%
      group_by(SR_IED, SR_DaneIED) %>%
      summarise(
        `Horas Semanales` = sum(disHorasSem, na.rm = TRUE),
        `Cantidad Rutas`  = n(),
        `Beneficiarios`   = sum(SR_TotalEst, na.rm = TRUE),
        `Vehículos`       = sum(tapply(factor_veh, SR_Veh_Aj_2_clean, function(f) ceiling(length(f) / f[1])), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(`Colegio` = SR_IED, `Código DANE` = SR_DaneIED) %>%
      mutate(`Código DANE` = as.character(`Código DANE`))
    
    datatable(
      datos,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('Listado_rutas_agrupado_colegio'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Horas Semanales"), digits = 2, interval = 3, mark = ".", dec.mark = ",") %>%
      formatRound(columns = c("Cantidad Rutas", "Beneficiarios", "Vehículos"), digits = 0, interval = 3, mark = ".", dec.mark = ",")
  })
  
  output$T1_tabla_rutas_detalle <- renderDT({
    data <- rutas_filtradas_reactivas()
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
        'Colegio' = SR_IED,
        'ColegioDane' = SR_DaneIED
      )
    datatable(
      datos,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('Listado_rutas'))
      ),
      rownames = FALSE
    )
  })
  
  output$T1_tabla_horas <- renderDT({
    datos_clus <- rutas_filtradas_reactivas()
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(TotalHoras = sum(disHorasSem, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = SR_Tip_Ruta, values_from = TotalHoras, values_fill = 0) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
    
    datatable(
      tabla_resumen,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('04_Horas_contratadas'))
      ),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 2:ncol(tabla_resumen), digits = 2, interval = 3, mark = ".", dec.mark = ",")
  })
  
  output$T1_tabla_resumen_km <- renderDT({
    datos_clus <- rutas_filtradas_reactivas()
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Total_KM = sum(disRutaSem, na.rm = TRUE)/1000, .groups = "drop") %>%
      pivot_wider(names_from = SR_Tip_Ruta, values_from = Total_KM, values_fill = 0) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
    
    datatable(
      tabla_resumen,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('01_dist_km_semanal'))
      ),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 2:ncol(tabla_resumen), digits = 2, interval = 3, mark = ".", dec.mark = ",")
  })
  
  output$T1_tabla_rutas <- renderDT({
    datos_clus <- rutas_filtradas_reactivas()
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      pivot_wider(names_from = SR_Tip_Ruta, values_from = Cantidad, values_fill = 0) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
    
    datatable(
      tabla_resumen,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('01_conteo_rutas'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(tabla_resumen), digits = 0, interval = 3, mark = ".", dec.mark = ",")
  })
  
  T1_tabla_veh_data <- reactive({
    datos <- rutas_filtradas_reactivas()
    if (is.null(datos) || nrow(datos) == 0) return(data.frame())
    
    tabla_resumen <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      pivot_wider(names_from = SR_Tip_Ruta, values_from = Cantidad, values_fill = 0) %>% 
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
  
  output$T1_tabla_veh <- renderDT({
    tabla_resumen <- T1_tabla_veh_data()
    if (nrow(tabla_resumen) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    datatable(
      tabla_resumen,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('02_calculo_vehiculos_factor'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(tabla_resumen), digits = 0, interval = 3, mark = ".", dec.mark = ",")
  })
  
  # Sub-pestaña Energía Tab 1
  T1ConsSubDataset <- reactive({
    datos <- rutas_filtradas_reactivas()
    factor_perdidas <- 1 + dplyr::coalesce(CFG$modif_consumo$perdidas, 0)
    factor_kmvac    <- 1 + dplyr::coalesce(CFG$modif_km_vacio$km_vacio_perc, 0)
    
    req(datos, nrow(datos) > 0)
    
    tabla_Km <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Total_disRutaSem = sum(disRutaSem/1000, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = SR_Tip_Ruta, values_from = Total_disRutaSem, values_fill = 0) %>%
      rename(`Tipo de Vehículo` = SR_Veh_Aj_2) %>%
      mutate(across(where(is.numeric), ~ .x * factor_kmvac))
    
    cols_rutas <- setdiff(names(tabla_Km), "Tipo de Vehículo")
    
    if (isTruthy(input$T1_EscenarioKm) && input$T1_EscenarioKm == "extras") {
      req(input$T1_Kms_ad, T1_tabla_veh_data())
      tabla_adicional <- T1_tabla_veh_data() %>%
        mutate(
          factor_veh = purrr::map_dbl(`Tipo de Vehículo`, ~ dplyr::coalesce(CFG$factor_consumo[[as.character(.x)]], 1.0)),
          across(where(is.numeric) & !matches("factor_veh"), ~ .x * (input$T1_Kms_ad * factor_perdidas * factor_veh))
        )
      tabla_Km <- tabla_Km %>%
        left_join(tabla_adicional, by = "Tipo de Vehículo", suffix = c("", "_extra")) %>%
        mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
    }
    
    tabla_demanda <- tabla_Km %>%
      rowwise() %>%
      mutate(
        clave_vehiculo = tolower(gsub("[ -]", "_", `Tipo de Vehículo`)),
        factor_veh = purrr::map_dbl(`Tipo de Vehículo`, ~ dplyr::coalesce(CFG$factor_consumo[[as.character(.x)]], 1.0))
      ) %>%
      mutate(across(all_of(cols_rutas), ~ .x * factor_veh * (factor_perdidas))) %>%
      ungroup() %>%
      select(-clave_vehiculo, -factor_veh)
    
    return(tabla_demanda)
  })
  
  output$T1_demanda_energetica <- renderDT({
    df_demanda <- req(T1ConsSubDataset())
    df_demanda <- df_demanda %>% select(-any_of(c("Total")))
    
    if (nrow(df_demanda) == 0) {
      return(datatable(data.frame("Estado" = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_con_totales <- df_demanda %>%
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total") %>%
      dplyr::rename("Consumo total (kWh)" = Total)
    
    datatable(
      tabla_con_totales,
      extensions = 'Buttons',
      rownames   = FALSE,
      class      = 'cell-border stripe hover compact',
      options    = list(
        pageLength = 10,
        scrollX    = TRUE,
        dom        = 'Bfrtip',
        buttons    = list(js_csv_btn(paste0('02_demanda_energetica_', Sys.Date())))
      )
    ) %>% formatCurrency(
      columns  = setdiff(names(tabla_con_totales), "Tipo de Vehículo"),
      currency = "", interval = 3, mark = ".", dec.mark = ",", digits = 0
    )
  })
  
  output$T1_demanda_energetica_plot <- renderPlotly({
    df_demanda <- req(T1ConsSubDataset())
    if (nrow(df_demanda) == 0) {
      return(plotly_empty(type = "scatter", mode = "text") %>% layout(title = "No hay datos disponibles"))
    }
    
    df_long <- df_demanda %>%
      tidyr::pivot_longer(
        cols      = setdiff(names(df_demanda), "Tipo de Vehículo"),
        names_to  = "Tipo_Ruta",
        values_to = "Consumo_kWh"
      ) %>%
      dplyr::filter(Consumo_kWh > 0) %>%
      dplyr::mutate(Consumo_fmt = format(round(Consumo_kWh), big.mark = ".", decimal.mark = ","))
    
    plot_ly(
      data      = df_long,
      x         = ~`Tipo de Vehículo`,
      y         = ~Consumo_kWh,
      color     = ~Tipo_Ruta,
      type      = "bar",
      text      = ~paste0("<b>Vehículo:</b> ", `Tipo de Vehículo`, "<br><b>Tipo de Ruta:</b> ", Tipo_Ruta, "<br><b>Consumo:</b> ", Consumo_fmt, " kWh"),
      hoverinfo = "text"
    ) %>%
      layout(
        separators = ",.",
        barmode = "stack",
        xaxis = list(title = ""),
        yaxis = list(title = "Consumo (kWh)"),
        legend = list(orientation = "h", x = 0, y = 1.15)
      )
  })
  
  output$T1_horas_act <- renderPlotly({
    data <- req(rutas_filtradas_reactivas())
    cols <- c("SR_H_Ini_R1", "SR_H_Ini_Jornada", "SR_H_Fin_Jornada", "SR_H_Ini_R2")
    fecha_ref <- Sys.Date()
    
    grid_intervalos <- data.frame(
      intervalo = seq(from = as.POSIXct(paste(fecha_ref, "04:00:00")), to = as.POSIXct(paste(fecha_ref, "20:00:00")), by = "15 mins")
    ) %>% mutate(intervalo_texto = format(intervalo, "%H:%M"))
    
    data_proc <- data %>%
      mutate(across(all_of(cols), ~ ifelse(.x == "N/A" | is.na(.x), NA, .x))) %>%
      mutate(across(all_of(cols), ~ {
        time_obj <- lubridate::parse_date_time(.x, orders = c("HM", "HMS"))
        lubridate::floor_date(time_obj, "15 mins")
      }))
    
    resumen <- data_proc %>%
      pivot_longer(cols = all_of(cols), names_to = "Variable", values_to = "Hora_Redondeada") %>%
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
        labels = c("Hora inicio ida", "Hora inicio clases", "Hora fin clases", "Hora inicio regreso")
      ))
    
    plot_ly(data = datos_grafica, x = ~Hora, y = ~Conteo, color = ~Variable, type = "bar") %>%
      layout(barmode = "group", xaxis = list(title = "Intervalo (15 min)", tickangle = -45), yaxis = list(title = "Servicios"))
  })
  
  output$T1_cons_diario <- renderUI({
    consData <- req(T1ConsSubDataset(), input$T1_demanda_tipo_calculo)
    total_energia <- consData %>% select(-any_of("Total")) %>% select(where(is.numeric)) %>% as.matrix() %>% sum(na.rm = TRUE)
    divisor <- if (input$T1_demanda_tipo_calculo == "promedio") 5 else { req(input$T1_demanda_factor_slider); input$T1_demanda_factor_slider }
    resultado <- total_energia / divisor
    total_fmt <- format(round(resultado, 2), big.mark = ".", decimal.mark = ",")
    valueBox(value = paste0(total_fmt, " kWh"), subtitle = "Energía diaria", icon = icon("bolt"), color = "blue", width = NULL)
  })
  
  output$T1_pot_req <- renderUI({
    consData <- req(T1ConsSubDataset(), input$T1_demanda_tipo_calculo, input$T1_VentanaCarga)
    total_energia <- consData %>% select(-any_of("Total")) %>% select(where(is.numeric)) %>% as.matrix() %>% sum(na.rm = TRUE)
    divisor <- if (input$T1_demanda_tipo_calculo == "promedio") 5 else { req(input$T1_demanda_factor_slider); input$T1_demanda_factor_slider }
    resultado <- total_energia / divisor / input$T1_VentanaCarga
    total_fmt <- format(round(resultado, 2), big.mark = ".", decimal.mark = ",")
    valueBox(value = paste0(total_fmt, " kW"), subtitle = "Potencia diaria", icon = icon("plug"), color = "green", width = NULL)
  })
  
  output$T1_cargadores_req <- renderUI({
    consData <- req(T1ConsSubDataset(), input$T1_demanda_tipo_calculo, input$T1_VentanaCarga)
    total_energia <- consData %>% select(-any_of("Total")) %>% select(where(is.numeric)) %>% as.matrix() %>% sum(na.rm = TRUE)
    divisor <- if (input$T1_demanda_tipo_calculo == "promedio") 5 else { req(input$T1_demanda_factor_slider); input$T1_demanda_factor_slider }
    resultado <- total_energia / divisor / input$T1_VentanaCarga
    cargadores <- ceiling(round(resultado, 2) / 150)
    valueBox(value = paste0(cargadores), subtitle = "Cargadores 150 kW", icon = icon("charging-station"), color = "purple", width = NULL)
  })
  
  # Sub-pestaña Impacto Ambiental Tab 1
  T1_KmSubdataset <- reactive({
    datos <- req(rutas_filtradas_reactivas())
    req(datos, nrow(datos) > 0)
    
    fac_esco <- dplyr::coalesce(CFG$factor_exp$semana_esco, 40)
    fac_gral <- dplyr::coalesce(CFG$factor_exp$semana_gral, 52)
    
    tabla_Km <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Total_disRutaSem = sum(disRutaSem / 1000, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = SR_Tip_Ruta, values_from = Total_disRutaSem, values_fill = 0) %>%
      rename(`Tipo de Vehículo` = SR_Veh_Aj_2)
    
    if (isTruthy(input$T1_EscenarioKm) && input$T1_EscenarioKm == "extras") {
      req(input$T1_Kms_ad, T1_tabla_veh_data())
      tabla_adicional <- T1_tabla_veh_data() %>%
        mutate(across(where(is.numeric), ~ .x * input$T1_Kms_ad)) %>%
        select(-matches("^total$", ignore.case = TRUE))
      tabla_Km <- tabla_Km %>%
        left_join(tabla_adicional, by = "Tipo de Vehículo", suffix = c("", "_extra")) %>%
        mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
    }
    
    tabla_Km <- tabla_Km %>%
      rowwise() %>%
      mutate(
        Km_escolares = sum(c_across(where(is.numeric) & !ends_with("_extra")), na.rm = TRUE),
        Km_Extras = if (any(endsWith(names(.), "_extra"))) sum(c_across(ends_with("_extra")), na.rm = TRUE) else 0
      ) %>%
      ungroup() %>%
      select(`Tipo de Vehículo`, Km_escolares, Km_Extras) %>%
      mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
      mutate(
        Km_escolares = Km_escolares * fac_esco,
        Km_Extras    = Km_Extras * fac_gral,
        Km_totales   = Km_escolares + Km_Extras
      )
    return(tabla_Km)
  })
  
  T1_emisionesSubDataset <- reactive({
    tabla_Km <- req(T1_KmSubdataset())
    emisiones_yaml <- CFG$emisiones
    
    tabla_emisiones <- emisiones_yaml %>%
      dplyr::bind_rows(.id = "Tipo de Vehículo") %>%
      rename_with(~ toupper(.x), -`Tipo de Vehículo`) %>%
      pivot_longer(cols = -`Tipo de Vehículo`, names_to = "Contaminante", values_to = "Factor") %>%
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
  
  output$T1_emisionesCO2eq <- renderPlotly({
    data <- req(T1_emisionesSubDataset())
    data_filtrada <- data %>%
      mutate(Contaminante = toupper(as.character(Contaminante))) %>%
      filter(Contaminante == "CO2EQ")
    
    req(nrow(data_filtrada) > 0)
    plot_ly(data = data_filtrada, x = ~Contaminante, y = ~Emisiones_Escolar, name = 'Escolar', type = 'bar') %>%
      add_trace(y = ~Emisiones_Extra, name = 'Extra') %>%
      layout(separators = ",.", barmode = 'stack', yaxis = list(title = 'Toneladas', tickformat = ',.1f'))
  })
  
  output$T1_emisiones_plot <- renderPlotly({
    data <- req(T1_emisionesSubDataset())
    contaminantes_sel <- req(input$T1_filtro_t_emision)
    
    data_filtrada <- data %>%
      mutate(Contaminante = toupper(as.character(Contaminante))) %>%
      filter(Contaminante %in% toupper(contaminantes_sel))
    
    req(nrow(data_filtrada) > 0)
    plot_ly(data = data_filtrada, x = ~Contaminante, y = ~Emisiones_Escolar, name = 'Escolar', type = 'bar') %>%
      add_trace(y = ~Emisiones_Extra, name = 'Extra') %>%
      layout(separators = ",.", barmode = 'stack', yaxis = list(title = 'Toneladas', tickformat = ',.1f'))
  })
  
  output$T1_Km_ano_table <- renderDT({
    data <- req(T1_KmSubdataset())
    datatable(
      data,
      extensions = 'Buttons',
      options = list(pageLength = 7, dom = 'Bfrtip', buttons = list(js_csv_btn('01_km_anuales'))),
      rownames = FALSE
    ) %>% formatRound(columns = 2:ncol(data), digits = 0, interval = 3, mark = ".", dec.mark = ",")
  })
  
  output$T1_emisiones_table <- renderDT({
    data <- req(T1_emisionesSubDataset())
    datatable(
      data,
      extensions = 'Buttons',
      options = list(pageLength = 7, dom = 'Bfrtip', buttons = list(js_csv_btn('02_emisiones_evitadas'))),
      rownames = FALSE
    ) %>% formatRound(columns = c("Emisiones_Escolar", "Emisiones_Extra", "Emisiones_Total"), digits = 2, interval = 3, mark = ".", dec.mark = ",")
  })
  

  ##--------------------------------------------------------------------------##
  ## SECCIÓN 2: PESTAÑA TAB_MAPA2 (Exploración & Impacto - Código Original)
  ##--------------------------------------------------------------------------##
  
  output$mapa_clusteres <- renderLeaflet({
    datos <- poligonos_filtrados()
    bbox  <- sf::st_bbox(poligonosV2)
    
    leaflet(datos, options = leafletOptions(minZoom = 10, maxZoom = 18)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setMaxBounds(
        lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]),
        lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"])
      ) %>%
      addPolygons(
        layerId = ~id, fillColor = ~factpal_patios(Cluster), fillOpacity = 0.5,
        color = "#f7f7f7", weight = 1.5, group = "Zonas hexagonales",
        label = ~paste("Zona:", id, "| Cluster:", Cluster)
      ) %>%
      addLegend(pal = factpal_patios, values = ~Cluster, title = "Grupo de rutas", position = "bottomright") %>%
      addLayersControl(
        overlayGroups = c("Zonas hexagonales", "Rutas Clúster", "Colegios (DANE)"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    proxy <- leafletProxy("mapa_clusteres")
    poligonos_sub <- poligonos_filtrados()
    colegios <- colegios_pun
    rutas_clus <- rutasSubDataset()
    
    proxy %>% 
      clearGroup("Zonas hexagonales") %>% 
      clearGroup("Rutas Clúster") %>% 
      clearGroup("Colegios (DANE)") %>% 
      clearControls()
    
    if (nrow(poligonos_sub) > 0) {
      proxy %>%
        addPolygons(
          data = poligonos_sub, layerId = ~id, fillColor = ~factpal_patios(Cluster),
          fillOpacity = 0.5, color = "#f7f7f7", weight = 1.5, group = "Zonas hexagonales",
          label = ~paste("Zona:", id, "| Cluster:", Cluster)
        ) %>%
        addLegend(pal = factpal_patios, values = poligonos_sub$Cluster, title = "Grupo de rutas", position = "bottomright")
    }
    
    if (!is.null(rutas_clus) && nrow(rutas_clus) > 0 && !is.null(colegios)) {
      cods_dane_rutas <- unique(na.omit(rutas_clus$Dane_IED2))
      colegios_sub <- colegios[colegios$COD_DANE %in% cods_dane_rutas, ]
      
      if (nrow(colegios_sub) > 0) {
        proxy %>%
          addAwesomeMarkers(
            data = colegios_sub, icon = colegio_icon, group = "Colegios (DANE)",
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
            paste0(format(round(rutas_clus$disRutaSem / 1000, 2), big.mark = ".", decimal.mark = ","), " Km")
          )
          
          proxy %>%
            addPolylines(
              data = rutas_clus, group = "Rutas Clúster", color = paleta(vec_color),
              weight = 3.5, opacity = 0.85,
              popup = ~paste0(
                "<b>Código de la ruta: </b>", ifelse(is.na(CodigoRuta), "N/A", CodigoRuta), "<br>",
                "<b>Tipo de Ruta: </b>", ifelse(is.na(SR_Tip_Ruta), "N/A", SR_Tip_Ruta), "<br>",
                "<b>Tipo Vehículo: </b>", ifelse(is.na(SR_Veh_Aj_2), "N/A", SR_Veh_Aj_2), "<br>",
                "<b>Distancia semanal: </b>", dist_km
              )
            ) %>%
            addLegend(position = "bottomleft", pal = paleta, values = vec_color, title = titulo_leyenda, opacity = 0.9)
        }
      }
    }
  })
  
  poligonos_filtrados <- reactive({
    if (is.null(input$filtro_cluster) || length(input$filtro_cluster) == 0) return(poligonosV2[0, ])
    if ("Todos" %in% input$filtro_cluster) return(poligonosV2)
    return(poligonosV2[poligonosV2$Cluster %in% input$filtro_cluster, ])
  })
  
  rutasSubDataset <- reactive({
    req(poligonos_filtrados())
    if (nrow(poligonos_filtrados()) == 0) return(rutasv2R1[0, ])
    
    ids_presentes <- poligonos_filtrados()$id
    datos_filtrados <- rutasv2R1 %>% filter(Id_Hexagono %in% ids_presentes)
    
    if (!is.null(input$filtro_tipo_ruta_clus) && !"Todos" %in% input$filtro_tipo_ruta_clus) {
      datos_filtrados <- datos_filtrados %>% filter(SR_Tip_Ruta %in% input$filtro_tipo_ruta_clus)
    }
    
    if (!is.null(input$filtro_tipo_veh_clus) && !"Todos" %in% input$filtro_tipo_veh_clus) {
      datos_filtrados <- datos_filtrados %>% filter(SR_Veh_Aj_2 %in% input$filtro_tipo_veh_clus)
    }
    
    if (inherits(datos_filtrados, "sf") && nrow(datos_filtrados) > 0) {
      datos_filtrados <- datos_filtrados %>% filter(!st_is_empty(.)) %>% sf::st_make_valid()
    }
    
    return(datos_filtrados)
  })
  
  output$CL_tabla_beneficiarios_colegio <- renderDT({
    data <- rutasSubDataset()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    datos <- data %>%
      sf::st_drop_geometry() %>%
      group_by(SR_IED, SR_DaneIED) %>%
      summarise(Beneficiarios = sum(SR_TotalEst, na.rm = TRUE), .groups = "drop") %>%
      rename("Colegio" = SR_IED, "Código DANE" = SR_DaneIED) %>%
      janitor::adorn_totals(where = "row", fill = "-", na.rm = TRUE, name = "Total")
    
    datatable(
      datos,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('01_beneficiarios_colegio'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = "Beneficiarios", digits = 0, interval = 3, mark = ".", dec.mark = ",")
  })
  
  output$CL_tabla_beneficiarios_ruta_veh <- renderDT({
    datos_clus <- rutasSubDataset()
    if (is.null(datos_clus) || nrow(datos_clus) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_resumen <- datos_clus %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(TotalEst = sum(SR_TotalEst, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = SR_Tip_Ruta, values_from = TotalEst, values_fill = 0) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
    
    datatable(
      tabla_resumen,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('02_beneficiarios_ruta_vehiculo'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(tabla_resumen), digits = 0, interval = 3, mark = ".", dec.mark = ",")
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
      pivot_wider(names_from = SR_Tip_Ruta, values_from = Total_KM, values_fill = 0) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
    
    datatable(
      tabla_resumen,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('01_dist_km_semanal'))
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
      pivot_wider(names_from = SR_Tip_Ruta, values_from = Cantidad, values_fill = 0) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
    
    datatable(
      tabla_resumen,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('01_conteo_rutas'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(tabla_resumen), digits = 0, interval = 3, mark = ".", dec.mark = ",")
  })
  
  CL_tabla_veh_data <- reactive({
    datos <- rutasSubDataset()
    if (is.null(datos) || nrow(datos) == 0) return(data.frame())
    
    tabla_resumen <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      pivot_wider(names_from = SR_Tip_Ruta, values_from = Cantidad, values_fill = 0) %>% 
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
  
  output$CL_tabla_veh <- renderDT({
    tabla_resumen <- CL_tabla_veh_data()
    if (nrow(tabla_resumen) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    datatable(
      tabla_resumen,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('02_calculo_vehiculos_factor'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(tabla_resumen), digits = 0, interval = 3, mark = ".", dec.mark = ",")
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
        'Colegio' = SR_IED,
        'ColegioDane' = SR_DaneIED
      )
    datatable(
      datos,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('Listado_rutas'))
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
          SR_Veh_Aj_2_clean == "BUS"                       ~ 1.5,
          SR_Veh_Aj_2_clean == "BUSETA"                    ~ 2.5,
          SR_Veh_Aj_2_clean %in% c("MICROBÚS", "MICROBUS") ~ 1.5,
          SR_Veh_Aj_2_clean == "VAN"                       ~ 1.0,
          SR_Veh_Aj_2_clean == "CAMIONETA"                 ~ 2.5,
          TRUE ~ 1.0
        )
      ) %>%
      group_by(SR_IED, SR_DaneIED) %>%
      summarise(
        `Horas Semanales` = sum(disHorasSem, na.rm = TRUE),
        `Cantidad Rutas`  = n(),
        `Beneficiarios`   = sum(SR_TotalEst, na.rm = TRUE),
        `Vehículos`       = sum(tapply(factor_veh, SR_Veh_Aj_2_clean, function(f) ceiling(length(f) / f[1])), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(`Colegio` = SR_IED, `Código DANE` = SR_DaneIED) %>%
      mutate(`Código DANE` = as.character(`Código DANE`))
    
    datatable(
      datos,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('Listado_rutas_agrupado_colegio'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Horas Semanales"), digits = 2, interval = 3, mark = ".", dec.mark = ",") %>%
      formatRound(columns = c("Cantidad Rutas", "Beneficiarios", "Vehículos"), digits = 0, interval = 3, mark = ".", dec.mark = ",")
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
      pivot_wider(names_from = SR_Tip_Ruta, values_from = TotalHoras, values_fill = 0) %>% 
      rename("Tipo de Vehículo" = SR_Veh_Aj_2) %>% 
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
    
    datatable(
      tabla_resumen,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(js_csv_btn('04_Horas_contratadas'))
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
  
  output$ben_atendidos <- renderUI({
    total_atendidos <- beneficiarios_atendidos()
    span(format(total_atendidos, big.mark = ".", decimal.mark = ","))
  })
  
  output$metaPCBE <- renderUI({
    meta_val <- CFG$meta_pcbe$beneficiarios
    span(format(meta_val, big.mark = ".", decimal.mark = ","))
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
    
    plot_ly(
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
  })
  
  CLConsSubDataset <- reactive({
    datos <- rutasSubDataset()
    factor_perdidas <- 1 + dplyr::coalesce(CFG$modif_consumo$perdidas, 0)
    factor_kmvac    <- 1 + dplyr::coalesce(CFG$modif_km_vacio$km_vacio_perc, 0)
    
    req(datos, nrow(datos) > 0)
    
    tabla_Km <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Total_disRutaSem = sum(disRutaSem/1000, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = SR_Tip_Ruta, values_from = Total_disRutaSem, values_fill = 0) %>%
      rename(`Tipo de Vehículo` = SR_Veh_Aj_2) %>%
      mutate(across(where(is.numeric), ~ .x * factor_kmvac))
    
    cols_rutas <- setdiff(names(tabla_Km), "Tipo de Vehículo")
    
    if (isTruthy(input$EscenarioKm) && input$EscenarioKm == "extras") {
      req(input$Kms_ad, CL_tabla_veh_data())
      tabla_adicional <- CL_tabla_veh_data() %>%
        mutate(
          factor_veh = purrr::map_dbl(`Tipo de Vehículo`, ~ dplyr::coalesce(CFG$factor_consumo[[as.character(.x)]], 1.0)),
          across(where(is.numeric) & !matches("factor_veh"), ~ .x * (input$Kms_ad * factor_perdidas * factor_veh))
        )
      tabla_Km <- tabla_Km %>%
        left_join(tabla_adicional, by = "Tipo de Vehículo", suffix = c("", "_extra")) %>%
        mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
    }
    
    tabla_demanda <- tabla_Km %>%
      rowwise() %>%
      mutate(
        clave_vehiculo = tolower(gsub("[ -]", "_", `Tipo de Vehículo`)),
        factor_veh = purrr::map_dbl(`Tipo de Vehículo`, ~ dplyr::coalesce(CFG$factor_consumo[[as.character(.x)]], 1.0))
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
      return(datatable(data.frame("Estado" = "No hay datos para la combinación de filtros seleccionada.")))
    }
    
    tabla_con_totales <- df_demanda %>%
      janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total") %>%
      dplyr::rename("Consumo total (kWh)" = Total)
    
    datatable(
      tabla_con_totales,
      extensions = 'Buttons',
      rownames   = FALSE,
      class      = 'cell-border stripe hover compact',
      options    = list(
        pageLength = 10,
        scrollX    = TRUE,
        dom        = 'Bfrtip',
        buttons    = list(js_csv_btn(paste0('02_demanda_energetica_', Sys.Date())))
      )
    ) %>% formatCurrency(
      columns  = setdiff(names(tabla_con_totales), "Tipo de Vehículo"),
      currency = "", interval = 3, mark = ".", dec.mark = ",", digits = 0
    )
  })
  
  output$CL_demanda_energetica_plot <- renderPlotly({
    df_demanda <- req(CLConsSubDataset())
    if (nrow(df_demanda) == 0) {
      return(plotly_empty(type = "scatter", mode = "text") %>% layout(title = "No hay datos disponibles"))
    }
    
    df_long <- df_demanda %>%
      tidyr::pivot_longer(
        cols      = setdiff(names(df_demanda), "Tipo de Vehículo"),
        names_to  = "Tipo_Ruta",
        values_to = "Consumo_kWh"
      ) %>%
      dplyr::filter(Consumo_kWh > 0) %>%
      dplyr::mutate(Consumo_fmt = format(round(Consumo_kWh), big.mark = ".", decimal.mark = ","))
    
    plot_ly(
      data      = df_long,
      x         = ~`Tipo de Vehículo`,
      y         = ~Consumo_kWh,
      color     = ~Tipo_Ruta,
      type      = "bar",
      text      = ~paste0("<b>Vehículo:</b> ", `Tipo de Vehículo`, "<br><b>Tipo de Ruta:</b> ", Tipo_Ruta, "<br><b>Consumo:</b> ", Consumo_fmt, " kWh"),
      hoverinfo = "text"
    ) %>%
      layout(
        separators = ",.",
        barmode = "stack",
        xaxis = list(title = ""),
        yaxis = list(title = "Consumo (kWh)"),
        legend = list(orientation = "h", x = 0, y = 1.15)
      )
  })
  
  output$CL_horas_act <- renderPlotly({
    data <- req(rutasSubDataset())
    cols <- c("SR_H_Ini_R1", "SR_H_Ini_Jornada", "SR_H_Fin_Jornada", "SR_H_Ini_R2")
    fecha_ref <- Sys.Date()
    
    grid_intervalos <- data.frame(
      intervalo = seq(from = as.POSIXct(paste(fecha_ref, "04:00:00")), to = as.POSIXct(paste(fecha_ref, "20:00:00")), by = "15 mins")
    ) %>% mutate(intervalo_texto = format(intervalo, "%H:%M"))
    
    data_proc <- data %>%
      mutate(across(all_of(cols), ~ ifelse(.x == "N/A" | is.na(.x), NA, .x))) %>%
      mutate(across(all_of(cols), ~ {
        time_obj <- lubridate::parse_date_time(.x, orders = c("HM", "HMS"))
        lubridate::floor_date(time_obj, "15 mins")
      }))
    
    resumen <- data_proc %>%
      pivot_longer(cols = all_of(cols), names_to = "Variable", values_to = "Hora_Redondeada") %>%
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
        labels = c("Hora inicio ida", "Hora inicio clases", "Hora fin clases", "Hora inicio regreso")
      ))
    
    plot_ly(data = datos_grafica, x = ~Hora, y = ~Conteo, color = ~Variable, type = "bar") %>%
      layout(barmode = "group", xaxis = list(title = "Intervalo (15 min)", tickangle = -45), yaxis = list(title = "Servicios"))
  })
  
  output$CL_cons_diario <- renderUI({
    consData <- req(CLConsSubDataset(), input$demanda_tipo_calculo)
    total_energia <- consData %>% select(-any_of("Total")) %>% select(where(is.numeric)) %>% as.matrix() %>% sum(na.rm = TRUE)
    divisor <- if (input$demanda_tipo_calculo == "promedio") 5 else { req(input$demanda_factor_slider); input$demanda_factor_slider }
    resultado <- total_energia / divisor
    total_fmt <- format(round(resultado, 2), big.mark = ".", decimal.mark = ",")
    valueBox(value = paste0(total_fmt, " kWh"), subtitle = "Energía diaria", icon = icon("bolt"), color = "blue", width = NULL)
  })
  
  output$CL_pot_req <- renderUI({
    consData <- req(CLConsSubDataset(), input$demanda_tipo_calculo, input$VentanaCarga)
    total_energia <- consData %>% select(-any_of("Total")) %>% select(where(is.numeric)) %>% as.matrix() %>% sum(na.rm = TRUE)
    divisor <- if (input$demanda_tipo_calculo == "promedio") 5 else { req(input$demanda_factor_slider); input$demanda_factor_slider }
    resultado <- total_energia / divisor / input$VentanaCarga
    total_fmt <- format(round(resultado, 2), big.mark = ".", decimal.mark = ",")
    valueBox(value = paste0(total_fmt, " kW"), subtitle = "Potencia diaria", icon = icon("plug"), color = "green", width = NULL)
  })
  
  output$CL_cargadores_req <- renderUI({
    consData <- req(CLConsSubDataset(), input$demanda_tipo_calculo, input$VentanaCarga)
    total_energia <- consData %>% select(-any_of("Total")) %>% select(where(is.numeric)) %>% as.matrix() %>% sum(na.rm = TRUE)
    divisor <- if (input$demanda_tipo_calculo == "promedio") 5 else { req(input$demanda_factor_slider); input$demanda_factor_slider }
    resultado <- total_energia / divisor / input$VentanaCarga
    cargadores <- ceiling(round(resultado, 2) / 150)
    valueBox(value = paste0(cargadores), subtitle = "Cargadores 150 kW", icon = icon("charging-station"), color = "purple", width = NULL)
  })
  
  CL_KmSubdataset <- reactive({
    datos <- req(rutasSubDataset())
    req(datos, nrow(datos) > 0)
    
    fac_esco <- dplyr::coalesce(CFG$factor_exp$semana_esco, 40)
    fac_gral <- dplyr::coalesce(CFG$factor_exp$semana_gral, 52)
    
    tabla_Km <- datos %>%
      sf::st_drop_geometry() %>%
      group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
      summarise(Total_disRutaSem = sum(disRutaSem / 1000, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = SR_Tip_Ruta, values_from = Total_disRutaSem, values_fill = 0) %>%
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
        Km_Extras = if (any(endsWith(names(.), "_extra"))) sum(c_across(ends_with("_extra")), na.rm = TRUE) else 0
      ) %>%
      ungroup() %>%
      select(`Tipo de Vehículo`, Km_escolares, Km_Extras) %>%
      mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
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
      pivot_longer(cols = -`Tipo de Vehículo`, names_to = "Contaminante", values_to = "Factor") %>%
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
    data_filtrada <- data %>%
      mutate(Contaminante = toupper(as.character(Contaminante))) %>%
      filter(Contaminante == "CO2EQ")
    
    req(nrow(data_filtrada) > 0)
    plot_ly(data = data_filtrada, x = ~Contaminante, y = ~Emisiones_Escolar, name = 'Escolar', type = 'bar') %>%
      add_trace(y = ~Emisiones_Extra, name = 'Extra') %>%
      layout(separators = ",.", barmode = 'stack', yaxis = list(title = 'Toneladas', tickformat = ',.1f'))
  })
  
  output$CL_emisiones_plot <- renderPlotly({
    data <- req(emisionesSubDataset())
    contaminantes_sel <- req(input$filtro_t_emision)
    
    data_filtrada <- data %>%
      mutate(Contaminante = toupper(as.character(Contaminante))) %>%
      filter(Contaminante %in% toupper(contaminantes_sel))
    
    req(nrow(data_filtrada) > 0)
    plot_ly(data = data_filtrada, x = ~Contaminante, y = ~Emisiones_Escolar, name = 'Escolar', type = 'bar') %>%
      add_trace(y = ~Emisiones_Extra, name = 'Extra') %>%
      layout(separators = ",.", barmode = 'stack', yaxis = list(title = 'Toneladas', tickformat = ',.1f'))
  })
  
  output$CL_Km_ano_table <- renderDT({
    data <- req(CL_KmSubdataset())
    datatable(
      data,
      extensions = 'Buttons',
      options = list(pageLength = 7, dom = 'Bfrtip', buttons = list(js_csv_btn('01_km_anuales'))),
      rownames = FALSE
    ) %>% formatRound(columns = 2:ncol(data), digits = 0, interval = 3, mark = ".", dec.mark = ",")
  })
  
  output$CL_emisiones_table <- renderDT({
    data <- req(emisionesSubDataset())
    datatable(
      data,
      extensions = 'Buttons',
      options = list(pageLength = 7, dom = 'Bfrtip', buttons = list(js_csv_btn('02_emisiones_evitadas'))),
      rownames = FALSE
    ) %>% formatRound(columns = c("Emisiones_Escolar", "Emisiones_Extra", "Emisiones_Total"), digits = 2, interval = 3, mark = ".", dec.mark = ",")
  })
  
}

shinyApp(ui = ui, server = server)