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
library(config)
library(lubridate)

##----------------------------------------------------------------------------##
##Abrir archivos necesarios
##----------------------------------------------------------------------------##
## WD
setwd(here())
##----------------------------------------------------------------------------##
##Carga config file
##----------------------------------------------------------------------------##
Sys.setlocale("LC_ALL", "en_US.UTF-8")

CFG <- config::get(file = "config.yml")

# Archivos
poligonosV2 <- readRDS("Assets/RDS/poligonosV2.rds")
rutasv2 <- readRDS("Assets/RDS/rutas.rds")

pat_ele_buff <- readRDS("Assets/RDS/P_Elec_buff.rds")
pat_ele_punt <- readRDS("Assets/RDS/P_Elec_punt.rds")
punt_ad_buff <- readRDS("Assets/RDS/P_Ad_buff.rds")
punt_ad_punt <- readRDS("Assets/RDS/P_Ad_punt.rds")
colegios_pun <- readRDS("Assets/RDS/colegios.rds")
llaves_rutas <- read.csv2("Assets/csv/260901_Rutas_colegio3.csv")

##----------------------------------------------------------------------------##
##Variables adicionales y Paletas de Colores Estáticas
##----------------------------------------------------------------------------##
## Paleta de colores para 'nom_patio' en mapa_clusteres
factpal_patios <- colorFactor(
  palette = "Accent", 
  domain  = poligonosV2$Cluster
)

## Paletas de colores fijas para las rutas en ambos mapas
paleta_tipo_ruta <- colorFactor(
  palette = "RdYlGn",
  domain  = unique(rutasv2$SR_Tip_Ruta),
  na.color = "#808080"
)

paleta_tipo_vehiculo <- colorFactor(
  palette = "PuOr",
  domain  = unique(rutasv2$SR_Veh_Aj_2),
  na.color = "#808080"
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

#rutasv2R1$SR_DaneIED <- as.numeric(rutasv2R1$SR_DaneIED)
#colegios_pun$COD_DANE     <- as.numeric(colegios_pun$COD_DANE)

## Calcular variables semanales (distancia semanal y horas semanales)
rutasv2R1 <- rutasv2R1 %>% mutate(disRutaSem = Dis_ruta_m * SR_Tot_Dias)
rutasv2R1 <- rutasv2R1 %>% mutate(disHorasSem = SR_Toal_H * SR_Tot_Dias)
rutasv2R1$SR_DaneIED <- gsub("\\.", "", rutasv2R1$SR_DaneIED)

poligonosV2$NoRutas <- sapply(poligonosV2$id, function(x) sum(rutasv2R1$id_2 == x, na.rm = TRUE))

## Filtrado de colegios existentes en las rutas según SR_DaneIED vs COD_DANE
dane_existentes <- unique(na.omit(rutasv2R1$SR_DaneIED))
colegios_filtrados_dane <- colegios_pun
## Ícono personalizado de colegio
colegio_icon <- makeAwesomeIcon(
  icon        = "graduation-cap",
  iconColor   = "white",
  markerColor = "blue",
  library     = "fa"
)

##----------------------------------------------------------------------------##
##CSS Print
##----------------------------------------------------------------------------##
css_impresion <- "
@media print {
  .main-header, .main-sidebar, .btn-imprimir {
    display: none !important;
  }
  .content-wrapper, .right-side {
    margin-left: 0 !important;
    background-color: #ffffff !important;
  }
}
"

##----------------------------------------------------------------------------##
##UI
##----------------------------------------------------------------------------##
ui <- dashboardPage(

  title = "Visor de rutas escolares",
  
## A. HEADER----------------------------------------------------------------##
  header = dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Rutas Escolares"),
      span(class = "logo-mini", "WRI GS")
    ),
    rightUi = userOutput("skin_dropdown")
  ),

##--------------------------------------------------------------------------##  
## B. SIDEBAR---------------------------------------------------------------##
  sidebar = dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "tab_seleccionada",
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

##--------------------------------------------------------------------------##  
## C. BODY------------------------------------------------------------------##
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
    /* Estilos generales (pantalla) */
    .content-wrapper, .right-side {
      background-color: #f4f6f9;
    }
    #mapa_interactivo, #mapa_clusteres {
      height: calc(100vh - 200px) !important;
    }

    /* Reglas para modo impresión continua en 1 sola página */
    @media print {
      @page {
        size: A1 portrait;
        margin: 5mm;
      }

      .main-header, .main-sidebar, .btn-imprimir, .main-footer {
        display: none !important;
      }

      html, body, .wrapper, .content-wrapper, .right-side {
        width: 100% !important;
        height: 100% !important;
        margin: 0 !important;
        padding: 0 !important;
        background-color: #ffffff !important;
        overflow: visible !important;
      }

      .row {
        display: flex !important;
        flex-direction: row !important;
        flex-wrap: nowrap !important;
        page-break-inside: avoid !important;
      }

      .col-sm-6, .col-md-6 { width: 50% !important; float: left !important; }
      .col-sm-4, .col-md-4 { width: 33.333% !important; float: left !important; }
      .col-sm-3, .col-md-3 { width: 25% !important; float: left !important; }
      .col-sm-12, .col-md-12 { width: 100% !important; }

      .box {
        margin-bottom: 10px !important;
        page-break-inside: avoid !important;
      }

      #mapa_interactivo, #mapa_clusteres {
        height: 420px !important;
        width: 100% !important;
        page-break-inside: avoid !important;
      }

      .content {
        transform: scale(0.80);
        transform-origin: top left;
        width: 108% !important;
      }
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
        
        fluidRow(
          valueBoxOutput("box_beneficiarios", width = 12)
        ),
        
        fluidRow(
          column(
            width = 9,
            box(
              title = "Análisis por sector",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              collapsible = FALSE,
              leafletOutput("mapa_interactivo") 
            )
          ),
          
          column(
            width = 3,
            box(
              title = "Filtros y Controles",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              collapsible = TRUE,
              
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
              
              selectizeInput(
                inputId  = "filtro_tipo_ruta",
                label    = "Seleccionar Tipo de Ruta:",
                choices  = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Todas las rutas", plugins = list("remove_button"))
              ),
              
              tags$hr(style = "border-top: 1px solid #e0e0e0; margin: 10px 0;"),
              
              selectizeInput(
                inputId  = "filtro_tipo_vehiculo",
                label    = "Seleccionar Tipo de Vehículo:",
                choices  = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Todos los vehículos", plugins = list("remove_button"))
              )
            ),
            
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
          column(
            width = 8,
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
            width  = 4,
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
                selected = NULL,
                multiple = TRUE
              ),
              radioButtons(
                inputId = "var_color_ruta_clus",
                label = "Colorear Rutas por:",
                choices = c(
                  "Tipo de Ruta" = "SR_Tip_Ruta",
                  "Tipo de Vehículo" = "SR_Veh_Aj_2"
                ),
                selected = "SR_Tip_Ruta"
              ),
              selectInput(
                inputId = "filtro_tipo_ruta_clus",
                label = "Seleccione los tipos de ruta",
                choices = c("Todos", sort(unique(rutasv2R1$SR_Tip_Ruta))),
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
            box(
              width = 12,
              title = "Cumplimiento de la meta del PCBE",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              fluidRow(
                column(
                  width = 6,
                  h4("Beneficiarios atendidos"),
                  valueBoxOutput("ben_atendidos", width = 12)  
                ),
                column(
                  width = 6,
                  h4("Meta de la política pública"),
                  valueBoxOutput("metaPCBE", width = 12)  
                )
              ),
              h5("Porcentaje"),
              plotlyOutput("plotly_gauge", height = "250px")
            )
          )
        ),
        box(
          width = 12,
          title = "Parámetros operacionales de la propuesta",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          
          box(
            width = 12,
            title = "Rutas que componen la propuesta",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            DTOutput("CL_tabla_rutas_detalle")
          ),
          box(
            width = 12,
            title = "Horas contratadas a la semana",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            h5("Total de horas en una semana"),
            DTOutput("CL_tabla_horas")
          ),
          box(
            width = 12,
            title = "Cantidad de rutas",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            DTOutput("CL_tabla_rutas")
          ),
          box(
            width = 12,
            title = "Distancias (Km) semanales por tipo de vehiculo y tipo de ruta",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            DTOutput("tabla_resumen_km")
          ),
          box(
            width = 12,
            title = "Cantidad de vehículos (Aproximado al siguiente entero)",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            h5("Parte de los factores de utilización promedio"),
            DTOutput("CL_tabla_veh")
          )
        ),
        
        box(
          width = 12,
          title = "Demanda energética e infraestructura de recarga",
          status = 'primary',
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          h3(em("Demanda de energía semanal")),
          hr(),
          h4("Defina el escenario que quiere analizar"),
          fluidRow(
            column(
              width = 4,
              radioButtons(
                inputId = "EscenarioKm",
                label = "Seleccione el escenario:",
                choices = c(
                  "Operación más 26% de recorridos en vacío" = "base",
                  "Operación más 26% de vacío y servicios adicionales" = "extras"
                ),
                selected = "base"
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                condition = 'input.EscenarioKm == "extras"',
                numericInput(
                  inputId = "Kms_ad",
                  label = "Ingrese la distancia operada adicional por otros servicios (Km semanales)",
                  value = 140,
                  min = 0,
                  max = 2100,
                  step = 20
                )
              )
            )
          ),
          hr(),
          h5(em("Consumo estimado si se implementan el 100% de las rutas de la zona, en las tipologías vehiculares y tipo de rutas seleccionados")),
          hr(),
          plotlyOutput("CL_demanda_energetica_plot", height = "450px"),
          h4(em("Tabla de datos de consumo en kWh")),
          DTOutput("CL_demanda_energetica"),
          hr(),
          box(
            width = 12,
            title = "Estimación de necesidades de infraestructura",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            h5(em("Dinámica de las zonas (Horas de entrada/salida para estimar ventanas de carga)")),
            plotlyOutput("CL_horas_act", height = "350px")
          ),

          h4(em("Variables para la estimación de las necesidades de infraestructura")),
          h5(em("Seleccione el método de estimación, promedio: consumo semanal promedio o elegir el número de días de recarga")),
          radioButtons(
            inputId  = "demanda_tipo_calculo",
            label    = "Método de estimación:",
            choices  = c("Promedio" = "promedio", "Carga semanal" = "factor"),
            selected = "promedio",
            inline   = TRUE
          ),
          conditionalPanel(
            condition = "input.demanda_tipo_calculo == 'factor'",
            div(
              style = "max-width: 350px;",
              sliderInput(
                inputId = "demanda_factor_slider",
                label   = "Elija el número de días en los que se repartirá la recarga semanal",
                min     = 1,
                max     = 7,
                value   = 3,
                step    = 1
              )
            )
          ),
          h5(em("Seleccione las horas disponibles de la ventana de recarga")),
          div(
            style = "max-width: 350px;",
            sliderInput(
              inputId = "VentanaCarga",
              label = "Horas de la ventana de recarga",
              min = 1.5,
              max = 12,
              value = 3,
              step = 0.5
            )
          ),
          h6(em("Promedio: Carga homogénea a lo largo de 5 días de la semana con el consumo promedio semanal")),
          h6(em("Factor de utilización: Factor de carga diaria respecto al consumo semanal. 1: 1 carga semanal, 0,2: 5 cargas semanales")),
          hr(),
          fluidRow(
            column(
              width = 4,
              h4(em("Energía diaria requerida")),
              uiOutput("CL_cons_diario")
            ),
            column(
              width = 4,
              h4(em("Potencia total")),
              uiOutput("CL_pot_req")
            ),
            column(
              width = 4,
              h4(em("Cargadores requeridos")),
              uiOutput("CL_cargadores_req")
            )
          ),
          box(
            width = 12,
            title = "Emisiones evitadas",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            selectInput(
              inputId = 'filtro_t_emision',
              label = 'Seleccione los contaminantes',
              choices = c("CO", "VOC", "NOX", "SOX", "PM25", "PM10", "CO2EQ"),
              selected = c("CO", "VOC", "NOX", "SOX", "PM25", "PM10", "CO2EQ"),
              multiple = TRUE
            ),
            
            h4(em("Emisiones evitadas por el proyecto al año")),
            plotlyOutput("CL_emisiones_plot", height = "350px"),
            h4(em("Kilómetros anuales")),
            DTOutput("CL_Km_ano_table"),
            h4(em("Contaminantes anuales (Toneladas)")),
            DTOutput("CL_emisiones_table")
          )
        ),
        box(
          actionButton(
            inputId = "btn_imprimir",
            label = "Exportar resultado en pdf",
            icon = icon("file-pdf"),
            class = "btn-success btn-imprimir",
            onclick = "window.print();"
          )
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
      addProviderTiles(providers$CartoDB.Positron) %>%
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
            fieldSeparator = ";"
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
      addProviderTiles(providers$CartoDB.Positron) %>%
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
                "<b>Tipo de Ruta: </b>", ifelse(is.na(SR_Tip_Ruta), "N/A", SR_Tip_Ruta), "<br>",
                "<b>Tipo Vehículo: </b>", ifelse(is.na(SR_Veh_Aj_2), "N/A", SR_Veh_Aj_2), "<br>",
                "<b>Hexágono ID: </b>", ifelse(is.na(Id_Hexagono), "N/A", Id_Hexagono), "<br>",
                "<b>Distancia: </b>", dist_km
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
            fieldSeparator = ";"
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
            fieldSeparator = ";"
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
            fieldSeparator = ";"
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
            fieldSeparator = ";"
          )
        )
      ),
      rownames = FALSE
    ) 
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
            fieldSeparator = ";"
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

}

##----------------------------------------------------------------------------##
## Ejecutar App
##----------------------------------------------------------------------------##
shinyApp(ui = ui, server = server)