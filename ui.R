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

##--------------------------------------------------------------------------##  
## c. BODY------------------------------------------------------------------##

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
        #fluidRow(
        #  column(width = 4, valueBoxOutput("box_beneficiarios1", width = 12)),
        #  column(width = 4, valueBoxOutput("box_beneficiarios2", width = 12)),
        #  column(width = 4, valueBoxOutput("box_beneficiarios3", width = 12))
        #),
        column(
          width = 5,
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
          width = 7,
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
          ),
          box(
            width = 12,
            title = "Cumplimiento de la meta del PCBE",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
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
        ),
        ### Sección de demanda energética
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
              width =4,
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
          h3(em("Estimación de necesidades de infraestructura")),
          h5(em("Dinámica de las zonas (Horas de entrada/salida para estimar ventanas de carga)")),
          plotlyOutput("CL_horas_act", height = "350px"),
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
              width = 5,
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
          h5("Consumo energetico semanal en Kwh"),
          
          h4("Estimación de infraestructura de carga"),
          h5("Dinámica de operación de la zona")
          #DTOutput("CL_tabla_horaria"),
          
          
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
## 1. Introducción ---------------------------------------------------------##
##--------------------------------------------------------------------------##

##--------------------------------------------------------------------------##
## 2. Mapa creación de proyectos
##--------------------------------------------------------------------------##

  ## 2.1. Observer del mapa interactivo (Identificar zonas seleccionadas)-----##
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
##--------------------------------------------------------------------------##  
## 2.2. Mapa interactivo base-----------------------------------------------##
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
##--------------------------------------------------------------------------##  
## 2.3. Vector zonas seleccioandas------------------------------------------##

  seleccionados <- reactiveVal(character(0)) # Almacena IDs de polígonos seleccionados

##--------------------------------------------------------------------------##
## 2.4.Resaltar polígonos seleccionados-------------------------------------##
  
  observe({
    vector_actual <- seleccionados()
    print(vector_actual)
    
    proxy <- leafletProxy("mapa_interactivo")
    proxy %>% clearGroup("seleccion_roja")
    
    # Si hay zonas seleccionadas, agregue polígonos
    
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
## 2.6.Reactivo de Filtrado Conjunto (Espacial + Controles UI)--------------##

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
    
    # 4. Filtrar geometrías vacías o no válidas que rompen addPolylines
    if (inherits(datos, "sf") && nrow(datos) > 0) {
      datos <- datos %>% 
        filter(!st_is_empty(.)) %>%    # Remueve geometrías vacías
        sf::st_make_valid()           # Corrige posibles geometrías corruptas
    }
    
    return(datos)
  })

##--------------------------------------------------------------------------##
## 2.7. Dibuja y Colorea las Rutas en el Mapa Interactivo Principal---------##

  observe({
    # 1. Validar selección
    ids <- seleccionados()
    if (length(ids) == 0) {
      leafletProxy("mapa_interactivo") %>% 
        clearGroup("Rutas Filtradas") %>% 
        clearControls()
      return()
    }
    
    rutas_sub <- rutas_filtradas_reactivas()
    proxy     <- leafletProxy("mapa_interactivo")
    
    # Limpieza previa del grupo y la leyenda
    proxy %>% 
      clearGroup("Rutas Filtradas") %>% 
      clearControls()
    
    # 2. Validar que existan datos válidos
    if (!is.null(rutas_sub) && nrow(rutas_sub) > 0) {
      
      # Determinar variable de color de forma segura
      var_color <- "SR_Tip_Ruta"
      if (!is.null(input$var_color_ruta) && is.character(input$var_color_ruta) && nzchar(input$var_color_ruta)) {
        var_color <- input$var_color_ruta
      }
      
      # Validar si la columna existe en el dataset
      if (var_color %in% names(rutas_sub)) {
        
        # Extraer la columna de valores
        vec_color <- rutas_sub[[var_color]]
        
        # Omitir valores NA de los únicos para el dominio
        valores_unicos <- sort(unique(na.omit(vec_color)))
        
        if (length(valores_unicos) > 0) {
          # Crear la paleta asignando un dominio completo (incluyendo NAs implícitamente)
          paleta <- colorFactor(palette = "Set1", domain = vec_color, na.color = "#808080")
          
          titulo_leyenda <- if (identical(var_color, "SR_Tip_Ruta")) "Tipo de Ruta" else "Tipo de Vehículo"
          
          # Calcular la distancia formateada de forma segura contra NAs
          dist_km <- ifelse(
            is.na(rutas_sub$disRutaSem), 
            "N/A", 
            paste0(round(rutas_sub$disRutaSem / 1000, 2), " Km")
          )
          
          # 3. Dibujar en el mapa
          proxy %>%
            addPolylines(
              data        = rutas_sub,
              group       = "Rutas Filtradas",
              color       = paleta(vec_color), # Se pasa el vector directamente evaluado por la función paleta
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
## 2.9. Pivot table Km-------------------------------------------------##   

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
## 3. Lógica de Pestaña: Mapa de Clústeres----------------------------------##
##--------------------------------------------------------------------------##
## 3.1. Render mapa base de zonas-------------------------------------------##
  
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
##--------------------------------------------------------------------------##
## 3.2. Lógica de filtrar zonas---------------------------------------------##

  poligonos_filtrados <- reactive({
    # Si se selecciona "Todos" o no hay nada seleccionado, retorna todo el dataset
    if (is.null(input$filtro_cluster) || "Todos" %in% input$filtro_cluster) {
      return(poligonosV2)
    } else {
      return(poligonosV2[poligonosV2$Cluster %in% input$filtro_cluster, ])
    }
  })
##--------------------------------------------------------------------------##
## 3.3. Filtrar rutas por poligonos-----------------------------------------##

rutasSubDataset <- reactive({
  # Activa reactividad previa 
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
##--------------------------------------------------------------------------##
## 3.3. Tablas de datos por categoría---------------------------------------##

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
## 2.3.1.3. Pivot table Cantidad de vehículos (Solo datos)
CL_tabla_veh_data <- reactive({
  # 1. Validar requerimiento de datos
  datos <- req(rutasSubDataset())
  
  # Si el dataset resultante no tiene filas, retorna data frame vacío de forma limpia
  if (nrow(datos) == 0) {
    return(data.frame())
  }
  
  # 2. Procesar y estructurar el data frame
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
    # Asignar factor por vehículo, dividir y redondear al entero superior
    mutate(
      factor = case_when(
        toupper(`Tipo de Vehículo`) == "BUS"                      ~ 1.5,
        toupper(`Tipo de Vehículo`) == "BUSETA"                   ~ 2.5,
        toupper(`Tipo de Vehículo`) %in% c("MICROBÚS", "MICROBUS") ~ 1.5,
        toupper(`Tipo de Vehículo`) == "VAN"                      ~ 1.0,
        toupper(`Tipo de Vehículo`) == "CAMIONETA"                ~ 2.5,
        TRUE ~ 1.0
      ),
      across(where(is.numeric) & !c(factor), ~ ceiling(.x / factor))
    ) %>% 
    select(-factor) %>% 
    janitor::adorn_totals(where = c("row", "col"), fill = "-", na.rm = TRUE, name = "Total")
  
  return(tabla_resumen)
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
## 2.3.1.1. Calculo beneficiarios

beneficiarios_atendidos <- reactive({
  req(rutasSubDataset()) # Asegura que los datos existan antes de sumar
  data <- rutasSubDataset()
  sum(data$SR_TotalEst, na.rm = TRUE)
})

## 2.3.1.2. value box beneficiarios y pcbe

output$ben_atendidos <- renderValueBox({
  total_atendidos <- beneficiarios_atendidos()
  
  valueBox(
    value = format(total_atendidos, big.mark = "."), # Formato con separador de miles
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
  beneficiarios <- sum(data$SR_TotalEst, na.rm = TRUE)
  cumplimiento <- beneficiarios/meta*100
  
  fig <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = cumplimiento,
    number = list(suffix = "%"),
    title = list(text = "Nivel de Avance", font = list(size = 16)),
    gauge = list(
      axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "gray"),
      bar = list(color = "#2b2b2b"),
      bgcolor = "white",
      borderwidth = 1,
      bordercolor = "gray",
      steps = list(
        list(range = c(0, 30), color = "#f8d7da"),   # Crítico (rojo claro)
        list(range = c(30, 65), color = "#fff3cd"),  # Advertencia (amarillo claro)
        list(range = c(65, 100), color = "#d1e7dd")  # Meta (verde claro)
      )
    )
  ) %>%
    layout(
      margin = list(l = 20, r = 20, t = 40, b = 20),
      font = list(family = "Arial")
    )
  
  fig
})


## 2.3.2. Reactividad consumo (Dataset)
CLConsSubDataset <- reactive({
  datos <- rutasSubDataset()
  print("rutasSubDataset")
  print(datos)
  
  # Factor pérdidas 
  factor_perdidas <- 1 + dplyr::coalesce(CFG$modif_consumo$perdidas, 0)
  factor_kmvac <- 1 + dplyr::coalesce(CFG$modif_km_vacio$km_vacio_perc, 0)
  
  # Validación de datos requeridos
  req(datos, nrow(datos) > 0)
  
  tabla_Km <- datos %>%
    # Eliminar geometría si es un objeto sf/spatial
    sf::st_drop_geometry() %>%
    group_by(SR_Veh_Aj_2, SR_Tip_Ruta) %>%
    summarise(
      Total_disRutaSem = sum(disRutaSem/1000, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Pivotear para tener Tipos de Ruta en columnas y Vehículos en filas
    pivot_wider(
      names_from = SR_Tip_Ruta,
      values_from = Total_disRutaSem,
      values_fill = 0
    ) %>%
    rename(`Tipo de Vehículo` = SR_Veh_Aj_2) %>%
    mutate(across(where(is.numeric), ~ .x * factor_kmvac))
  
  ## Multiplicar cada fila con el factor de config
  ### Identificar columnas que corresponden a los tipos de ruta
  cols_rutas <- setdiff(names(tabla_Km), "Tipo de Vehículo")
  
  # BLOQUE CONDICIONAL SOLICITADO
  if (isTruthy(input$EscenarioKm) && input$EscenarioKm == "extras") {
    req(input$Kms_ad, CL_tabla_veh_data())
    ## Multiplicar los km otros servicios
    tabla_adicional <- CL_tabla_veh_data() %>%
      mutate(across(where(is.numeric), ~ .x * input$Kms_ad))
    tabla_Km <- tabla_Km %>%
      left_join(tabla_adicional, by = "Tipo de Vehículo", suffix = c("", "_extra")) %>%
      mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  }
  tabla_demanda <- tabla_Km %>%
    rowwise() %>%
    mutate(
      # Normalizar el nombre del vehículo a minúsculas y sin espacios para coincidir con las claves de config
      clave_vehiculo = tolower(gsub("[ -]", "_", `Tipo de Vehículo`)),
      
      # Obtener el factor del config. Si no existe la clave para algún vehículo, usa 1 por defecto
      factor_veh = dplyr::coalesce(CFG$factor_consumo[[clave_vehiculo]], 1.0)
    ) %>%
    # Multiplicar los valores de los kilómetros por el factor de consumo
    mutate(across(all_of(cols_rutas), ~ .x * factor_veh * (factor_perdidas))) %>%
    ungroup() %>%
    # Eliminar las columnas auxiliares
    select(-clave_vehiculo, -factor_veh)

  return(tabla_demanda)
})
### 2.3.3. Render tabla

output$CL_demanda_energetica <- renderDT({
  # 1. Validar datos reactivos
  df_demanda <- req(CLConsSubDataset())
  # eliminar total para evitar doble consumo
  df_demanda <- df_demanda %>%
    select(-any_of(c("Total")))
  # 2. Manejo de dataset vacío
  if (nrow(df_demanda) == 0) {
    return(
      datatable(
        data.frame("Estado" = "No hay datos para la combinación de filtros seleccionada."),
        rownames = FALSE,
        options = list(
          dom = 't',
          ordering = FALSE
        )
      )
    )
  }
  
  # 3. Calcular totales y renombrar columna de total general
  tabla_con_totales <- df_demanda %>%
    janitor::adorn_totals(
      where = c("row", "col"), 
      fill  = "-", 
      na.rm = TRUE, 
      name  = "Total"
    ) %>%
    dplyr::rename("Consumo total (kWh)" = Total)

  # 4. Renderizado DT con alineación centrada uniforme
  datatable(
    tabla_con_totales,
    extensions = 'Buttons',
    rownames   = FALSE,
    class      = 'cell-border stripe hover compact',
    options    = list(
      pageLength = 10,
      scrollX    = TRUE,
      autoWidth  = FALSE, # Se recomienda FALSE al usar widths fijos explícitos
      dom        = 'Bfrtip',
      columnDefs = list(
        list(
          width = '140px', 
          className = 'dt-center', # Alinea encabezados y celdas al centro
          targets = '_all'
        )
      ),
      # Forzar ajuste de columnas al inicializar la tabla (corrige desalineaciones por scrollX)
      initComplete = JS(
        "function(settings, json) {",
        "  $(this.api().table().header()).css({'text-align': 'center'});",
        "  this.api().columns.adjust();",
        "}"
      ),
      language   = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
      ),
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

##2.3.4. Render gráfica consumos

output$CL_demanda_energetica_plot <- renderPlotly({
  # 1. Validar datos reactivos
  df_demanda <- req(CLConsSubDataset())
  print(df_demanda)
  
  # 2. Manejo de dataset vacío
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
  
  # 3. Transformar tabla de formato ancho a largo para Plotly
  df_long <- df_demanda %>%
    tidyr::pivot_longer(
      cols      = setdiff(names(df_demanda), "Tipo de Vehículo"),
      names_to  = "Tipo_Ruta",
      values_to = "Consumo_kWh"
    ) %>%
    dplyr::filter(Consumo_kWh > 0) %>% # Opcional: ignorar valores en 0
    dplyr::mutate(
      Consumo_fmt = format(round(Consumo_kWh), big.mark = ".", decimal.mark = ",")
    )
  
  # 4. Construir gráfico de barras apiladas
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
    hoverinfo = "text"
  ) %>%
    layout(
      barmode = "stack",
      xaxis = list(
        title = "",
        tickangle = 0
      ),
      yaxis = list(
        title = "Consumo (kWh)",
        zeroline = TRUE
      ),
      legend = list(
        orientation = "h",
        x = 0,
        y = 1.15,
        title = list(text = "")
      ),
      margin = list(l = 50, r = 20, t = 40, b = 40),
      hoverlabel = list(bgcolor = "white")
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
## 2.3.2.1. Datos de consumo
## 2.3.2.2. Dinámica de la zona

output$CL_horas_act <- renderPlotly({
  data <- req(rutasSubDataset())
  
  cols <- c("SR_H_Ini_R1", "SR_H_Ini_Jornada", "SR_H_Fin_Jornada", "SR_H_Ini_R2")
  
  # 1. Creación del eje X continuo (04:00 a 20:00 cada 15 mins)
  fecha_ref <- Sys.Date()
  grid_intervalos <- data.frame(
    intervalo = seq(
      from = as.POSIXct(paste(fecha_ref, "04:00:00")),
      to   = as.POSIXct(paste(fecha_ref, "20:00:00")),
      by   = "15 mins"
    )
  ) %>% 
    mutate(intervalo_texto = format(intervalo, "%H:%M"))
  
  # 2. Procesamiento de horas y redondeo hacia abajo
  data_proc <- data %>%
    mutate(across(all_of(cols), ~ ifelse(.x == "N/A" | is.na(.x), NA, .x))) %>%
    mutate(across(all_of(cols), ~ {
      time_obj <- lubridate::parse_date_time(.x, orders = c("HM", "HMS"))
      lubridate::floor_date(time_obj, "15 mins")
    }))
  
  # 3. Transformar y resumir conteos por intervalo y variable
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
  
  # 4. Cruce con el eje completo, rellenado de vacíos y ASIGNACIÓN DE ORDEN/ETIQUETAS
  datos_grafica <- grid_intervalos %>%
    select(Hora = intervalo_texto) %>%
    left_join(resumen, by = c("Hora" = "intervalo_texto")) %>%
    tidyr::complete(Hora, Variable = cols, fill = list(Conteo = 0)) %>%
    filter(!is.na(Variable)) %>%
    # Factorizar para forzar el orden y mapear las etiquetas personalizadas
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
  
  # 5. Generar gráfica de barras agrupadas
  plot_ly(
    data = datos_grafica,
    x = ~Hora,
    y = ~Conteo,
    color = ~Variable,
    type = "bar"
  ) %>%
    layout(
      barmode = "group",
      xaxis = list(
        title = "Intervalo de Tiempo (15 min)",
        tickangle = -45,
        type = "category"
      ),
      yaxis = list(title = "Cantidad de servicios"),
      legend = list(orientation = "h", x = 0, y = 1.15),
      margin = list(b = 80)
    )
})
}


##----------------------------------------------------------------------------##
## Ejecutar App
##----------------------------------------------------------------------------##
shinyApp(ui = ui, server = server)