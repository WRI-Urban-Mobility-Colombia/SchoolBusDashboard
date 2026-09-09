library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)

# ==============================================================================
# 1. INTERFAZ DE USUARIO (UI)
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # --- Cabecera ---
  dashboardHeader(title = "Plataforma Analítica PCBE"),
  
  # --- Menú Lateral ---
  dashboardSidebar(
    sidebarMenu(
      id = "tab_seleccionada",
      menuItem("Cree sus zonas (V1)", tabName = "tab_mapa", icon = icon("binoculars")),
      menuItem("Cree sus zonas (V2 Rediseño)", tabName = "tab_mapa_v2", icon = icon("magic")),
      menuItem("Explore zonas creadas", tabName = "tab_mapa2", icon = icon("map")),
      menuItem("Créditos", tabName = "creditos", icon = icon("users"))
    )
  ),
  
  # --- Cuerpo Principal ---
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Estilos base v2 y tipografía */
        body {
          font-family: 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
        }
        
        /* Tarjetas personalizadas con elevación suave */
        .v2-card {
          background: #ffffff;
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.05);
          border: 1px solid #e9ecef;
          margin-bottom: 20px;
          padding: 16px;
          transition: all 0.2s ease-in-out;
        }
        
        .v2-card-header {
          font-size: 16px;
          font-weight: 600;
          color: #1e293b;
          margin-bottom: 12px;
          display: flex;
          align-items: center;
          gap: 8px;
          border-bottom: 2px solid #f1f5f9;
          padding-bottom: 8px;
        }

        /* Contenedor del Mapa */
        .v2-map-container {
          border-radius: 8px;
          overflow: hidden;
          border: 1px solid #cbd5e1;
        }
        
        /* REGLAS EXCLUSIVAS PARA EXPORTACIÓN A PDF / IMPRESIÓN */
        @media print {
          @page {
            size: A4 portrait;
            margin: 10mm;
          }
          
          .main-header, .main-sidebar, .btn-imprimir, .nav-tabs, .control-sidebar {
            display: none !important;
          }
          
          .content-wrapper, .right-side, .bg-color {
            background-color: #ffffff !important;
            margin-left: 0 !important;
            padding: 0 !important;
          }

          .collapse {
            display: block !important;
            height: auto !important;
          }
          
          #mapa_interactivo, .plotly, .js-plotly-plot {
            max-height: 400px !important;
            page-break-inside: avoid !important;
          }

          .v2-card {
            box-shadow: none !important;
            border: 1px solid #ccc !important;
            page-break-inside: avoid !important;
            margin-bottom: 15px !important;
          }

          .row {
            display: flex !important;
            flex-direction: row !important;
            flex-wrap: wrap !important;
          }
        }
      "))
    ),
    
    tabItems(
      # Pestaña Legacy V1
      tabItem(
        tabName = "tab_mapa",
        h2("Pestaña original (V1)"),
        p("Pestaña disponible para compatibilidad previa.")
      ),
      
      # Pestaña V2 REDISEÑADA
      tabItem(
        tabName = "tab_mapa_v2",
        
        # Fila superior de controles y acciones globales
        fluidRow(
          column(
            width = 12,
            div(
              class = "v2-card",
              style = "display: flex; justify-content: space-between; align-items: center;",
              div(
                h3("Configurador de Zonas y Análisis Operacional", style = "margin:0; font-weight:700; color:#0f172a;"),
                p("Seleccione polígonos en el mapa para delimitar zonas y analizar el comportamiento operativo y energético.", style = "margin:0; color:#64748b;")
              ),
              actionButton(
                inputId = "tab1_btn_imprimir",
                label = " Exportar Informe (PDF)",
                icon = icon("file-pdf"),
                class = "btn-success btn-imprimir",
                style = "font-weight:600; border-radius:6px; padding: 8px 16px;",
                onclick = "window.print();"
              )
            )
          )
        ),

        # Fila principal: Mapa + Panel lateral de métricas
        fluidRow(
          column(
            width = 8,
            div(
              class = "v2-card v2-map-container",
              div(class = "v2-card-header", icon("map-marked-alt"), "Visor Espacial Interactivo"),
              leafletOutput("mapa_interactivo", height = "580px")
            )
          ),
          column(
            width = 4,
            # Tarjeta de Filtros
            div(
              class = "v2-card",
              div(class = "v2-card-header", icon("filter"), "Parámetros de Filtrado"),
              radioButtons(
                inputId = "var_color_ruta",
                label = "Colorear Rutas por:",
                choices = c("Tipo de Ruta" = "SR_Tip_Ruta", "Tipo de Vehículo" = "SR_Veh_Aj_2"),
                selected = "SR_Tip_Ruta",
                inline = TRUE
              ),
              hr(style = "margin: 10px 0;"),
              selectizeInput(
                inputId  = "filtro_tipo_ruta",
                label    = "Tipo de Ruta:",
                choices  = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Todas las rutas", plugins = list("remove_button"))
              ),
              selectizeInput(
                inputId  = "filtro_tipo_vehiculo",
                label    = "Tipo de Vehículo:",
                choices  = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Todos los vehículos", plugins = list("remove_button"))
              )
            ),
            # Tarjeta KPI Cumplimiento
            div(
              class = "v2-card",
              div(class = "v2-card-header", icon("chart-pie"), "Meta Política Pública (PCBE)"),
              fluidRow(
                column(6, valueBoxOutput("tab1_ben_atendidos", width = 12)),
                column(6, valueBoxOutput("tab1_metaPCBE", width = 12))
              ),
              div(
                style = "margin-top: 10px;",
                plotlyOutput("tab1_plotly_gauge", height = "180px")
              )
            )
          )
        ),

        # Sección Inferior Organizadora mediante Pestañas UI (Tabs)
        fluidRow(
          column(
            width = 12,
            tabBox(
              width = 12,
              id = "tabset_resultados_v2",
              side = "left",
              selected = "tab_operacional",
              
              # Pestaña 1: Parámetros Operacionales
              tabPanel(
                title = tagList(icon("bus"), " Operación y Rutas"),
                value = "tab_operacional",
                br(),
                fluidRow(
                  column(
                    width = 6,
                    div(
                      class = "v2-card",
                      div(class = "v2-card-header", icon("route"), "Resumen Distancias (Km / Semanales)"),
                      DTOutput("tabla_resumen_rutas")
                    )
                  ),
                  column(
                    width = 6,
                    div(
                      class = "v2-card",
                      div(class = "v2-card-header", icon("shuttle-van"), "Estimación de Flotas y Vehículos"),
                      DTOutput("tab1_CL_tabla_veh")
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    div(
                      class = "v2-card",
                      div(class = "v2-card-header", icon("clock"), "Horas Contratadas a la Semana"),
                      DTOutput("tab1_CL_tabla_horas")
                    )
                  ),
                  column(
                    width = 6,
                    div(
                      class = "v2-card",
                      div(class = "v2-card-header", icon("list-ol"), "Conteo General de Rutas"),
                      DTOutput("tab1_CL_tabla_rutas")
                    )
                  )
                ),
                div(
                  class = "v2-card",
                  div(class = "v2-card-header", icon("table"), "Detalle Completo de Rutas"),
                  DTOutput("tab1_CL_tabla_rutas_detalle")
                )
              ),
              
              # Pestaña 2: Demanda Energética e Infraestructura
              tabPanel(
                title = tagList(icon("bolt"), " Energía e Infraestructura"),
                value = "tab_energia",
                br(),
                fluidRow(
                  column(
                    width = 4,
                    div(
                      class = "v2-card",
                      div(class = "v2-card-header", icon("sliders-h"), "Escenario Operativo"),
                      radioButtons(
                        inputId = "tab1_EscenarioKm",
                        label = "Configuración del escenario:",
                        choices = c(
                          "Base (Op. + 26% vacío)" = "base",
                          "Extendido (+ Servicios adic.)" = "extras"
                        ),
                        selected = "base"
                      ),
                      conditionalPanel(
                        condition = 'input.tab1_EscenarioKm == "extras"',
                        numericInput(
                          inputId = "tab1_Kms_ad",
                          label = "Km adic. semanales:",
                          value = 140, min = 0, max = 2100, step = 20
                        )
                      )
                    )
                  ),
                  column(
                    width = 8,
                    div(
                      class = "v2-card",
                      div(class = "v2-card-header", icon("charging-station"), "Estimación Demanda Semanal (kWh)"),
                      plotlyOutput("tab1_CL_demanda_energetica_plot", height = "280px")
                    )
                  )
                ),
                div(
                  class = "v2-card",
                  div(class = "v2-card-header", icon("plug"), "Dimensionamiento de Infraestructura y Recarga"),
                  fluidRow(
                    column(
                      width = 4,
                      radioButtons(
                        inputId  = "tab1_demanda_tipo_calculo",
                        label    = "Método de Carga:",
                        choices  = c("Promedio" = "promedio", "Carga semanal" = "factor"),
                        selected = "promedio", inline = TRUE
                      ),
                      conditionalPanel(
                        condition = "input.tab1_demanda_tipo_calculo == 'factor'",
                        sliderInput("tab1_demanda_factor_slider", "Días recarga semanal:", min = 1, max = 7, value = 3)
                      ),
                      sliderInput("tab1_VentanaCarga", "Ventana de Recarga (Horas):", min = 1.5, max = 12, value = 3, step = 0.5)
                    ),
                    column(width = 8, plotlyOutput("tab1_CL_horas_act", height = "250px"))
                  ),
                  hr(),
                  fluidRow(
                    column(4, div(class = "well text-center", h5("Energía Diaria Requerida"), uiOutput("tab1_CL_cons_diario"))),
                    column(4, div(class = "well text-center", h5("Potencia Total Necesaria"), uiOutput("tab1_CL_pot_req"))),
                    column(4, div(class = "well text-center", h5("Cargadores Requeridos"), uiOutput("tab1_CL_cargadores_req")))
                  )
                )
              ),

              # Pestaña 3: Impacto Ambiental y Emisiones
              tabPanel(
                title = tagList(icon("leaf"), " Impacto Ambiental"),
                value = "tab_emisiones",
                br(),
                div(
                  class = "v2-card",
                  div(class = "v2-card-header", icon("smog"), "Filtro de Contaminantes Evaluados"),
                  selectInput(
                    inputId = 'tab1_filtro_t_emision',
                    label = 'Contaminantes a incluir:',
                    choices = c("CO", "VOC", "NOX", "SOX", "PM25", "PM10", "CO2EQ"),
                    selected = c("CO", "VOC", "NOX", "SOX", "PM25", "PM10", "CO2EQ"),
                    multiple = TRUE
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    div(
                      class = "v2-card",
                      div(class = "v2-card-header", icon("chart-bar"), "Emisiones Evitadas al Año"),
                      plotlyOutput("tab1_CL_emisiones_plot", height = "320px")
                    )
                  ),
                  column(
                    width = 6,
                    div(
                      class = "v2-card",
                      div(class = "v2-card-header", icon("leaf"), "Toneladas Evitadas / Año"),
                      DTOutput("tab1_CL_emisiones_table")
                    )
                  )
                )
              )
            )
          )
        )
      ),

      # Otras pestañas de navegación
      tabItem(tabName = "tab_mapa2", h2("Exploración de Zonas Creadas")),
      tabItem(tabName = "creditos", h2("Créditos del Proyecto"))
    )
  )
)

# ==============================================================================
# 2. LÓGICA DEL SERVIDOR (SERVER)
# ==============================================================================

server <- function(input, output, session) {
  
  # --- 1. Renderizado de Leaflet Mapa ---
  output$mapa_interactivo <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -74.0721, lat = 4.7110, zoom = 11)
  })

  # --- 2. Indicadores KPIs y Gauge ---
  output$tab1_ben_atendidos <- renderValueBox({
    valueBox("1,250", "Beneficiarios", icon = icon("users"), color = "blue")
  })
  
  output$tab1_metaPCBE <- renderValueBox({
    valueBox("85%", "Meta PCBE", icon = icon("bullseye"), color = "green")
  })
  
  output$tab1_plotly_gauge <- renderPlotly({
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = 85,
      title = list(text = "Cumplimiento"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(NULL, 100)),
        bar = list(color = "#10b981"),
        steps = list(
          list(range = c(0, 50), color = "#fee2e2"),
          list(range = c(50, 80), color = "#fef3c7"),
          list(range = c(80, 100), color = "#d1fae5")
        )
      )
    ) %>% 
    layout(margin = list(l=20, r=20, t=30, b=20))
  })

  # --- 3. Tablas DT (Sección Operacional) ---
  output$tabla_resumen_rutas <- renderDT({
    datatable(
      data.frame(Categoría = c("Ruta Urbana", "Ruta Rural"), Distancia_Km = c(450.5, 120.3)),
      options = list(dom = 't', pageLength = 5),
      rownames = FALSE
    )
  })
  
  output$tab1_CL_tabla_veh <- renderDT({
    datatable(
      data.frame(Vehículo = c("Bus Eléctrico", "Microbús"), Cantidad = c(12, 5)),
      options = list(dom = 't', pageLength = 5),
      rownames = FALSE
    )
  })

  output$tab1_CL_tabla_horas <- renderDT({
    datatable(
      data.frame(Día = c("Lunes-Viernes", "Sábado"), Horas = c(40, 8)),
      options = list(dom = 't', pageLength = 5),
      rownames = FALSE
    )
  })

  output$tab1_CL_tabla_rutas <- renderDT({
    datatable(
      data.frame(Tipo = c("Directa", "Alimentadora"), Conteo = c(8, 4)),
      options = list(dom = 't', pageLength = 5),
      rownames = FALSE
    )
  })

  output$tab1_CL_tabla_rutas_detalle <- renderDT({
    datatable(
      data.frame(
        ID_Ruta = paste0("R-", 1:5),
        Origen = c("Zona A", "Zona B", "Zona C", "Zona D", "Zona E"),
        Frecuencia_Min = c(10, 15, 20, 12, 30)
      ),
      options = list(pageLength = 5),
      rownames = FALSE
    )
  })

  # --- 4. Gráficos de Energía e Infraestructura ---
  output$tab1_CL_demanda_energetica_plot <- renderPlotly({
    plot_ly(
      x = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom"),
      y = c(1200, 1150, 1250, 1180, 1300, 800, 600),
      type = 'bar',
      marker = list(color = '#3b82f6')
    ) %>% layout(title = "", yaxis = list(title = "kWh"), xaxis = list(title = "Día"))
  })

  output$tab1_CL_horas_act <- renderPlotly({
    plot_ly(
      x = c("Carga Lenta", "Carga Rápida"),
      y = c(6, 2),
      type = 'bar',
      marker = list(color = '#8b5cf6')
    ) %>% layout(yaxis = list(title = "Horas requeridas"))
  })

  output$tab1_CL_cons_diario <- renderUI({ h4("1,142 kWh/día", style="color:#2563eb; font-weight:bold;") })
  output$tab1_CL_pot_req <- renderUI({ h4("380 kW", style="color:#2563eb; font-weight:bold;") })
  output$tab1_CL_cargadores_req <- renderUI({ h4("4 Cargadores (100kW)", style="color:#2563eb; font-weight:bold;") })

  # --- 5. Gráficos y Tablas Ambientales ---
  output$tab1_CL_emisiones_plot <- renderPlotly({
    plot_ly(
      x = c("CO2EQ", "NOX", "PM25"),
      y = c(450, 12, 1.5),
      type = 'bar',
      marker = list(color = '#10b981')
    ) %>% layout(yaxis = list(title = "Toneladas/Año"))
  })

  output$tab1_CL_emisiones_table <- renderDT({
    datatable(
      data.frame(Contaminante = c("CO2EQ", "NOX", "PM25"), Toneladas_Reducidas = c(450, 12, 1.5)),
      options = list(dom = 't'),
      rownames = FALSE
    )
  })
}

# ==============================================================================
# 3. EJECUCIÓN DE LA APLICACIÓN
# ==============================================================================

shinyApp(ui = ui, server = server)