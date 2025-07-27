
# ==========================================
# PROYECTO EN R: APP PARA CREAR REPORTES CONTABLES
# ==========================================


# ==== Cargar Paquetes ====
library(shiny)        # Para crear la aplicación web
library(readxl)       # Para leer archivos Excel
library(dplyr)        # Manipulación de datos
library(tidyr)        # Transformación de tablas
library(lubridate)    # Manejo de fechas
library(DT)           # Tablas interactivas
library(openxlsx)     # Para exportar a Excel

# ==== INTERFAZ DE LA APP ====

shiny <- fluidPage(
  titlePanel("Reportes Contables PyME"),
  
  sidebarLayout(
    sidebarPanel(
      # Subir archivo Excel
      fileInput("file", "Subir archivo Excel", accept = ".xlsx"),
      # Botón para procesar el archivo
      actionButton("procesar", "Procesar"),
      # Botón para descargar el reporte
      downloadButton("descargar", "Descargar Reporte Excel")
    ),
    
    mainPanel(
      # Pestañas para mostrar los 3 reportes
      tabsetPanel(
        tabPanel("Estado de Resultados", DTOutput("tabla_resultados")),
        tabPanel("Balance General", DTOutput("tabla_balance")),
        tabPanel("Flujo de Caja", DTOutput("tabla_flujo"))
      )
    )
  )
)

# ==== LÓGICAS DE LA APP====

logicas <- function(input, output) {
  # Variables reactivas donde se guardarán los resultados
  data_estado <- reactiveVal()
  data_balance <- reactiveVal()
  data_flujo <- reactiveVal()
  
  # ==== Al hacer clic en "Procesar" ====
  observeEvent(input$procesar, {
    req(input$file)  # Validar que haya un archivo cargado
    
    # 1. Leer las 3 hojas del Excel
    transacciones <- read_excel(input$file$datapath, sheet = "Transacciones")
    plan_cuentas <- read_excel(input$file$datapath, sheet = "Plan_de_Cuentas")
    movimientos_caja <- read_excel(input$file$datapath, sheet = "Movimientos_Caja")
    
    # 2. Construir Estado de Resultados
    estado_resultados <- transacciones %>%
      left_join(plan_cuentas, by = "Cuenta Contable") %>%   # Unir con plan de cuentas
      filter(`Tipo Cuenta` %in% c("Ingreso", "Egreso")) %>% # Solo ingresos y egresos
      group_by(`Tipo Cuenta`, `Cuenta Contable`) %>%
      summarise(Total = sum(Monto, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = `Tipo Cuenta`, values_from = Total) %>%
      replace_na(list(Ingreso = 0, Egreso = 0)) %>%         # Reemplazar NA por 0
      mutate(`Utilidad Neta` = Ingreso + Egreso)            # Calcular utilidad neta
    
    # Agregar fila de totales
    totales <- estado_resultados %>%
      summarise(
        `Cuenta Contable` = "TOTAL",
        Ingreso = sum(Ingreso, na.rm = TRUE),
        Egreso = sum(Egreso, na.rm = TRUE),
        `Utilidad Neta` = sum(`Utilidad Neta`, na.rm = TRUE)
      )
    estado_resultados <- bind_rows(estado_resultados, totales)
    
    # 3. Construir Balance General
    balance_general <- transacciones %>%
      left_join(plan_cuentas, by = "Cuenta Contable") %>%
      filter(`Tipo Cuenta` %in% c("Activo", "Pasivo", "Patrimonio")) %>%
      group_by(`Tipo Cuenta`, `Cuenta Contable`) %>%
      summarise(Saldo = sum(Monto, na.rm = TRUE), .groups = "drop") %>%
      arrange(factor(`Tipo Cuenta`, levels = c("Activo", "Pasivo", "Patrimonio")))
    
    # 4. Construir Flujo de Caja
    flujo_caja <- movimientos_caja %>%
      mutate(Mes = floor_date(as.Date(Fecha), "month")) %>% # Agrupar por mes
      group_by(Mes, Tipo) %>%
      summarise(Total = sum(Monto, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = Tipo, values_from = Total, values_fill = 0) %>%
      mutate(`Flujo Neto` = Entrada + Salida)
    
    # Guardar los resultados en las variables reactivas
    data_estado(estado_resultados)
    data_balance(balance_general)
    data_flujo(flujo_caja)
    
    # Renderizar las tablas en la interfaz
    output$tabla_resultados <- renderDT({ datatable(estado_resultados) })
    output$tabla_balance <- renderDT({ datatable(balance_general) })
    output$tabla_flujo <- renderDT({ datatable(flujo_caja) })
  })
  
  # ==== Descargar Excel ====
  
  output$descargar <- downloadHandler(
    filename = function() {
      # Nombre dinámico con la fecha actual
      paste0("Reportes_Contables_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Crear un nuevo archivo Excel
      reporte <- createWorkbook()
      addWorksheet(reporte, "Estado de Resultados")
      addWorksheet(reporte, "Balance General")
      addWorksheet(reporte, "Flujo de Caja")
      
      # Escribir los dataframes en cada hoja
      writeData(reporte, "Estado de Resultados", data_estado())
      writeData(reporte, "Balance General", data_balance())
      writeData(reporte, "Flujo de Caja", data_flujo())
      
      # Dar estilo básico (negrita en la fila de encabezados)
      addStyle(reporte, "Estado de Resultados", createStyle(textDecoration = "bold"),
               rows = 1, cols = 1:ncol(data_estado()), gridExpand = TRUE)
      addStyle(reporte, "Balance General", createStyle(textDecoration = "bold"),
               rows = 1, cols = 1:ncol(data_balance()), gridExpand = TRUE)
      addStyle(reporte, "Flujo de Caja", createStyle(textDecoration = "bold"),
               rows = 1, cols = 1:ncol(data_flujo()), gridExpand = TRUE)
      
      # Guardar el archivo
      saveWorkbook(reporte, file, overwrite = TRUE)
    }
  )
}

# ==== EJECUTAR LA APP ====

shinyApp(shiny, logicas)
