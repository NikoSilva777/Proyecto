## Proyecto en R

# Cargar paquetes
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

# Ruta del archivo
archivo <- "Contabilidad_Pyme_Simulada.xlsx"  # Se encuentra en la carpeta del proyecto

# Leer hojas
transacciones <- read_excel(archivo, sheet = "Transacciones")
plan_cuentas <- read_excel(archivo, sheet = "Plan_de_Cuentas")
movimientos_caja <- read_excel(archivo, sheet = "Movimientos_Caja")

# Validación de cuentas 
cuentas_faltantes <- setdiff(
  unique(transacciones$`Cuenta Contable`),
  plan_cuentas$`Cuenta Contable`
) # Obtiene y compara las cuentas que están en la hoja Transacciones y Plan_de_Cuentas
if (length(cuentas_faltantes) > 0) {
  warning("⚠️ Cuentas sin clasificar en el plan de cuentas: ",
    paste(cuentas_faltantes, collapse = ", ") 
  ) # Entrega una advertencia con las cuentas faltantes 
}


# Se genera el Estado de Resultados
estado_resultados <- transacciones %>%
  left_join(plan_cuentas, by = "Cuenta Contable") %>% # Une (por la izquierda) la tabla de transacciones con el plan de cuentas
  filter(`Tipo Cuenta` %in% c("Ingreso", "Egreso")) %>%
  group_by(`Tipo Cuenta`, `Cuenta Contable`) %>%
  summarise(Total = sum(Monto, na.rm = TRUE), .groups = "drop") %>%
  mutate(Total = round(Total, 0)) %>%
  pivot_wider(
    names_from = `Tipo Cuenta`,
    values_from = Total
  ) %>%
  replace_na(list(Ingreso = 0, Egreso = 0)) %>%  
  mutate(`Utilidad Neta` = Ingreso + Egreso)

# Agregar fila de totales
totales <- estado_resultados %>%
  summarise(
    `Cuenta Contable` = "TOTAL",
    Ingreso = sum(Ingreso, na.rm = TRUE),
    Egreso = sum(Egreso, na.rm = TRUE),
    `Utilidad Neta` = sum(`Utilidad Neta`, na.rm = TRUE)
  )

# Agregar la fila TOTAL al final
estado_resultados <- bind_rows(estado_resultados, totales)

# Se genera el Balance General
balance_general <- transacciones %>%
  left_join(plan_cuentas, by = "Cuenta Contable") %>%
  filter(`Tipo Cuenta` %in% c("Activo", "Pasivo", "Patrimonio")) %>%
  group_by(`Tipo Cuenta`, `Cuenta Contable`) %>%
  summarise(Saldo = sum(Monto, na.rm = TRUE), .groups = "drop") %>%
  mutate(Saldo = round(Saldo, 0)) %>%
  arrange(factor(`Tipo Cuenta`, levels = c("Activo", "Pasivo", "Patrimonio")))

# Se crea el Flujo de Caja
flujo_caja <- movimientos_caja %>%
  mutate(Mes = floor_date(as.Date(Fecha), "month")) %>%
  group_by(Mes, Tipo) %>%
  summarise(Total = sum(Monto, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Tipo, values_from = Total, values_fill = 0) %>%
  mutate(`Flujo Neto` = Entrada + Salida)

# Visualización de resultados
cat("\n=== ESTADO DE RESULTADOS ===\n")
print(estado_resultados)

cat("\n=== BALANCE GENERAL ===\n")
print(balance_general)

cat("\n=== FLUJO DE CAJA ===\n")
print(flujo_caja)


