## Proyecto en R

# Cargar paquetes
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

# Ruta del archivo
archivo <- "Plantilla_Contabilidad_Pyme.xlsx"  # Se encuentra en la carpeta del proyecto

# Leer hojas
transacciones <- read_excel(archivo, sheet = "Transacciones")
plan_cuentas <- read_excel(archivo, sheet = "Plan_de_Cuentas")
movimientos_caja <- read_excel(archivo, sheet = "Movimientos_Caja")

# Se genera el Estado de Resultados
estado_resultados <- transacciones %>%
  left_join(plan_cuentas, by = "Cuenta Contable") %>%
  filter(`Tipo Cuenta` %in% c("Ingreso", "Egreso")) %>%
  group_by(`Tipo Cuenta`, `Cuenta Contable`) %>%
  summarise(Total = sum(Monto, na.rm = TRUE), .groups = "drop") %>%
  mutate(Total = round(Total, 0))

# Resultado final
estado_resultados <- estado_resultados %>%
  pivot_wider(names_from = `Tipo Cuenta`, values_from = Total, values_fill = 0) %>%
  mutate(`Utilidad Neta` = Ingreso + Egreso)

# Se genera el Balance General
balance_general <- plan_cuentas %>%
  filter(`Tipo Cuenta` %in% c("Activo", "Pasivo", "Patrimonio")) %>%
  left_join(
    transacciones %>% group_by(`Cuenta Contable`) %>%
      summarise(Saldo = sum(Monto, na.rm = TRUE)),
    by = "Cuenta Contable"
  ) %>%
  mutate(Saldo = replace_na(Saldo, 0)) %>%
  arrange(factor(`Tipo Cuenta`, levels = c("Activo", "Pasivo", "Patrimonio")))

# Se crea el Flujo de Caja
flujo_caja <- movimientos_caja %>%
  mutate(Mes = floor_date(as.Date(Fecha), "month")) %>%
  group_by(Mes, Tipo) %>%
  summarise(Total = sum(Monto, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Tipo, values_from = Total, values_fill = 0) %>%
  mutate(`Flujo Neto` = Entrada + Salida)

# Visualización de resultados
print("Estado de Resultados")
print(estado_resultados)

print("Balance General")
print(balance_general)

print("Flujo de Caja")
print(flujo_caja)



