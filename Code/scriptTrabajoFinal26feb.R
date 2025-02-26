
# Establecer directorio de trabajo
setwd("C:/Users/Heitz/Desktop/Macro de LP/Trabajo FInal")
#install.packages("stringr")  # Solo si no tienes el paquete instalado
#install.packages("dplyr")    # Para asegurarnos de que dplyr está disponible
# 1️ Cargar librerías"
#_______________________________________________________________________________
#Desde aqui vuelvo a empezar para ver como va la vuelta 
# Cargar librerías
library(dplyr)
library(readr)
library(readxl)

# 1️⃣ Cargar bases de datos
reports <- read_csv("reports.csv")
clients <- read_csv("clients.csv")
pib_sectores <- read_csv("DatosPIBConNaics.csv")

# 2️⃣ Limpiar reports y clients para seleccionar las variables de interés
reports_limpia <- reports %>% 
  select(lob_id, registrant_id, filing_year, filing_period_code, amount, is_no_activity)

clients_limpia <- clients %>% 
  select(lob_id, naics, client_name) %>% 
  filter(!is.na(naics) & naics != "")  # Eliminar valores NA en NAICS

# 3️⃣ Unir reports con clients usando lob_id
reports_clients <- reports_limpia %>%
  left_join(clients_limpia, by = "lob_id") %>%
  filter(!is.na(naics))  # Asegurar que no haya NA en NAICS

# 4️⃣ Filtrar para eliminar períodos H1 y H2
reports_clients <- reports_clients %>%
  filter(!(filing_period_code %in% c("H1", "H2")))

# 5️⃣ Guardar la base limpia de reports_clients
write_csv(reports_clients, "reports_clients.csv")

# _______________________________________________________________________________
# 🔹 LIMPIEZA DE PIB Y PEGUE CON REPORTS_CLIENTS
# _______________________________________________________________________________

# 6️⃣ Filtrar PIB eliminando categorías agregadas
categorias_excluir <- c("National income without capital consumption adjustment", 
                        "Domestic industries", 
                        "Private industries", 
                        "Rest of the world")

pib_sectores_filtrado <- pib_sectores %>% 
  filter(!sector %in% categorias_excluir)

# 7️⃣ Convertir NAICS en PIB a texto para evitar problemas en la unión
pib_sectores_filtrado <- pib_sectores_filtrado %>%
  mutate(naics = as.character(naics))

# Guardar la base limpia de PIB
write_csv(pib_sectores_filtrado, "DatosPIBConNaics_Filtrado_SinAgregados.csv")

# _______________________________________________________________________________
# 🔹 PEGUE POR NAICS CONSIDERANDO 4 Y 2 DÍGITOS
# _______________________________________________________________________________

# 8️⃣ Extraer los primeros 4 y 2 dígitos de NAICS en reports_clients
reports_clients <- reports_clients %>%
  mutate(naics4 = substr(naics, 1, 4), 
         naics2 = substr(naics, 1, 2))  # Extraer también los primeros 2 dígitos

# 9️⃣ Intentar el cruce con NAICS de 4 dígitos
reports_clients_pib <- reports_clients %>%
  left_join(pib_sectores_filtrado, by = c("naics4" = "naics"))

# 🔹 Para los que quedaron sin PIB, intentar con los primeros 2 dígitos
reports_clients_pib <- reports_clients_pib %>%
  mutate(naics = ifelse(is.na(pib), naics2, naics)) %>%
  left_join(pib_sectores_filtrado, by = c("naics" = "naics"), suffix = c("", "_alt"))

# 🔹 Si un NAICS de 2 dígitos logró el match, usar ese PIB
reports_clients_pib <- reports_clients_pib %>%
  mutate(pib = ifelse(is.na(pib), pib_alt, pib)) %>%
  select(-pib_alt)  # Eliminar la columna auxiliar

colnames(reports_clients_pib)

# Seleccionar solo las variables necesarias
reports_clients_pib <- reports_clients_pib %>%
  select(lob_id, amount, is_no_activity, naics, naics4, naics2, 
         sector_alt, año_alt, cuartil_alt, pib)

# Guardar la base final con las variables ajustadas
write_csv(reports_clients_pib, "reports_clients_pib_final.csv")

¿


