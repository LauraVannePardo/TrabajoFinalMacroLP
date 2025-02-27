# ==============================================================================
#                            🌍 MACROECONOMÍA AVANZADA DE LARGO PLAZO 🌍
# ==============================================================================
#                               📌 TRABAJO FINAL 📌
# ------------------------------------------------------------------------------
#                                    AUTORAS
# ==============================================================================

# Establecer directorio de trabajo
setwd("C:/Users/Heitz/Desktop/Macro de LP/Trabajo FInal")
# ==============================================================================
# 1️⃣ CARGAR LIBRERÍAS
# ==============================================================================
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(scales)
library(ggrepel)
library(RColorBrewer)
# ==============================================================================
# 2️⃣ CARGAR BASES DE DATOS
# ==============================================================================


reports <- read_csv("reports.csv")
clients <- read_csv("clients.csv")
pib_sectores <- read_csv("DatosPIBConNaics.csv")

#  Limpiar reports y clients para seleccionar las variables de interés
reports_limpia <- reports %>% 
  select(lob_id, registrant_id, filing_year, filing_period_code, amount, is_no_activity)

clients_limpia <- clients %>% 
  select(lob_id, naics, client_name) %>% 
  filter(!is.na(naics) & naics != "")  # Eliminar valores NA en NAICS

#  Unir reports con clients usando lob_id
reports_clients <- reports_limpia %>%
  left_join(clients_limpia, by = "lob_id") %>%
  filter(!is.na(naics))  # Asegurar que no haya NA en NAICS

#  Filtrar para eliminar períodos H1 y H2
reports_clients <- reports_clients %>%
  filter(!(filing_period_code %in% c("H1", "H2")))

#  Guardar la base limpia de reports_clients
write_csv(reports_clients, "reports_clients.csv")

# ==============================================================================
# 3️⃣ LIMPIEZA Y PREPARACIÓN DE DATOS
# ==============================================================================

# Filtrar PIB eliminando categorías agregadas
categorias_excluir <- c("National income without capital consumption adjustment", 
                        "Domestic industries", 
                        "Private industries", 
                        "Rest of the world")

pib_sectores_filtrado <- pib_sectores %>% 
  filter(!sector %in% categorias_excluir)

#  Convertir NAICS en PIB a texto para evitar problemas en la unión
pib_sectores_filtrado <- pib_sectores_filtrado %>%
  mutate(naics = as.character(naics))

# Guardar la base limpia de PIB
write_csv(pib_sectores_filtrado, "DatosPIBConNaics_Filtrado_SinAgregados.csv")

# ==============================================================================
# 4️⃣ UNIÓN POR NAICS CONSIDERANDO 4 Y 2 DÍGITOS
# ==============================================================================

#Extraer los primeros 4 y 2 dígitos de NAICS en reports_clients
reports_clients <- reports_clients %>%
  mutate(naics4 = substr(naics, 1, 4), 
         naics2 = substr(naics, 1, 2))  # Extraer también los primeros 2 dígitos

# Intentar el cruce con NAICS de 4 dígitos
reports_clients_pib <- reports_clients %>%
  left_join(pib_sectores_filtrado, by = c("naics4" = "naics"))

#  Para los que quedaron sin PIB, intentar con los primeros 2 dígitos
reports_clients_pib <- reports_clients_pib %>%
  mutate(naics = ifelse(is.na(pib), naics2, naics)) %>%
  left_join(pib_sectores_filtrado, by = c("naics" = "naics"), suffix = c("", "_alt"))

# Si un NAICS de 2 dígitos logró el match, usar ese PIB
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

# ==============================================================================
# 5️⃣ AJUSTE A VALOR PRESENTE Y FILTRADO
# ==============================================================================

# Eliminar observaciones donde amount sea NA o cero
reports_clients_pib <- reports_clients_pib %>%
  filter(!is.na(amount) & amount > 0)

# Ajustar amount a valor presente usando el PIB como deflactor
max_pib <- max(reports_clients_pib$pib, na.rm = TRUE)  # Tomar el PIB más alto disponible
reports_clients_pib <- reports_clients_pib %>%
  mutate(amount_vp = amount * (max_pib / pib))

# Verificar cambios
summary(reports_clients_pib$amount_vp)

# ==============================================================================
# 7️⃣ CÁLCULO DE CRECIMIENTO DEL PIB POR SECTOR
# ==============================================================================

#  Colapsar la base sumando el PIB total por sector y trimestre
pib_sectorial <- reports_clients_pib %>%
  group_by(sector_alt, año_alt, cuartil_alt) %>%
  summarise(
    pib_total = sum(pib, na.rm = TRUE),
    amount_total = sum(amount, na.rm = TRUE)  # Sumar el amount también
  ) %>% 
  ungroup()
# Calcular logaritmo natural del PIB total por sector
pib_sectorial <- pib_sectorial %>%
  mutate(log_pib = log(pib_total))

#  Ordenar la base por sector y tiempo (año y cuartil)
pib_sectorial <- pib_sectorial %>%
  arrange(sector_alt, año_alt, cuartil_alt)

#  Calcular la tasa de crecimiento como la diferencia de logaritmos
pib_sectorial <- pib_sectorial %>%
  group_by(sector_alt) %>%
  mutate(tasa_crecimiento = log_pib - lag(log_pib)) %>%
  ungroup()

# Verificar el resultado
summary(pib_sectorial$tasa_crecimiento)

pib_sectorial <- pib_sectorial %>%
  mutate(log_amount_total = log(amount_total))


# Guardar la base final con PIB por sector
write_csv(pib_sectorial, "pib_sectorial.csv")

colnames(pib_sectorial)

# ==============================================================================
# 8️⃣ GRÁFICAS Y VISUALIZACIONES
# ==============================================================================
library(ggplot2)
library(dplyr)
library(scales)
# Eliminar NAs
pib_sectorial_agg <- pib_sectorial_clean %>%
  group_by(sector_alt) %>%
  summarise(
    log_amount_total = mean(log_amount_total, na.rm = TRUE),
    tasa_crecimiento = mean(tasa_crecimiento, na.rm = TRUE)
  )
scatter_lobby <- ggplot(pib_sectorial_agg, aes(x = log_amount_total, y = tasa_crecimiento)) +
  geom_point(color = "#1f78b4", size = 4, alpha = 0.7) +  
  geom_smooth(method = "lm", color = "gray40", linetype = "dashed", size = 1) +  
  labs(
    title = "Inversión en Lobby vs. Crecimiento del PIB por Sector",
    subtitle = "Escala logarítmica para inversión en lobby",
    x = "Log(Amount Total Lobby)",
    y = "Tasa Promedio de Crecimiento del PIB",
    caption = "Fuente: Datos de reports_clients_pib"
  ) +
  scale_x_continuous(labels = comma_format()) +  
  theme_bw() +  
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 10, color = "gray50")
  )

print(scatter_lobby)

# 🔍 **2. Limpieza de datos**
summary(pib_sectorial$año_alt)
sum(is.na(pib_sectorial$año_alt))
sum(is.infinite(pib_sectorial$año_alt))

pib_sectorial <- pib_sectorial %>% filter(!is.na(año_alt))
pib_sectorial$año_alt <- as.numeric(as.character(pib_sectorial$año_alt))

scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# 📈 **3. Evolución del PIB por sector**
pib_sectorial_anual <- pib_sectorial %>%
  group_by(sector_alt, año_alt) %>%
  summarise(tasa_crecimiento = mean(tasa_crecimiento, na.rm = TRUE))

# Filtrar solo algunos sectores para evitar saturación
sectores_destacados <- c("Mining", "Finance", "Construction", "Utilities", "Agriculture, forestry, fishing, and hunting")
pib_sectorial_filtrado <- pib_sectorial_anual %>% filter(sector_alt %in% sectores_destacados)

# Paleta de colores vibrantes
colores_vibrantes <- c("#E63946", "#457B9D", "#F4A261", "#2A9D8F", "#8E44AD", "#F77F00", "#1D3557")

grafico_pib <- ggplot(pib_sectorial_filtrado, aes(x = año_alt, y = tasa_crecimiento, color = sector_alt)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  # 🔹 Reducido el tamaño de los puntos
  theme_minimal() +  
  scale_color_manual(values = colores_vibrantes) +  
  labs(
    title = "Tasa de Crecimiento del PIB por Sector en el Tiempo",
    x = "Año",
    y = "Tasa de Crecimiento del PIB (%)",
    color = "Sector",
    caption = "Fuente: Datos de reports_clients_pib"
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",  
    legend.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
  )  

print(grafico_pib)

ggsave("grafico_pib_final.png", grafico_pib, width = 12, height = 7, dpi = 400, bg = "white")


#Ahora un histograma de amount total, que es la variable de inversion en lobby 
library(ggplot2)

# Crear histograma con estilo sofisticado
histograma_lobby <- ggplot(pib_sectorial, aes(x = log_amount_total)) +
  geom_histogram(fill = "#264653",  # Azul petróleo oscuro
                 color = "#E9C46A", # Amarillo dorado para bordes
                 bins = 50,         # Más bins para mejor detalle
                 alpha = 0.9) +     # Opacidad alta para elegancia
  labs(
    title = "Distribución de la Inversión en Lobby",
    subtitle = "Escala logarítmica aplicada",
    x = "Log(Monto Total de Lobby)",
    y = "Frecuencia",
    caption = "Fuente: Datos de reports_clients_pib"
  ) +
  scale_x_continuous(labels = scales::comma) +  # Formato de número con comas
  theme_minimal(base_family = "serif") +  
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#264653"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
    axis.title = element_text(face = "bold", size = 16, color = "#2A9D8F"),
    axis.text = element_text(size = 12, color = "black"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 1),
    panel.grid.minor = element_blank() # Oculta las líneas menores
  )  

# Mostrar el gráfico
print(histograma_lobby)


histograma_sectorial <- ggplot(pib_sectorial, aes(x = reorder(sector_alt, log(amount_total)), y = log(amount_total), fill = sector_alt)) +
  geom_col(color = "white", alpha = 0.8) +  
  labs(
    title = "Inversión Total en Lobby por Sector (Escala Logarítmica)",
    x = "Sector",
    y = "Log(Monto Total de Lobby)",
    fill = "Sector",
    caption = "Fuente: Datos de reports_clients_pib"
  ) +
  scale_fill_viridis_d(option = "plasma") +  
  scale_y_continuous(labels = scales::comma) +  
  theme_minimal(base_family = "serif") +  
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),  
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) + 
  coord_flip()  # Barras horizontales

print(histograma_sectorial)


ggsave("histograma_sectorial.png", histograma_sectorial, width = 12, height = 7, dpi = 400, bg = "white")





# ==============================================================================
# 🎯 FIN DEL SCRIPT 🎯
# ==============================================================================
