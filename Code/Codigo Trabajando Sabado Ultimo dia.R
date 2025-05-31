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

pib_sectorial <- read_csv("pib_sectorial.csv")

# Supongamos que ya cargaste tu base:
pib_sectorial <- read_csv("pib_sectorial.csv")

# Calcula y guarda la variable ‘log_lobby_relativo’ en el mismo data frame:
pib_sectorial <- pib_sectorial %>%
  mutate(log_lobby_relativo = log(amount_total / pib_total))

# Ahora ejecuta la prueba de correlación (Pearson) entre log_lobby_relativo y tasa_crecimiento:
cor.test(
  x = pib_sectorial$log_lobby_relativo,
  y = pib_sectorial$tasa_crecimiento,
  use = "pairwise.complete.obs"
)

# Comentario importante, sin normalizar los datos obtenemos una grafica con 
#correlacion negativa, tal vez mas significativa que la positiva que obtuvimos antes\

#grafico 

# ————————————————
# Cargar bibliotecas necesarias
# ————————————————
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(ggrepel)

# ————————————————
# 1️⃣ Leer la base ya procesada
# ————————————————
pib_sectorial <- read_csv("pib_sectorial.csv")

# ————————————————
# 2️⃣ Calcular log_lobby_relativo “al vuelo”
# ————————————————
pib_sectorial <- pib_sectorial %>%
  mutate(
    # log(amount_total / pib_total)
    log_lobby_relativo = log(amount_total / pib_total)
  ) %>%
  # Filtramos sólo las filas en las que ambas variables son finitas
  filter(is.finite(log_lobby_relativo), is.finite(tasa_crecimiento))

# ————————————————
# 3️⃣ Ver cuántas observaciones válidas hay (opcional)
# ————————————————
cat("Observaciones válidas:", nrow(pib_sectorial), "\n")

# ————————————————
# 4️⃣ Gráfico de dispersión + recta de regresión
# ————————————————
ggplot(pib_sectorial, aes(x = log_lobby_relativo, y = tasa_crecimiento)) +
  geom_point(size = 3, color = "#33a02c", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "gray30", linetype = "dashed") +
  labs(
    title    = "Lobby Relativo (log) vs Tasa de Crecimiento del PIB Sectorial",
    subtitle = "Relación trimestral sectorial en escala logarítmica",
    x        = "Log(Lobby relativo)",
    y        = "Δ log(PIB sectorial)",
    caption  = "Fuente: BEA y LobbyView"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title    = element_text(size = 13, face = "bold"),
    axis.text     = element_text(size = 11)
  )

###############################################################################
#         Pegue de PIB por trabajador
###############################################################################

#############################################################################
#         NORMALIZACION DE LOS DATOS
#############################################################################

# —————————————————————————————————————————————————————————————————————
# 1️⃣ Asumimos que ya cargaste tu base “pib_sectorial.csv” y las librerías:
#
#    library(dplyr)
#    library(readr)
#    library(ggplot2)
#    library(scales)
#    library(ggrepel)
# 
#    pib_sectorial <- read_csv("pib_sectorial.csv")
# —————————————————————————————————————————————————————————————————————

# —————————————————————————————————————————————————————————————————————
# 2️⃣ Filtrar y crear Z‐scores (normalizar) de log_lobby_relativo y tasa_crecimiento
# —————————————————————————————————————————————————————————————————————

pib_norm <- pib_sectorial %>%
  # Primero, dejamos sólo las filas donde ambas variables sean finitas
  filter(is.finite(log_lobby_relativo), is.finite(tasa_crecimiento)) %>%
  # Creamos dos nuevas columnas con su Z‐score (media = 0, sd = 1)
  mutate(
    z_log_lobby = as.numeric(scale(log_lobby_relativo)),    # Z‐score de log(lobby_relativo)
    z_crec       = as.numeric(scale(tasa_crecimiento))      # Z‐score de tasa_crecimiento
  )

# Cantidad de observaciones tras filtrar y normalizar
cat("Observaciones válidas para normalizar y correlacionar:", nrow(pib_norm), "\n\n")

# —————————————————————————————————————————————————————————————————————
# 3️⃣ Calcular correlación de Pearson sobre los Z‐scores normalizados
# —————————————————————————————————————————————————————————————————————

cat("--- CORRELACIÓN ENTRE Z‐scores ---\n")
corr_norm <- cor.test(pib_norm$z_log_lobby, pib_norm$z_crec)
print(corr_norm)

# Si lo prefieres, también puedes extraer sólo el coeficiente r:
r_value <- corr_norm$estimate
cat("\nCoeficiente r (Pearson) normalizado:", round(r_value, 3), "\n")
cat("p‐value:", signif(corr_norm$p.value, 3), "\n\n")

# —————————————————————————————————————————————————————————————————————
# 4️⃣ Gráfico en escala Z‐score (puntos + recta de regresión)
# —————————————————————————————————————————————————————————————————————

ggplot(pib_norm, aes(x = z_log_lobby, y = z_crec)) +
  geom_hline(yintercept = 0, color = "gray80") +
  geom_vline(xintercept = 0, color = "gray80") +
  geom_point(size = 3, color = "#1f78b4", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", linetype = "dashed") +
  labs(
    title    = "Lobby Relativo vs Crecimiento del PIB (Z‐scores)",
    subtitle = "Cada punto es un sector × trimestre, normalizado a Z‐score",
    x        = "Z‐score de log(Lobby relativo)",
    y        = "Z‐score de Δ log(PIB sectorial)",
    caption  = "Fuente: BEA y LobbyView (datos normalizados)"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title    = element_text(size = 13, face = "bold"),
    axis.text     = element_text(size = 11)
  )





# ==============================================================================
# 🎯 FIN DEL SCRIPT 🎯
# ==============================================================================


library(readxl)
# Carga la hoja entera para inspeccionar sus columnas
trabajadores_raw <- read_excel("DatosTrabajadorPorIndustria.xlsx", sheet = "Table", skip = 5)
# Muestra los nombres de todas las columnas
print(names(trabajadores_raw))
# Y también un vistazo rápido a las primeras filas:
head(trabajadores_raw)



# ==============================================================================
#                          🔍  PIVOT + JOIN CON PIB POR TRABAJADOR 
# ==============================================================================

# 0️⃣ Instalar/cargar paquetes
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  readr,
  readxl,
  ggplot2,
  scales,
  ggrepel,
  tidyr
)

# ------------------------------------------------------------------------
# 1️⃣  CARGAR LOS DATOS YA PREVIAMENTE PROCESADOS DE PIB POR SECTOR
# ------------------------------------------------------------------------
#   (a) Esta tabla debe contener al menos: sector_alt, año_alt, cuartil_alt,
#       pib_total, amount_total, log_pib, tasa_crecimiento, log_amount_total.
#   (b) Por lo que entiendo, ya la guardaste en "pib_sectorial.csv".
# ------------------------------------------------------------------------
pib_sectorial <- read_csv("pib_sectorial.csv",
                          show_col_types = FALSE
)

# Ver las primeras filas y columnas
cat("\n--- glimpse(pib_sectorial) ---\n")
glimpse(pib_sectorial)

# ------------------------------------------------------------------------
# 2️⃣  CARGAR LA TABLA DE TRABAJADORES DESDE EL EXCEL
# ------------------------------------------------------------------------
#   (a) Tu hoja se llama "Table" y empezaste a leer a partir de la fila 6 (skip = 5).
#   (b) Mostraremos los nombres de columnas para ver cómo se llaman los años.
# ------------------------------------------------------------------------
trabajadores_raw <- read_excel(
  "DatosTrabajadorPorIndustria.xlsx",
  sheet = "Table",
  skip = 5
)

cat("\n--- names(trabajadores_raw) ---\n")
print(names(trabajadores_raw))

cat("\n--- head(trabajadores_raw) ---\n")
print(head(trabajadores_raw, n = 6))


# ------------------------------------------------------------------------
# 3️⃣  PIVOTAR DE “ANCHO” A “LARGO” (Sector × Año × Trabajadores)
# ------------------------------------------------------------------------
#   (a) Por el head que mostraste, vemos que la segunda columna se llama "...2",
#       y que las columnas “2016” a “2023” sí existen, por ejemplo: `2016`, `2017`, ..., `2023`.
#   (b) Renombraremos “...2” como “Industria” y mantendremos solo esas columnas.
# ------------------------------------------------------------------------
trabajadores_long <- trabajadores_raw %>%
  rename(Industria = `...2`) %>%
  # Filtramos:
  filter(
    !is.na(Industria),
    Industria != "", 
    # Si quieres descartar ciertos agregados como “Domestic industries” o “Private industries”,
    # puedes añadir: 
    # !(Industria %in% c("Domestic industries", "Private industries"))
  ) %>%
  # Ahora aseguramos que existan las columnas 2016–2023
  select(Industria, `2016`:`2023`) %>%
  pivot_longer(
    cols      = -Industria,
    names_to  = "año_alt",
    values_to = "trabajadores"
  ) %>%
  mutate(
    año_alt     = as.integer(año_alt),
    trabajadores = as.numeric(trabajadores)
  ) %>%
  filter(!is.na(trabajadores))

cat("\n--- head(trabajadores_long) ---\n")
print(head(trabajadores_long, n = 6))

# ------------------------------------------------------------------------
# 4️⃣  RECODIFICAR “Industria” → “sector_alt”
# ------------------------------------------------------------------------
#   (a) Inspecciona los valores únicos en ambas tablas:
cat("\n--- unique(trabajadores_long$Industria) ---\n")
print(sort(unique(trabajadores_long$Industria)))

cat("\n--- unique(pib_sectorial$sector_alt) ---\n")
print(sort(unique(pib_sectorial$sector_alt)))

#   (b) Define un vector de equivalencias EXACTAS (caso‐sensible). 
#       Asegúrate de que el texto a la derecha coincide con sector_alt de pib_sectorial.
equivalencias <- c(
  "Agriculture, forestry, fishing, and hunting"     = "Agriculture, forestry, fishing, and hunting",
  "Mining"                                          = "Mining",
  "Utilities"                                       = "Utilities",
  "Construction"                                    = "Construction",
  "Manufacturing"                                   = "Manufacturing",
  "Wholesale trade"                                 = "Wholesale trade",
  "Retail trade"                                    = "Retail trade",
  "Transportation and warehousing"                  = "Transportation and warehousing",
  "Information"                                     = "Information",
  "Finance and insurance"                           = "Finance and insurance",
  "Real estate and rental and leasing"              = "Real estate and rental and leasing",
  "Professional, scientific, and technical services"= "Professional, scientific, and technical services",
  "Management of companies and enterprises"          = "Management of companies and enterprises",
  "Administrative and support and waste management and remediation services" = 
    "Administrative and support and waste management and remediation services",
  "Educational services"                            = "Educational services",
  "Health care and social assistance"                = "Health care and social assistance",
  "Arts, entertainment, and recreation"              = "Arts, entertainment, and recreation",
  "Accommodation and food services"                  = "Accommodation and food services",
  "Other services (except government and government enterprises)" = 
    "Other services (except government and government enterprises)",
  "Federal, civilian, and military, and postal"       = "Federal, civilian, and military, and postal",
  "State and local government and other government enterprises" = 
    "State and local government and other government enterprises"
  # … Agrega todas las que veas necesarias hasta cubrir todos los nombres que 
  #   aparecen en trabajador_long y que correspondan a algún sector_alt en pib_sectorial
)

#   (c) Recodifica y filtra sólo las que emparejan:
trabajadores_long <- trabajadores_long %>%
  mutate(
    sector_alt = recode(Industria, !!!equivalencias)
  ) %>%
  filter(!is.na(sector_alt))

cat("\n--- head(trabajadores_long tras recode) ---\n")
print(head(trabajadores_long, n = 6))

# ------------------------------------------------------------------------
# 5️⃣  HACER EL JOIN: pib_sectorial + trabajadores_long
# ------------------------------------------------------------------------
#   Asegúrate de que ambos tienen año_alt como entero e idéntico.
pib_sectorial <- pib_sectorial %>%
  mutate(año_alt = as.integer(año_alt))

trabajadores_long <- trabajadores_long %>%
  mutate(año_alt = as.integer(año_alt))

#   (a) JOIN por sector_alt + año_alt
base_final <- pib_sectorial %>%
  left_join(
    trabajadores_long,
    by = c("sector_alt", "año_alt")
  ) %>%
  filter(!is.na(trabajadores) & trabajadores > 0)

cat("\nFilas tras el JOIN (sector_alt × año_alt):", nrow(base_final), "\n")

#   Si esto imprime 0, revisa: 
#     • Que cada sector_alt×año_alt exista en ambas tablas.
#     • Que no haya espacios en blanco extra o mayúsculas/minúsculas distintas.
# ------------------------------------------------------------------------
#  Ver un ejemplo de unas cuantas filas
cat("\n--- head(base_final) ---\n")
print(head(base_final, n = 6))


# ------------------------------------------------------------------------
# 6️⃣  CALCULAR PIB POR TRABAJADOR, LOGARITMOS Y “LOBBY RELATIVO”
# ------------------------------------------------------------------------
if (nrow(base_final) >= 3) {
  base_final <- base_final %>%
    mutate(
      # PIB per cápita laboral
      pib_por_trabajador   = pib_total / trabajadores,
      log_pib_trabajador   = log(pib_por_trabajador),
      # Lobby relativo = amount_total / pib_total
      lobby_relativo       = amount_total / pib_total,
      log_lobby_relativo   = log(lobby_relativo)
    ) %>%
    filter(
      is.finite(log_pib_trabajador),
      is.finite(log_lobby_relativo)
    )
  
  cat("\nFilas válidas con PIB por trabajador y lobby_relativo:", nrow(base_final), "\n")
  
  # ----------------------------------------------------------------------
  # 7️⃣  CORRELACIÓN Y REGRESIÓN (LOG-LOG)
  # ----------------------------------------------------------------------
  cat("\n--- CORRELACIÓN (log_lobby_relativo vs log_pib_trabajador) ---\n")
  corr_norm <- cor.test(
    base_final$log_lobby_relativo,
    base_final$log_pib_trabajador
  )
  print(corr_norm)
  
  cat("\n--- REGRESIÓN LINEAL (log_pib_trabajador ~ log_lobby_relativo) ---\n")
  modelo <- lm(log_pib_trabajador ~ log_lobby_relativo, data = base_final)
  print(summary(modelo))
  
  # ──────────────────────────────────────────────────────────────────────────────
  # 8️⃣ GRÁFICO CON CORRELACIÓN ANOTADA
  # ──────────────────────────────────────────────────────────────────────────────
  
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  
  # (1) Supongamos que ya tienes 'base_final' EXACTAMENTE como en el paso anterior,
  #     con estas columnas imprescindibles:
  #     • sector_alt
  #     • año_alt
  #     • cuartil_alt
  #     • pib_total
  #     • amount_total
  #     • trabajadores
  #     • log_pib_trabajador
  #     • log_lobby_relativo
  
  #    Además, 'puntos_representativos' (medianas por sector) quedó así:
  puntos_representativos <- base_final %>%
    group_by(sector_alt) %>%
    summarise(
      mediana_log_lobby   = median(log_lobby_relativo, na.rm = TRUE),
      mediana_log_pib_trab = median(log_pib_trabajador,  na.rm = TRUE)
    ) %>%
    ungroup()
  
  # --------------------------------------------------------------------------
  # (2) Calculamos la correlación Pearson y extraemos r y p-value
  # --------------------------------------------------------------------------
  corr_test <- cor.test(
    base_final$log_lobby_relativo,
    base_final$log_pib_trabajador
  )
  
  r_value <- corr_test$estimate      # coeficiente r
  p_value <- corr_test$p.value       # p-value
  
  # Formateamos los valores para que se vean “limpios” en la etiqueta
  r_txt <- sprintf("r = %.3f", r_value)
  p_txt <- sprintf("p = %.3f", p_value)
  etiqueta_correl <- paste0(r_txt, ",   ", p_txt)
  
  # --------------------------------------------------------------------------
  # (3) Construimos el gráfico y anotamos en la esquina superior izquierda
  # --------------------------------------------------------------------------
  ggplot() +
    # ── (a) nube completa, en gris muy tenue ─────────────────────────────────
    geom_point(
      data  = base_final,
      aes(x = log_lobby_relativo, y = log_pib_trabajador),
      color = "grey70",
      alpha = 0.4,
      size  = 2
    ) +
    # ── (b) puntos “mediana” de cada sector, en azul ─────────────────────────
    geom_point(
      data  = puntos_representativos,
      aes(x = mediana_log_lobby, y = mediana_log_pib_trab),
      color = "#1f78b4",
      size  = 3.5
    ) +
    # ── (c) etiquetas de texto en la mediana ────────────────────────────────
    geom_text_repel(
      data = puntos_representativos,
      aes(x = mediana_log_lobby, y = mediana_log_pib_trab, label = sector_alt),
      size         = 3.0,
      segment.color = "grey50",
      box.padding   = 0.3,
      max.overlaps  = Inf
    ) +
    # ── (d) recta de regresión con banda de confianza ────────────────────────
    geom_smooth(
      data     = base_final,
      aes(x = log_lobby_relativo, y = log_pib_trabajador),
      method   = "lm",
      se       = TRUE,
      color    = "black",
      linetype = "dashed",
      size     = 0.6,
      fill     = "grey80"
    ) +
    # ── (e) anotación de r y p‐value en la esquina superior izquierda ─────────
    annotate(
      "text",
      x       = -Inf,       # posicionamiento automático en el borde izquierdo
      y       = Inf,        # posicionamiento automático en el borde superior
      label   = etiqueta_correl,
      hjust   = -0.1,       # un poquito hacia la izquierda fuera del gráfico
      vjust   =  1.2,       # un poquito hacia arriba fuera del gráfico
      size    = 4.0,
      color   = "black",
      fontface= "bold"
    ) +
    # ── (f) títulos y ejes ────────────────────────────────────────────────────
    labs(
      title    = "log(PIB por Trabajador) vs log(Lobby Relativo)",
      subtitle = "Cada punto gris = sector × año (trimestre) │ Medianas azules = cada sector",
      x        = "log(Lobby Relativo = amount_total / pib_total)",
      y        = "log(PIB por Trabajador)",
      caption  = "Fuente: BEA y LobbyView\nCorrelación: r y p‐value anotados arriba"
    ) +
    scale_x_continuous(
      labels = comma_format(accuracy = 0.01)
    ) +
    scale_y_continuous(
      labels = comma_format(accuracy = 0.01)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle    = element_text(size = 12, hjust = 0.5),
      plot.caption     = element_text(size =  9, hjust = 1, color = "grey40"),
      axis.title       = element_text(face = "bold", size = 13),
      axis.text        = element_text(size = 11),
      panel.grid.minor = element_blank()
    )
  
