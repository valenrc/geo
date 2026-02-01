# fuentes: https://www.argentina.gob.ar/seguridad/servicio-nacional-de-manejo-del-fuego/evaluacion-de-peligro-y-alerta-temprana/reporte

library(tidyverse)
library(sf)
library(ggrepel)
library(scales)
library(showtext)
library(viridis)
library(ggspatial)

#fecha <- format(Sys.Date(), "%d de %B de %Y")
#fecha_title <- format(Sys.Date(), "%d_%m_%Y")

fecha <-"13 de Enero de 2026"
fecha_title <- "13_01_2026"

map_name <- "incendios_" %>% paste0(fecha_title)
output_path <- "img/incendios/"

# configurar fuente
font_add_google("IBM Plex Sans Condensed", "ibm")
showtext_auto()

# paleta de colores para tema oscuro (black maps reference :p)
col_bg <- "#1a1a1a"
col_map_fill <- "#2d2d2d"
col_map_border <- "#404040"
col_text <- "#e8e8e8"
col_text_secondary <- "#888888"

# función para convertir coordenadas DMS a decimal
dms_to_decimal <- function(dms_string) {
  numeros <- str_extract_all(dms_string, "[0-9]+\\.?[0-9]*")[[1]]
  grados <- as.numeric(numeros[1])
  minutos <- as.numeric(numeros[2])
  segundos <- as.numeric(numeros[3])
  
  # calcular decimal
  decimal <- grados + minutos / 60 + segundos / 3600
  
  # determinar signo (S y O son negativos)
  if (str_detect(dms_string, "S|O")) {
    decimal <- -decimal
  }
  
  return(decimal)
}

# cargar geopackage con poligonos del territorio argentino
provincias <- st_read("data/geopackage/pcias_continental.gpkg")

# cargar .csv con los datos
incendios <- read_csv("data/snmf/incendios_13_01_2026.csv")

# procesar todos los incendios
incendios_procesado <- incendios %>%
  rowwise() %>%
  mutate(
    # convertir coordenadas DMS a decimal
    lon = dms_to_decimal(LONGITUD),
    lat = dms_to_decimal(LATITUD),
    # limpiar estado (quitar fecha de "Extinguido (fecha)")
    estado = str_extract(ESTADO, "^[A-Za-z]+")
  ) %>%
  ungroup()

# crear objeto sf con todos los puntos
incendios_sf <- st_as_sf(
  incendios_procesado,
  coords = c("lon", "lat"),
  crs = 4326
)

# definir colores para cada estado (ajustados para fondo oscuro)
colores_estado <- c(
  "Activo"     = "#ff4444",
  "Contenido"  = "#ff8c42",
  "Controlado" = "#ffd166",
  "Extinguido" = "#06d6a0"
)

# crear el mapa
ggplot() +
  geom_sf(
    data = provincias,
    fill = col_map_fill,
    color = col_map_border,
    linewidth = 0.15
  ) +
  # agregar puntos de incendios coloreados por estado
  geom_sf(
    data = incendios_sf,
    aes(fill = estado),
    shape = 21,
    size = 2,
    color = "gray60",
    stroke = 0.2,
    alpha = 0.4
  ) +
  scale_fill_manual(
    values = colores_estado,
    name = NULL
  ) +
  # escala de distancia
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    #pad_x = unit(0.5, "cm"),
    #pad_y = unit(0.5, "cm"),
    bar_cols = c(col_text_secondary, col_bg),
    line_col = col_text_secondary,
    text_col = col_text_secondary,
    text_family = "ibm",
    text_cex = 0.8,
    style = "ticks"
  ) +
  # flecha de norte
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(1, "cm"),
    height = unit(1.8, "cm"),
    width = unit(1.6, "cm"),
    style = north_arrow_minimal(
      line_width = 0.7,
      line_col = col_text_secondary,
      fill = col_map_fill,
      text_col = "white",
      text_family = "",
      text_face = NULL,
      text_size = 10
    )
  ) +
  labs(
    title = "REPORTE DE INCENDIOS FORESTALES, RURALES Y/O DE INTERFASE URBANO FORESTAL",
    subtitle = fecha,
    caption = "Fuente: Servicio Nacional de Manejo del Fuego · snmf.gob.ar"
  ) +
  # tema oscuro minimalista
  theme_void() +
  theme(
    # meridianos y paralelos
    panel.grid.major = element_line(color = "gray14", linewidth = 0.2, linetype="dotted"),
    # fondo
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    
    # títulos
    plot.title = element_text(
      family = "ibm",
      face = "bold",
      size = 45,
      hjust = 0.5,
      color = col_text,
      margin = margin(t = 15, b = 5)
    ),
    plot.subtitle = element_text(
      family = "ibm",
      size = 35,
      hjust = 0.5,
      color = col_text,
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      family = "ibm",
      size = 30,
      hjust = 0.5,
      color = col_text_secondary,
      margin = margin(t = 15)
    ),
    
    # leyenda a la derecha
    legend.position = c(0.92, 0.25),
    legend.justification = c(1, 0),
    legend.background = element_rect(
      fill = alpha(col_bg, 0.8),
      color = col_map_border,
      linewidth = 0.3
    ),
    legend.margin = margin(8, 10, 8, 10),
    legend.text = element_text(
      family = "ibm",
      size = 40,
      color = col_text_secondary
    ),
    legend.key.size = unit(1, "cm"),
    legend.key = element_rect(fill = NA, color = NA),
    
    # márgenes generales
    plot.margin = margin(10, 10, 10, 10)
  )

# guardar el mapa
ggsave(
  paste0(output_path, map_name, ".png"),
  width = 25,
  height = 30,
  units = "cm",
  dpi = 300,
  bg = col_bg
)
browseURL(paste0(output_path, map_name, ".png"))
