library(tidyverse)
library(sf)
library(ggrepel)
library(scales)
library(showtext) # Para fuentes personalizadas
library(viridis) # Para paletas de colores más estéticas

# Cargar fuentes
font_add_google("Roboto Condensed", "roboto")
font_add_google("Roboto Mono", "roboto_mono")
showtext_auto()

# Cargar datos
barrios <- st_read("data/geojson/caba-barrios.geojson")
delitos <- read_csv2("data/ba-data/delitos_2023.csv")

# Limpiar datos
barrios <- barrios %>% mutate(nombre = toupper(nombre))

# Contar delitos por barrio
delitos_por_barrio <- delitos %>%
  mutate(barrio = case_when(
    barrio %in% c("0", "SD", "Sin geo", "NULL", "NO ESPECIFICADA", NA) ~ "NO DATA",
    TRUE ~ barrio
  )) %>% 
  group_by(barrio) %>%
  summarise(cant_delitos = n()) %>%
  filter(cant_delitos > 3)

# Unir datos
delitos_mapa <- left_join(barrios, delitos_por_barrio, by = c("nombre" = "barrio"))

# Calcular centroides y añadir coordenadas
caba_centr <- st_centroid(delitos_mapa) %>% 
  cbind(st_coordinates(.))

# Definir límites para la escala de color
max_delitos <- max(delitos_por_barrio$cant_delitos, na.rm = TRUE)
breaks <- seq(0, max_delitos, length.out = 5) %>% round()

# Crear mapa mejorado
ggplot() + 
  # Fondo negro para un aspecto nocturno
  theme_minimal(base_family = "roboto", base_size = 16) +
  theme(
    plot.background = element_rect(fill = "#332728", color = NA),
    panel.background = element_rect(fill = "#332728", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(family = "roboto", size = 40, color = "white", face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(family = "roboto", size = 30, color = "#CCCCCC", hjust = 0.5, margin = margin(b = 20)),
    plot.caption = element_text(family = "roboto_mono", size = 25, color = "#999999", hjust = 1, margin = margin(t = 20)),
    legend.title = element_text(family = "roboto", color = "white"),
    legend.text = element_text(family = "roboto", color = "#CCCCCC"),
    legend.background = element_rect(fill = "#332728", color = NA),
    legend.position = "bottom",
    legend.key.width = unit(6, "cm"),
    legend.key.height = unit(2, "cm")
  ) +
  
  # Barrios con delitos
  geom_sf(
    data = delitos_mapa,
    aes(fill = cant_delitos),
    color = "#333333",
    size = 0.2
  ) +
  
  # Nombres de todos los barrios
  geom_text_repel(
    data = caba_centr,
    aes(x = X, y = Y, label = nombre),
    size = 8,
    color = "white",
    family = "roboto",
    fontface = "bold",
    bg.color = "#00000080",
    bg.r = 0.1,
    segment.color = "#FFFFFF50",
    segment.size = 0.2,
    force = 3,
    max.overlaps = 30,
    min.segment.length = 0.1
  ) +
  
  # Paleta de colores personalizada de amarillo a rojo
  scale_fill_gradient(
    low = "#FFDA00",
    high = "#FF2400",
    name = "Cantidad de delitos",
    labels = scales::comma,
    breaks = breaks,
    na.value = "#333333",
    guide = guide_colorbar(
      title.size = 20,
      barwidth = 23,
      barheight = 1.3,
      frame.color = "white",
      ticks.color = "white"
    )
  ) +
  
  # Títulos con estilo
  labs(
    title = "MAPA DE DELITOS* EN BUENOS AIRES",
    subtitle = "Distribución de incidentes por barrio · 2023",
    caption = "* Incluye: Viales/Lesiones/Amenazas/Robos  |  Fuente: BA Data"
  ) +
  
  # Coordenadas fijas
  coord_sf()

# Guardar el mapa con dimensiones específicas
ggsave("img/delitos_por_barrio_caba", width = 22, height = 22, units = "cm", dpi = 300, bg = "#332728")
browseURL("img/delitos_barrio_caba_cool.png")

