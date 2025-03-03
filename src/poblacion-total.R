library(sf)
library(ggplot2)
library(readxl)
library(dplyr)
library(viridis)
library(scales)
library(ggrepel)

# Leer datos
provincias <- st_read("data/geopackage/pcias_continental.gpkg")
poblacion <- read_excel("data/indec/c2022_tp_est_c1.xlsx",
                        sheet = "Cuadro 1 ", skip = 2)

# Limpiar los datos
poblacion[26,2] <- "Tierra del Fuego, Antártida e Islas del Atlántico Sur"
poblacion <- poblacion %>% select("Jurisdicción", "Población 2022")
colnames(poblacion)[2] <- "Poblacion"
poblacion <- poblacion[-c(4,5),]
poblacion <- poblacion[-c(26,27),]

# Unir los datos de poblacion con los poligonos de cada provincia
provincias_pob <- provincias %>%
  left_join(poblacion, by = c("NAM" = "Jurisdicción"))

# Crear centroides para cada provincia
prov_centroides <- st_centroid(provincias_pob)
prov_centroides <- cbind(prov_centroides, st_coordinates(prov_centroides))

# Crear el mapa
ggplot() +
  geom_sf(
    data = provincias_pob,
    aes(fill = Poblacion),
    color = "gray15",
    linewidth = 0.3) +
  
  scale_fill_gradient(low="#fee8c8",
                      high="#e34a33",
                      name = "Población",
                      trans = "log10",
                      ) +
  
  # labels mostrando la poblacion en cada centroide
  geom_text_repel(
    data = prov_centroides,
    aes(x = X, y = Y, label = scales::comma(Poblacion)),
    size = 4,
    color = "gray15",
    box.padding = 0.3,
    point.padding = 0,
    segment.color = "grey50",
    fontface = "bold",
  ) +
  
  # titulos
  labs(
    title = "Población por provincia en Argentina",
    subtitle = "Censo 2022",
    caption = "Fuente: INDEC",
  ) +
  
  # tema
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    plot.caption = element_text(size = 8, color = "grey40"),
    legend.position = "right",
    
    panel.background = element_rect(fill = "wheat1", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

# Guardar el mapa
ggsave("img/poblacion-total.png", width = 20, height = 30, units="cm", dpi = 300, bg = "wheat1")
browseURL("img/poblacion-total.png")
