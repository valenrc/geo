library(sf)
library(ggplot2)
library(readxl)
library(dplyr)
library(viridis)
library(scales)
library(ggrepel)

# Cargar el archivo geopackage con los poligonos del territorio
provincias <- st_read("data/geopackage/pcias_continental.gpkg")

# Cargar el spreadsheet con la informacion
poblacion <- read_excel("data/indec/c2022_tp_est_c1.xlsx", 
                        sheet = "Cuadro 1 ", skip = 2)

# Limpiar los datos
poblacion[26,2] <- "Tierra del Fuego, Antártida e Islas del Atlántico Sur"
poblacion <- poblacion %>% select("Jurisdicción", "Población 2022")
colnames(poblacion)[2] <- "Poblacion"
poblacion <- poblacion[-c(4,5),]
poblacion <- poblacion[-c(26,27),]

# Unir los datos de poblacion con las provincias
provincias_pob <- provincias %>%
  left_join(poblacion, by = c("NAM" = "Jurisdicción"))

# calcular la densidad poblacional para cada provincia
provincias_pob <- provincias_pob %>%
  mutate(Area = as.numeric(st_area(geom)) / 1000000, # convertir a km2
         Densidad = Poblacion / Area)

# crear centroides para cada provincia
prov_centroides <- st_centroid(provincias_pob)
prov_centroides <- cbind(prov_centroides, st_coordinates(prov_centroides))

# crear el plot del mapa
ggplot() +
  geom_sf(
    data = provincias_pob,
    aes(fill = Densidad),
    color = "gray15",
    linewidth = 0.3) +
  
  scale_fill_gradient(
    low="#deebf7",
    high="#3182bd",
    trans = "log10",
    labels = label_number(big.mark = ".", decimal.mark = ","),
    name = "Densidad Poblacional") +
  
  # titulos
  labs(
    title = "Densidad poblacional por provincia en Argentina",
    subtitle = "Habitantes por kilometro cuadrado",
    caption = "Fuente: INDEC - Censo 2022",
  ) +
  
  # labels mostrando la densidad en cada centroide
  geom_text_repel(
    data = prov_centroides,
    aes(x = X, y = Y, label = scales::number(Densidad, accuracy = 0.1)),
    size = 4,
    color = "black",
    box.padding = 0.5,
    point.padding = 0,
    segment.color = "grey50",
    fontface= "bold",
  ) +
  
  # tema
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    plot.caption = element_text(size = 8, color = "grey40"),
    legend.position = "right",
    
    panel.background = element_rect(fill = "aliceblue", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )


# Guardarlo como imagen
ggsave("img/densidad-poblacional.png", width=20, height = 30, units = "cm", dpi=300, bg = "aliceblue")
browseURL("mapa.png")
