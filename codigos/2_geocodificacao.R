
# abre as bibliotecas
library(readxl)
library(tidyverse)
library(sf)
library(stringr)

# importa os dados
cadunico <- read_csv2("resultados/dados1.csv", locale(encoding = "latin1"), col_names = TRUE, col_types = NULL)

# arruma a geocodificacao de alguns dados
sugestao <- read_excel("dados/geocodificacao/Sugestao_LAT_LON.xlsx") %>%
  st_as_sf(coords = c("LON_Maps", "LAT_Maps"), crs = 4326) %>%
  select(`Código familiar`)

geocode <- read_sf("dados/geocodificacao/Familias_CadUnico_TOTAL_Finalizado.shp") %>%
  rename("Código familiar" = "Código.fa") %>%
  mutate(`Código familiar` = as.numeric(`Código familiar`)) %>%
  filter(!`Código familiar` %in% sugestao$`Código familiar`) %>%
  select(`Código familiar`) %>%
  rbind(sugestao)

# uniao entre cadunico e geocode
cadunico_geocode <- full_join(geocode, cadunico)
cadunico_geocode

# exporta os shapefiles
write_sf(cadunico_geocode, "resultados/dados2.gpkg")
