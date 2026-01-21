# Instalando os pacotes -----

install.packages(c("tidyverse",
                   "sf",
                   "vegan",
                   "terra",
                   "tidyterra",
                   "betapart"))

# Carregando os pacotes ----

library(tidyverse)

library(magrittr)

library(sf)

library(vegan)

library(terra)

library(tidyterra)

library(betapart)

# Dados ----

## Lista de espécies ----

### Importando ----

sps <- readr::read_csv("ATLANTIC_AMPHIBIANS_species.csv")

### Visualizando ----

sps

sps |> dplyr::glimpse()

### Tratando ----

sps %<>%
  dplyr::select(id, valid_name) %<>%
  dplyr::filter(!valid_name |> is.na()) %<>%
  dplyr::rename("species" = valid_name)

sps

## Ocorrências ----

### Importando ----

occ <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")

### Visualizando ----

occ

occ |> dplyr::glimpse()

### Transformando em shapefile ----

occ_sf <- occ |>
  dplyr::select(id, longitude, latitude) |>
  dplyr::left_join(sps, by = "id") |>
  dplyr::filter(!longitude |> is.na() & !latitude |> is.na()) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4674)

occ_sf

ggplot() +
  geom_sf(data = occ_sf)

## Shapefile da Mata Atlântica ----

### Importando ----

### Visualizando ----

## Shapefile do Brasil ----

### Importando ----

### Visualizando ----

# Criando a grade ----

# Montando as comunidades por grades ----

# Distribuição dos valores de riqueza ----

# Distribuição dos valores de diversidade alfa ----

# Distribuição dos valores de diversidade beta ----

