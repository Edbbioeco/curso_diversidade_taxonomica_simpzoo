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

ma <- sf::st_read("mata_atlantica.shp")

### Visualizando ----

ma

ggplot() +
  geom_sf(data = ma, color = "darkgreen", fill = "forestgreen") +
  geom_sf(data = occ_sf)

## Shapefile do Brasil ----

### Importando ----

br <- sf::st_read("brasil.shp")

### Visualizando ----

br

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ma, color = "darkgreen", fill = "forestgreen") +
  geom_sf(data = occ_sf)

# Criando a grade ----

## Escolhando a resolução ----

res_grade <- (50 * 1) / 111.3194

res_grade

## Gerando a grade geral ----

grade <- ma |>
  sf::st_make_grid(cellsize = res_grade) |>
  sf::st_sf() |>
  sf::st_join(ma) |>
  tidyr::drop_na() |>
  dplyr::mutate(ID = dplyr::row_number())

## Visualizando a grade ----

grade

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = grade, color = "darkgreen", fill = NA) +
  geom_sf(data = occ_sf)

# Montando as comunidades por grades ----

comp_occ <- occ_sf |>
  sf::st_join(grade) |>
  tidyr::drop_na() |>
  as.data.frame() |>
  dplyr::summarise(Abundancia = dplyr::n(),
                   .by = c(ID, species)) |>
  tidyr::pivot_wider(names_from = species,
                     values_from = Abundancia,
                     values_fill = 0)

comp_occ

# Raster de base ----

# Distribuição dos valores de riqueza ----

# Distribuição dos valores de diversidade alfa ----

# Distribuição dos valores de diversidade beta ----

