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

library(terra)

library(vegan)

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

template <- grade |>
  terra::vect() |>
  terra::rast(res = res_grade)

template

# Distribuição dos valores de riqueza ----

## Calculando a riqueza ----

riqueza <- comp_occ |>
  tibble::column_to_rownames("ID") |>
  vegan::specnumber()

riqueza

## Gerando um dataframe com os dados de riqueza ----

df_riqueza <- tibble::tibble(ID = comp_occ$ID,
                             Riqueza = riqueza)

df_riqueza

## Adicionando uma coluna no shapefile de grade com as informações de riqueza ----

grade %<>%
  dplyr::left_join(df_riqueza,
                   by = "ID")

grade

## Rasterizando ----

# Distribuição dos valores de diversidade alfa ----

# Distribuição dos valores de diversidade beta ----

