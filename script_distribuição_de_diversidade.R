# Instalando os pacotes -----

install.packages(c("tidyverse",
                   "sf",
                   "vegan",
                   "terra",
                   "tidyterra",
                   "betapart"))

# Carregando os pacotes ----

library(tidyverse)

library(sf)

library(vegan)

library(terra)

library(tidyterra)

library(betapart)

# Dados ----

## Lista de espécies ----

### Importando ----

### Visualizando ----

## Ocorrências ----

### Importando ----

occ <- readr::read_csv("ATLANTIC_AMPHIBIANS_species.csv")

### Visualizando ----

occ

occ |> dplyr::glimpse()

### Transformando em shapefile ----

occ |>
  dplyr::filter(!longitude |> is.na() & !latitude |> is.na()) |>


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

