# Pacotes ----

library(tidyverse)

library(vegan)

library(iNEXT)

# Dados ----

## Importando ----

com <- readr::read_csv("composicao_anuros.csv")

## Visualizando -----

com

com |> dplyr::glimpse()

# Rarefação baseada em incidencia (Chao1) ----

## Calculando ----

chao1 <- com |>
  vegan::estaccumR(permutations = 100) |>
  summary(display = c("chao", "S"))

chao1

## Tratando os dados ----

dados_chao <- chao1[[1]] |>
  as.data.frame() |>
  dplyr::rename("Riqueza" = Chao) |>
  dplyr::mutate(`Tipo de riqueza` = "Estimado (Chao1)")

dados_chao

dados_s <- chao1[[2]] |>
  as.data.frame() |>
  dplyr::rename("Riqueza" = S) |>
  dplyr::mutate(`Tipo de riqueza` = "Observado")

dados_s

chao1_trat <- dplyr::bind_rows(dados_s,
                               dados_chao)

chao1_trat

## Gráfico ----

# Rarefação baseada em abundância ----

## Chao 2 ----

## Jacknife 2 ----

# Extrapolação baseada em bootstraping ----
