# Instalando pacotes ----

install.packages(c("tidyverse",
                   "betapart",
                   "reshape2",
                   "devtools"))

devtools::install_github("Edbbioeco/ordenaR")

# Carregando pacotes ----

library(tidyverse)

library(ordenaR)

library(betapart)

library(reshape2)

# Dados ----

## Importando ----

com <- readr::read_csv("composicao_anuros.csv")

## Visualizando ----

com

com |> dplyr::glimpse()

## Adicionar nome às linhas ----

rownames(com) <- c(paste0("Comunidade 0", 1:9),
                   paste0("Comunidade ", 10:14))

com |> rownames()

# Gráfico de composição de espécies por abundância ----

## Gráfico de barras ----

com |>
  tibble::rownames_to_column() |>
  ordenaR::order_bar(gradient = "rowname",
                     species = 2:18,
                     direct = FALSE) +
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = "diversidade_taxonomica_beta_grafico_barras.png",
       height = 10,
       width = 12)

## Gráfico de circulos ----

com |>
  tibble::rownames_to_column() |>
  ordenaR::order_circle(gradient = "rowname",
                        species = 2:18,
                        direct = FALSE) +
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = "diversidade_taxonomica_beta_grafico_circulos.png",
       height = 10,
       width = 12)

# Diversidade beta baseada em incidência de espécies ----

## Índice de Sorensen ----

## Índice de Jaccard ----

# Diversdade beta baseada em abundância de espécies ----
