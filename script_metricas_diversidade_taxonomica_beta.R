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

# Gráfico de barras de composição de espécies ----

# Diversidade beta baseada em incidência de espécies ----

## Índice de Sorensen ----

## Índice de Jaccard ----

# Diversdade beta baseada em abundância de espécies ----
