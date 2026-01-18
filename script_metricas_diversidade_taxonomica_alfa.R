# Pacotes ----

library(tidyverse)

library(vegan)

library(scales)

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

# Diagrama de Whittaker ----

## Calculando ----

whitakker <- com |>
  tibble::rownames_to_column() |>
  dplyr::rename("Comunidade" = rowname) |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Especie",
                      values_to = "Abundancia") |>
  dplyr::summarise(Abundancia = Abundancia |> sum(),
                   .by = c(Especie, Comunidade)) |>
  dplyr::arrange(Abundancia |> dplyr::desc()) |>
  dplyr::mutate(Rank = dplyr::row_number(),
                .by = Comunidade) |>
  dplyr::filter(Abundancia > 0)

whitakker

## Gráfico ----

whitakker |>
  ggplot(aes(Rank, Abundancia)) +
  geom_line(linewidth = 1,
            color = "black") +
  facet_wrap(~Comunidade, scales = "free") +
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  theme_classic()

ggsave(filename = "diversidade_taxonomica_whittaker.png",
       height = 10,
       width = 12)

# Índices clássicos de diversidade ----

## Riqueza ----

## Shannon-Wiener ----

## Gini-Simpson ----

## Equitabilidade de Pielou ----

# Indices de Hill ----

# Modelos lineares usando diversidade taxonômica alfa ----
