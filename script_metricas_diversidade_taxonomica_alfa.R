# Pacotes ----

library(tidyverse)

library(scales)

library(vegan)

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
  facet_wrap(~Comunidade, scales = "free_x") +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  theme_classic()

ggsave(filename = "diversidade_taxonomica_whittaker.png",
       height = 10,
       width = 12)

# Índices clássicos de diversidade ----

## Riqueza ----

com |> vegan::specnumber()

df_div <- tibble::tibble(Comunidade = com |> rownames(),
                         Riqueza = com |> vegan::specnumber())

df_div

df_div |>
  ggplot(aes(Comunidade, Riqueza)) +
  geom_col(color = "black", fill = "black") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "diversidade_taxonomica_riqueza.png",
       height = 10,
       width = 12)

## Shannon-Wiener ----

com |> vegan::diversity()

df_div <- tibble::tibble(Comunidade = com |> rownames(),
                         Riqueza = com |> vegan::specnumber(),
                         `Shannon-Wiener` = com |> vegan::diversity())

df_div

df_div |>
  ggplot(aes(Comunidade, `Shannon-Wiener`)) +
  geom_col(color = "black", fill = "black") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "diversidade_taxonomica_shannon_wienner.png",
       height = 10,
       width = 12)

## Gini-Simpson ----

com |> vegan::diversity(index = "simpson")

df_div <- tibble::tibble(Comunidade = com |> rownames(),
                         Riqueza = com |> vegan::specnumber(),
                         `Shannon-Wiener` = com |> vegan::diversity(),
                         `Gini-Simpson` = com |> vegan::diversity(index = "simpson"))

df_div

df_div |>
  ggplot(aes(Comunidade, `Shannon-Wiener`)) +
  geom_col(color = "black", fill = "black") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "diversidade_taxonomica_shannon_wienner.png",
       height = 10,
       width = 12)

## Equitabilidade de Pielou ----

# Indices de Hill ----

# Modelos lineares usando diversidade taxonômica alfa ----
