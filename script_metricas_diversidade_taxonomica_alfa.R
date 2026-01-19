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

riqueza <- com |> vegan::specnumber()

df_div <- tibble::tibble(Comunidade = com |> rownames(),
                         Riqueza = riqueza)

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

shannon_wiener <- com |> vegan::diversity()

df_div <- tibble::tibble(Comunidade = com |> rownames(),
                         Riqueza = riqueza,
                         `Shannon-Wiener` = shannon_wiener)

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

gini_simpson <- com |> vegan::diversity(index = "simpson")

df_div <- tibble::tibble(Comunidade = com |> rownames(),
                         Riqueza = riqueza,
                         `Shannon-Wiener` = shannon_wiener,
                         `Gini-Simpson` = gini_simpson)

df_div

df_div |>
  ggplot(aes(Comunidade, `Gini-Simpson`)) +
  geom_col(color = "black", fill = "black") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "diversidade_taxonomica_gini_simpson.png",
       height = 10,
       width = 12)

## Equitabilidade de Pielou ----

pielou <- shannon_wiener / log(riqueza)

df_div <- tibble::tibble(Comunidade = com |> rownames(),
                         Riqueza = riqueza,
                         `Shannon-Wiener` = shannon_wiener,
                         `Gini-Simpson` = gini_simpson,
                         `Equitabilidade de Pielou` = pielou)

df_div

df_div |>
  ggplot(aes(Comunidade, `Equitabilidade de Pielou`)) +
  geom_col(color = "black", fill = "black") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "diversidade_taxonomica_pielou.png",
       height = 10,
       width = 12)

# Indices de Hill ----

## Números de Hill ----

## Equitabilidade de Pielou ----
