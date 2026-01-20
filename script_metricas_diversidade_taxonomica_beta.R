# Instalando pacotes ----

install.packages(c("tidyverse",
                   "betapart",
                   "reshape2",
                   "devtools"))

devtools::install_github("Edbbioeco/ordenaR")

# Carregando pacotes ----

library(tidyverse)

library(ordenaR)

library(vegan)

library(betapart)

library(reshape2)

library(ggview)

# Dados ----

## Importando ----

com <- readr::read_csv("composicao_anuros.csv")

## Visualizando ----

com

com |> dplyr::glimpse()

## Adicionar nome às linhas ----

rownames(com) <- c(paste0("Com 0", 1:9),
                   paste0("Com ", 10:14))

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

### Total ----

sorensen_total <- com |>
  vegan::decostand(method = "pa") |>
  betapart::beta.multi()

sorensen_total

### Par-a-par ----

#### Calculando ----

sorensen_parapar <- com |>
  vegan::decostand(method = "pa") |>
  betapart::beta.pair()

sorensen_parapar

#### Gráfico ----

sorensen_matriz <- function(id, indice){

  sorensen_matriz <- sorensen_parapar[[id]] |>
    as.matrix()

  sorensen_matriz[upper.tri(sorensen_matriz)] <- NA

  sorensen_matriz_df <- sorensen_matriz |>
    reshape2::melt() |>
    dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "sim",
                                           .default = "não"),
                  indice = paste0(indice,
                                  " = ",
                                  sorensen_total[[id]] |> round(2)),
                  value = value |> round(2)) |>
    dplyr::filter(!value |> is.na() & igual == "não") |>
    dplyr::select(-igual) |>
    dplyr::rename("Índice de Sorensen" = value)

  assign(paste0("sorensen_df_", indice),
         sorensen_matriz_df,
         envir = globalenv())

}

id <- 1:3

indice <- c("Substituição", "Aninhamento", "Sorensen")

purrr::map2(id, nome, sorensen_matriz)

df_sorensen <- ls(pattern = "sorensen_df_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(indice = indice |> forcats::fct_relevel(c("Sorensen = 0.78",
                                                          "Substituição = 0.68",
                                                          "Aninhamento = 0.11")))

df_sorensen

df_sorensen |>
  ggplot(aes(Var1, Var2,
             fill = `Índice de Sorensen`, label = `Índice de Sorensen`)) +
  geom_tile(color = "black", linewidth = 0.5) +
  facet_wrap(~indice) +
  geom_text(color = "black", size = 2.5, fontface = "bold") +
  labs(x = NULL,
       y = NULL) +
  scale_fill_viridis_c(guide = guide_colorbar(title.position = "top",
                                              title.hjust = 0.5,
                                              barwidth = 20,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 1),
                       limits = c(0, 1)) +
  coord_equal() +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.text.x = element_text(angle = 90, hjust = 0),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 20)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "diversidade_taxonomica_beta_soresen.png",
       height = 10, width = 12)

## Índice de Jaccard ----

### Total ----

### Par-a-par ----

#### Calculando ----

#### Gráfico ----

# Diversdade beta baseada em abundância de espécies ----

## Total ----

## Par-a-par ----

#### Calculando ----

#### Gráfico ----

# Espécies compartilhadas ----

## Baseado em incidência de espécies ----

## Baseado em abundância de espécies ----
