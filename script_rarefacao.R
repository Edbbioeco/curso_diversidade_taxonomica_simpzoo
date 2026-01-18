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
  vegan::estaccumR(permutations = 1000) |>
  summary(display = c("chao", "S"))

chao1

## Tratando os dados ----

dados_chao <- chao1[[1]] |>
  as.data.frame() |>
  dplyr::rename("Riqueza" = Chao,
                "Unidades amostrais" = N) |>
  dplyr::mutate(`Tipo de riqueza` = "Estimado (Chao1)")

dados_chao

dados_s <- chao1[[2]] |>
  as.data.frame() |>
  dplyr::rename("Riqueza" = S,
                "Unidades amostrais" = N) |>
  dplyr::mutate(`Tipo de riqueza` = "Observado")

dados_s

chao1_trat <- dplyr::bind_rows(dados_s,
                               dados_chao)

chao1_trat

## Gráfico ----

chao1_trat |>
  ggplot(aes(`Unidades amostrais`, Riqueza,
             color = `Tipo de riqueza`, fill = `Tipo de riqueza`)) +
  geom_ribbon(aes(x = `Unidades amostrais`,
                  ymin = Riqueza - Std.Dev,
                  ymax = Riqueza + Std.Dev),
              alpha = 0.3,
              color = NA) +
  geom_line(linewidth = 1) +
  geom_point(shape = 21,
             color = "black",
             stroke = 1,
             size = 3) +
  scale_color_manual(values = c("royalblue", "orange")) +
  scale_fill_manual(values = c("royalblue", "orange")) +
  scale_x_continuous(breaks = seq(0, 14, 2)) +
  theme_classic()

ggsave(filename = "rarefacao_chao1.png",
       height = 10,
       width = 12)

# Rarefação baseada em abundância ----

## Chao 2 ----

## Jacknife 2 ----

# Extrapolação baseada em bootstraping ----
