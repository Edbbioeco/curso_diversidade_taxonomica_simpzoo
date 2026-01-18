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

dados_chao1 <- chao1[[1]] |>
  as.data.frame() |>
  dplyr::rename("Riqueza" = Chao,
                "Unidades amostrais" = N) |>
  dplyr::mutate(`Tipo de riqueza` = "Estimado (Chao1)")

dados_chao1

dados_s <- chao1[[2]] |>
  as.data.frame() |>
  dplyr::rename("Riqueza" = S,
                "Unidades amostrais" = N) |>
  dplyr::mutate(`Tipo de riqueza` = "Observado")

dados_s

chao1_trat <- dplyr::bind_rows(dados_s,
                               dados_chao1)

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

### Calculando ----

chao2 <- com |>
  vegan::poolaccum(permutations = 1000) |>
  summary(display = c("chao", "S"))

chao2

### Tratando os dados ----

dados_chao2 <- chao2[[1]] |>
  as.data.frame() |>
  dplyr::rename("Riqueza" = Chao,
                "Unidades amostrais" = N) |>
  dplyr::mutate(`Tipo de riqueza` = "Estimado (Chao2)")

dados_chao2

dados_s <- chao2[[2]] |>
  as.data.frame() |>
  dplyr::rename("Riqueza" = S,
                "Unidades amostrais" = N) |>
  dplyr::mutate(`Tipo de riqueza` = "Observado")

dados_s

chao2_trat <- dplyr::bind_rows(dados_s,
                               dados_chao2)

chao2_trat

### Gráfico ----

chao2_trat |>
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

ggsave(filename = "rarefacao_chao2.png",
       height = 10,
       width = 12)

## Jacknife 1 ----

### Calculando ----

jack1 <- com |>
  vegan::poolaccum(permutations = 1000) |>
  summary(display = c("jack1", "S"))

jack1

### Tratando os dados ----

dados_jack1 <- jack1[[1]] |>
  as.data.frame() |>
  dplyr::rename("Riqueza" = `Jackknife 1`,
                "Unidades amostrais" = N) |>
  dplyr::mutate(`Tipo de riqueza` = "Estimado (Jackknife1)")

dados_jack1

dados_s <- jack1[[2]] |>
  as.data.frame() |>
  dplyr::rename("Riqueza" = S,
                "Unidades amostrais" = N) |>
  dplyr::mutate(`Tipo de riqueza` = "Observado")

dados_s

jack1_trat <- dplyr::bind_rows(dados_s,
                               dados_jack1)

jack1_trat

### Gráfico ----

jack1_trat |>
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

ggsave(filename = "rarefacao_jack1.png",
       height = 10,
       width = 12)

# Extrapolação baseada em bootstraping ----
