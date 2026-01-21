# Instalando os pacotes -----

install.packages(c("tidyverse",
                   "sf",
                   "vegan",
                   "terra",
                   "tidyterra",
                   "betapart"))

# Carregando os pacotes ----

library(tidyverse)

library(magrittr)

library(sf)

library(terra)

library(vegan)

library(tidyterra)

library(betapart)

# Dados ----

## Lista de espécies ----

### Importando ----

sps <- readr::read_csv("ATLANTIC_AMPHIBIANS_species.csv")

### Visualizando ----

sps

sps |> dplyr::glimpse()

### Tratando ----

sps %<>%
  dplyr::select(id, valid_name) %<>%
  dplyr::filter(!valid_name |> is.na()) %<>%
  dplyr::rename("species" = valid_name)

sps

## Ocorrências ----

### Importando ----

occ <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")

### Visualizando ----

occ

occ |> dplyr::glimpse()

### Transformando em shapefile ----

occ_sf <- occ |>
  dplyr::select(id, longitude, latitude) |>
  dplyr::left_join(sps, by = "id") |>
  dplyr::filter(!longitude |> is.na() & !latitude |> is.na()) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4674)

occ_sf

ggplot() +
  geom_sf(data = occ_sf)

## Shapefile da Mata Atlântica ----

### Importando ----

ma <- sf::st_read("mata_atlantica.shp")

### Visualizando ----

ma

ggplot() +
  geom_sf(data = ma, color = "darkgreen", fill = "forestgreen") +
  geom_sf(data = occ_sf)

## Shapefile do Brasil ----

### Importando ----

br <- sf::st_read("brasil.shp")

### Visualizando ----

br

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ma, color = "darkgreen", fill = "forestgreen") +
  geom_sf(data = occ_sf)

# Criando a grade ----

## Escolhando a resolução ----

res_grade <- (50 * 1) / 111.3194

res_grade

## Gerando a grade geral ----

grade <- ma |>
  sf::st_make_grid(cellsize = res_grade) |>
  sf::st_sf() |>
  sf::st_join(ma) |>
  tidyr::drop_na() |>
  dplyr::mutate(ID = dplyr::row_number())

## Visualizando a grade ----

grade

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = grade, color = "darkgreen", fill = NA) +
  geom_sf(data = occ_sf)

# Montando as comunidades por grades ----

comp_occ <- occ_sf |>
  sf::st_join(grade) |>
  tidyr::drop_na() |>
  as.data.frame() |>
  dplyr::summarise(Abundancia = dplyr::n(),
                   .by = c(ID, species)) |>
  tidyr::pivot_wider(names_from = species,
                     values_from = Abundancia,
                     values_fill = 0)

comp_occ

# Raster de base ----

template <- grade |>
  terra::vect() |>
  terra::rast(res = res_grade)

template

# Distribuição dos valores de riqueza ----

## Calculando a riqueza ----

riqueza <- comp_occ |>
  tibble::column_to_rownames("ID") |>
  vegan::specnumber()

riqueza

## Gerando um dataframe com os dados de riqueza ----

df_riqueza <- tibble::tibble(ID = comp_occ$ID,
                             Riqueza = riqueza)

df_riqueza

## Adicionando uma coluna no shapefile de grade com as informações de riqueza ----

grade %<>%
  dplyr::left_join(df_riqueza,
                   by = "ID") %<>%
  dplyr::mutate(Riqueza = dplyr::case_when(Riqueza |> is.na() ~ 0,
                                           .default = Riqueza))

grade

## Rasterizando ----

raster_riqueza <- terra::rasterize(grade |> terra::vect(),
                                   template,
                                   field = "Riqueza")

raster_riqueza

## Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black") +
  tidyterra::geom_spatraster(data = raster_riqueza) +
  geom_sf(data = br, color = "black", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_c(na.value = NA,
                       guide = guide_colorbar(title = "Riqueza",
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              barheight = 0.5,
                                              barwidth = 15,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 0.5)) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(filename = "mapa_distribuicao_riqueza.png",
       height = 10, width = 12)

# Distribuição dos valores de diversidade alfa ----

## Shannon-Wiener ----

## Calculando a diversidade ----

shannon <- comp_occ |>
  tibble::column_to_rownames("ID") |>
  vegan::diversity()

shannon

## Gerando um dataframe com os dados de Shannon-Winner ----

df_shannon <- tibble::tibble(ID = comp_occ$ID,
                             `Shannon-Winner` = shannon)

df_shannon

## Adicionando uma coluna no shapefile de grade com as informações de Shannon-Winner ----

grade %<>%
  dplyr::left_join(df_shannon,
                   by = "ID") %<>%
  dplyr::mutate(`Shannon-Winner` = dplyr::case_when(`Shannon-Winner` |> is.na() ~ 0,
                                                     .default = `Shannon-Winner`))

grade

## Rasterizando ----

raster_shannon <- terra::rasterize(grade |> terra::vect(),
                                   template,
                                   field = "Shannon-Winner")

raster_shannon

## Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black") +
  tidyterra::geom_spatraster(data = raster_shannon) +
  geom_sf(data = br, color = "black", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_c(na.value = NA,
                       guide = guide_colorbar(title = "Shannon-Winner",
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              barheight = 0.5,
                                              barwidth = 15,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 0.5)) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(filename = "mapa_distribuicao_shannon_winner.png",
       height = 10, width = 12)

## Gini-Simpson ----

## Calculando a Gini-Simpson ----

simpson <- comp_occ |>
  tibble::column_to_rownames("ID") |>
  vegan::diversity(index = "simpson")

simpson

## Gerando um dataframe com os dados de Gini-Simpson ----

df_simpson <- tibble::tibble(ID = comp_occ$ID,
                             `Gini-Simpson` = simpson)

df_simpson

## Adicionando uma coluna no shapefile de grade com as informações de Gini-Simpson ----

grade %<>%
  dplyr::left_join(df_simpson,
                   by = "ID") %<>%
  dplyr::mutate(`Gini-Simpson` = dplyr::case_when(`Gini-Simpson` |> is.na() ~ 0,
                                                  .default = `Gini-Simpson`))

grade

## Rasterizando ----

raster_simpson <- terra::rasterize(grade |> terra::vect(),
                                   template,
                                   field = "Gini-Simpson")

raster_simpson

## Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black") +
  tidyterra::geom_spatraster(data = raster_simpson) +
  geom_sf(data = br, color = "black", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_c(na.value = NA,
                       guide = guide_colorbar(title = "Gini-Simpson",
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              barheight = 0.5,
                                              barwidth = 15,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 0.5)) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(filename = "mapa_distribuicao_gini_simpson.png",
       height = 10, width = 12)

## Índices de Hill ----

## Calculando a Hill ----

hill <- comp_occ |>
  tibble::column_to_rownames("ID") |>
  vegan::renyi(scales = 1:2, hill = TRUE)

hill

## Gerando um dataframe com os dados de Hill ----

df_hill <- tibble::tibble(ID = comp_occ$ID,
                             `Q = 1` = hill$`1`,
                             `Q = 2` = hill$`2`)

df_hill

## Adicionando uma coluna no shapefile de grade com as informações de Hill ----

grade %<>%
  dplyr::left_join(df_hill,
                   by = "ID") %<>%
  dplyr::mutate(`Q = 1` = dplyr::case_when(`Q = 1` |> is.na() ~ 0,
                                           .default = `Q = 1`),
                `Q = 2` = dplyr::case_when(`Q = 2` |> is.na() ~ 0,
                                           .default = `Q = 2`))

grade

## Rasterizando ----

raster_hill_q1 <- terra::rasterize(grade |> terra::vect(),
                                   template,
                                   field = "Q = 1")

raster_hill_q2 <- terra::rasterize(grade |> terra::vect(),
                                      template,
                                      field = "Q = 2")

raster_hill <- c(raster_simpson_q1, raster_simpson_q2)

raster_hill

## Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black") +
  tidyterra::geom_spatraster(data = raster_hill) +
  geom_sf(data = br, color = "black", fill = NA, linewidth = 0.5) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(na.value = NA,
                       guide = guide_colorbar(title = "Índice de Hill",
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              barheight = 0.5,
                                              barwidth = 15,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 0.5)) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(filename = "mapa_distribuicao_hill.png",
       height = 10, width = 12)

# Distribuição dos valores de diversidade beta ----

## Sorensen ----

### Calculando a diversidade beta ----

sorensen <- comp_occ |>
  tibble::column_to_rownames("ID") |>
  vegan::decostand(method = "pa") |>
  betapart::beta.pair() %>%
  .$beta.sor |>
  as.matrix() |>
  as.data.frame() |>
  rowMeans()

sorensen

### Gerando o dataframe com os valores ----

df_sorensen <- tibble::tibble(ID = comp_occ$ID,
                              Sorensen = sorensen)

df_sorensen

### Adicionando uma coluna no shapefile de grade com as informações de Sorensen ----

grade %<>%
  dplyr::left_join(df_sorensen,
                   by = "ID") %<>%
  dplyr::mutate(Sorensen = dplyr::case_when(Sorensen |> is.na() ~ 0,
                                           .default = Sorensen))

grade

### Rasterizando ----

raster_soresen <- terra::rasterize(grade |> terra::vect(),
                                   template,
                                   field = "Sorensen")

raster_soresen

### Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black") +
  tidyterra::geom_spatraster(data = raster_soresen) +
  geom_sf(data = br, color = "black", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_c(na.value = NA,
                       guide = guide_colorbar(title = "Índice de Dissimilaridade de Sorensen",
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              barheight = 0.5,
                                              barwidth = 15,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 0.5),
                       limits = c(0, 1)) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(filename = "mapa_distribuicao_sorensen.png",
       height = 10, width = 12)

## Jaccard ----

### Calculando a diversidade beta ----

jaccard <- comp_occ |>
  tibble::column_to_rownames("ID") |>
  vegan::decostand(method = "pa") |>
  betapart::beta.pair(index.family = "jaccard") %>%
  .$beta.jac |>
  as.matrix() |>
  as.data.frame() |>
  rowMeans()

jaccard

### Gerando o dataframe com os valores ----

df_jaccard <- tibble::tibble(ID = comp_occ$ID,
                             Jaccard = jaccard)

df_jaccard

### Adicionando uma coluna no shapefile de grade com as informações de Jaccard ----

grade %<>%
  dplyr::left_join(df_jaccard,
                   by = "ID") %<>%
  dplyr::mutate(Jaccard = dplyr::case_when(Jaccard |> is.na() ~ 0,
                                           .default = Jaccard))

grade

### Rasterizando ----

raster_jaccard <- terra::rasterize(grade |> terra::vect(),
                                   template,
                                   field = "Jaccard")

raster_jaccard

### Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black") +
  tidyterra::geom_spatraster(data = raster_jaccard) +
  geom_sf(data = br, color = "black", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_c(na.value = NA,
                       guide = guide_colorbar(title = "Índice de Dissimilaridade de Jaccard",
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              barheight = 0.5,
                                              barwidth = 15,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 0.5),
                       limits = c(0, 1)) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(filename = "mapa_distribuicao_jaccard.png",
       height = 10, width = 12)

## Bray Curtis ----

### Calculando a diversidade beta ----

bray <- comp_occ |>
  tibble::column_to_rownames("ID") |>
  betapart::beta.pair.abund() %>%
  .$beta.bray |>
  as.matrix() |>
  as.data.frame() |>
  rowMeans()

bray

### Gerando o dataframe com os valores ----

df_bray <- tibble::tibble(ID = comp_occ$ID,
                          `Bray-Curtis` = bray)

df_bray

### Adicionando uma coluna no shapefile de grade com as informações de Bray-Curtis ----

grade %<>%
  dplyr::left_join(df_bray,
                   by = "ID") %<>%
  dplyr::mutate(`Bray-Curtis` = dplyr::case_when(`Bray-Curtis` |> is.na() ~ 0,
                                                 .default = `Bray-Curtis`))

grade

### Rasterizando ----

raster_bray <- terra::rasterize(grade |> terra::vect(),
                                template,
                                field = "Bray-Curtis")

raster_bray

## Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black") +
  tidyterra::geom_spatraster(data = raster_bray) +
  geom_sf(data = br, color = "black", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_c(na.value = NA,
                       guide = guide_colorbar(title = "Índice de Dissimilaridade de Bray-Curtis",
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              barheight = 0.5,
                                              barwidth = 15,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 0.5),
                       limits = c(0, 1)) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(filename = "mapa_distribuicao_bray_curtis.png",
       height = 10, width = 12)

## Espécies compartilhadas ----

### Calculando a diversidade beta ----

sps_comp <- comp_occ |>
  tibble::column_to_rownames("ID") |>
  vegan::decostand(method = "pa") |>
  betapart::betapart.core() %>%
  .$shared |>
  as.matrix() |>
  as.data.frame() |>
  rowMeans()

sps_comp

### Gerando o dataframe com os valores ----

df_sps_comp <- tibble::tibble(ID = comp_occ$ID,
                              sps_comp = sps_comp)

df_sps_comp

### Adicionando uma coluna no shapefile de grade com as informações de Bray-Curtis ----

grade %<>%
  dplyr::left_join(df_sps_comp,
                   by = "ID") %<>%
  dplyr::mutate(sps_comp = dplyr::case_when(sps_comp |> is.na() ~ 0,
                                            .default = sps_comp))

grade

### Rasterizando ----

raster_sps_comp <- terra::rasterize(grade |> terra::vect(),
                                    template,
                                    field = "sps_comp")

raster_sps_comp

## Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black") +
  tidyterra::geom_spatraster(data = raster_sps_comp) +
  geom_sf(data = br, color = "black", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_c(na.value = NA,
                       guide = guide_colorbar(title = "Quantidade de Espécies Compartilhadas",
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              barheight = 0.5,
                                              barwidth = 15,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 0.5),
                       limits = c(0, 13)) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(filename = "mapa_distribuicao_especies_compartilhadas.png",
       height = 10, width = 12)

## Abundância compartilhada ----

### Calculando a diversidade beta ----

abud_comp <- comp_occ |>
  tibble::column_to_rownames("ID") |>
  betapart::betapart.core.abund() %>%
  .$pair.shared.abund |>
  as.matrix() |>
  as.data.frame() |>
  rowMeans()

abud_comp

### Gerando o dataframe com os valores ----

df_abud_comp <- tibble::tibble(ID = comp_occ$ID,
                               abud_comp = abud_comp)

df_abud_comp

### Adicionando uma coluna no shapefile de grade com as informações de Bray-Curtis ----

grade %<>%
  dplyr::left_join(df_abud_comp,
                   by = "ID") %<>%
  dplyr::mutate(abud_comp = dplyr::case_when(abud_comp |> is.na() ~ 0,
                                             .default = abud_comp))

grade

### Rasterizando ----

raster_abud_comp <- terra::rasterize(grade |> terra::vect(),
                                     template,
                                     field = "abud_comp")

raster_abud_comp

## Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black") +
  tidyterra::geom_spatraster(data = raster_abud_comp) +
  geom_sf(data = br, color = "black", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_c(na.value = NA,
                       guide = guide_colorbar(title = "Abundância compartilhada média",
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              barheight = 0.5,
                                              barwidth = 15,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 0.5)) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(filename = "mapa_distribuicao_abundancia_compartilhada.png",
       height = 10, width = 12)

