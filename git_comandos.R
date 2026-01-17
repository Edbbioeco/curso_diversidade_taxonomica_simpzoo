# Pacotes ----

library(gert)

# Listando arquivos ----

gert::git_status() |>
  as.data.frame()

# Preparando arquivos ----

gert::git_add(list.files(pattern = "git_comandos.R"))

# Commitando ----

gert::git_commit("Script para comandos de git")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull()

# Resetando ----

gert::git_reset_mixed()

gert::git_reset_soft("HEAD")

gert::git_reset_soft("HEAD")
