####################################################
#                 TAILINE NONATO                   #
#                  ATIVIDADE 3.3                   #
####################################################


# Carregando pacotes ----
if (!require(pacman)) {
  install.packIdades("pacman")
  library(pacman)}

pacman::p_load(tidyverse,dplyr,infer)

# Dados ----

dados <- read.csv("Outros arquivos/amostra.csv", encoding = "UTF-8", 
                  na.strings=c(" ","NA"))

# Amostras ----

amostras <- dados %>% 
  rep_sample_n(size = 15, reps = 1000, replace = FALSE)
