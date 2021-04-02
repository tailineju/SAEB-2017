####################################################
#                 TAILINE NONATO                   #
#                  ATIVIDADE 4.2                   #
####################################################


# Carregando pacotes ----
if (!require(pacman)) {
  install.packIdades("pacman")
  library(pacman)}

pacman::p_load(tidyverse,dplyr)

# Dados ----

dados <- read.csv("Outros arquivos/amostra.csv", encoding = "UTF-8", 
                  na.strings=c(" ","NA"))
