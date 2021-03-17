# Carregando pacotes ----
if (!require(pacman)) {
  install.packIdades("pacman")
  library(pacman)}

pacman::p_load(tidyverse,dplyr)

# Dados ----

dados <- read.csv("Outros arquivos/amostra.csv", encoding = "UTF-8", 
                  na.strings=c(" ","NA"))

# Limpeza das variáveis a serem analisadas ----


# Amostras de tamanho 30 ----

for (i in 1:50){
  assign(paste("tam30",i,sep="_"),dados %>% sample_n(30))
}

# Amostras de tamanho 100 ----

for (i in 1:50){
  assign(paste("tam100",i,sep="_"),dados %>% sample_n(100))
}

# Proporção de alunos que estudam em escolas do interior ----

# Proporção de alunas (sexo feminino) ----

# Média Nota_LP ----

# Média Nota_MT ----
