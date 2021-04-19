####################################################
#                 TAILINE NONATO                   #
#                  ATIVIDADE 4.2                   #
####################################################


# Carregando pacotes ----
if (!require(pacman)) {
  install.packIdades("pacman")
  library(pacman)}

pacman::p_load(tidyverse)

theme.t <- function(position_legend = "top"){
  return(list(
    theme_bw(),
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")),
    theme(legend.position=position_legend)))}

# Dados ----

a.100 <- read.csv("Outros arquivos/tam100.csv", encoding = "UTF-8", 
                  na.strings=c(" ","NA"))
a.30 <- read.csv("Outros arquivos/tam30.csv", encoding = "UTF-8", 
                  na.strings=c(" ","NA"))

# Amostra tam.100 ----

# NOTA_MT e LOCALIZAÇÃO
# Gráficos 

# Resumo

# Testes Associação

# NOTA_LP e DEPENDENCIA_ADM
# Gráficos 

# Resumo

# Testes Associação

# Amostra tam.30 ----

# NOTA_MT e LOCALIZAÇÃO
# Gráficos 

# Resumo

# Testes Associação

# NOTA_LP e DEPENDENCIA_ADM
# Gráficos 

# Resumo

# Testes Associação