####################################################
#                 TAILINE NONATO                   #
#                  ATIVIDADE 4.5                   #
####################################################


#Carregando pacotes ----
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

#Dados ----
pop <- read.csv("Outros arquivos/amostra.csv", encoding = "UTF-8", 
                na.strings=c(" ","NA"))

amostra <- pop[sample(1:nrow(pop),500,replace=F),]
