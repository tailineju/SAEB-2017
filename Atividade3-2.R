####################################################
#                 TAILINE NONATO                   #
#                  ATIVIDADE 3.2                   #
####################################################


# Carregando pacotes ----
if (!require(pacman)) {
  install.packIdades("pacman")
  library(pacman)}

pacman::p_load(tidyverse,dplyr,infer)

# Dados ----

dados <- read.csv("Outros arquivos/amostra.csv", encoding = "UTF-8", 
                  na.strings=c(" ","NA"))

# AMOSTRA TAMANHO 100 ----

a100 <- dados %>% 
  rep_sample_n(size = 100, replace = FALSE)

# NOTA_MT em faixas ----
a100$FAIXAS_MT <- cut(a100$NOTA_MT,
                       breaks = seq(112,357,49),
                       labels = c("112 a 160",
                                  "161 a 209",
                                  "210 a 258",
                                  "259 a 307",
                                  "308 a 355"),
                       include.lowest = TRUE)

# Freq. de NOTA_MT por faixa ----

tabela1 <- a100 %>%
  group_by(FAIXAS_MT) %>%
  summarise(Ni= n())

# Verificação para normalidade ----


# AMOSTRA TAMANHO 30 ----

a30 <- dados %>% 
  rep_sample_n(size = 30, replace = FALSE)

# Shapiro-Wilk ----
# NOTA_LP

# NOTA_MT

# Anderson-Darling ----
# NOTA_LP

# NOTA_MT

# Kolmogorov ----
# NOTA_LP

# NOTA_MT

