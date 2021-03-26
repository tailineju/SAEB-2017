####################################################
#                 TAILINE NONATO                   #
#                  ATIVIDADE 3.2                   #
####################################################


# Carregando pacotes ----
if (!require(pacman)) {
  install.packIdades("pacman")
  library(pacman)}

pacman::p_load(tidyverse,dplyr,infer,nortest)

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
  summarise(Ni= n()) %>%
  mutate(Fi=Ni/sum(Ni), esp=5*Fi) #??????????????

# como calcular valor esperado?

# Verificação para normalidade ----
# h0: Nota em matemática segue distribuição normal
# ha: Nota em matemática não segue distribuição normal

chi <-  ((tabela1$Ni-tabela1$esp)^2)/tabela1$esp

chi.obs <- sum(chi)

alpha <- .05
chi.tab <- qchisq(1-alpha,5-1)

chi.obs>chi.tab #se TRUE rejeitar h0

# AMOSTRA TAMANHO 30 ----

a30 <- dados %>% 
  rep_sample_n(size = 30, replace = FALSE)

# Shapiro-Wilk ----
# NOTA_LP
shapiro.test(amostra30$NOTA_LP)

# NOTA_MT
shapiro.test(amostra30$NOTA_MT)

# Anderson-Darling ----
# NOTA_LP
ad.test(amostra30$NOTA_LP)

# NOTA_MT
ad.test(amostra30$NOTA_MT)

# Kolmogorov ----
# NOTA_LP
ks.test(amostra30$NOTA_LP,"pnorm") #verificar propósito do y

# NOTA_MT
ks.test(amostra30$NOTA_MT,"pnorm") #verificar propósito do y
