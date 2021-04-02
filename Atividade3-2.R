####################################################
#                 TAILINE NONATO                   #
#                  ATIVIDADE 3.2                   #
####################################################


# Carregando pacotes ----
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)}

pacman::p_load(tidyverse,dplyr,infer,nortest)

# AMOSTRA TAMANHO 100 ----

a100 <- read.csv("Outros arquivos/tam100.csv")%>%
  filter(replicate == 1)

# NOTA_MT em faixas ----

a100$FAIXAS_MT <- cut(a100$NOTA_MT,
                       breaks = seq(133,353,44),
                       labels = c("133 a 176",
                                  "177 a 220",
                                  "221 a 264",
                                  "265 a 308",
                                  "309 a 353"),
                       include.lowest = TRUE)

# Verificação para normalidade ----
# h0: Nota em matemática segue distribuição normal
# ha: Nota em matemática não segue distribuição normal

Li <- c(133,177,221,265,309)
Ls <- c(176,220,264,308,353)


tabela1 <- a100 %>%
  group_by(FAIXAS_MT) %>%
  summarise(Ni= n()) %>%
  mutate(Li, Ls, Xi=(Li+Ls)/2, XiFi=Xi*Ni,media=sum(XiFi)/sum(Ni), Wi=Ni*(Xi-media)^2, 
         var=sum(Wi)/sum(Ni),dp=sqrt(var), z=(Ls-media)/dp, p=pnorm(z),pi=p)

for (i in 2:5){
  tabela1$pi[i] <- tabela1$p[i]-tabela1$p[i-1]}

tabela1$pi[5] <- 1-tabela1$p[5]

tabela1 <- tabela1 %>%
  mutate(Ei=pi*sum(Ni),chi=((Ni-Ei)^2)/Ei)

chi.obs <- sum(tabela1$chi)
alpha <- .05
chi.tab <- qchisq(1-alpha,5-1)

chi.obs>chi.tab #se TRUE rejeitar h0

# AMOSTRA TAMANHO 30 ----

a30 <- read.csv("Outros arquivos/tam30.csv")%>%
  filter(replicate == 1)

# Shapiro-Wilk ----
# NOTA_LP
shapiro.test(a30$NOTA_LP)

# NOTA_MT
shapiro.test(a30$NOTA_MT)

# Anderson-Darling ----
# NOTA_LP
ad.test(a30$NOTA_LP)

# NOTA_MT
ad.test(a30$NOTA_MT)

# Kolmogorov ----
# NOTA_LP
ks.test(a30$NOTA_LP,"pnorm") #verificar propósito do y

# NOTA_MT
ks.test(a30$NOTA_MT,"pnorm") #verificar propósito do y
