####################################################
#            TAILINE NONATO - 190038144            #
#                  ATIVIDADE 2.2                   #
####################################################


# Carregando pacotes ----
if (!require(pacman)) {
  install.packIdades("pacman")
  library(pacman)}

pacman::p_load(tidyverse,dplyr,stringr,infer)

#############################################################
# VERIFICAR USO DE PARÂMETROS POPULACIONAIS NO IC           #
#############################################################

# Dados ----

dados <- read.csv("Outros arquivos/amostra.csv", encoding = "UTF-8", 
                  na.strings=c(" ","NA"))

# Limpeza das variáveis a serem analisadas ----


# Amostras de tamanho 30 ----

a.tam30 <- dados %>% 
  rep_sample_n(size = 30, reps = 50, replace = FALSE)

# Amostras de tamanho 100 ----

a.tam100 <- dados %>% 
  rep_sample_n(size = 100, reps = 50, replace = FALSE)

# Normal com 95% de confiança ----

z <- qnorm((1-.95)/2)

# Proporção de alunos que estudam em escolas do interior ----

# Populacional
p.interior <- (nrow(dados[dados$AREA == "2", ])/2000)

# Intervalos - n=30

ic.interior_a30 <- a.tam30 %>%
  filter(AREA == "2")%>%
  summarise(n = n())%>%
  summarise(n=n,p=n/30)%>%
  mutate(superior = p - z*sqrt(p*(1-p)/30),
         inferior = p + z*sqrt(p*(1-p)/30),
         valor_verdadeiro = ifelse(p.interior < superior & p.interior > inferior,
                                   "Sim", "Não"))


# Intervalos - n=100
ic.interior_a100 <- a.tam100 %>%
  filter(AREA == "2")%>%
  summarise(n = n())%>%
  summarise(n=n,p=n/100)%>%
  mutate(superior = p - z*sqrt(p*(1-p)/100),
         inferior = p + z*sqrt(p*(1-p)/100),
         valor_verdadeiro = ifelse(p.interior < superior & p.interior > inferior,
                                   "Sim", "Não"))

# Proporção de alunas (sexo feminino) ----

# Populacional
p.alunas <- (nrow(dados[dados$SEXO == "B", ])/2000)

# Intervalos - n=30
ic.alunas_a30 <- a.tam30 %>%
  filter(SEXO == "B")%>%
  summarise(n = n())%>%
  summarise(n=n,p=n/30)%>%
  mutate(superior = p - z*sqrt(p*(1-p)/30),
         inferior = p + z*sqrt(p*(1-p)/30),
         valor_verdadeiro = ifelse(p.alunas < superior & p.alunas > inferior,
                                   "Sim", "Não"))

# Intervalos - n=100
ic.alunas_a100 <- a.tam100 %>%
  filter(SEXO == "B")%>%
  summarise(n = n())%>%
  summarise(n=n,p=n/100)%>%
  mutate(superior = p - z*sqrt(p*(1-p)/100),
         inferior = p + z*sqrt(p*(1-p)/100),
         valor_verdadeiro = ifelse(p.alunas < superior & p.alunas > inferior,
                                   "Sim", "Não"))

# Média Nota_LP ----

# Populacional
m.lp <- round(mean(dados$NOTA_LP),2)

# Intervalos - n=30
ic.lp_a30 <- a.tam30 %>%
  summarise(superior=mean(NOTA_LP)-z*(sd(NOTA_LP)/sqrt(30)), 
            inferior=mean(NOTA_LP)+z*(sd(NOTA_LP)/sqrt(30)))%>%
  mutate(valor_verdadeiro = ifelse(m.lp < superior & m.lp > inferior, "Sim", "Não"))

# Intervalos - n=100
ic.lp_a100 <- a.tam100 %>%
  summarise(superior=mean(NOTA_LP)-z*(sd(NOTA_LP)/sqrt(100)), 
            inferior=mean(NOTA_LP)+z*(sd(NOTA_LP)/sqrt(100)))%>%
  mutate(valor_verdadeiro = ifelse(m.lp < superior & m.lp > inferior, "Sim", "Não"))

# Média Nota_MT ----

# Populacional
m.mt <- round(mean(dados$NOTA_MT),2)

# Intervalos - n=30
ic.mt_a30 <- a.tam30 %>%
  summarise(superior=mean(NOTA_MT)-z*(sd(NOTA_MT)/sqrt(30)), 
            inferior=mean(NOTA_MT)+z*(sd(NOTA_MT)/sqrt(30)))%>%
  mutate(valor_verdadeiro = ifelse(m.lp < superior & m.lp > inferior, "Sim", "Não"))

# Intervalos - n=100
ic.mt_a100 <- a.tam100 %>%
  summarise(superior=mean(NOTA_MT)-z*(sd(NOTA_MT)/sqrt(100)), 
            inferior=mean(NOTA_MT)+z*(sd(NOTA_MT)/sqrt(100)))%>%
  mutate(valor_verdadeiro = ifelse(m.lp < superior & m.lp > inferior, "Sim", "Não"))

#links https://ggplot2.tidyverse.org/reference/geom_linerange.html