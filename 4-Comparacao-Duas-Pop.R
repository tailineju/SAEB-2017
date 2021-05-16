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

a.100$LOCALIZACAO <- a.100$LOCALIZACAO %>% 
  str_replace("1", "Urbana")%>% 
  str_replace("2", "Rural")

a.100$DEPENDENCIA_ADM <- a.100$DEPENDENCIA_ADM %>% 
  str_replace("1", "Estadual + Federal")%>% 
  str_replace("2", "Estadual + Federal")%>% 
  str_replace("3", "Municipal")%>% 
  str_replace("4", "Privada")

a.30$LOCALIZACAO <- a.30$LOCALIZACAO %>% 
  str_replace("1", "Urbana")%>% 
  str_replace("2", "Rural")

a.30$DEPENDENCIA_ADM <- a.30$DEPENDENCIA_ADM %>% 
  str_replace("1", "Estadual + Federal")%>% 
  str_replace("2", "Estadual + Federal")%>% 
  str_replace("3", "Municipal")%>% 
  str_replace("4", "Privada")


##################################################################
################## AMOSTRAS DE TAMANHO 100 ########################

# NOTA_MT e LOCALIZAÇÃO
# Gráficos 
a.100 %>%
  ggplot(aes(x=LOCALIZACAO, y=NOTA_MT)) +
  geom_boxplot(fill=c("#7AA3CC"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Localização", y="Nota em Matemática")+
  theme.t()+
  ggsave("Outros arquivos/imagens/loc_nmt100.png", width = 158, height = 93, units = "mm")

# Resumo
urb.100 <- a.100[a.100$LOCALIZACAO == "Urbana",]
rural.100 <- a.100[a.100$LOCALIZACAO == "Rural",]

summary(urb.100$NOTA_MT)
sd(urb.100$NOTA_MT)

summary(rural.100$NOTA_MT)
sd(rural.100$NOTA_MT)
# Testes Associação


#T de Student
t.test(urb.100$NOTA_MT, rural.100$NOTA_MT)
qt(.95,5000-2)

#Wilcoxon
wilcox.test(urb.100$NOTA_MT, rural.100$NOTA_MT)

#Kolmogorov/Smirnov
ks.test(urb.100$NOTA_MT, rural.100$NOTA_MT)

# NOTA_LP e DEPENDENCIA_ADM
# Gráficos
a.100 %>%
  #filter(DEPENDENCIA_ADM != "Privada") %>%
  ggplot(aes(x=DEPENDENCIA_ADM, y=NOTA_LP)) +
  geom_boxplot(fill=c("#7AA3CC"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Dependência Administrativa", y="Nota em Língua Portuguesa")+
  theme.t()+
  ggsave("Outros arquivos/imagens/dep_nlp100.png", width = 158, height = 93, units = "mm")

# Resumo
est.100 <- a.100[a.100$DEPENDENCIA_ADM == "Estadual + Federal",]
mun.100 <- a.100[a.100$DEPENDENCIA_ADM == "Municipal",]

summary(est.100$NOTA_LP)
sd(est.100$NOTA_LP)

summary(mun.100$NOTA_LP)
sd(mun.100$NOTA_LP)

# Testes Associação

#T de Student
t.test(est.100$NOTA_LP, mun.100$NOTA_LP)

#Wilcoxon
wilcox.test(est.100$NOTA_LP, mun.100$NOTA_LP)

#Kolmogorov/Smirnov
ks.test(est.100$NOTA_LP, mun.100$NOTA_LP)


##################################################################
################## AMOSTRAS DE TAMANHO 30 ########################

# NOTA_MT e LOCALIZAÇÃO
# Gráficos 

a.30 %>%
  ggplot(aes(x=LOCALIZACAO, y=NOTA_MT)) +
  geom_boxplot(fill=c("#7AA3CC"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Localização", y="Nota em Matemática")+
  theme.t()+
  ggsave("Outros arquivos/imagens/loc_nmt30.png", width = 158, height = 93, units = "mm")

# Resumo
urb.30 <- a.30[a.30$LOCALIZACAO == "Urbana",]
rural.30 <- a.30[a.30$LOCALIZACAO == "Rural",]

summary(urb.30$NOTA_MT)
sd(urb.30$NOTA_MT)

summary(rural.30$NOTA_MT)
sd(rural.30$NOTA_MT)

# Testes Associação

#T de Student
t.test(urb.30$NOTA_MT, rural.30$NOTA_MT)

#Wilcoxon
wilcox.test(urb.30$NOTA_MT, rural.30$NOTA_MT)

#Kolmogorov/Smirnov
ks.test(urb.30$NOTA_MT, rural.30$NOTA_MT)

# NOTA_LP e DEPENDENCIA_ADM
# Gráficos 
a.30 %>%
  #filter(DEPENDENCIA_ADM != "Privada") %>%
  ggplot(aes(x=DEPENDENCIA_ADM, y=NOTA_LP)) +
  geom_boxplot(fill=c("#7AA3CC"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Dependência Administrativa", y="Nota em Língua Portuguesa")+
  theme.t()+
  ggsave("Outros arquivos/imagens/dep_nlp30.png", width = 158, height = 93, units = "mm")

# Resumo
est.30 <- a.30[a.30$DEPENDENCIA_ADM == "Estadual + Federal",]
mun.30 <- a.30[a.30$DEPENDENCIA_ADM == "Municipal",]

summary(est.30$NOTA_LP)
sd(est.30$NOTA_LP)

summary(mun.30$NOTA_LP)
sd(mun.30$NOTA_LP)

# Testes Associação
#T de Student
t.test(est.30$NOTA_LP, mun.30$NOTA_LP)

#Wilcoxon
wilcox.test(est.30$NOTA_LP, mun.30$NOTA_LP)

#Kolmogorov/Smirnov
ks.test(est.30$NOTA_LP, mun.30$NOTA_LP)
