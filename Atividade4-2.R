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
  str_replace("1", "Federal")%>% 
  str_replace("2", "Estadual")%>% 
  str_replace("3", "Municipal")%>% 
  str_replace("4", "Privada")

a.30$LOCALIZACAO <- a.30$LOCALIZACAO %>% 
  str_replace("1", "Urbana")%>% 
  str_replace("2", "Rural")

a.30$DEPENDENCIA_ADM <- a.30$DEPENDENCIA_ADM %>% 
  str_replace("1", "Federal")%>% 
  str_replace("2", "Estadual")%>% 
  str_replace("3", "Municipal")%>% 
  str_replace("4", "Privada")

# Amostra tam.100 ----

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
resumo1 <- c(summary(a.100$NOTA_MT),sd(a.100$NOTA_MT)^2)

# Testes Associação

# NOTA_LP e DEPENDENCIA_ADM
# Gráficos

# federal+estadual
a.100 %>%
  ggplot(aes(x=DEPENDENCIA_ADM, y=NOTA_LP)) +
  geom_boxplot(fill=c("#7AA3CC"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Dependência Administrativa", y="Nota em Língua Portuguesa")+
  theme.t()+
  ggsave("Outros arquivos/imagens/dep_nlp100.png", width = 158, height = 93, units = "mm")

# Resumo
resumo2 <- c(summary(a.100$NOTA_LP),sd(a.100$NOTA_LP)^2)

# Testes Associação

# Amostra tam.30 ----

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
resumo1 <- c(summary(a.30$NOTA_MT),sd(a.30$NOTA_MT)^2)


# Testes Associação

# NOTA_LP e DEPENDENCIA_ADM
# Gráficos 

# federal+estadual
a.30 %>%
  ggplot(aes(x=DEPENDENCIA_ADM, y=NOTA_LP)) +
  geom_boxplot(fill=c("#7AA3CC"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Dependência Administrativa", y="Nota em Língua Portuguesa")+
  theme.t()+
  ggsave("Outros arquivos/imagens/dep_nlp30.png", width = 158, height = 93, units = "mm")

# Resumo
resumo2 <- c(summary(a.30$NOTA_LP),sd(a.30$NOTA_LP)^2)


# Testes Associação