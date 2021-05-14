####################################################
#                 TAILINE NONATO                   #
#                  ATIVIDADE 4.5                   #
####################################################


#Carregando pacotes ----
if (!require(pacman)) {
  install.packIdades("pacman")
  library(pacman)}

pacman::p_load(tidyverse,nortest)

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
amostra <- amostra %>% na.omit(amostra$USO_TEMPO_TELAS)

amostra$REGIAO <- amostra$REGIAO%>% 
  str_replace("1", "Norte")%>% 
  str_replace("2", "Nordeste")%>% 
  str_replace("3", "Sudeste")%>% 
  str_replace("4", "Sul")%>% 
  str_replace("5", "Centro-Oeste")

for (i in 1:500){
  if(amostra$USO_TEMPO_TELAS[i]=="A"){
    amostra$USO_TEMPO_TELAS[i] <- amostra$USO_TEMPO_TELAS[i] %>% str_replace("A", "Não vê TV..... + Menos de 1 hora")}
  if(amostra$USO_TEMPO_TELAS[i]=="B"){
    amostra$USO_TEMPO_TELAS[i] <- amostra$USO_TEMPO_TELAS[i] %>% str_replace("B", "Entre 1 e 2 horas")} 
  if(amostra$USO_TEMPO_TELAS[i]=="C"){
    amostra$USO_TEMPO_TELAS[i] <- amostra$USO_TEMPO_TELAS[i] %>% str_replace("C", "Mais de 2, até 3 horas")}
  if(amostra$USO_TEMPO_TELAS[i]=="D"){
    amostra$USO_TEMPO_TELAS[i] <- amostra$USO_TEMPO_TELAS[i] %>% str_replace("D", "Mais de 3 horas")} 
  if(amostra$USO_TEMPO_TELAS[i]=="E"){
    amostra$USO_TEMPO_TELAS[i] <- amostra$USO_TEMPO_TELAS[i] %>% str_replace("E", "Não vê TV..... + Menos de 1 hora")}
}


#Testes de Normalidade ----

shapiro.test(amostra$NOTA_MT)

shapiro.test(amostra$NOTA_LP)

#Homocedasticidade ----
bartlett.test (amostra$NOTA_MT~amostra$REGIAO)
bartlett.test(amostra$NOTA_LP~amostra$USO_TEMPO_TELAS)

# NOTA_MT vs REGIAO ----
# Descritiva
amostra %>%
  ggplot(aes(x=REGIAO, y=NOTA_MT)) +
  geom_boxplot(fill=c("#7AA3CC"), width=0.5) +
  stat_summary(fun="mean",geom="point",shape=23, size=3, fill="white")+
  labs(x="Região", y="Nota em Matemática") +
  theme.t()+
  ggsave("Outros arquivos/imagens/comp-regiao-mt.png", width = 158, height = 93, units = "mm")

# Kruskal
kruskal.test(amostra$NOTA_MT, amostra$REGIAO)

# Comparação de Médias
pairwise.wilcox.test(amostra$NOTA_MT, amostra$REGIAO, p.adjust.method="bonferroni")

# Caso assumisse normalidade
# Anova 
anova.mt <- aov(amostra$NOTA_MT~amostra$REGIAO)
summary(anova.mt)

# Comparação de Médias
pairwise.t.test(amostra$NOTA_MT, amostra$REGIAO, p.adjust.method = "bonferroni" )


# NOTA_MT vs USO_TEMPO_TELAS ----
# Descritiva
amostra %>%
  ggplot(aes(x=USO_TEMPO_TELAS, y=NOTA_LP)) +
  geom_boxplot(fill=c("#7AA3CC"), width=0.5) +
  stat_summary(fun="mean",geom="point",shape=23, size=3, fill="white")+
  labs(x="Tempo de Uso de Telas", y="Nota em Língua Portuguesa") +
  theme.t()+
  coord_flip()+
  ggsave("Outros arquivos/imagens/comp-telas-lp.png", width = 158, height = 93, units = "mm")

# Anova
anova.lp <- aov(amostra$NOTA_LP~amostra$USO_TEMPO_TELAS)
summary(anova.lp)

# Comparação de Médias
pairwise.t.test(amostra$NOTA_LP, amostra$USO_TEMPO_TELAS, p.adjust.method = "bonferroni" )


# Caso não fosse normal
# Kruskal
kruskal.test(amostra$NOTA_LP, amostra$USO_TEMPO_TELAS)

# Comparação de Médias
pairwise.wilcox.test(amostra$NOTA_LP, amostra$USO_TEMPO_TELAS, p.adjust.method="bonferroni")
