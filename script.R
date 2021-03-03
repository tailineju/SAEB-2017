####
####

library(tidyverse)
library(dplyr)
library(stringr)
library(fmsb)

setwd("\\Users\\taili\\Documents\\Academia\\UnB\\4SEM\\ME2\\A1")

# Orientacoes ----

read.delim("orientacoes.txt", encoding = "UTF-8")

# Dados ----

dados <- read.csv("amostra.csv", encoding = "UTF-8", 
                  na.strings=c(" ","NA"))
dados$NOTA_LP <- as.numeric(dados$NOTA_LP)
dados$NOTA_MT <- as.numeric(dados$NOTA_MT)

# Limpeza das variáveis a serem analisadas ----
dados$REGIAO <- dados$REGIAO%>% 
  str_replace("1", "Norte")%>% 
  str_replace("2", "Nordeste")%>% 
  str_replace("3", "Sudeste")%>% 
  str_replace("4", "Sul")%>% 
  str_replace("5", "Centro-Oeste")

dados$SEXO <- dados$SEXO%>% 
  str_replace("A", "Masculino")%>% 
  str_replace("B", "Feminino")

dados$IDADE <- dados$IDADE %>% 
  str_replace("A", "8 anos ou menos")%>% 
  str_replace("B", "9 anos")%>% 
  str_replace("C", "10 anos")%>% 
  str_replace("D", "11 anos")%>% 
  str_replace("E", "12 anos")%>% 
  str_replace("F", "13 anos")%>% 
  str_replace("G", "14 anos")%>% 
  str_replace("H", "15 anos ou mais")


# Liste as variaveis ----

tipo <- c("listar")
var <- data.frame(colnames(dados),list(1:19))
colnames(var) <- c("Variavel","Tipo")

# Categoricas ----

#1 REGIAO 

tabela1 <- dados %>% 
  group_by(REGIAO) %>% 
  summarise(Ni= n())%>%
  mutate(Fi =round((Ni/sum(Ni))*100,2))
percent <- str_c(tabela1$Ni, " (",tabela1$Fi, "%",")")%>%str_replace("\\.",",")

tabela1%>%
  ggplot(aes(x = REGIAO, y = Fi, label = percent)) + 
  geom_bar(stat = "identity", fill = "#7aa3cc") +
  geom_text(vjust = -0.5, size = 3.6) +
  scale_y_continuous(limits = c(0,45), 
                     breaks = seq(0,45,15),
                     labels = paste0(seq(0,45,15),"%"))+  
  labs(x = "Regiões", y = "Frequência Relativa") +
  theme_bw() +
  theme(
    axis.title.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(colour = "black", size = 12),
    axis.text = element_text(colour = "black", size = 9.5),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))
ggsave("imagens/regioes.png", width = 158, height = 93, units = "mm")

#2 SEXO
tabela2 <- dados %>% 
  group_by(SEXO) %>% 
  summarise(Ni= n())%>%
  mutate(Fi =round((Ni/sum(Ni))*100,2))
percent <- str_c(tabela2$Ni, " (",tabela2$Fi, "%",")")%>%str_replace("\\.",",")

tabela2%>%
  ggplot(aes(x = SEXO, y = Fi, label = percent)) + 
  geom_bar(stat = "identity", fill = "#7aa3cc") +
  geom_text(vjust = -0.5, size = 3.6) +
  scale_y_continuous(limits = c(0,55), 
                     breaks = seq(0,55,11),
                     labels = paste0(seq(0,55,11),"%"))+  
  labs(x = "Regiões", y = "Frequência Relativa") +
  theme_bw() +
  theme(
    axis.title.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(colour = "black", size = 12),
    axis.text = element_text(colour = "black", size = 9.5),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))
ggsave("imagens/sexo.png", width = 158, height = 93, units = "mm")

#3 IDADE

#TA DANDO ERRADOOOOOOOO
tabela3 <- dados %>%
  group_by(IDADE) %>%
  summarise(Ni= n())%>%
  mutate(Fi =round((Ni/sum(Ni))*100,2))
percent <- str_c(tabela3$Ni, " (",tabela3$Fi, "%",")")%>%str_replace("\\.",",")
percent <- percent[1:8] #tirar NAs

tabela3%>%
  drop_na()%>%#tirar NAs
  ggplot(aes(x = IDADE, y = Fi, label = percent))+  
  geom_bar(stat = "identity", fill = "#7aa3cc") +
  geom_text(vjust = 0.5,hjust=-0.02, size = 3) +
  scale_y_continuous(limits = c(0,50), 
                     breaks = seq(0,50,10),
                     labels = paste0(seq(0,50,10),"%"))+  
  labs(x = "Idade", y = "Frequência Relativa") +
  theme_bw() +
  theme(
    axis.title.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(colour = "black", size = 12),
    axis.text = element_text(colour = "black", size = 9.5),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))+
  coord_flip()
ggsave("imagens/idade_bar.png", width = 158, height = 93, units = "mm")

#opção radar chart
tabela31 <- dados %>%
  group_by(IDADE) %>%
  summarise(Ni= n())
tabela31 <- tabela31%>%
  drop_na()%>%
  gather(IDADE,Ni)%>%
  spread(IDADE,Ni)
tabela31 <- rbind(rep(max(tabela31),length(tabela31)) , rep(0,length(tabela31)), tabela31)

png("imagens/idade.png")
radarchart(tabela31, axistype=1, 
           pcol=rgb(.48,.64,.80,0.9) , pfcol=rgb(.48,.64,.80,0.7), 
           plwd=4, cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,1104,184), cglwd=0.8, vlcex=0.8 )
dev.off()

#4 RAÇA/COR

tabela4 <- dados %>% 
  group_by(RACA_COR) %>% 
  summarise(Ni= n())%>%
  mutate(Fi =round((Ni/sum(Ni))*100,2))

tabela4%>%
  ggplot(aes(x = RACA_COR, y = Fi, label = Ni)) + 
  geom_bar(stat = "identity", fill = "#7aa3cc") +
  geom_text(vjust = -0.5, size = 3.6) +
  scale_y_continuous(limits = c(0,55), 
                     breaks = seq(0,55,11),
                     labels = paste0(seq(0,55,11),"%"))+  
  labs(x = "Raça/Cor", y = "Frequência Relativa") +
  theme_bw() +
  theme(
    axis.title.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(colour = "black", size = 12),
    axis.text = element_text(colour = "black", size = 9.5),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))
ggsave("imagens/racial.png", width = 158, height = 93, units = "mm")

#5 MORA PAI/MÃE
tabela5 <- dados %>% 
  group_by(MORA_MÃE,MORA_PAI) %>% 
  summarise(Ni= n())%>%
  mutate(Fi =round((Ni/sum(Ni))*100,2))

tabela5%>%
  ggplot(aes(x=MORA_MÃE, y=Ni, fill=MORA_PAI))+ 
  geom_col(position = position_dodge2(preserve = 'single', padding = 0)) + 
  #scale_fill_manual(name="Tempo de uso em anos de bisfosfonatos", values=c("#A11D21", "#003366")) +
  scale_y_continuous(limits = c(0,1100), expand = c(0,0), 
                     breaks = seq(0,1100,110),
                     labels = seq(0,1100,110)) +
  labs(x="Mora com a mãe", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")
ggsave("imagens/moradia.png", width = 158, height = 93, units = "mm")

# NOTA_LP ----

# Distribuição de frequências, com intervalos de classe
# Histograma
hist(dados$NOTA_LP)

ggplot(dados, aes(x=NOTA_LP)) + 
  geom_histogram(colour="white", fill="#7aa3cc",binwidth=26)+
  labs(x="Nota em Língua Portuguesa", y="Frequência Absoluta") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 
ggsave("imagens/hist_lp.png", width = 158, height = 93, units = "mm")


# Medidas de posição, variabilidade, assimetria e curtose.
summary(dados$NOTA_LP)
sd(dados$NOTA_LP)

# Box-plot

ggplot(dados, aes(x=factor(""), y=NOTA_LP)) +
  geom_boxplot(fill=c("#7aa3cc"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Nota em Língua Portuguesa")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("imagens/box_lp.png", width = 158, height = 93, units = "mm")


# NOTA_MT ----

# Distribuição de frequências, com intervalos de classe
# Histograma
hist(dados$NOTA_MT)

ggplot(dados, aes(x=NOTA_MT)) + 
  geom_histogram(colour="white", fill="#7aa3cc",binwidth=26)+
  labs(x="Nota em Matemática", y="Frequência Absoluta") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 
ggsave("imagens/hist_mt.png", width = 158, height = 93, units = "mm")


# Medidas de posição, variabilidade, assimetria e curtose.
summary(dados$NOTA_MT)
sd(dados$NOTA_MT)

# Box-plot

ggplot(dados, aes(x=factor(""), y=NOTA_MT)) +
  geom_boxplot(fill=c("#7aa3cc"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Nota em Matemática")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("imagens/box_mt.png", width = 158, height = 93, units = "mm")
