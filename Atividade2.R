####################################################
#                 TAILINE NONATO                   #
#                  ATIVIDADE 2.2                   #
####################################################


# Carregando pacotes ----
if (!require(pacman)) {
  install.packIdades("pacman")
  library(pacman)}

pacman::p_load(tidyverse,dplyr)

# Dados ----

dados <- read.csv("Outros arquivos/amostra.csv", encoding = "UTF-8", 
                  na.strings=c(" ","NA"))

# Amostras de tamanho 30 ----

a.tam30 <- dados %>% 
  rep_sample_n(size = 30, reps = 50, replace = FALSE)

# Amostras de tamanho 100 ----

a.tam100 <- dados %>% 
  rep_sample_n(size = 100, reps = 50, replace = FALSE)

# Normal com 95% de confiança ----

z <- qnorm((1-.95)/2)

# Proporção de alunos que estudam em escolas do interior ----

# Parâmetro populacional
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
# Gráfico - n=30
ic.interior_a30 <- data.frame(id = c(1:50, 1:50),
                            intervalo = c(ic.interior_a30$superior, ic.interior_a30$inferior),
                            legenda = c(ic.interior_a30$valor_verdadeiro, ic.interior_a30$valor_verdadeiro))


ic.interior_a30 %>%
  ggplot(aes(x=intervalo, y=id, group=id, color=legenda)) +
  geom_point(size = 1) +  
  geom_line(size=.74) +
  scale_colour_manual(name="Contém proporção populacional", values = c("#003366", "#7AA3CC"))+
  labs(x="Intervalos de Confiança", y="Amostras"#,
       #title="Proporção de estudantes em escolas do interior",
       #subtitle="Amostra de tamanho 30"
       ) +
  geom_vline(xintercept = p.interior, color = "black")+
  annotate(x = p.interior,y=0, label=expression(p == 0.806),
           geom="text",angle=0,vjust=0,hjust=.7, size=2.5) +
  theme_bw() +
  theme(axis.title.y = element_text(colour = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(legend.position = "top")+
  coord_flip()
ggsave("Outros arquivos/imagens/ic_interior30.png", width = 158, height = 93, units = "mm")

# Intervalos - n=100
ic.interior_a100 <- a.tam100 %>%
  filter(AREA == "2")%>%
  summarise(n = n())%>%
  summarise(n=n,p=n/100)%>%
  mutate(superior = p - z*sqrt(p*(1-p)/100),
         inferior = p + z*sqrt(p*(1-p)/100),
         valor_verdadeiro = ifelse(p.interior < superior & p.interior > inferior,
                                   "Sim", "Não"))
# Gráfico - n=100
ic.interior_a100 <- data.frame(id = c(1:50, 1:50),
                            intervalo = c(ic.interior_a100$superior, ic.interior_a100$inferior),
                            legenda = c(ic.interior_a100$valor_verdadeiro, ic.interior_a100$valor_verdadeiro))

ic.interior_a100 %>%
  ggplot(aes(x=intervalo, y=id, group=id, color=legenda)) +
  geom_point(size = 1) +  
  geom_line(size = .74) +
  scale_colour_manual(name="Contém proporção populacional", values = c("#003366", "#7AA3CC"))+
  labs(x="Intervalos de Confiança", y="Amostras"#,
       #title="Proporção de estudantes em escolas do interior",
       #subtitle="Amostra de tamanho 100"
  ) +
  geom_vline(xintercept = p.interior, color = "black")+
  annotate(x = p.interior,y=0, label=expression(p == 0.806),
           geom="text",angle=0,vjust=0,hjust=.7, size=2.5) +
  theme_bw() +
  theme(axis.title.y = element_text(colour = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(legend.position = "top")+
  coord_flip()
ggsave("Outros arquivos/imagens/ic_interior100.png", width = 158, height = 93, units = "mm")



# Proporção de alunas (sexo feminino) ----

# Parâmetro populacional
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
# Gráfico - n=30
ic.alunas_a30 <- data.frame(id = c(1:50, 1:50),
                            intervalo = c(ic.alunas_a30$superior, ic.alunas_a30$inferior),
                            legenda = c(ic.alunas_a30$valor_verdadeiro, ic.alunas_a30$valor_verdadeiro))

ic.alunas_a30 %>%
  ggplot(aes(x=intervalo, y=id, group=id, color=legenda)) +
  geom_point(size = 1) +  
  geom_line(size = .74) +
  scale_colour_manual(name="Contém proporção populacional", values = c("#003366", "#7AA3CC"))+
  labs(x="Intervalos de Confiança", y="Amostras",
       title="Proporção de estudantes meninas",
       subtitle="Amostra de tamanho 30") +
  geom_vline(xintercept = p.alunas, color = "black")+
  annotate(x = p.alunas,y=0, label=expression(p == 0.501),
           geom="text",angle=0,vjust=0,hjust=.7, size=2.5) +
  theme_bw() +
  theme(axis.title.y = element_text(colour = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(legend.position = "top")+
  coord_flip()
ggsave("Outros arquivos/imagens/ic_alunas30.png", width = 158, height = 93, units = "mm")


# Intervalos - n=100
ic.alunas_a100 <- a.tam100 %>%
  filter(SEXO == "B")%>%
  summarise(n = n())%>%
  summarise(n=n,p=n/100)%>%
  mutate(superior = p - z*sqrt(p*(1-p)/100),
         inferior = p + z*sqrt(p*(1-p)/100),
         valor_verdadeiro = ifelse(p.alunas < superior & p.alunas > inferior,
                                   "Sim", "Não"))
# Gráfico - n=100
ic.alunas_a100 <- data.frame(id = c(1:50, 1:50),
                 intervalo = c(ic.alunas_a100$superior, ic.alunas_a100$inferior),
                 legenda = c(ic.alunas_a100$valor_verdadeiro, ic.alunas_a100$valor_verdadeiro))

ic.alunas_a100 %>%
  ggplot(aes(x=intervalo, y=id, group=id, color=legenda)) +
  geom_point(size = 1) +  
  geom_line(size = .74) +
  scale_colour_manual(name="Contém proporção populacional", 
                      values = c("#003366", "#7AA3CC"))+
  labs(x="Intervalos de Confiança", y="Amostras"#,
       #title="Proporção de estudantes meninas",
       #subtitle="Amostra de tamanho 100"
  ) +
  geom_vline(xintercept = p.alunas, color = "black")+
  annotate(x = p.alunas,y=0, label=expression(p == 0.501),
           geom="text",angle=0,vjust=0,hjust=.7, size=2.5) +
  theme_bw() +
  theme(axis.title.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(colour = "black", size = 12),
    axis.text = element_text(colour = "black", size = 9.5),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))+
  theme(legend.position = "top")+
  coord_flip()
ggsave("Outros arquivos/imagens/ic_alunas100.png", width = 158, height = 93, units = "mm")

# Média Nota_LP ----

# Parâmetro populacional
m.lp <- round(mean(dados$NOTA_LP),2)

# Intervalos - n=30
ic.lp_a30 <- a.tam30 %>%
  summarise(superior=mean(NOTA_LP)-z*(sd(NOTA_LP)/sqrt(30)), 
            inferior=mean(NOTA_LP)+z*(sd(NOTA_LP)/sqrt(30)))%>%
  mutate(valor_verdadeiro = ifelse(m.lp < superior & m.lp > inferior, "Sim", "Não"))

# Gráfico - n=30
ic.lp_a30 <- data.frame(id = c(1:50, 1:50),
                        intervalo = c(ic.lp_a30$superior, ic.lp_a30$inferior),
                        legenda = c(ic.lp_a30$valor_verdadeiro, ic.lp_a30$valor_verdadeiro))

ic.lp_a30 %>%
  ggplot(aes(x=intervalo, y=id, group=id, color=legenda)) +
  geom_point(size = 1) +  
  geom_line(size = .74) +
  scale_colour_manual(name="Contém média populacional", 
                      values = c("#003366", "#7AA3CC"))+
  labs(x="Intervalos de Confiança", y="Amostras"#,
       #title="Nota média em Língua Portuguesa",
       #subtitle="Amostra de tamanho 30"
       ) +
  annotate(x = m.lp,y=0, label=expression(mu == 214.56),
           geom="text",angle=0,vjust=0,hjust=.6, size=2.5) +
  geom_vline(xintercept = m.lp, color = "black")+
  theme_bw() +
  theme(axis.title.y = element_text(colour = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(legend.position = "top")+
  coord_flip()
ggsave("Outros arquivos/imagens/ic_lp30.png", width = 158, height = 93, units = "mm")

# Intervalos - n=100
ic.lp_a100 <- a.tam100 %>%
  summarise(superior=mean(NOTA_LP)-z*(sd(NOTA_LP)/sqrt(100)), 
            inferior=mean(NOTA_LP)+z*(sd(NOTA_LP)/sqrt(100)))%>%
  mutate(valor_verdadeiro = ifelse(m.lp < superior & m.lp > inferior, "Sim", "Não"))

# Gráfico - n=100
ic.lp_a100 <- data.frame(id = c(1:50, 1:50),
                         intervalo = c(ic.lp_a100$superior, ic.lp_a100$inferior),
                         legenda = c(ic.lp_a100$valor_verdadeiro, ic.lp_a100$valor_verdadeiro))

ic.lp_a100 %>%
  ggplot(aes(x=intervalo, y=id, group=id, color=legenda)) +
  geom_point(size = 1) +  
  geom_line(size = .74) +
  scale_colour_manual(name="Contém média populacional", 
                      values = c("#003366", "#7AA3CC"))+
  labs(x="Intervalos de Confiança", y="Amostras"#,
       #title="Nota média em Língua Portuguesa",
       #subtitle="Amostra de tamanho 100"
  ) +
  annotate(x = m.lp,y=0, label=expression(mu == 214.56),
           geom="text",angle=0,vjust=0,hjust=.6, size=2.5) +
  geom_vline(xintercept = m.lp, color = "black")+
  theme_bw() +
  theme(axis.title.y = element_text(colour = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(legend.position = "top")+
  coord_flip()
ggsave("Outros arquivos/imagens/ic_lp100.png", width = 158, height = 93, units = "mm")

# Média Nota_MT ----

# Parâmetro populacional
m.mt <- round(mean(dados$NOTA_MT),2)

# Intervalos - n=30
ic.mt_a30 <- a.tam30 %>%
  summarise(superior=mean(NOTA_MT)-z*(sd(NOTA_MT)/sqrt(30)), 
            inferior=mean(NOTA_MT)+z*(sd(NOTA_MT)/sqrt(30)))%>%
  mutate(valor_verdadeiro = ifelse(m.mt < superior & m.mt > inferior, "Sim", "Não"))

# Gráfico - n=100
ic.mt_a30 <- data.frame(id = c(1:50, 1:50),
                         intervalo = c(ic.mt_a30$superior, ic.mt_a30$inferior),
                         legenda = c(ic.mt_a30$valor_verdadeiro, ic.mt_a30$valor_verdadeiro))

ic.mt_a30 %>%
  ggplot(aes(x=intervalo, y=id, group=id, color=legenda)) +
  geom_point(size = 1) +  
  geom_line(size = .74) +
  scale_colour_manual(name="Contém média populacional", 
                      values = c("#003366", "#7AA3CC"))+
  labs(x="Intervalos de Confiança", y="Amostras",
       title="Nota média em Matemática",
       subtitle="Amostra de tamanho 30") +
  annotate(x = m.mt,y=0, label=expression(mu == 221.98),
           geom="text",angle=0,vjust=0,hjust=.6, size=2.5) +
  geom_vline(xintercept = m.mt, color = "black")+
  theme_bw() +
  theme(axis.title.y = element_text(colour = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(legend.position = "top")+
  coord_flip()
ggsave("Outros arquivos/imagens/ic_mt30.png", width = 158, height = 93, units = "mm")

# Intervalos - n=100
ic.mt_a100 <- a.tam100 %>%
  summarise(superior=mean(NOTA_MT)-z*(sd(NOTA_MT)/sqrt(100)), 
            inferior=mean(NOTA_MT)+z*(sd(NOTA_MT)/sqrt(100)))%>%
  mutate(valor_verdadeiro = ifelse(m.mt < superior & m.mt > inferior, "Sim", "Não"))

# Gráfico - n=100
ic.mt_a100 <- data.frame(id = c(1:50, 1:50),
                         intervalo = c(ic.mt_a100$superior, ic.mt_a100$inferior),
                         legenda = c(ic.mt_a100$valor_verdadeiro, ic.mt_a100$valor_verdadeiro))

ic.mt_a100 %>%
  ggplot(aes(x=intervalo, y=id, group=id, color=legenda)) +
  geom_point(size = 1) +  
  geom_line(size = .74) +
  scale_colour_manual(name="Contém média populacional", 
                      values = c("#003366", "#7AA3CC"))+
  labs(x="Intervalos de Confiança", y="Amostras"#,
       #title="Nota média em Matemática",
       #subtitle="Amostra de tamanho 100"
  ) +
  annotate(x = m.mt,y=0, label=expression(mu == 221.98),
           geom="text",angle=0,vjust=0,hjust=.6, size=2.5) +
  geom_vline(xintercept = m.mt, color = "black")+
  theme_bw() +
  theme(axis.title.y = element_text(colour = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(legend.position = "top")+
  coord_flip()
ggsave("Outros arquivos/imagens/ic_mt100.png", width = 158, height = 93, units = "mm")
