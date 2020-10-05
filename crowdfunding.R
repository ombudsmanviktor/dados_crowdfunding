
# Requisitar bibliotecas
library(ggplot2)
library(dplyr)
library(forcats)
library(hrbrthemes)
#install.packages("waffle", repos = "https://cinc.rud.is")
library(magrittr)
library(waffle)
library(patchwork)
library(cowplot)

# GRÁFICO: Quantidade em março e setembro
apoiase_total_duo <- as.data.frame(c('Março'=63, 'Setembro'=  110))
apoiase_total_duo <- apoiase_total_duo %>% 
  rename(frequencia = `c(Março = 63, Setembro = 110)`)
library(tibble)
apoiase_total_duo <- rownames_to_column(apoiase_total_duo, var="mes") %>% head
# PLOT 1:
ggplot(apoiase_total_duo, aes(x=reorder(mes, -frequencia), y=frequencia)) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  coord_flip() +
  theme_ipsum() + 
  labs(title = "Gráfico 1: Campanhas de financiamento coletivo de direita", 
       subtitle = "Propostas disponíveis em março e setembro no site Apoia.se (N = 173)",
       x = "Mês",
       y = "Quantidade (valores absolutos)",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR")


# GRÁFICO: Waffle
apoiase_total_marco <- apoiase %>%
  filter(Raspagem == "Março") %>% 
  count()
apoiase_total_setembro <- apoiase %>%
  filter(Raspagem == "Setembro") %>% 
  count()
apoiase_total_ambos <- apoiase %>%
  filter(Raspagem == "c(Março,Setembro)") %>% 
  count()
apoiase_total <- c(16, 47, 63)
apoiase_total <- c('Exclusivamente em março'=16, 'Em Marco e Setembro'=  47, 
            'Exclusivamente em Setembro'=63)
waffle(apoiase_total, rows=10,
       colors = c("#ed5544", "#f6a21c", "#4eb19a"), legend_pos = "bottom") + theme_enhance_waffle() +
  #scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme_ipsum() + 
  labs(title = "Gráfico 2: Campanhas por período de coleta", 
       subtitle = "Propostas disponíveis em março e setembro no site Apoia.se (N = 126)",
       x = "Quantidade (valores absolutos)",
       y = "",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR")

# GRÁFICO: Finalidades
apoiase_finalidade_marco <- apoiase %>%
  filter(Raspagem == "Março" | Raspagem == "c(Março,Setembro)") %>% 
  count(Finalidade)
apoiase_finalidade_marco <- apoiase_finalidade_marco %>% 
  mutate(percent = (n/63*100))
apoiase_finalidade_setembro <- apoiase %>%
  filter(Raspagem == "Setembro" | Raspagem == "c(Março,Setembro)") %>% 
  count(Finalidade)
apoiase_finalidade_setembro <- apoiase_finalidade_setembro %>% 
  mutate(percent = (n/110*100))
#PLOT 1
p1 <- ggplot(apoiase_finalidade_marco, aes(x=percent, y=Finalidade)) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  coord_flip() +
  xlim(0,100) +
  theme_ipsum() + 
  labs(title = "Gráfico 3: Finalidades das campanhas em março",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
    subtitle = "N = 63",
       x = "Quantidade %",
       y = "Finalidade")
# PLOT 2
p2 <- ggplot(apoiase_finalidade_setembro, aes(x=percent, y=Finalidade)) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  coord_flip() +
  xlim(0,100) +
  theme_ipsum() + 
  labs(title = "Gráfico 4: Finalidades das campanhas em setembro",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
    subtitle = "N = 110",
       x = "Quantidade %",
       y = "Finalidade")
#install.packages("patchwork")
p1 + p2

# GRÁFICO: Tipo de Apoio
apoiase_tipo_marco <- apoiase %>%
  filter(Raspagem == "Março" | Raspagem == "c(Março,Setembro)") %>% 
  count(Apoio_tipo)
apoiase_tipo_marco <- apoiase_tipo_marco %>% 
  mutate(percent = (n/63)*100)
apoiase_tipo_setembro <- apoiase %>%
  filter(Raspagem == "Setembro" | Raspagem == "c(Março,Setembro)") %>% 
  count(Apoio_tipo)
apoiase_tipo_setembro <- apoiase_tipo_setembro %>% 
  mutate(percent = (n/110)*100)
#PLOT 1
p1 <- ggplot(apoiase_tipo_marco, aes(y=percent, x=Apoio_tipo)) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  #coord_flip() +
  ylim(0,70) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Gráfico 5: Ações financiadas em março",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 63",
       x = "Destino do apoio",
       y = "Quantidade %")
# PLOT 2
p2 <- ggplot(apoiase_tipo_setembro, aes(y=percent, x=Apoio_tipo)) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  #coord_flip() +
  ylim(0,70) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Gráfico 6: Ações financiadas em setembro",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 110",
       x = "Destino do apoio",
       y = "Quantidade %")
#install.packages("patchwork")
p1 + p2


# GRÁFICO: Mídia
library(tidyr)
apoiase_midia_marco <- apoiase %>%
  filter(Raspagem == "Março" | Raspagem == "c(Março,Setembro)") %>% 
  count(Midia) %>% drop_na(Midia)
apoiase_midia_marco <- apoiase_midia_marco %>% 
  mutate(percent = (n/43)*100)
apoiase_midia_setembro <- apoiase %>%
  filter(Raspagem == "Setembro" | Raspagem == "c(Março,Setembro)") %>% 
  count(Midia) %>% drop_na(Midia)
apoiase_midia_setembro <- apoiase_midia_setembro %>% 
  mutate(percent = (n/79)*100)
#PLOT 1
p1 <- ggplot(apoiase_midia_marco, aes(y=percent, x=Midia)) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  #coord_flip() +
  ylim(0,35) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Gráfico 7: Canais de comunicação financiados em março",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 43",
       x = "Tipo de Mídia",
       y = "Quantidade %")
# PLOT 2
p2 <- ggplot(apoiase_midia_setembro, aes(y=percent, x=Midia)) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  #coord_flip() +
  ylim(0,35) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Gráfico 8: Canais de comunicação financiados em setembro",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 79",
       x = "Tipo de Mídia",
       y = "Quantidade %")
#install.packages("patchwork")
p1 + p2


# GRÁFICO: Estado
apoiase_estado_marco <- apoiase %>%
  filter(Raspagem == "Março" | Raspagem == "c(Março,Setembro)") %>% 
  count(Estado) %>% drop_na(Estado)
apoiase_estado_marco <- apoiase_estado_marco %>% 
  mutate(percent = (n/63)*100)
apoiase_estado_setembro <- apoiase %>%
  filter(Raspagem == "Setembro" | Raspagem == "c(Março,Setembro)") %>% 
  count(Estado) %>% drop_na(Estado)
apoiase_estado_setembro <- apoiase_estado_setembro %>% 
  mutate(percent = (n/110)*100)
#PLOT 1
p1 <- ggplot(apoiase_estado_marco, aes(y=percent, x=reorder(Estado, percent))) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  coord_flip() +
  ylim(0,25) +
  #scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Gráfico 9: Estado de origem das campanhas em março",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 63",
       x = "Estado",
       y = "Quantidade %")
# PLOT 2
p2 <- ggplot(apoiase_estado_setembro, aes(y=percent, x=reorder(Estado, percent))) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  coord_flip() +
  ylim(0,25) +
  #scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Gráfico 10: Estado de origem das campanhas em setembro",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 110",
       x = "Estado",
       y = "Quantidade %")
#install.packages("patchwork")
p1 + p2


# GRÁFICO: Regiao
apoiase_regiao_marco <- apoiase %>%
  filter(Raspagem == "Março" | Raspagem == "c(Março,Setembro)") %>% 
  count(Regiao) %>% drop_na(Regiao)
apoiase_regiao_marco <- apoiase_regiao_marco %>% 
  mutate(percent = (n/52)*100)
apoiase_regiao_setembro <- apoiase %>%
  filter(Raspagem == "Setembro" | Raspagem == "c(Março,Setembro)") %>% 
  count(Regiao) %>% drop_na(Regiao)
apoiase_regiao_setembro <- apoiase_regiao_setembro %>% 
  mutate(percent = (n/80)*100)
#PLOT 1
p1 <- ggplot(apoiase_regiao_marco, aes(y=percent, x=reorder(Regiao, percent))) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  #coord_flip() +
  ylim(0,70) +
  #scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Região de origem das campanhas em março",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 63",
       x = "Região do Brasil",
       y = "Quantidade %")
# PLOT 2
p2 <- ggplot(apoiase_regiao_setembro, aes(y=percent, x=reorder(Regiao, percent))) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  #coord_flip() +
  ylim(0,70) +
  #scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Região de origem das campanhas em setembro",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 110",
       x = "Região do Brasil",
       y = "Quantidade %")
#install.packages("patchwork")
p1 + p2


# GRÁFICO: Apoiadores Faixa
apoiase_apoiadores_marco <- apoiase %>%
  filter(Raspagem == "Março" | Raspagem == "c(Março,Setembro)") %>% 
  count(Apoiadores_faixa) %>% 
  mutate(percent = (n/63)*100)
apoiase_apoiadores_setembro <- apoiase %>%
  filter(Raspagem == "Setembro" | Raspagem == "c(Março,Setembro)") %>% 
  count(Apoiadores_faixa)  %>% 
  mutate(percent = (n/110)*100)
#PLOT 1
p1 <- ggplot(apoiase_apoiadores_marco, aes(y=percent, x=Apoiadores_faixa)) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  #coord_flip() +
  ylim(0,100) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Apoiadores das campanhas de crowdfunding em março de 2020",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 108",
       x = "Quantidade de apoiadores",
       y = "Frequência %")
# PLOT 2
p2 <- ggplot(apoiase_apoiadores_setembro, aes(y=percent, x=Apoiadores_faixa)) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  #coord_flip() +
  ylim(0,100) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Apoiadores das campanhas de crowdfunding em setembro de 2020",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 156",
       x = "Quantidade de apoiadores",
       y = "Frequência %")
#install.packages("patchwork")
p1 + p2


# GRÁFICO: Meta Faixa
apoiase_metas_marco <- apoiase %>%
  filter(Raspagem == "Março" | Raspagem == "c(Março,Setembro)") %>% 
  count(Meta_faixa) %>% 
  mutate(percent = (n/63)*100)
apoiase_metas_marco$Meta_faixa <- as.character(apoiase_metas_marco$Meta_faixa)
#apoiase_metas_marco$Meta_faixa[apoiase_metas_marco$Meta_faixa == "Mais de 10 mil"] <- "mail de R$10 mil"
apoiase_metas_marco$Meta_faixa  <-  factor(apoiase_metas_marco$Meta_faixa,levels = c("R$ 0.00",
                                                  "entre R$1 e R$499", "entre R$500 e R$999",
                                                  "entre R$1.000 e R$4.999", "entre R$5.000 e R$9.999",
                                                  "mais de R$10 mil"))
apoiase_metas_setembro <- apoiase %>%
  filter(Raspagem == "Setembro" | Raspagem == "c(Março,Setembro)") %>% 
  count(Meta_faixa)  %>% 
  mutate(percent = (n/110)*100)
apoiase_metas_setembro$Meta_faixa <- as.character(apoiase_metas_setembro$Meta_faixa)
#apoiase_metas_setembro$Meta_faixa[apoiase_metas_setembro$Meta_faixa == "Mais de 10 mil"] <- "mail de R$10 mil"
apoiase_metas_setembro$Meta_faixa  <-  factor(apoiase_metas_setembro$Meta_faixa,levels = c("R$ 0.00",
                                                  "entre R$1 e R$499", "entre R$500 e R$999",
                                                  "entre R$1.000 e R$4.999", "entre R$5.000 e R$9.999",
                                                  "mais de R$10 mil"))
#PLOT 1
p1 <- ggplot(apoiase_metas_marco, aes(y=percent, x=Meta_faixa)) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  #coord_flip() +
  ylim(0,40) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Gráfico 11: Metas das campanhas em março",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 63",
       x = "Valores das metas",
       y = "Quantidade %")
# PLOT 2
p2 <- ggplot(apoiase_metas_setembro, aes(y=percent, x=Meta_faixa)) + 
  geom_bar(stat = "identity", fill="#ed5544", alpha=.8, width=.9) +
  #coord_flip() +
  ylim(0,40) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_ipsum() + 
  labs(title = "Gráfico 12: Metas das campanhas em setembro",
       caption = "Fonte: coLAB/UFF + CPOP/UFPR",
       subtitle = "N = 110",
       x = "Valores das metas",
       y = "Quantidade %")
#install.packages("patchwork")
p1 / p2


#FIM






