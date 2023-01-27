
# Qualidade de educação - Democracia e Autocracia ------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 26/01/23 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/quality-of-education ------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### O Program for International Student Assessment (PISA), que é coordenado pelo OECD, é a 
### mais bem conhecida avaliação internacional dos resultados de aprendizagem

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

pisa_read <- read.csv("pisa-test-score-mean-performance-on-the-reading-scale.csv")
view(pisa_read)
names(pisa_read)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

pisa_read <- pisa_read %>%
  select(-Code) %>%
  rename(performance = PISA..Mean.performance.on.the.reading.scale) %>%
  view()

pisa_read1 <- pisa_read %>%
  filter(Entity %in% c("China", "United States", "Japan", "Germany"),
         Year %in% c("2009", "2012", "2015")) %>%
  group_by(Entity) %>%
  summarise(media = mean(performance),
            sd = sd(performance), n = n(),
            se = sd/sqrt(n)) %>%
  view()

pisa_read2 <- pisa_read %>%
  filter(Entity %in% c("China", "United States", "Japan", "Germany"),
         Year %in% c("2009", "2012", "2015")) %>%
  view()

pisa_read3 <- pisa_read %>%
  filter(Entity %in% c("China", "United States", "Brazil"),
         Year %in% c("2009", "2012", "2015")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 4)

ggplot(pisa_read1, aes(x = fct_reorder(Entity, media), 
                       y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2, size = 0.8) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Estados Unidos", "Alemanha",
                              "Japão", "China")) +
  labs(x = "Países", y = "Desempenho em leitura \nPISA test score") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(pisa_read2, aes(x = factor(Year), y = performance,
                       color = Entity, group = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                                "#DDCC77", "#117733"),
                     labels = c("China", "Alemanha",
                                "Japão", "Estados Unidos")) +
  scale_x_discrete(expand = expansion(mult = c(0.1,0.1))) +
  labs(x = "Tempo (anos)", 
       y = "Desempenho em leitura \nPISA test score",
       color = "Países") +
  theme_ipsum(axis_text_size = 14, 
              axis_title_size = 16) +
  theme(axis.text = element_text(color = "black"))
                                 
ggplot(pisa_read3, aes(x = factor(Year), y = performance,
                       color = Entity, group = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  scale_x_discrete(expand = expansion(mult = c(0.1,0.1))) +
  labs(x = "Tempo (anos)", 
       y = "Desempenho em leitura \nPISA test score",
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
