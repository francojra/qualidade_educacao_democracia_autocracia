
# Qualidade de educação - Democracia e Autocracia ------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 26/01/23 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/quality-of-education ------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### 

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
