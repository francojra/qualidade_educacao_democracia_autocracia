
# Qualidade de educação - Democracia e Autocracia ------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 26/01/23 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/quality-of-education ------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### O Program for International Student Assessment (PISA), que é coordenado pelo OECD, é a 
### mais bem conhecida avaliação internacional dos resultados de aprendizagem. O primeiro
### estudo do PISA foi conduzido em 1997 e desde então ele foi repetido a cada três anos.

### O estudo não seleciona estudantes por grau, mas por idade e a partir dos 15 anos, qualquer
### que seja o nível escolar. Em duas horas de avaliação, as competências em leitura, matemática
### e ciências desses estudantes são avaliadas. Trata-se de um compromisso muito substancial e
### até 2017 "meio milhão de estudantes representando 28 milhões com 15 anos de idade em 72 países
### e economias tem participado do PISA" de acordo com a OECD. A cobertura do PISA pode ser vista
### nesse mapa sobre os resultados da dimensão em leitura, ele obviamente inclui os países mais
### ricos da OECD e infelizmente apenas poucos países mais pobres. Países mais pobres também não
### são testados regularmente e participaram apenas em uma etapa, e adicionalmente ele pode ter
### sido o caso nem todos os estudantes desses países mais pobres foram selecionados, mas em vez 
### disso, apenas em regiões específicas.

### Enquanto o TIMSS foca sobre o conteúdo que é coberto na escola, o PISA tem o objetivo de 
### "avaliar a aplicação das habilidades para problemas da vida real" e "enfatizar a importância
### do contexto em que estudantes deveriam ser capazes de usar as habilidades deles (escolas, casas
### sociedade).

### O teste do PISA avalia os estudantes em três dimensões, que são definidas como as seguintes:

### Alfabetização científica: definida como a habilidade para engajar com temas relacionados à
### ciência, e com ideias sobre ciência, como um cidadão reflexivo. Uma pessoa cientificamente
### alfabetizada está disposta a engajar em discursos sobre ciência e tecnologia, que requer as 
### competências para explicar fenômenos científicos, avaliar e conceber investigação científica,
### e interpretar dados e evidências cientificamente.

### Alfabetização em leitura: é definida como a habilidade dos estudantes para entender, usar,
### refletir sobre e engajar com textos escritos a fim de alcançar os objetivos deles, 
### desenvolver o conhecimento e potencial deles, e participar em sociedade.

### Alfabetização em matemática: é definida como a capacidade dos estudantes de formular, empregar
### e interpretar matemática em uma variedade de contextos. Ele inclui raciocínio matemático e uso de
### conceitos, procedimentos, fatos e ferramentas da matemática para descrever, explicar e predizer
### fenômenos. Ela ajuda os indivíduos no reconhecimento do papel da matemática no mundo e para
### fazer bem fundados julgamentos e decisões necessárias para cidadãos construtivos, empenhados
### e reflexivos.

### Países que não são da OECD e que apresentaram o teste PISA, não incluem estudantes de 
### todas as regiões. Por exemplo, a China apresentou teste para apenas quatro provícias chinesas:
### Beijing, Shanghai, Jiangsu and Guangdong. Essas regiões não são representativas da China como
### um todo e existe razão para esperar que os estudantes dessas províncias tenham acesso a melhor
### educação que a média dos chineses com 15 anos de idade. As quatro regiões estão entre as mais
### ricas da China e a renda é duas vezes maior que a média do país.

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
