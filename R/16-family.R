library(ggplot2)
library(emoGG)
library(dplyr)
library(tidyr)
library(survey)
library(scales)

#carga de datos

casen_1990 <- read_sav("input/casen1990.sav")
casen_2000 <- read_sav("input/casen2000.sav")
casen_2011 <- read_sav("input/casen2011.sav")
casen_2017 <- read_sav("input/casen2017.sav")

# casen_1990

cols<- c("sexo","pco1")
casen_1990[cols] <- lapply(casen_1990[cols], as.factor)
casen_1990 <-subset(casen_1990, pco1==1)

casensvy <- survey::svydesign(id=~1,
                              data = casen_1990,
                              weights=~casen_1990$expr)

svymean(~sexo, casensvy) %>% 
  as.data.frame() %>% 
  select(-c(SE)) %>% 
  mutate(mean=round(mean*100,1),
         sexo=ifelse(row.names(.) == "sexo1", "Hombre", "Mujer")) %>% 
  pivot_wider(names_from = sexo, values_from = mean) %>% 
  mutate(prop_diff=Hombre-Mujer,
         año=1990) ->df_1990


# casen_2000

casen_2000[cols] <- lapply(casen_2000[cols], as.factor)
casen_2000 <-subset(casen_2000, pco1==1)

casensvy <- survey::svydesign(id=~1,
                              data = casen_2000,
                              weights=~casen_2000$expr)

svymean(~sexo, casensvy) %>% 
  as.data.frame() %>% 
  select(-c(SE)) %>% 
  mutate(mean=round(mean*100,1),
         sexo=ifelse(row.names(.) == "sexo1", "Hombre", "Mujer")) %>% 
  pivot_wider(names_from = sexo, values_from = mean) %>% 
  mutate(prop_diff=Hombre-Mujer,
         año=2000) ->df_2000

# casen_2011

casen_2011[cols] <- lapply(casen_2011[cols], as.factor)
casen_2011 <-subset(casen_2011, pco1==1)

casensvy <- survey::svydesign(id=~1,
                              data = casen_2011,
                              weights=~casen_2011$expr_full)


svymean(~sexo, casensvy) %>% 
  as.data.frame() %>% 
  select(-c(SE)) %>% 
  mutate(mean=round(mean*100,1),
         sexo=ifelse(row.names(.) == "sexo1", "Hombre", "Mujer")) %>% 
  pivot_wider(names_from = sexo, values_from = mean) %>% 
  mutate(prop_diff=Hombre-Mujer,
         año=2011) ->df_2011

# casen_2017

casen_2017[cols] <- lapply(casen_2017[cols], as.factor)
casen_2017 <-subset(casen_2017, pco1==1)

casensvy <- survey::svydesign(id=~1,
                              data = casen_2017,
                              weights=~casen_2017$expr)

svymean(~sexo, casensvy) %>% 
  as.data.frame() %>% 
  select(-c(SE)) %>% 
  mutate(mean=round(mean*100,1),
         sexo=ifelse(row.names(.) == "sexo1", "Hombre", "Mujer")) %>% 
  pivot_wider(names_from = sexo, values_from = mean) %>% 
  mutate(prop_diff=Hombre-Mujer,
         año=2017) ->df_2017

# join df

casen_df2 <- rbind(df_1990, df_2000, df_2011, df_2017) %>% 
  mutate(label=round(prop_diff,0),
         Hombre=Hombre/100,
         Mujer=Mujer/100,
         prop_diff=prop_diff/100)


# tema grafico

theme_desuc <- list(theme_minimal(base_family = 'Impact'),
                    theme(text = element_text(size = 30),
                          plot.title.position = 'plot',
                          legend.position = 'none',
                          plot.caption = element_text(color = "#636363", size = rel(0.3), family = "Helvetica"),
                          plot.title = element_text(color = "#F07167", face = "bold",size = rel(1.5)),
                          plot.subtitle = element_text(color="#F49690", size = rel(1.0), margin = margin(b = 20)),
                          panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
                          axis.title = element_text(size = rel(0.8)), 
                          axis.text.y = element_text(size = rel(.7)),
                          axis.title.x = element_blank(),
                          axis.text.x = element_text(size = rel(.7)),
                          plot.background = element_rect(fill = '#FFFAF2', color = NA)))



#grafico

casen_df2 %>% 
  ggplot(aes(x = año, y = prop_diff)) +
  geom_segment(aes(x = as.character(año), y = Mujer, xend = as.character(año), yend = Hombre),linewidth=1.5, color= '#00AFB9')+
  geom_emoji(aes(x = as.character(año), y = Mujer), emoji = "1f469") +
  geom_emoji(aes(x = as.character(año), y = Hombre), emoji = "1f466")+
  geom_text(aes(x = as.character(año), y = (Mujer + Hombre) / 2, label = label), 
            size =5, fontface = "bold", color = "#0081A7", vjust = -0.5, family = 'Impact')+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Diferencia* en jefatura\nde hogares en Chile, 1990-2017",
       subtitle = "Proporción de jefes de familia por género",
       caption = "\n*La diferencia se muestra en puntos porcentuales\nCASEN (1990-2017)",
       x = "", y = "Porcentaje (%)") + theme_desuc


casen_df2 %>%
  ggplot(aes(x = año, y = prop_diff)) +
  geom_segment(aes(x = as.numeric(año), y = Mujer, xend = as.numeric(año), yend = Hombre), 
               linewidth=1.5, color= '#00AFB9')+
  geom_emoji(aes(x = año, y = Mujer), emoji = "1f469") +
  geom_emoji(aes(x = año, y = Hombre), emoji = "1f466")+
  geom_text(aes(x = año, y = (Mujer + Hombre) / 2, label = label), 
            size =5, fontface = "bold", color = "#0081A7", vjust = -0.5, family = 'Impact')+
  coord_flip()+
  scale_x_continuous( breaks = unique(as.numeric(casen_df2$año))) +
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Diferencia* en jefatura\nde hogares en Chile, 1990-2017",
       subtitle = "Proporción de jefes de familia por género",
       caption = "\n*La diferencia se muestra en puntos porcentuales\nCASEN (1990-2017)",
       x = "", y = "Porcentaje (%)")+ theme_desuc ->plot


gg_save_desuc(plot, 
              width = 28,
              height = 25,
              'day16_family.png')

gg_save_desuc(plot, 
              width = 50,
              height = 25,
              'day16_family_2.png')



