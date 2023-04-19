library(tidyverse)
library(sjmisc)
library(png)
library(grid)

library(strex)
library(desuctools)

goodreads <- read_csv("input/goodreads.csv")

colnames(goodreads)

goodreads <- goodreads %>% 
  select(title, author, rating, pages, genres, publishDate, numRatings, ratingsByStars, likedPercent, language) %>% 
  slice_head(n = 30)

goodreads <- goodreads %>% 
  mutate(estrella_1 = str_nth_number(ratingsByStars, n = 5),
         estrella_2 = str_nth_number(ratingsByStars, n = 4),
         estrella_3 = str_nth_number(ratingsByStars, n = 3),
         estrella_4 = str_nth_number(ratingsByStars, n = 2),
         estrella_5 = str_nth_number(ratingsByStars, n = 1))

goodreads <- goodreads %>% 
  select(!c(language, numRatings, ratingsByStars, likedPercent))

goodreads_longer <- goodreads %>% 
  pivot_longer(cols = 7:11, names_to = "estrellas", values_to = "n_votaciones")

goodreads_longer <- goodreads_longer %>% 
  group_by(title) %>% 
  mutate(estrellas_per = n_votaciones/sum(n_votaciones)) %>% 
  mutate(estrellas_rec = if_else(estrellas %in% c("estrella_1","estrella_2"), estrellas_per*-1, estrellas_per*1))

goodreads_longer <- goodreads_longer %>% 
  ungroup() %>% 
  mutate(estrellas = as_factor(estrellas)) %>% 
  mutate(title = fct_reorder_cat(.f = title, .cat = estrellas, .val = estrellas_per, cat_orden = "estrella_3")) %>% 
  mutate(filtro = if_else(estrellas_per > 0.05, estrellas_per, NA))

colores <- c("#006ba6","#0496ff","#ffbc42","#d81159","#8f2d56")

gg_goodreads <- goodreads_longer %>% 
  ggplot(aes(x = title, y = estrellas_rec, group = title, fill = estrellas)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "#db7f8e", linewidth = 1) +
  geom_text(aes(label = round(filtro*100)),
            family = "Roboto Condensed", fontface = "bold",
            color = "#eaf2d7",
            position = position_stack(vjust = .5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_y_continuous(label = scales::percent, limits = c(-.5,1)) +
  scale_fill_manual(values = colores, 
                    name = "Número de estrellas",
                    #labels = c("🔥","🔥🔥","🔥🔥🔥","🔥🔥🔥🔥","🔥🔥🔥🔥🔥"),
                    labels = c("1 estrella","2 estrellas","3 estrellas","4 estrellas","5 estrellas")) +
  guides(fill = guide_legend(override.aes = list(shape = 20, size = 5))) + 
  labs(title = "#30DíasDeGráficos: Positivo-Negativo",
       subtitle = "Porcentaje de puntuaciones recibidas por estrella en Goodreads.",
       x = "", y = "Porcentaje de puntuaciones",
       caption = "Fuente: Datos extraídos desde Goodreads. Primeras 25 observaciones.") +
  coord_flip() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = 10, color = "#eaf2d7"),
        axis.ticks = element_blank(),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        legend.position = "bottom",
        legend.justification = "right",
        legend.background = element_rect(fill = "#db7f8e"),
        legend.text = element_text(family = "Martel", color = "#eaf2d7", size = 14),
        legend.title = element_text(family = "Martel", color = "#eaf2d7", face = "bold"),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.key = element_rect(fill = "#db7f8e"),
        panel.background = element_rect(fill = "#ffdbda"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#8f2d56", linetype = "dotted"),
        plot.background = element_rect(fill = "#db7f8e"),
        plot.title = element_text(family = "Work Sans", size = 24, color = "#8f2d56"),
        plot.caption = element_text(family = "IM FELL French Canon", color = "#eaf2d7", size = 14),
        plot.subtitle = element_text(family = "IM FELL French Canon", color = "#eaf2d7", size = 16),
        plot.title.position = "plot",
        axis.text.y = element_text(family = "Martel", color = "#eaf2d7", size = 10),
        axis.title = element_text(family = "Martel", color = "#eaf2d7"))

gg_goodreads

ggsave(gg_goodreads, width = 25, height = 25, units = "cm",filename = "output/day15_positive-negative.png")

