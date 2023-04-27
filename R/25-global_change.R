
library(rio)
library(dplyr)
library(ggplot2)



theme_desuc <- list(theme_minimal(base_family = 'Roboto'),
                    theme(text = element_text(size = 14),
                          plot.caption = element_text(color = 'grey40', size = rel(0.5)),
                          plot.title = element_text(color = "grey40", face = "bold", size = rel(1.2), family = "Roboto"),
                          plot.subtitle = element_text(color = 'grey40', size = rel(0.9)),
                          axis.text.x=element_blank(),
                          axis.text.y=element_blank(),
                          panel.grid = element_blank(),
                          legend.position = "bottom"))


df <- import('mp10.csv')
df3 <- import('df2.csv')



df3_s <- df3 %>% 
  filter(COMUNA %in% c("Las Condes", "El Bosque"))



mp1 <- df3_s %>% ggplot( aes(x=FECHA2, y=TOTAL, group=COMUNA, color=COMUNA)) +
  geom_line() + 
  transition_reveal(FECHA2) +
  labs(title = "Cantidad de material particulado (10)",
       subtitle = "Comunas: Las Condes, El Bosque. Invierno 2022",
       caption = "Fuente: Sistema de información Nacional de Calidad del Aire.\nEl gráfico muestra la cantidad de material particulado (10) entre el 21 de junio 2022 y el 21 de septiembre del 2022",
       x = "Invierno", y = "Número de Material Particulado en aire") + theme_desuc


# Save at gif:
anim_save("mp1.gif",
          height = 6, 
          width = 8, 
          units = "in", 
          res = 150)
