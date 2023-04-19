# Carga de paquetes --------

library(tidyverse)
library(readxl)
library(janitor)
library(ggsci)
library(ggExtra)
library(countrycode)

# Definiciones -----------

theme_desuc <- list(theme_minimal(base_family = 'Roboto Condensed'),
                    theme(text = element_text(size = 25),
                          plot.title.position = 'plot',
                          legend.position = 'bottom',
                          legend.title=element_blank(),
                          legend.text = element_text(size = rel(.6)),
                          plot.caption = element_text(color = 'grey30', size = rel(0.5)),
                          plot.title = element_text(color = "#DC0000B2", face = "bold", size = rel(1.9), family = "Roboto Condensed"),
                          axis.title.y = element_text(size = rel(0.8), color = '#0072B5FF'), 
                          axis.title.x = element_text(size = rel(0.8), color = '#7876B1FF'), 
                          axis.text.y = element_text(size = rel(.7)),
                          axis.text.x = element_text(size = rel(.7), angle = 90),
                          plot.background = element_rect(fill = 'white', color = NA)))

# Guardar

gg_save_desuc <- function(gg_chart, name,
                          width = 22,
                          height = 13) {
  ggsave(filename = paste0('output/', name),
         plot = gg_chart,
         width = width,
         height = height,
         units = 'cm')
}

# Lectura bases ------------------

pobreza <- read_xlsx('input/ilc_peps01n_page_spreadsheet.xlsx',
                     sheet = 'Sheet 1',
                     range = 'A10:N54') %>% # obtenido en: https://ec.europa.eu/eurostat/databrowser/view/ilc_peps01n/default/table?lang=en
  clean_names() %>% 
  select(time,x2020)

inequidad <- read_xlsx('input/tespm151_page_spreadsheet.xlsx',
                       sheet = 'Sheet 1',
                       range = 'A10:V55') %>% # obtenido en: https://ec.europa.eu/eurostat/databrowser/view/TESPM151/default/table
  clean_names() %>% 
  select(time,x2021)

# Manipulación base ------------

df <- left_join(inequidad,
                pobreza,
                by = 'time')

rm(inequidad,pobreza)

df <- df[9:45,]

df <- df %>% 
  mutate_at(vars(starts_with('x')), ~as.numeric(.)) %>% 
  filter(!(is.na(x2020))) %>% 
  filter(!(is.na(x2021))) %>% 
  mutate(time = gsub("Germany.*","Germany",time))

nombres <- countrycode::codelist

# Gráfico --------------

gg <- df %>% 
  ggplot(aes(x=x2020, y=x2021, label=time)) +
  geom_point(size = 3, color = '#7E6148B2')  +
  geom_text(nudge_x = 0.35, nudge_y = 0.35, check_overlap = T, size = 8,
            family = 'Roboto Condensed') + 
  scale_x_continuous(limits = c(0,38)) +
  scale_y_continuous(limits = c(0,10)) + 
  theme_desuc 

gg1 <- gg +
  labs(title = 'Relación entre\nInequidad en la distribución del ingreso\ny % de personas en riesgo de pobreza',
       subtitle = 'En países de Europa',
       caption = 'Fuente: Eurostat\nNota: datos de pobreza del año 2020 y de inequidad del año 2021\nInequidad: razón entre el ingreso total recibido por el quintil superior y el recibido por el quintil más bajo de ingresos',
       x = '% personas en riesgo de pobreza',
       y = 'Índice de inequidad en distribución del ingreso')

gg1 <- ggMarginal(gg1, type="histogram",
            xparams = list(fill = '#7876B1FF'),
            yparams = list(fill = '#0072B5FF')) 

gg_save_desuc(gg1, 
              width = 50,
              height = 25,
              'day18_eurostat_2.png') 

gg2 <- gg +
  labs(title = 'Relación entre Inequidad en\ndistribución del ingreso\ny % personas en riesgo\nde pobreza',
       subtitle = 'En países de Europa',
       caption = 'Fuente: Eurostat\nNota: datos de pobreza del año 2020 y de inequidad del año 2021\nInequidad: razón entre el ingreso total recibido por el quintil superior y el recibido por el quintil más bajo de ingresos',
       x = '% personas en riesgo de pobreza',
       y = 'Índice de inequidad en distribución del ingreso')

gg2 <- ggMarginal(gg2, type="histogram",
                  xparams = list(fill = '#7876B1FF'),
                  yparams = list(fill = '#0072B5FF')) 


gg_save_desuc(gg2,
               width = 28,
               height = 25,
               'day18_eurostat_1.png')
