# ---------------------------------------------------------------------------- #
#             Script #30DayChartChallenge 2023 - Día 20: correlation
#
#  Analisis de correlación entre la esperanza de vida y la media de escolaridad
#
#  Datos se pueden encontrar en: 
#     Link institucional: https://hdr.undp.org/data-center/human-development-index#/indicies/HDI
#     Link directo: https://hdr.undp.org/sites/default/files/2021-22_HDR/HDR21-22_Statistical_Annex_HDI_Table.xlsx
#
# ---------------------------------------------------------------------------- #



# Librerias ---------------------------------------------------------------

library(tidyverse)
library(readxl)

# Funcion para guardar grafico
gg_save_desuc <- function(gg_chart, name,
                          width = 22,
                          height = 13) {
  ggsave(filename = paste0('output/', name),
         plot = gg_chart,
         width = width,
         height = height,
         units = 'cm')
}

# Cargar datos ------------------------------------------------------------

## Base
df <- read_xlsx('./input/HDR21-22_Statistical_Annex_HDI_Table.xlsx')

## Quitar columnas en blanco
df <- df %>% 
  select(!starts_with('...'))


# Procesamiento -----------------------------------------------------------

## Transformar varaibles a numericas
df <- df %>% 
  mutate(across(!starts_with('country'), as.numeric))


## Redondear valores
df <- df %>% 
  mutate(across(!starts_with('country'), ~round(., 2)))


## Seleccionar paises de sudamerica
df <- df %>% 
  filter(Country %in% c('Argentina', 'Bolivia (Plurinational State of)',
                        'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Guyana',
                        'Paraguay', 'Peru', 'Suriname', 'Trinidad and Tobago',
                        'Uruguay', 'Venezuela (Bolivarian Republic of)',
                        'Guyana'))


## Traducir paises a espaniol
df <- df %>% 
  mutate(Country = case_when(str_detect(Country, 'Bolivia') ~ 'Bolivia',
                             str_detect(Country, 'Tri') ~ 'Trinidad y Tobago',
                             str_detect(Country, 'Ven') ~ 'Venezuela',
                             Country == 'Brazil' ~ 'Brasil',
                             Country == 'Suriname' ~ 'Surinam',
                             Country == 'Peru' ~ 'Perú',
                             TRUE ~ Country))


## Labels
df_labels <- df %>% 
  select(Country, `Gross national income (GNI) per capita 2021`, `Human Development Index (HDI) 2021`) %>% 
  mutate(`Human Development Index (HDI) 2021` = 
           case_when(
             Country %in% c('Chile', 'Argentina', 'Uruguay', 'Perú', 'Ecuador',
                            'Surinam', 'Colombia') ~ `Human Development Index (HDI) 2021` + 0.014,
             Country %in% c('Trinidad y Tobago', 'Guyana', 'Brasil', 'Paraguay',
                            'Bolivia') ~ `Human Development Index (HDI) 2021` - 0.015,
             Country == 'Venezuela' ~ `Human Development Index (HDI) 2021` - 0.015,
             TRUE ~ `Human Development Index (HDI) 2021`),
         `Gross national income (GNI) per capita 2021` = 
           case_when(
             Country == 'Venezuela' ~ `Gross national income (GNI) per capita 2021` + 800,
             TRUE ~ `Gross national income (GNI) per capita 2021`))

# Grafico -----------------------------------------------------------------

gg <- df %>% 
  ggplot(aes(x = `Gross national income (GNI) per capita 2021`,
             y = `Human Development Index (HDI) 2021`,
             color = Country)) + 
  geom_point(size = 4,
             show.legend = T) +
  geom_smooth(method = "lm", se = T, color = "#8ecae6", size = 1) +
  scale_y_continuous(limits = c(0.5, 1)) +
  scale_x_continuous(labels = scales::dollar_format(big.mark = ".")) + 
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.14) +
  geom_text(data = df_labels,
            aes(x = `Gross national income (GNI) per capita 2021`,
                y = `Human Development Index (HDI) 2021`,
                label = Country),
            size = 5,
            show.legend = FALSE) +
  labs(title = 'Relación entre Indice de Desarrollo \nHumano y PIB percápita',
       subtitle = 'Países Sudamericanos',
       x = 'PIB percápita',
       y = 'Indice de Desarrollo Humano',
       caption = 'Funte: PNUD, 2021',
       color = NULL) +
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        panel.background = element_rect(fill = "#FFEBD7", color = NA),
        plot.background = element_rect(fill = "#FFEBD7", color = NA),
        panel.grid.major = element_line(colour = '#CCCCCC', linetype = 'dotted'), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "#219ebc", size = rel(0.6)),
        axis.text = element_text(color = "#fb8500", size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.8)),
        axis.text.y = element_text(size = rel(1.8)),
        legend.position = 'none',
        legend.background = element_rect(fill = "#FFEBD7", color = NA),
        title = element_text(color = "#fb8500", face = "bold", size = rel(2.5)), 
        plot.caption = element_text(size = rel(0.7)),
        plot.subtitle = element_text(size = rel(0.8), face = 'italic'))



## Exportar
gg_save_desuc(gg,
              name = "day20_correlation.png",
              width = 25,
              height = 25)
