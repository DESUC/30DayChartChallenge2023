library(rvest) #For Scraping Web pages
library(magrittr) #Pipe operators
library(scales) # Scale function for visualization
library(lubridate) #Date and time made easy
library(dplyr) #Data manipulation package
library(ggplot2) #Grammar of Graphics for visualization
library(tidyverse)
library(knitr) # for beautiful table display
library(xml2) #par
library(rvest)

# Target Web site URL
hot100page <- "https://www.billboard.com/artist/taylor-swift/chart-history/hsi/"

taylor <- read_html(hot100page)

taylor
str(taylor)

body_nodes <- taylor |>
  html_node("body") |> 
  html_children()
#

title <- taylor |> 
  rvest::html_nodes('h3.c-title.a-no-trucate.a-font-primary-bold-s.u-letter-spacing-0021') |> 
  rvest::html_text() |> 
  trimws()
length(title)

weeks_on_chart <- taylor |> 
  rvest::html_nodes('span.c-label.a-font-primary-m.artist-chart-row-week-on-chart') |> 
  rvest::html_text() |> 
  trimws()

debut_date <- taylor %>% 
  html_nodes('span.c-label.a-font-primary-m.lrv-u-border-b-1.lrv-u-border-color-brand-primary.artist-chart-row-debut-date') %>% 
  html_text() %>% 
  trimws()

peak_date <- taylor %>% 
  html_nodes('span.c-label.a-font-primary-m.lrv-u-border-b-1.lrv-u-border-color-brand-primary.artist-chart-row-peak-date') %>% 
  html_text() %>% 
  trimws()
  
peak_position <- taylor %>% 
  html_nodes('span.c-label.a-font-primary-bold-m') %>% 
  html_text() %>% 
  trimws()

taylor_swift <- data.frame(title_song = title, 
                           debut_date = debut_date, 
                           peak_position = peak_position, 
                           peak_date = peak_date, 
                           weeks_on_chart = weeks_on_chart)

#saveRDS(taylor_swift, "taylor_billboard.rds")

taylor_swift <- taylor_swift %>% 
  mutate_at(vars(peak_date, debut_date), ~lubridate::mdy(.)) %>% 
  mutate_at(vars(peak_position, weeks_on_chart), ~as.numeric(.))

taylor_swift <- taylor_swift %>% 
  mutate(date_out = debut_date %m+% weeks(weeks_on_chart))

taylor_swift <- taylor_swift %>% 
  select(title_song, peak_position, weeks_on_chart, debut_date, date_out, peak_date)

taylor_swift <- taylor_swift %>% 
  mutate(run_interval = interval(ymd(debut_date), ymd(date_out)))

library(purrr)

taylor_swift_1 <- taylor_swift %>% 
  mutate(numb_times_with_overlap = imap_int(
    run_interval, 
    ~sum(
      int_overlaps(.x, run_interval)
    ) - 1L
  ))

taylor_swift_2 <- taylor_swift_1 %>% 
  mutate(año = year(ymd(peak_date)),
         mes = month(ymd(peak_date)))

calendario <- taylor_swift_2 %>% 
  select(año, mes, peak_date, numb_times_with_overlap, peak_position) %>% 
  arrange(año, mes)

calendario_ts <- calendario %>% 
  ggplot(aes(x = año, y = mes, fill = numb_times_with_overlap)) +
  geom_tile() +
  scale_y_continuous(breaks = seq(1, 12, 1),
                     labels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2007, 2023, 1), expand = c(0,0)) +
  scale_fill_gradient(low = "#ff99b6", high = "#757bc8", 
                      name = "Número de canciones dentro de los 100 de Billboard:",
                      guide = guide_legend(title.position = "top", nrow = 1, override.aes = list(shape = 1, size = 3)),
                      labels = c("1","5","10","15","20","25+")) +
  labs(title = "Billboard Hot 100: We never go out of style,\nwe never go out of style!",
       subtitle = "¿Cuantas canciones de Taylor Swift estuvieron dentro del Hot 100 de Billboard?\nDesde el 2007 a abril del 2023.",
       caption = "Fuente: Billboard, archivo de Taylor Swift.\nSe elige la fecha peak de la canción en la lista de Billboard.", y = "", x = "") +
  coord_equal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = c(0,0),
        legend.background = element_rect(fill = "#4f518c"),
        legend.key = element_rect(fill = "#4f518c"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        plot.title = element_text(family = "Pragmatica Medium", size = 22, color = "#ff99b6"),
        plot.title.position = "panel",
        plot.caption = element_text(size = 12),
        text = element_text(family = "Pragmatica Medium", color = "#e0c3fc"),
        axis.text.y = element_text(size = 12, color = "#e0c3fc"),
        axis.text.x = element_text(size = 10, color = "#e0c3fc"),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#f2ebfb"),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "#4f518c", color = "#4f518c"))

ggsave(calendario_ts, width = 25, height = 25, units = "cm",filename = "output/day23_tiles.png")  
  
# taylor_swift_1 %>% 
#   filter(weeks_on_chart > 9) %>% 
#   ggplot() + 
#   geom_segment(aes(y = fct_reorder(title_song, weeks_on_chart), yend = title_song, x = debut_date, xend = date_out), size = 5)
