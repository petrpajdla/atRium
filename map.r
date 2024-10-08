library(tidyverse)
library(sf)

sh <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1CSlE5E7muNGm4hXLQOPSj9G69XuuQeGdfdhf2gPb7_A/edit?gid=0#gid=0")

co <- giscoR::gisco_countries

sh[!sh$`Org Country` %in% co$NAME_ENGL, ]
colnames(sh)

origins <- sh %>% filter(Confirmation == 1) %>% 
  group_by(`Org Country`) %>% 
  count() %>% 
  mutate(n = as.character(n)) %>% 
  left_join(co, by = join_by(`Org Country` == NAME_ENGL)) %>% 
  st_as_sf()

ggplot() +
  geom_sf(data = co) +
  geom_sf(data = origins, aes(fill = n)) +
  coord_sf(xlim = c(-10, 40), ylim = c(35, 65)) +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_minimal() +
  guides(fill = guide_legend(position = "inside", title = "")) +
  theme(legend.position.inside = c(0.9, 0.9), 
        plot.background = element_rect(fill = "white", color = NA))

ggsave(here::here("map.png"), width = 8, height = 7.3)  
