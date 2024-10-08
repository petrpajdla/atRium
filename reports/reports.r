library(tidyverse)

sh <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1CSlE5E7muNGm4hXLQOPSj9G69XuuQeGdfdhf2gPb7_A/edit?usp=sharing") %>% 
  filter(Confirmation) %>% 
  mutate(p = str_extract(Name, ".+(?=,)"),
         p = str_to_lower(p)) %>% 
  select(p, Name, Report, Reference, Organisation)

fl <- list.files(here::here("reports/"), pattern = "\\.pdf") %>% 
  as_tibble() %>% 
  mutate(p = str_extract(value, "(?<=report_).+(?=\\.pdf$)"),
         p = str_to_lower(p))

full_join(sh, fl, by = join_by("p")) %>% 
  mutate(path = if_else(Report, paste0("reports/", value), NA),
         Report = if_else(Report, paste0("[Report (PDF)](", path, ")"), "*To be submitted.*")) %>% 
  select(Name, Report) %>% 
  write_csv(here::here("reports/list.csv"))
  
