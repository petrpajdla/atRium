library(tidyverse)

sh <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1CSlE5E7muNGm4hXLQOPSj9G69XuuQeGdfdhf2gPb7_A/edit?gid=0#gid=0")

part <- sh %>% filter(Confirmation) %>% 
  select(Name, Email, Reference) %>% 
  mutate(text = paste(
    "We hereby confirm that", 
    Name, 
    "has participated on the computational archaeology training school 'atRium', taking place at the Institute of Archaeology of the Czech Academy of Sciences, Brno, 16. -- 20. September 2024." 
  ))

part %>% pull(text)
