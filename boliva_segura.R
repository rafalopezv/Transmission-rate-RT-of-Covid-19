# sobre: descargar información desde sitio oficial por primera vez
# fecha de realización: 4 de enero, fecha de la información base: 3 de enero 

library(rvest)
library(tidyverse)


url <- "https://www.boliviasegura.gob.bo/datos.php"

url %>%
  read_html() %>%
  html_table(fill = TRUE) -> temp1

temp1[[2]] %>% 
  select(Departamento, confirmados = Acumulado, fallecidos = Decesos) %>% 
  mutate(
    confirmados = str_replace(confirmados, ",", ""),
    confirmados = as.numeric(confirmados),
    fallecidos = str_replace(fallecidos, ",", ""),
    fallecidos = as.numeric(fallecidos)
  ) %>% 
  gather(base, n, -Departamento) %>% 
  spread(Departamento, n) %>% 
  mutate(Fecha = as.Date("2021-01-03")) %>% 
  readr::write_rds("input/base_bolsegura.Rds")


  




