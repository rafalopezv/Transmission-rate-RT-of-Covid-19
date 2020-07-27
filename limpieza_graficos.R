# sobre: Covid Bolivia

library(tidyverse)
library(magrittr)
library(highcharter)
library(rvest)
library(EpiEstim)
source("sa.R")

#----------------------------------------
# limpieza datos departamentales Bolivia
#----------------------------------------

# descarga datos de poblacion
url <- "https://www.ine.gob.bo/subtemas_cuadros/demografia_html/PC20106.htm"

url %>%
  read_html() %>%
  html_table(fill = TRUE) -> poblacion_bol

poblacion_bol <- poblacion_bol[[1]] 
poblacion_bol %<>% 
  slice(-(1:2)) 
colnames(poblacion_bol) <- poblacion_bol %>% slice(1)
poblacion_bol %<>% slice(-1)
poblacion_bol %<>% 
  filter(AÑO == "2020") %>% 
  dplyr::select(-BOLIVIA, -AÑO) %>% 
  gather(pais_region, poblacion) %>% 
  mutate(
    poblacion = str_replace(poblacion, "\\.", ""),
    poblacion = str_replace(poblacion, "\\.", ""),
    poblacion = as.numeric(poblacion)
  )
rm(url)

deptos2 <- read_csv("https://raw.githubusercontent.com/mauforonda/covid19-bolivia/master/confirmados.csv") %>% 
  mutate(base = "confirmados")
dptos2_m <- read_csv("https://raw.githubusercontent.com/mauforonda/covid19-bolivia/master/decesos.csv") %>% 
  mutate(base = "fallecidos")

deptos2 %<>% bind_rows(., dptos2_m)
rm(dptos2_m)

# verticalizar y juntar con base de poblacion
deptos2 %>% 
  rename(fecha = Fecha) %>% 
  gather(pais_region, casos_acumulados, -fecha, -base) %>% 
  mutate(pais_region = toupper(pais_region)) %>% 
  left_join(., poblacion_bol, by = "pais_region") -> df_dptos

rm(deptos2, poblacion_bol)

df_dptos %>% 
  filter(casos_acumulados != 0) %>% 
  group_by(base, pais_region, fecha) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_split(base, pais_region) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., dias = 1:nrow(.),
                 total_semanas = nrow(.)/7)) %>% 
  map(., ~mutate_if(., is.numeric, round, 0)) %>% 
  bind_rows() %>% 
  arrange(pais_region, fecha) -> df_dptos

tibble(
  semana = rep((1:52), 7)
) %>% 
  arrange(semana) %>% 
  mutate(dias = 1:nrow(.)) -> temp

df_dptos %<>%
  merge(., temp, all.x = T) %>% 
  filter(fecha != "2020-07-27")

rm(temp)

df_dptos %>% 
  group_split(base, pais_region) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., incidencia = lag(casos_acumulados),
                 incidencia = casos_acumulados - incidencia,
                 incidencia = abs(incidencia))) %>%
  bind_rows() %>% 
  filter(!is.na(incidencia)) -> df_dptos

#----------------------
# cambio de lenguage en fechas
lang <- getOption("highcharter.lang")
lang$months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

lang$shortMonths <- c("En", "Feb", "Mar", "Abr", "Mayo", "Jun", 
                      "Jul", "Ag", "Sep", "Oct", "Nov", "Dic")

lang$weekdays <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", 
                   "Sábado", "Domingo")

options(highcharter.lang = lang)


# incidencia confirmados
hchart(
  df_dptos %>% filter(base == "confirmados"),
  "line",
  hcaes(
    x = fecha, y = incidencia, group = pais_region
  )
) %>%
  hc_tooltip(table = T, shared = T, sort = T) %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Nuevos casos por día")) %>%
  hc_chart(style = list(fontFamily = "Source Code Pro")) %>%
  hc_plotOptions(
    series = list(
      marker = list(radius = 3, enabled = FALSE, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>%
  hc_colors(colors = ggthemes::tableau_color_pal()(9)) %>%
  hc_title(text = "Nuevos casos confirmados por día") %>%
  hc_subtitle(text = "Desde paciente '0' en cada departamento") -> hc
 
# incidencia fallecidos
hchart(
  df_dptos %>% filter(base == "fallecidos"),
  "line",
  hcaes(
    x = fecha, y = incidencia, group = pais_region
  )
) %>%
  hc_tooltip(table = T, shared = T, sort = T) %>%
  #hc_legend(layout = "proximate", align = "right") %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Nuevos casos por día")) %>%
  hc_chart(style = list(fontFamily = "Source Code Pro")) %>%
  hc_plotOptions(
    series = list(
      marker = list(radius = 3, enabled = FALSE, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>%
  hc_colors(colors = ggthemes::tableau_color_pal()(9)) %>%
  hc_title(text = "Nuevos fallecidos por día") %>%
  hc_subtitle(text = "Desde fallecido '0' en cada departamento") -> hc1

# prevalencia confirmados
hchart(
  df_dptos %>% filter(base == "confirmados"),
  "line",
  hcaes(
    x = fecha, y = casos_acumulados, group = pais_region
  )
) %>%
  hc_tooltip(table = T, shared = T, sort = T) %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Casos acumulados")) %>%
  hc_chart(style = list(fontFamily = "Source Code Pro")) %>%
  hc_title(text = "Casos confirmados acumulados por día") %>%
  hc_subtitle(text = "Desde confirmado '0' en cada departamento") %>%
  hc_colors(colors = ggthemes::tableau_color_pal()(9)) %>%
  hc_legend(layout = "proximate", align = "right") %>%
  hc_plotOptions(line = list(
    lineWidth = 3,
    connectNulls = F,
    animation = list(
      duration = 3000
    ),
    marker = list(
      enabled = F,
      symbol = "circle",
      radius = 2
    )
  )
) -> hc2

# prevalencia fallecidos
hchart(
  df_dptos %>% filter(base == "fallecidos"),
  "line",
  hcaes(
    x = fecha, y = casos_acumulados, group = pais_region
  )
) %>%
  hc_tooltip(table = T, shared = T) %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Nuevos casos por día")) %>%
  hc_chart(style = list(fontFamily = "Source Code Pro")) %>%
  hc_title(text = "Fallecidos acumulados por día") %>%
  hc_subtitle(text = "Desde fallecido '0' en cada departamento") %>%
  hc_colors(colors = ggthemes::tableau_color_pal()(9)) %>%
  hc_legend(layout = "proximate", align = "right") %>%
  hc_plotOptions(line = list(
    lineWidth = 3,
    connectNulls = F,
    animation = list(
      duration = 3000
    ),
    marker = list(
      enabled = F,
      symbol = "circle",
      radius = 2
    )
  )
) -> hc3

# porcentaje confirmados
df_dptos %>%
  group_by(base, pais_region) %>%
  summarise(
    incidencia = sum(incidencia)
  ) %>%
  filter(base == "confirmados") %>%
  mutate(
    prop = prop.table(incidencia)*1000,
    prop = round(prop, 0),
    prop_1 = prop/10
  ) %>%
  hchart(
    "item",
    hcaes(name = pais_region, y = prop),
    marker = list(symbol = 'square'),
    showInLegend = TRUE,
    style = list(fontFamily = "Source Code Pro",
                 color = "black")
  ) %>%
  hc_tooltip(enabled = T, borderWidth = 0.01,
             pointFormat=paste("
                               Porcentaje de casos confirmados acumulados: <b>{point.prop_1} %</b><br>
                               Casos confirmados acumulados: <b>{point.incidencia}</b><br>"),
             style = list(fontFamily = "Source Code Pro",
                          color = "black")) %>%
  hc_colors(colors = ggthemes::tableau_color_pal()(9)) %>%
  hc_title(text = "Porcentaje de confirmados acumulados por departamento") -> hc4


# porcentaje fallecidos
df_dptos %>%
  group_by(base, pais_region) %>%
  summarise(
    incidencia = sum(incidencia)
  ) %>%
  filter(base == "fallecidos") %>%
  mutate(
    prop = prop.table(incidencia)*1000,
    prop = round(prop, 0),
    prop_1 = prop/10
  ) %>%
  hchart(
    "item",
    hcaes(name = pais_region, y = prop),
    marker = list(symbol = 'square'),
    showInLegend = TRUE,
    style = list(fontFamily = "Source Code Pro",
                 color = "black")
  ) %>%
  hc_tooltip(enabled = T, borderWidth = 0.01,
             pointFormat=paste("
                               Porcentaje de casos fallecidos acumulados: <b>{point.prop_1} %</b><br>
                               Casos fallecidos acumulados: <b>{point.incidencia}</b><br>"),
             style = list(fontFamily = "Source Code Pro",
                          color = "black")) %>%
  hc_colors(colors = ggthemes::tableau_color_pal()(9)) %>%
  hc_title(text = "Porcentaje de casos confirmados acumulados por departamento") -> hc5

# por millon confirmados
df_dptos %>%
  filter(base == "confirmados") %>%
  mutate(
    por_millon = casos_acumulados/poblacion *1000000,
    por_millon = round(por_millon, 0)
  ) %>%
  hchart(
    "line",
    hcaes(
      x = fecha, y = por_millon, group = pais_region
    )
  ) %>%
  hc_tooltip(table = T, shared = T, sort = T) %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Casos acumulados por millón de habitantes")) %>%
  hc_chart(style = list(fontFamily = "Source Code Pro")) %>%
  hc_title(text = "Casos confirmados por cada millón de habitantes") %>%
  hc_subtitle(text = "Desde confirmado '0' en cada departamento") %>%
  hc_colors(colors = ggthemes::tableau_color_pal()(9)) %>%
  hc_legend(layout = "proximate", align = "right") %>%
  hc_plotOptions(line = list(
    lineWidth = 3,
    connectNulls = F,
    animation = list(
      duration = 3000
    ),
    marker = list(
      enabled = F,
      symbol = "circle",
      radius = 2
    )
  )
) -> hc6


# por millon fallecidos
df_dptos %>%
  filter(base == "fallecidos") %>%
  mutate(
    por_millon = casos_acumulados/poblacion *1000000,
    por_millon = round(por_millon, 0)
  ) %>%
  hchart(
    "line",
    hcaes(
      x = fecha, y = por_millon, group = pais_region
    )
  ) %>%
  hc_tooltip(table = T, shared = T, sort = T) %>%
  hc_legend(layout = "proximate", align = "right") %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Casos acumulados por millón de habitantes")) %>%
  hc_chart(style = list(fontFamily = "Source Code Pro")) %>%
  hc_title(text = "Casos fallecidos por cada millón de habitantes") %>%
  hc_subtitle(text = "Desde confirmado '0' en cada departamento") %>%
  hc_colors(colors = ggthemes::tableau_color_pal()(9)) %>%
  hc_legend(layout = "proximate", align = "right") %>%
  hc_colors(colors = ggthemes::tableau_color_pal()(9)) %>%
  hc_plotOptions(line = list(
    lineWidth = 3,
    connectNulls = F,
    animation = list(
      duration = 3000
    ),
    marker = list(
      enabled = F,
      symbol = "circle",
      radius = 2
    )
  )
) -> hc7

# rt

df_dptos %>%
  group_split(base, pais_region)  -> temp

map(temp, epistim_intervalo_1) %>%
  bind_rows() -> temp

temp %>%
  filter(Base == "confirmados") %>%
  group_split(`País o Región`) %>%
  map(., ~arrange(., `Día de cierre`)) %>%
  map(., ~slice(., nrow(.))) %>%
  bind_rows() %>%
  arrange(Promedio) %>%
  janitor::clean_names() -> temp_1

highchart() %>%
  hc_add_series(data = temp_1, type = "errorbar",
                hcaes(x = pais_o_region, low = limite_inferior, high = limite_superior),
                color = "#CB1724",  id = "error",
                stemWidth = 3,  whiskerLength = 0) %>%
  hc_add_series(data = temp_1, "scatter", hcaes(x = pais_o_region, y = promedio),
                color = "#CB1724", name = "Rt", linkedTo = "error") %>%
  hc_chart(style = list(fontFamily = "Source Code Pro")) %>%
  hc_plotOptions(
    scatter = list(
      marker = list(radius = 5, enabled = T, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>%
  hc_xAxis(title = list(text = ""),
           categories = temp_1$pais_o_region) %>%
  hc_yAxis(title = list(text = "Rt"), min = 0,
           plotLines = list(
             list(label = list(text = "Objetivo"),
                  color = "#09283C",
                  width = 2,
                  value = 1))) %>%
  hc_tooltip(enabled = T, valueDecimals = 3, borderWidth = 0.01,
             pointFormat=paste("<b>{point.pais_nombre_corto}</b><br>
                               Rt última semana: <b>{point.promedio}</b><br>
                               Límite superior: <b>{point.limite_superior}</b><br>
                               Límite inferior: <b>{point.limite_inferior}</b><br>
                               Día de inicio de medición: <b>{point.dia_de_inicio}</b><br>
                               Día de cierre de medición: <b>{point.dia_de_cierre}</b><br>"),
             headerFormat = "<b>{point.pais_o_region}</b>") -> hc8


temp %>%
  filter(Base == "fallecidos") %>%
  group_split(`País o Región`) %>%
  map(., ~arrange(., `Día de cierre`)) %>%
  map(., ~slice(., nrow(.))) %>%
  bind_rows() %>%
  arrange(Promedio) %>%
  janitor::clean_names() -> temp_1

highchart() %>%
  hc_add_series(data = temp_1, type = "errorbar",
                hcaes(x = pais_o_region, low = limite_inferior, high = limite_superior),
                color = "#09283C",  id = "error",
                stemWidth = 3,  whiskerLength = 0) %>%
  hc_add_series(data = temp_1, "scatter", hcaes(x = pais_o_region, y = promedio),
                color = "#09283C", name = "Rt", linkedTo = "error") %>%
  hc_chart(style = list(fontFamily = "Source Code Pro")) %>%
  hc_plotOptions(
    scatter = list(
      marker = list(radius = 5, enabled = T, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>%
  hc_xAxis(title = list(text = ""),
           categories = temp_1$pais_o_region) %>%
  hc_yAxis(title = list(text = "Rt"), min = 0,
           plotLines = list(
             list(label = list(text = "Objetivo"),
                  color = "#09283C",
                  width = 2,
                  value = 1))) %>%
  hc_tooltip(enabled = T, valueDecimals = 3, borderWidth = 0.01,
             pointFormat=paste("<b>{point.pais_nombre_corto}</b><br>
                               Rt última semana: <b>{point.promedio}</b><br>
                               Límite superior: <b>{point.limite_superior}</b><br>
                               Límite inferior: <b>{point.limite_inferior}</b><br>
                               Día de inicio de medición: <b>{point.dia_de_inicio}</b><br>
                               Día de cierre de medición: <b>{point.dia_de_cierre}</b><br>"),
             headerFormat = "<b>{point.pais_o_region}</b>") %>%
  hc_title(text = "Rt de la última semana por departamento en base a casos confirmados") -> hc9

# serie de tiempo Rt
df_dptos %>%
  group_split(base, pais_region) -> temp

# aplicar funciones: epistim_intervalo_1 == ICL
furrr::future_map(temp, epistim_intervalo_1, .progress = T) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  group_split(pais_o_region) -> temp_1


# ejecutar función para graficos
map(temp_1, rt_tiempo) -> graficos















