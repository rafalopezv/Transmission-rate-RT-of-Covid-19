# sobre: Covid Bolivia
library(tidyverse)
library(magrittr)
library(highcharter)
library(rvest)
library(EpiEstim)
source("codigo/funciones.R")

#----------------------------------------
# limpieza datos departamentales Bolivia
#----------------------------------------

# descarga datos de poblacion
poblacion_bol <- rio::import("input/proyeccion_poblacion.xlsx")
colnames(poblacion_bol)[1] <- "pais_region"
colnames(poblacion_bol)[2] <- "poblacion"

poblacion_bol %<>% 
  select(pais_region, poblacion) 

# descarga de datos oficiales compilados en este repositorio
deptos2 <- read_csv("https://raw.githubusercontent.com/mauforonda/covid19-bolivia/master/confirmados.csv") %>% 
  mutate(base = "confirmados")
dptos2_m <- read_csv("https://raw.githubusercontent.com/mauforonda/covid19-bolivia/master/decesos.csv") %>% 
  mutate(base = "fallecidos")

deptos2 %<>% 
  bind_rows(., dptos2_m) %>% 
  # remover desde el 23 de noviembre en adelante por tener fechas repetidas en este reposotorio
  filter(Fecha < "2020-11-23")
rm(dptos2_m)

# jalar datos de a OPS desde el 23 de noviembre en adelante
deptos3 <- read_csv("https://raw.githubusercontent.com/mauforonda/covid19-bolivia/opsoms/confirmados.csv") %>% 
  mutate(base = "confirmados") %>% 
  # corrección de dato de Tarija en base a https://www.boliviasegura.gob.bo/datos.php
  mutate(Tarija = case_when(
    Fecha == "2020-12-25" ~ 16891,
    T ~ Tarija
  ))

dptos2_m <- read_csv("https://raw.githubusercontent.com/mauforonda/covid19-bolivia/opsoms/decesos.csv") %>% 
  mutate(base = "fallecidos")

deptos3 %>% 
  bind_rows(., dptos2_m) %>% 
  # remover desde el 23 de noviembre en adelante por tener fechas repetidas en este repositorio
  filter(Fecha > "2020-11-22") %>% 
  bind_rows(., deptos2) -> deptos2

rm(deptos3, dptos2_m)

# importar de bolivia segura, compilarla con su base, exportara y juntarla con otras dos bases
# readr::read_rds("input/base_bolsegura.Rds") %>% 
#   unique() %>% # para eliminar la posibilidad de exportar la misma base varias veces
#   bind_rows(., bol_segura()) %>% 
#   unique() %>% # para eliminar la posibilidad de exportar la misma base varias veces
#   bind_rows(deptos2, .) -> deptos2
# 
# # exportar Bolvia segura
# readr::read_rds("input/base_bolsegura.Rds") %>% 
#   bind_rows(., bol_segura()) %>% 
#   write_rds("input/base_bolsegura.Rds")

# verticalizar y juntar con base de poblacion
deptos2 %>% 
  rename(fecha = Fecha) %>% 
  gather(pais_region, casos_acumulados, -fecha, -base) %>% 
  mutate(pais_region = toupper(pais_region)) %>% 
  left_join(., poblacion_bol, by = "pais_region")  -> df_dptos

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
  merge(., temp, all.x = T) 

rm(temp)

df_dptos %>% 
  group_split(base, pais_region) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., incidencia = lag(casos_acumulados),
                 incidencia = casos_acumulados - incidencia,
                 incidencia = abs(incidencia))) %>%
  bind_rows() %>% 
  mutate(pais_region = str_to_title(pais_region)) %>% 
  mutate(incidencia = replace_na(incidencia, 0)) -> df_dptos

df_dptos %>% 
  write_csv("bases/departamentos_bolivia.csv")

#----------------------------------------
# limpieza datos mundiales 
#----------------------------------------
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_m <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

# descarga y limpieza
df <- read_csv(url) %>% mutate(base = "confirmados")
df_m <- read_csv(url_m) %>% mutate(base = "fallecidos")

df <- bind_rows(df, df_m) 
rm(df_m, url, url_m)

df %<>% 
  rename(pais_region = `Country/Region`) %>% 
  dplyr::select(-matches("Lat|Long")) %>% 
  mutate(pais_region = case_when(
    str_detect(string = `Province/State`, "Hong Kong") ~ "Hong Kong SAR, China",
    str_detect(string = `Province/State`, "Macau") ~ "Macao SAR, China",
    T ~ pais_region
  )) %>% 
  dplyr::select(-`Province/State`) %>% 
  gather(fecha, casos_acumulados, -pais_region, -base) 

df$fecha %<>% as.Date(., format = "%m/%d/%y")

# estandarización desde pacientes 0 e individualización de países en formato lista
df %>% 
  filter(casos_acumulados != 0) %>% 
  group_by(pais_region, base, fecha) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_split(pais_region, base) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., dias = 1:nrow(.),
                 total_semanas = nrow(.)/7)) %>% 
  map(., ~mutate_if(., is.numeric, round, 0)) %>% 
  bind_rows() %>% 
  arrange(base, pais_region, fecha) -> df

# añadir variable de número de semanas: extensión de un año 
tibble(
  semana = rep((1:52), 7)
) %>% 
  arrange(semana) %>% 
  mutate(dias = 1:nrow(.)) -> temp

df %<>% merge(., temp, all.x = T)
rm(temp)

# descarga de población por país: fuente Banco Mundial
poblacion <- read_csv("input/API_SP.POP.TOTL_DS2_en_csv_v2_1120881/API_SP.POP.TOTL_DS2_en_csv_v2_1120881.csv", skip = 3)
poblacion %<>% 
  dplyr::select(pais_region = `Country Name`, poblacion = `2018`) 

# compatibilidad d datos población con base JHU
(df$pais_region %>% unique)[!(df$pais_region %>% unique) %in%  (poblacion$pais_region %>% unique)]

poblacion$pais_region %<>% gsub("Bahamas\\, The", "Bahamas", .)
poblacion$pais_region %<>% gsub("Brunei Darussalam", "Brunei", .)
poblacion$pais_region %<>% gsub("Egypt\\, Arab Rep\\.", "Egypt", .)
poblacion$pais_region %<>% gsub("Gambia\\, The", "Gambia", .)
poblacion$pais_region %<>% gsub("Iran\\, Islamic Rep\\.", "Iran", .)
poblacion$pais_region %<>% gsub("Korea\\, Rep\\.", "Korea, South", .)
poblacion$pais_region %<>% gsub("Kyrgyz Republic", "Kyrgyzstan", .)
poblacion$pais_region %<>% gsub("Russian Federation", "Russia", .)
poblacion$pais_region %<>% gsub("St\\. Lucia", "Saint Lucia", .)
poblacion$pais_region %<>% gsub("St\\. Vincent and the Grenadines", "Saint Vincent and the Grenadines", .)
poblacion$pais_region %<>% gsub("Slovak Republic", "Slovakia", .)
poblacion$pais_region %<>% gsub("United States", "US", .)
poblacion$pais_region %<>% gsub("Venezuela\\, RB", "Venezuela", .)
poblacion$pais_region %<>% gsub("Syrian Arab Republic", "Syria", .)
poblacion$pais_region %<>% gsub("Lao PDR", "Laos", .)
poblacion$pais_region %<>% gsub("St. Kitts and Nevis", "Saint Kitts and Nevis", .)
poblacion$pais_region %<>% gsub("Yemen\\, Rep\\.", "Yemen", .)
poblacion$pais_region %<>% gsub("Myanmar", "Burma", .)
poblacion$pais_region %<>% gsub("Czech Republic", "Czechia", .)
poblacion$pais_region %<>% gsub("Congo\\, Dem\\. Rep\\.", "Congo (Kinshasa)", .)
poblacion$pais_region %<>% gsub("Congo\\, Rep\\.", "Congo (Brazzaville)", .)


# Población de Taiwán no está reportada en China: https://databank.worldbank.org/reports.aspx?source=2&type=metadata&series=SP.POP.TOTL

# filtro la base de poblacion respecto a la data de JHU
poblacion %>%  
  filter(pais_region %in% (df %>% pull(pais_region) %>% unique)) %>% 
  dplyr::select(pais_region, poblacion) -> poblacion

df %<>% 
  merge(., poblacion, all.x = T) %>% 
  arrange(base, pais_region, fecha) 

# completar poblaciones manualmente
df %>% 
  filter(is.na(poblacion)) %>% 
  pull(pais_region) %>% 
  unique


df[df$pais_region == "Eritrea", "poblacion"] <- 6081196 # fuente cia.gov, año 2020
df[df$pais_region == "Taiwan*", "poblacion"] <- 23603049 # fuente cia.gov, año 2020
df[df$pais_region == "Holy See", "poblacion"] <- 1000 # fuente cia.gov, año 2019
df[df$pais_region == "Western Sahara", "poblacion"] <- 652271 # fuente cia.gov, año 2020


(df$pais_region %>% unique)[!(df$pais_region %>% unique) %in%  (poblacion$pais_region %>% unique)]
rm(poblacion)

# quitar a: Diamond Princess y MS Zaandam
df %<>%  
  filter(!is.na(poblacion))

# dividir entre países/semanas
df %>% 
  group_split(base, pais_region) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., incidencia = lag(casos_acumulados),
                 incidencia = casos_acumulados - incidencia,
                 incidencia = abs(incidencia))) %>%
  bind_rows() %>% 
  mutate(incidencia = replace_na(incidencia, 0)) -> df_mundo 

# exportar base mundo
df_mundo %>% 
  write_csv("bases/mundo_bolivia.csv")

rm(df)

# filtar bolivia de la base mundial en confirmados y fallecidos para juntar con los gráficos departamentales
bol_confirmados <- df_mundo %>% 
  filter(
    pais_region == "Bolivia",
    base == "confirmados"
  )

bol_fallecidos <- df_mundo %>% 
  filter(
    pais_region == "Bolivia",
    base == "fallecidos",
  )



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

#--------------------------------
# Visualizaciones
#--------------------------------
# paleta de colores
colores <- c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51", "#e63946", "#d90429", "#50514f", "#293241")

df_dptos %>%
  filter(base == "fallecidos") %>% 
  bind_rows(., bol_fallecidos) %>% 
  group_split(pais_region)  -> temp

map(temp, epistim_intervalo_1) %>%
  bind_rows() -> temp

temp %>% 
  group_split(`País o Región`) %>%
  map(., ~arrange(., `Día de cierre`)) %>%
  map(., ~slice(., nrow(.))) %>%
  bind_rows() %>%
  arrange(Promedio) %>%
  mutate(
    clase = case_when(
      `País o Región` == "Bolivia" ~ "si",
      T ~ "no"
    )
  ) %>% 
  janitor::clean_names() -> temp_1

highchart() %>%
  hc_add_series(data = temp_1, type = "errorbar",
                hcaes(x = pais_o_region, low = limite_inferior, high = limite_superior),
                id = "error", color = "#2F546B",
                stemWidth = 3,  whiskerLength = 0) %>%
  hc_add_series(data = temp_1, "scatter", hcaes(x = pais_o_region, y = promedio),
                name = "Rt", linkedTo = "error", color = "#2F546B") %>%
  hc_chart(style = list(fontFamily = "IBM Plex Mono")) %>%
  hc_plotOptions(
    scatter = list(
      marker = list(radius = 7, enabled = T, symbol = "circle"),
      states = list(hover = list(halo = list(size = 3)))
    )
  ) %>%
  hc_xAxis(title = list(text = ""),
           categories = temp_1$pais_o_region) %>%
  hc_yAxis(title = list(text = "Rt"), gridLineWidth =  0.5, 
           plotLines = list(
             list(label = list(text = "Rt = 1"),
                  color = "#D62828",
                  width = 5,
                  value = 1)))  %>% 
  hc_tooltip(enabled = T, valueDecimals = 3, borderWidth = 0.01, style = list(fontFamily = "IBM Plex Mono"),
             pointFormat=paste("<b>{point.pais_o_region}</b><br>
                               Rt última semana: <b>{point.promedio}</b><br>
                               Límite superior: <b>{point.limite_superior}</b><br>
                               Límite inferior: <b>{point.limite_inferior}</b><br>
                               Día de inicio de medición: <b>{point.dia_de_inicio}</b><br>
                               Día de cierre de medición: <b>{point.dia_de_cierre}</b><br>"),
             headerFormat = "<b>{point.pais_o_region}</b>") -> rt_ult_semana



# serie de tiempo Rt
temp %>% 
  janitor::clean_names() %>%
  group_split(pais_o_region) -> temp_1

#--------------------------------------------------------------
# correción de Rt por 1500 fallecidos no registrados en scz
#------------------------------ -------------------------------

temp_1[[2]] %<>% 
  mutate(
    promedio = case_when(
      dia_de_inicio != "2020-09-01"  ~ promedio
    ),
    limite_inferior = case_when(
      dia_de_inicio != "2020-09-01"  ~ limite_inferior
    ),
    limite_superior = case_when(
      dia_de_inicio != "2020-09-01"  ~ limite_superior
    )
  ) 

temp_1[[9]] %<>% 
  mutate(
    promedio = case_when(
      dia_de_inicio != "2020-09-01"  ~ promedio
    ),
    limite_inferior = case_when(
      dia_de_inicio != "2020-09-01"  ~ limite_inferior
    ),
    limite_superior = case_when(
      dia_de_inicio != "2020-09-01"  ~ limite_superior
    )
  )


# ejecutar función para graficos
map(temp_1, rt_tiempo) -> rt_tiempo_g

# confirmados por millón de habitantes
df_dptos %>%
  filter(base == "confirmados") %>%
  bind_rows(bol_confirmados) %>% 
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
  hc_tooltip(table = T, shared = T, sort = T, outside = T, borderWidth = 0.01, 
             style = list(fontFamily = "IBM Plex Mono")) %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Casos acumulados por millón de habitantes")) %>%
  hc_chart(style = list(fontFamily = "IBM Plex Mono")) %>%
  hc_colors(colors = colores) %>%
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
) -> millon_confirmados 


# fallecidos por millón de habitantes
df_dptos %>%
  filter(base == "fallecidos") %>%
  bind_rows(bol_fallecidos) %>% 
  mutate(
    por_millon = casos_acumulados/poblacion *1000000,
    por_millon = round(por_millon, 0)
  )  -> temp
  
temp_1 <- which(temp$fecha == "2020-09-06" &  temp$pais_region == "Bolivia")
temp[temp_1, "por_millon"] <- NA

temp_1 <- which(temp$fecha == "2020-09-06" &  temp$pais_region == "Santa Cruz")
temp[temp_1, "por_millon"] <- NA


temp %>% 
  hchart(
    "line",
    hcaes(
      x = fecha, y = por_millon, group = pais_region
    )
  ) %>%
  hc_tooltip(table = T, shared = T, sort = T, outside = T, borderWidth = 0.01, 
             style = list(fontFamily = "IBM Plex Mono")) %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Casos acumulados por millón de habitantes")) %>%
  hc_chart(style = list(fontFamily = "IBM Plex Mono")) %>%
  hc_colors(colors = colores) %>%
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
) -> millon_fallecidos 

# evolucion diaria confirmados, fallecidos
df_dptos %>% 
  filter(base == "confirmados") -> temp

colores <- c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51", "#e63946", "#d90429", "#50514f", "#293241")

# prevalencia confirmados
hchart(
  temp, "column", hcaes(fecha, casos_acumulados, group = pais_region),
  stacking = "percent",
  borderWidth = 0,
  groupPadding = 0,
  pointPadding  = 0
) %>% 
  hc_tooltip(shared = T, table = T, sort = T,  borderWidth = 0.01, outside = T, 
             style = list(fontFamily = "IBM Plex Mono")) %>% 
  hc_yAxis(visible = F) %>% 
  hc_xAxis(title = list(text = NULL)) %>% 
  hc_colors(colors = colores) %>% 
  hc_chart(style = list(fontFamily = "IBM Plex Mono")) %>% 
  hc_legend(reversed = T) -> evolucion_confirmados

df_dptos %>% 
  filter(base == "fallecidos") -> temp


temp_1 <- which(temp$fecha == "2020-09-06" &  temp$pais_region == "Santa Cruz")
temp[temp_1, "casos_acumulados"] <- NA

hchart(
  temp, "column", hcaes(fecha, casos_acumulados, group = pais_region),
  stacking = "percent",
  borderWidth = 0,
  groupPadding = 0,
  pointPadding  = 0
) %>% 
  hc_tooltip(shared = T, table = T, sort = T,  borderWidth = 0.01, outside = T, 
             style = list(fontFamily = "IBM Plex Mono")) %>% 
  hc_yAxis(visible = F) %>% 
  hc_xAxis(title = list(text = NULL)) %>% 
  hc_colors(colors = colores) %>% 
  hc_chart(style = list(fontFamily = "IBM Plex Mono")) %>% 
  hc_legend(reversed = T) -> evolucion_fallecidos

# aplanamiento de curvas confirmados
df_dptos %>% 
  filter(base == "confirmados") %>% 
  bind_rows(., bol_confirmados) %>% 
  group_by(pais_region, semana) %>% 
  mutate(n = n()) %>% 
  filter(n >= 6) %>% 
  summarise(
    incidencia = sum(incidencia)
  ) %>% 
  group_by(pais_region) %>% 
  mutate(total = sum(incidencia)) %>% 
  mutate(n = n()) %>% 
  filter(n >= 6) %>% 
  group_split(pais_region)  %>% 
  map(., ~(mutate(., ult_semana = pull(., incidencia) %>% last))) %>% 
  bind_rows() %>% 
  ungroup() %>% 
  mutate(etiqueta = paste0(pais_region, "\n", total, " casos acumulados", "\n", ult_semana, " casos ", "última semana")) %>%
  arrange(total) %>% 
  mutate(
    num = 1:nrow(.),
    clase = case_when(
      pais_region == "Bolivia" ~ "si",
      T ~ "no"
    )
  ) -> temp

temp %>% 
  arrange(total) %>% 
  mutate(num = 1:nrow(.)) %>% 
  ggplot(aes(semana, incidencia, fill = clase)) + 
  geom_col(color = NA, alpha = 0.9) + 
  facet_wrap(vars(fct_reorder(etiqueta, num, .desc = T)), scales = "free", ncol = 4) +
  hrbrthemes::theme_ipsum_rc(grid = F, base_family = "Source Code Pro Medium", strip_text_size = 21) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = "white", colour = NA),
    legend.position = "none"
  ) + 
  scale_fill_manual(values = c("#264653", "#E76F51")) -> curva_confirmados

# aplanamiento de curvas fallecidos
df_dptos %>%
  filter(base == "fallecidos") %>% 
  bind_rows(., bol_fallecidos) %>% 
  group_by(pais_region, semana) %>% 
  mutate(n = n()) %>% 
  filter(n >= 6) %>% 
  summarise(
    incidencia = sum(incidencia)
  ) %>% 
  group_by(pais_region) %>% 
  mutate(total = sum(incidencia)) %>% 
  group_split(pais_region)  %>% 
  map(., ~(mutate(., ult_semana = pull(., incidencia) %>% last))) %>% 
  bind_rows() %>% 
  ungroup() %>% 
  mutate(etiqueta = paste0(pais_region, "\n", total, " casos acumulados", "\n", ult_semana, " casos ", "última semana")) %>%
  arrange(total) %>% 
  mutate(
    num = 1:nrow(.),
    clase = case_when(
      pais_region == "Bolivia" ~ "si",
      T ~ "no"
    )
  ) -> temp

temp_1 <- temp$pais_region == "Santa Cruz" & temp$semana == 24
temp[temp_1, "incidencia"] <- NA

temp_1 <- temp$pais_region == "Bolivia" & temp$semana == 24
temp[temp_1, "incidencia"] <- NA

temp %>% 
  arrange(total) %>% 
  mutate(num = 1:nrow(.)) %>% 
  ggplot(aes(semana, incidencia, fill = clase)) + 
  geom_col(color = NA, alpha = 0.9) + 
  facet_wrap(vars(fct_reorder(etiqueta, num, .desc = T)), scales = "free", ncol = 4) +
  hrbrthemes::theme_ipsum_rc(grid = F, base_family = "Source Code Pro Medium", strip_text_size = 21) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = "white", colour = NA),
    legend.position = "none"
  ) + 
  scale_fill_manual(values = c("#264653", "#E76F51")) -> curva_fallecidos




