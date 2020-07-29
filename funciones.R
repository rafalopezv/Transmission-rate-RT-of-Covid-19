# sobre: funciones para cáluclo de Rt

# -------------------------------------------------------------------
# función sobre primedio == 6.48  y desviación estandar de == 3.83
# -------------------------------------------------------------------
epistim_intervalo_1 <- function(.data) {
  .data %>% 
    dplyr::select(base, pais_region, fecha, I = incidencia) -> temp
  
  # calculo rt
  if((temp %>% nrow) > 7) {
    
    RO <- estimate_R(incid = temp, method = "parametric_si",
                     config = make_config(list(mean_si = 6.48, std_si = 3.83)))  
    
    # acondicionar la salida
    .data %<>% 
      slice(-1) %>% 
      mutate(dias_c = lag(semana, 2)) %>% 
      fill(dias_c, .direction = "up") %>% 
      group_split(dias_c) %>% 
      map(., ~pull(., fecha)) -> temp_1
    
    inicio <- map(temp_1, 1) %>% map(., as.character) %>% unlist
    fin <- map(temp_1, last) %>% map(., as.character) %>% unlist
    
    RO <- RO[[1]]
    
    temp_1 <- rep((1:7), 52)
    temp_1 <- temp_1[1:(RO %>% nrow)]
    RO$dia_semana <- temp_1
    
    RO %<>% filter(dia_semana == 1)
    
    inicio <- inicio[1:(nrow(RO))]
    fin <- fin[1:(nrow(RO))]
    
    temp$pais_region  %>% unique -> pais
    temp$base %>% unique -> base
    
    # tabla final
    RO %<>% 
      mutate(
        `Día de inicio` = inicio,
        `Día de inicio` = as.Date(`Día de inicio`),
        `Día de cierre` = fin,
        `Día de cierre` = as.Date(`Día de cierre`),
        `País o Región` = pais,
        Base = base
      ) %>% 
      dplyr::select(Base, `País o Región`, `Día de inicio`, `Día de cierre`,  Promedio = `Mean(R)`, 
                    `Límite inferior` = `Quantile.0.025(R)`, `Límite superior` = `Quantile.0.975(R)`) %>% 
      mutate_if(is.numeric, round, 3)
    
    return(RO)
  }
  
}


# graficos sobre series de tiempo Rt
rt_tiempo <- function(.data) {
  pais <- .data %>% pull(pais_o_region) %>% unique()
  hchart(
    .data,
    "line",
    hcaes(dia_de_cierre, promedio),
    color = "#E76F51", 
    showInLegend = F
  ) %>% 
    hc_plotOptions(
      series = list(
        marker = list(radius = 1.5, enabled = T, symbol = "circle"),
        states = list(hover = list(halo = list(size = 1))),
        lineWidth = 4
      )
    ) %>% 
    hc_chart(style = list(fontFamily = "IBM Plex Mono")) %>% 
    hc_tooltip(enabled = T, valueDecimals = 3, borderWidth = 0.01,
               pointFormat=paste("<b>{point.pais_o_region}</b><br>
                               Rt: <b>{point.promedio}</b><br>
                               Límite superior: <b>{point.limite_superior}</b><br>
                               Límite inferior: <b>{point.limite_inferior}</b><br>
                               Día de inicio de medición: <b>{point.dia_de_inicio}</b><br>
                               Día de cierre de medición: <b>{point.dia_de_cierre}</b><br>"), 
               headerFormat = "") %>% 
    hc_size(height = 170)  %>% 
    hc_yAxis(title = list(text = "")) %>% 
    hc_xAxis(title = list(text = NULL)) %>% 
    hc_title(text = pais,
             align = "center") %>% 
    hc_legend(visible = F)
}
