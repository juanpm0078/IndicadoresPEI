# install.packages("readxl")
# install.packages("openxlsx")
# install.packages("zoo")
# install.packages("lubridate")
# install.packages("stringr")

library(readxl)
library(openxlsx)
library(zoo)
library(lubridate)
library(stringr)

source("RatoRE_seguimiento.R")

# En caso de que "mes_reporte" no se haya definido en otro lugar (el reporte auto-
# matizado, por ejemplo), se utiliza el mes actual

# mes_reporte = 5

if(!exists("mes_reporte")) {
  
  mes_reporte <-  month(Sys.Date()) - 1
  
}

# Se calcula el PCM de la siguiente manera:
# (1) Si el indicador es ascendente, se divide la meta alcanzada sobre la planificada y se multiplica por 100, 
# (2) Si el indicador es descendente, lo anterior se resta de 200
# (3) Si el indicador es ascedente y la meta alcanzada es mayor a la meta planificada, se asigna el valor de 100
# (4) Si el indicador es descendente y la meta alcanzadas es menor a la planificada, se asigna el valor de 100
# Se transforma los NAs en 0s y se redondea el resultado a 4 decimales.  

puntaje <- datos_ini3 %>% 
  mutate(direccion_de_meta = if_else(is.na(direccion_de_meta), "Ascendente", direccion_de_meta),  
         pcm = case_when(
           direccion_de_meta == "Ascendente" & metaalcanzada <= metaplanificada ~ metaalcanzada/metaplanificada*100, 
           direccion_de_meta == "Descendente" & metaalcanzada >= metaplanificada ~ 200 - (metaalcanzada/metaplanificada*100), 
           direccion_de_meta == "Ascendente" & metaalcanzada > metaplanificada ~ 100,
           direccion_de_meta == "Descendente" & metaalcanzada < metaplanificada ~ 100 
         )
  ) %>% 
  mutate(pcm = round(pcm, 4))

# Se crea una variable con el mes de inicio del reporte

puntaje_2 <- puntaje %>% 
  group_by(id) %>% 
  mutate(count_sin_na = if_else(!is.na(pcm), cumsum(!is.na(pcm)), NA)) %>%
  mutate(mes_inicio = which(count_sin_na == 1)[1])

# Cuando no existe información de un mes debido a su periodicidad, se arrastra el valor
# del C:ltimo mes reportado. 

puntaje_3 <- puntaje_2 %>%
  mutate(pcm = if_else(!between(mes, mes_inicio, mes_reporte), NA_real_, pcm)) %>%
  group_by(id) %>% 
  mutate(
    mes_2 = mes, 
    pcm_2 = 
      case_when(comportamiento %in% c("Acumulado", "Continuo") ~ 
                  case_when(
                    periodicidad == "Mensual" ~ pcm,
                    
                    periodicidad == "Bimestral" & between(mes, mes_inicio, mes_inicio + 1) ~ 
                      ifelse((mes_inicio) <= 12, pcm[mes == mes_inicio], NA), 
                    
                    periodicidad == "Bimestral" & between(mes, mes_inicio + 2, mes_inicio + 3) ~ 
                      ifelse((mes_inicio + 2) <= 12, pcm[mes == mes_inicio + 2], NA),
                    
                    periodicidad == "Bimestral" & between(mes, mes_inicio + 4, mes_inicio + 5) ~ 
                      ifelse((mes_inicio + 4) <= 12, pcm[mes == mes_inicio + 4], NA),
                    
                    periodicidad == "Bimestral" & between(mes, mes_inicio + 6, mes_inicio + 7) ~ 
                      ifelse((mes_inicio + 6) <= 12, pcm[mes == mes_inicio + 6], NA),
                    
                    periodicidad == "Bimestral" & between(mes, mes_inicio + 8, mes_inicio + 9) ~ 
                      ifelse((mes_inicio + 8) <= 12, pcm[mes == mes_inicio + 8], NA),
                    
                    periodicidad == "Bimestral" & between(mes, mes_inicio + 10, mes_inicio + 11) ~ 
                      ifelse((mes_inicio + 10) <= 12, pcm[mes == mes_inicio + 10], NA),
                    
                    periodicidad == "Bimestral" &  mes == mes_inicio + 11 ~ 
                      ifelse((mes_inicio + 11) <= 12, pcm, NA), 
                    
                    periodicidad == "Trimestral" & between(mes, mes_inicio, mes_inicio + 2) ~ 
                      ifelse((mes_inicio) <= 12, pcm[mes == mes_inicio], NA),
                    
                    periodicidad == "Trimestral" & between(mes, mes_inicio + 3, mes_inicio + 5) ~ 
                      ifelse((mes_inicio + 3) <= 12, pcm[mes == mes_inicio + 3], NA),
                    
                    periodicidad == "Trimestral" & between(mes, mes_inicio + 6, mes_inicio + 8) ~ 
                      ifelse((mes_inicio + 6) <= 12, pcm[mes == mes_inicio + 6], NA),
                    
                    periodicidad == "Trimestral" & between(mes, mes_inicio + 9, mes_inicio + 10) ~ 
                      ifelse((mes_inicio + 9) <= 12, pcm[mes == mes_inicio + 9], NA),
                    
                    periodicidad == "Trimestral" & mes == mes_inicio + 11 ~ 
                      ifelse((mes_inicio + 11) <= 12, pcm[mes == mes_inicio + 11], NA), 
                    
                    periodicidad == "Cuatrimestral" & between(mes, mes_inicio, mes_inicio + 3) ~ 
                      ifelse((mes_inicio) <= 12, pcm[mes == mes_inicio], NA),
                    
                    periodicidad == "Cuatrimestral" & between(mes, mes_inicio + 4, mes_inicio + 7) ~ 
                      ifelse((mes_inicio + 4) <= 12, pcm[mes == mes_inicio + 4], NA),
                    
                    periodicidad == "Cuatrimestral" & between(mes, mes_inicio + 8, mes_inicio + 10) ~ 
                      ifelse((mes_inicio + 8) <= 12, pcm[mes == mes_inicio + 8], NA),
                    
                    periodicidad == "Cuatrimestral" &  mes == mes_inicio + 11 ~ 
                      ifelse((mes_inicio + 11) <= 12, pcm, NA), 
                    
                    periodicidad == "Quimestral" & between(mes, mes_inicio, mes_inicio + 4) ~ 
                      ifelse((mes_inicio) <= 12, pcm[mes == mes_inicio], NA),
                    
                    periodicidad == "Quimestral" & between(mes, mes_inicio + 5, mes_inicio + 9) ~ 
                      ifelse((mes_inicio + 5) <= 12, pcm[mes == mes_inicio + 5], NA),
                    
                    periodicidad == "Quimestral" & mes == mes_inicio + 10 ~ 
                      ifelse((mes_inicio + 10) <= 12, pcm, NA),
                    
                    periodicidad == "Quimestral" &  mes == mes_inicio + 11 ~ 
                      ifelse((mes_inicio + 11) <= 12, pcm, NA), 
                    
                    periodicidad == "Semestral" & between(mes, mes_inicio, mes_inicio + 5) ~ 
                      ifelse((mes_inicio) <= 12, pcm[mes == mes_inicio], NA),
                    
                    periodicidad == "Semestral" &  mes == 12 ~ 
                      ifelse((mes_inicio + 11) <= 12, pcm, NA),
                    
                    periodicidad == "Anual"  ~ pcm
                  ), 
                comportamiento %in% c("Por periodo", "Periodico") & mes >= mes_inicio ~ 
                  mean(pcm, na.rm = T)
                # (cumsum(if_else(is.na(pcm), 0, pcm)))
      )
  ) %>% 
  mutate(pcm_2 = if_else(!between(mes, mes_inicio, mes_reporte), NA_real_, pcm_2)) %>%
  # mutate(a = if_else(!is.na(pcm), count_sin_na, 0)) %>%
  # mutate(a1 = case_when(
  #                       !is.na(pcm) & !is.na(pcm_2) ~ count_sin_na, 
  #                       is.na(pcm) ~ lag(a))) %>% 
  ungroup() 



a <- puntaje_2 %>% periodicidad == "Trimestral" & between(mes, mes_inicio + 6, mes_inicio + 8) ~ 1
  

# Assuming mes_inicio is defined as a specific month value

a <- puntaje_3 %>% 
  group_by(id) %>% 
  mutate(a = case_when(periodicidad == "Mensual" & mes %in% c(mes_inicio + (0:6)*2) ~ 1,
                       periodicidad == "Bimestral" & mes %in% c(mes_inicio + (0:6)*2) ~ 1, 
                       periodicidad == "Trimestral" & mes %in% c(mes_inicio + (0:6)*3) ~ 1, 
                       periodicidad == "Cuatrimestral" & mes %in% c(mes_inicio + (0:6)*4) ~ 1, 
                       periodicidad == "Quimestral" & mes %in% c(1 + (0:6)*5) ~ 1, 
                       periodicidad == "Semestral" & mes %in% c(1 + (0:6)*6) ~ 1)) %>% 
  select(id, periodicidad, mes, mes_inicio, a) 

# Se calcula el cndice de Gestión

indice <- puntaje_3 %>% 
  # Se suma los mismos PMCs de una misma jerarquC-a, mes y eje. 
  # Luego, se cuenta el nC:mero de indicadores que comparten jerarquC-a, mes y eje. 
  # Posteriormente, se divide la suma para el conteo.
  group_by(jerarquia, mes, eje) %>% 
  mutate(pcm_2_sum = if_else(!is.na(pcm_2), sum(pcm_2, na.rm = T), pcm_2),  
         ind_pcm_2 = if_else(!is.na(pcm_2), 1, pcm_2), 
         ind_num = if_else(!is.na(ind_pcm_2), sum(ind_pcm_2, na.rm = T), NA_real_), 
         prom1   = if_else(!is.na(ind_num), pcm_2_sum/ind_num, NA_real_)) %>% 
  ungroup() %>% 
  # El puntaje resultante el paso anterior se pondera multiplicando por 0.3
  # los ejes de Gestión y por 0.7 los ejes de resultado.
  mutate(ponderacion1 = if_else(jerarquia == "Gestión", prom1*0.3, prom1*0.7)) %>% 
  # Se mantiene solo una obs. por cada mes, jerarquC-a y eje.
  arrange(desc(ponderacion1)) %>% 
  distinct(mes, jerarquia, eje, .keep_all = T) %>% 
  # Se cuentan los indicadores que pertenencen a un mismo eje. Si hay dos indicadores, 
  # significa que uno es de Gestión y otro de resultados. Si esto C:ltimo es el caso, 
  # se suman ambos. Si solo hay un indicador, se asigna el valor del eje sin ponderación 
  group_by(mes, eje) %>% 
  mutate(count_ind = if_else(!is.na(ponderacion1), 1, NA_real_), 
         sum_ind = if_else(!is.na(count_ind), sum(count_ind, na.rm = T), NA_real_),   
         prom2 = case_when(
           is.na(sum_ind) ~ NA_real_, 
           sum_ind == 1 ~ sum(prom1, na.rm = T), 
           sum_ind == 2 ~ sum(ponderacion1, na.rm = T)
         )
  ) %>% 
  ungroup() %>% 
  # Se mantiene una sola observación por cada eje y por cada mes
  arrange(desc(prom2)) %>% 
  distinct(eje, mes, .keep_all = T) %>%
  # Se calcula el puntaje de cada mes, asignando a cada eje una valoración inversamente
  # igual a 1/n, donde "n" es es el nC:mero de ejes con valor distinto a 0.
  group_by(mes) %>% 
  mutate(count_eje = if_else(prom2 != 0, 1, NA_real_), 
         sum_eje =   if_else(!is.na(count_eje), sum(count_eje, na.rm = T), NA_real_),  
         eje_ponderado = if_else(!is.na(count_eje), prom2/sum_eje, NA_real_),  
         ig = sum(eje_ponderado, na.rm = T), 
         peso_eje = count_eje/sum_eje) %>% 
  ungroup()

a <- indice %>% filter(mes == 7, eje == "5. Talento Humano y Mejora de la  Gestión Institucional", jerarquia == "Gestión")


# Se generan las tablas de presentación de resultados

ig <- indice %>% select(eje, mes, ig_parcial = eje_ponderado) %>% arrange(mes, eje)

ig_2 <- pivot_wider(ig, names_from = mes, values_from = ig_parcial) %>% 
  mutate_if(is.numeric, ~ round(., 2)) 

colnames(ig_2) = c("ejes", meses)

peso_eje = NULL
for(i in 2:length(colnames(ig_2))) {
  peso_eje <- c(peso_eje, round(1/nrow(ig_2[i] %>% filter(. != 0)), 4))
}

ig_3 <- as_tibble(
  rbind(
    as.data.frame(ig_2), 
    c("peso_eje", peso_eje*100), 
    c("IG total", colSums(ig_2[-1], na.rm = T))
  ) %>% 
    mutate_if(!str_detect(colnames(.), "eje"), as.numeric) %>% 
    mutate_if(is.numeric, ~ round(., 4))
)


# Avance institucional

avance <- puntaje_3 %>% 
  filter(mes == mes_reporte) %>% 
  group_by(jerarquia, eje, institucion) %>% 
  mutate(
    ind_pcm_2 = if_else(!is.na(pcm_2), 1, NA_real_), 
    ind_num = if_else(!is.na(ind_pcm_2), sum(ind_pcm_2, na.rm = T), NA_real_), 
    pcm_2_sum = if_else(!is.na(pcm_2), sum(pcm_2, na.rm = T), NA_real_), 
    prom1 = if_else(!is.na(pcm_2_sum), pcm_2_sum/ind_num, NA_real_)
  ) %>% 
  ungroup() %>% 
  mutate(ponderacion1 = if_else(jerarquia == "Gestión", prom1*0.3, prom1*0.7)) %>% 
  arrange(desc(prom1)) %>% 
  distinct(jerarquia, eje, institucion, .keep_all = T) %>% 
  group_by(eje, institucion) %>% 
  mutate(
    count_ind = if_else(!is.na(ponderacion1), 1, NA_real_), 
    sum_ind = sum(count_ind, na.rm = T),  
    prom2 = case_when(
      is.na(sum_ind) ~ NA_real_, 
      sum_ind == 1 ~ if_else(!is.na(prom1), 
                             sum(prom1, na.rm = T), NA_real_), 
      sum_ind == 2 ~ if_else(!is.na(ponderacion1), 
                             sum(ponderacion1, na.rm = T), NA_real_)
    )
  ) %>% 
  ungroup() %>% 
  arrange(desc(prom2)) %>% 
  distinct(eje, institucion, .keep_all = T) %>%
  group_by(institucion) %>% 
  mutate(count_eje = if_else(!is.na(prom2), 1, NA_real_), 
         sum_eje = sum(count_eje, na.rm = T), 
         eje_ponderado = if_else(!is.na(sum_eje), prom2/sum_eje, NA_real_), 
         ai = sum(eje_ponderado, na.rm = T)) %>%
  filter(!is.na(ai)) %>% 
  select(eje, institucion, ig_parcial = eje_ponderado) %>% 
  arrange(eje, institucion) %>% 
  ungroup()


# a <- avance %>% filter(mes == 5, eje == "6. Corresponsabilidad y Transparencia")

# Tablas para reporte

# Opción 1: Ejes como columnas

# Se transforma el data frame a wider, con los nombres de los ejes como columnas

tabla_reporte_1 <- pivot_wider(avance, names_from = eje, values_from = ig_parcial) 

# Para obtener el peso de cada eje para cada institución, para cada fila, se di-
# vide 1 para el nC:mero de columnas que no tienen NA menos uno (para no contar 
# la columna de nombres).

peso_eje = NULL
for(i in 1:nrow(tabla_reporte_1)) {
  peso_eje <- c(
    peso_eje, 
    round(1/(ncol(tabla_reporte_1 %>% 
                    select(which(!is.na(tabla_reporte_1[i, ])))
    ) - 1), 4)
  )
}

# Se incluyen los pesos de los ejes y una columna de totales

tabla_reporte_1 <- 
  cbind(tabla_reporte_1, 
        data.frame(valor_max_eje = peso_eje*100), 
        ig_total = rowSums(tabla_reporte_1[-1], na.rm=T)) %>% 
  mutate(ig_total = if_else(valor_max_eje == Inf, NA, ig_total), 
         valor_max_eje = if_else(valor_max_eje == Inf, NA, valor_max_eje)) %>% 
  arrange(institucion) %>% 
  mutate_if(is.numeric, ~ round(., 2))

# Opción 2: instituciones como columnas

tabla_reporte_2 <-  pivot_wider(avance, names_from = institucion, values_from = ig_parcial) 

# Para obtener el peso del eje para cada institución, se divide 1 para el nC:mero de 
# filas que no tienen NA.

peso_eje = NULL
for(i in 2:length(colnames(tabla_reporte_2))) {
  peso_eje <- c(peso_eje, round(1/nrow(tabla_reporte_2[i] %>% filter(!is.na(.) )), 2))
}

peso_eje = if_else(peso_eje == Inf, NA, peso_eje)

# Se incluyen los pesos de los ejes y una columna de totales

tabla_reporte_2 <- rbind(tabla_reporte_2,
                         c("Valor mC!ximo de cada eje*", peso_eje*100), 
                         c("IG Total", colSums(tabla_reporte_2[-1], na.rm=T))) %>% 
  mutate_if(!str_detect(colnames(.), "eje"), as.numeric) %>% 
  mutate_if(!str_detect(colnames(.), "eje") & .[8, ] == 0, ~ NA)

# Aporte institucional

aporte_inst <- puntaje_3 %>% 
  group_by(jerarquia, mes, eje) %>% 
  mutate(pcm_2_sum = if_else(!is.na(pcm_2), sum(pcm_2, na.rm = T), pcm_2),  
         ind_pcm_2 = if_else(!is.na(pcm_2), 1, pcm_2), 
         ind_num = if_else(!is.na(ind_pcm_2), sum(ind_pcm_2, na.rm = T), NA_real_), 
         prom1   = if_else(!is.na(ind_num), pcm_2_sum/ind_num, NA_real_)) %>% 
  ungroup() %>% 
  mutate(ponderacion1 = if_else(jerarquia == "Gestión", prom1*0.3, prom1*0.7)) %>% 
  arrange(desc(ponderacion1)) %>% 
  filter(!is.na(ponderacion1)) %>% 
  group_by(mes) %>% 
  mutate(n_ejes= n_distinct(eje)) %>% 
  ungroup() %>% 
  group_by(mes, eje) %>% 
  mutate(aporte_ind = 
           case_when(
             n_distinct(jerarquia) == 1 ~ pcm_2/ind_num/n_ejes,  
             n_distinct(jerarquia) > 1 & jerarquia == "Gestión" ~ pcm_2/ind_num*0.3/n_ejes,
             n_distinct(jerarquia) > 1 & jerarquia == "Resultado" ~ pcm_2/ind_num*0.7/n_ejes
           )
  ) %>% 
  ungroup() %>% 
  group_by(institucion, mes, eje) %>% 
  mutate(aporte_inst = sum(aporte_ind)) %>% 
  distinct(institucion, .keep_all = T) %>% 
  arrange(mes, eje) %>%
  ungroup() %>% 
  left_join(indice %>% select(eje, eje_ponderado, mes), by= c('eje', 'mes')) %>% 
  group_by(mes, eje) %>% 
  mutate(n_inst = n_distinct(institucion), 
         eje_ponderado = eje_ponderado/n_inst) %>% 
  select(eje, mes, institucion, aporte_inst, eje_ponderado, n_inst, jerarquia) 


a <- aporte_inst %>% filter(mes == 4, eje == "2. Movilización de Recursos")
a <- aporte_inst %>% filter(eje == "1. Generación de entorno habilitador")
a <- aporte_inst %>% filter(mes == 4)


# a <- indice %>% filter(mes == 6, eje == "1. Generación de entorno habilitador", jerarquia == "Gestión")
# a <- ig  %>% filter(mes == 1, eje == "1. Generación de entorno habilitador")
# a <- ig %>% filter(mes == 1, eje == "3. Articulación Territorial")

write.xlsx(ig_3, file = "ig.xlsx", asTable = T)
write.xlsx(tabla_reporte_2, file = "ai.xlsx", asTable = T)

# a <- ig %>% filter(mes == 1, institucion == "BDE")
# a <- ig  %>% filter(mes == 1, eje == "1. Generación de entorno habilitador")
# a <- ig %>% filter(mes == 1, eje == "3. Articulación Territorial")
