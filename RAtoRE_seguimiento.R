# install.packages("dplyr")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("tidyr")
# install.packages("zoo")

library(dplyr)
library(readxl)
library(reshape2)
library(tidyr)

#Aqui se debe colocar la ubicación donde se encuentren todos los documentos
setwd("C:/Users/HP/Desktop/STECSDI/INDICADORES STECSDI/RScripts")
getwd()

#Cargar base de datos 
datos_ini <- read_excel("Matriz_seguimiento_vf_arrastre.xlsx", 
                        sheet = "Datos Iniciales")


# Se eliminan los acentos y se cambia las may?sculas por min?sculas en los nombres
# de variables

colnames(datos_ini) <- iconv(colnames(datos_ini), to = "ASCII//TRANSLIT")
colnames(datos_ini) <- tolower(colnames(datos_ini))


# Se cambian puntos, guiones y espacios por guiones bajos

names(datos_ini) <- gsub("[\\ \\-\\...\\.\\..]+", "_", names(datos_ini))
names(datos_ini) <- gsub("\\.", "_", names(datos_ini))
names(datos_ini) <- gsub("__|___", "_", names(datos_ini))


# Se define un vector de meses

meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
           "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

options(scipen = 100)

datos_ini2 <- datos_ini %>% 
              mutate(diciembre_34 = format(diciembre_35, scientific= F)) %>%   
              select(everything(), diciembre_34, 
                     lim = starts_with("lim_")) %>% 
              mutate_all(~ if_else(. %in% c("NA", "N/A", "Pendiente", "PENDIENTE", "pendiente"), 
                                   NA_character_, as.character(.))) %>% 
              mutate_if(grepl(paste0(meses, collapse = "|"), names(.)), as.numeric) %>% 
              mutate_if(is.numeric, ~ if_else(is.na(.), NA_real_, round(., 4))) %>%
              select(id  = no_, 
                     eje = eje_estrategico, 
                     accion = accion_especifica,
                     indicador = starts_with("nombre"),
                     institucion = institucion_externa, 
                     institucion_reporte = starts_with("unidad"), 
                     jerarquia, 
                     periodicidad, 
                     comportamiento, 
                     direccion_de_meta,
                     starts_with("lim"), 
                     meta_planificada_ = enero_12 | febrero_13 | marzo_14 | abril_15 | mayo_16 | junio_17 | julio_18 | 
                              agosto_19 | septiembre_20 | octubre_21 | noviembre_22 | diciembre_23,  
                     meta_alcanzada_ = enero_24 | febrero_25 | marzo_26 | abril_27 | mayo_28 | junio_29 | julio_30 |
                              agosto_31 | septiembre_32 | octubre_33 | noviembre_34 | diciembre_35 
                              )

datos_ini3 <- datos_ini2 %>% 
              pivot_longer(cols = c(-id, -eje, -indicador, -institucion, 
                                    -institucion_reporte, -jerarquia, 
                                    -periodicidad, -direccion_de_meta, -accion, -comportamiento, 
                                    -lim1, -lim2, -lim3, -lim4, -lim5, -lim6, -lim7, -lim8, -lim9, 
                                    -lim10, -lim11, -lim12), 
                           names_to = c(".value", ".value", "number"), 
                           names_pattern = "([^_]+)_([^_]+)_([0-9]+)") %>% 
                           rename(mes = number) %>% 
                           mutate(mes = as.numeric(mes)) 

#datso <- manda_pm_nd 