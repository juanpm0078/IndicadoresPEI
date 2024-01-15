library("tidyverse")

library(lubridate)

wd <- getwd()

source("Parametrizacion.R")

for (i in 1:(month(Sys.Date())-1)) {
  
  rmarkdown::render("RScripts/Reporte_seguimiento_vf.Rmd",
                    params = list(fecha = paste0("2023-0", i, "-01")), 
                    output_file =  paste0(wd, "/reportes/Global/reporte_global_2023-0", i, "-01")
                   )
}

for (i in 1:(month(Sys.Date())-1)) {
  
  rmarkdown::render("RScripts/Reporte_seguimiento_vf.Rmd",
                    params = list(fecha = paste0("2023-0", i, "-01"), 
                                  descarga = "TRUE"), 
                    output_file =  paste0(wd, "/reportes/Global_descarga/reporte_global_descarga_2023-0", i, "-01")
  )
}



for (x in unique(puntaje$institucion)){
 for(i in 1:(month(Sys.Date())-1)) {
   rmarkdown::render("RScripts/reporte_por_institucion.Rmd",
                     params = list(institucion = x,
                                   fecha = paste0("2023-0", i, "-01")), 
                    output_file =  paste0(wd, "/reportes/Institucional/reporte_", x, "_2023-0",i, "-01")
                     )
              }
}



