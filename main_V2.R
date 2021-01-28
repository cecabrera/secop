library(data.table)
library(RSocrata)
library(RSelenium)

for(file_ in list.files(path = "src", full.names = T, recursive = T)){
  source(file = file_, local = T, encoding = "utf-8")
}

driveUrl <- "https://docs.google.com/spreadsheets/d/1h4m7Fz1F3zhYFYiNH0e0eA1M5OIjyj8AZVSYV7F9Vdg/edit#gid=0"
secop2_pc <- "https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-II-Procesos-de-Contrataci-n/p6dx-8zbt"

keyWords <- c("datos","diseño web", "inteligencia artificial", "página web", "diseño de interfaz", "big data")

for (c in keyWords){
  where_ = paste0("&adjudicado=No&estado_de_apertura_del_proceso='Abierto'&estado_del_procedimiento='No Definido'&$q=",c,"&$order=fecha_de_publicacion_del DESC")
  
  d <- getDataTable(
    url = secop2_pc
    , where = where_
    , select = NULL
  )
  
  d[, fecha_de_publicacion_del := as.Date(fecha_de_publicacion_del)]
  
  d <- d[, c("adjudicado"
             , "ciudad_de_la_unidad_de"
             , "ciudad_entidad"
             , "entidad"
             , "descripci_n_del_procedimiento"
             , "urlproceso"
             , "duracion"
             , "ciudad_proveedor"
             , "codigo_pci"
             , "departamento_entidad"
             , "departamento_proveedor"
             , "estado_de_apertura_del_proceso"
             , "estado_del_procedimiento"       
             , "fase"
             , "fecha_de_publicacion_del"
             , "nombre_de_la_unidad_de")]
  
  
  if(c=="datos"){
    ant_ = d
  } else{
    l = list(ant_, d)
    ant_ <- rbindlist(l)
  }
  
  ant_ <- ant_[!duplicated(ant_$descripci_n_del_procedimiento), ]
}

test <- ant_[1:10,]

#Scrapping 
rD <- rsDriver(browser="chrome", port=4565L, verbose=F)
remDr <- rD$client

for(i in 1:nrow(test)){
  print(i)
  test$Active[i] <- scraping(test$urlproceso[i],remDr, ind = i)
  Sys.sleep(20)
}
