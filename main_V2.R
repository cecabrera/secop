library(data.table)
library(RSocrata)
library(RSelenium)
library(xlsx)

for(file_ in list.files(path = "src", full.names = T, recursive = T)){
  source(file = file_, local = T, encoding = "utf-8")
}

# Socrata documentation for queries
# https://dev.socrata.com/docs/queries/

driveUrl <- "https://docs.google.com/spreadsheets/d/1h4m7Fz1F3zhYFYiNH0e0eA1M5OIjyj8AZVSYV7F9Vdg/edit#gid=0"
secop2_pc <- "https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-II-Procesos-de-Contrataci-n/p6dx-8zbt"

keyWords <- c("datos", "limpieza de datos","analizar datos","diseño web", "inteligencia artificial", "página web", "diseño de interfaz", "big data", "Inteligencia de Negocios"
              , "hardware", "software")

#&modalidad_de_contratacion!= 'Contratación directa'

for (c in keyWords){
  where_ = paste0("&modalidad_de_contratacion='Solicitud de información a los Proveedores'&adjudicado=No&estado_de_apertura_del_proceso='Abierto'&estado_del_procedimiento='No Definido'&$q=",c,"&$order=fecha_de_publicacion_del DESC")
  
  d <- getDataTable(
    url = secop2_pc
    , where = where_
    , select = NULL
  )
  
  d[, fecha_de_publicacion_del := as.Date(fecha_de_publicacion_del)]
  
  d[, Keyword := c]
  
  d <- d[, c("adjudicado"
             , "ciudad_de_la_unidad_de"
             , "ciudad_entidad"
             , "Keyword"
             , "entidad"
             , "nit_entidad"
             , "descripci_n_del_procedimiento"
             , "urlproceso"
             , "duracion"
             , "precio_base"
             , "modalidad_de_contratacion"
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
    datosSecop2 = d
  } else{
    l = list(datosSecop2, d)
    datosSecop2 <- rbindlist(l)
  }
  
  datosSecop2 <- datosSecop2[!duplicated(datosSecop2$descripci_n_del_procedimiento), ]
}

datosSecop2 <- datosSecop2[order(fecha_de_publicacion_del,precio_base, decreasing = T),]
datosSecop2[, Active := "No"]

#Scrapping 
rD <- rsDriver(browser="chrome", chromever="88.0.4324.96", port=4579L, verbose=F)
remDr <- rD$client

for(i in 1:nrow(datosSecop2)){
  print(i)
  datosSecop2$Active[i] <- scraping(datosSecop2$urlproceso[i],remDr, ind = i)
  Sys.sleep(20)
}

sheet_overwrite(data = datosSecop2
                , ss = driveUrl
                , sheet = "SECOP II")

#Entidades con más proyectos 
entidades_ <- datosSecop2[#modalidad_de_contratacion != "Contratación directa"
                          , .(contratos = .N
                              , min_fecha = min(fecha_de_publicacion_del)
                              , max_fecha = max(fecha_de_publicacion_del)
                              , PromValorContrato = mean(as.numeric(precio_base))
                              , DeStandValrContrato = sd(as.numeric(precio_base))
                              )
                          , by = .(entidad, nit_entidad)]
entidades_ <- entidades_[order(contratos, decreasing = T)]

write.xlsx2(x = entidades_, file = "doc_anticorrupcion/entidades.xlsx")

sheet_overwrite(data = entidades_
                , ss = driveUrl
                , sheet = "Top Entidades")
