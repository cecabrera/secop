library(openxlsx)
library(data.table)

for(file_ in list.files(path = "src", full.names = T, recursive = T)){
  source(file = file_, local = T, encoding = "utf-8")
}

driveUrl <- "https://docs.google.com/spreadsheets/d/1h4m7Fz1F3zhYFYiNH0e0eA1M5OIjyj8AZVSYV7F9Vdg/edit#gid=0"

#Leer archivo con entidades potenciales 
entidades_ <- openxlsx::read.xlsx("doc_anticorrupcion/entidades.xlsx")
# entidades_ <- as.data.table(entidades_)

#Leer fuentes de informacion: indicadores de transparencia
desemInstitucional <- openxlsx::read.xlsx("doc_anticorrupcion/indi_desem_institu.xlsx", sheet="Nacional")
multasSecop <- fread('doc_anticorrupcion/multas_SECOP.csv', encoding = "UTF-8")
antecedSiri <- fread('doc_anticorrupcion/antecedentes_SIRI.csv', encoding = "UTF-8")
contratosSIC <- fread('doc_anticorrupcion/contratacion_SIC.csv', encoding = "UTF-8")
secop1_2020 <- fread('doc_anticorrupcion/SECOP_I_2020.csv', encoding = "UTF-8")
secop1_2019 <- fread('doc_anticorrupcion/SECOP_I_2019.csv', encoding = "UTF-8")
secop2 <- fread('doc_anticorrupcion/SECOP_II.csv', encoding = "UTF-8")

#Tratamiento de datos
multasSecop <- multasSecop[,.("sanciones_SECOP" = .N
                              , "valor_sancion" = sum(valor_sancion)), by = .(nombre_entidad)]
antecedSiri <- antecedSiri[ ,.("Procesos_SIRI"= .N
                               , "inhabilidad_SIRI" = unique(tipo_inhabilidad)
                               , "Meses_Juri_SIRI" = sum(mes_efectos_juridicos)
                               , "MinFecha_SIRI" = min(fecha_efectos_juridicos)
                               , "MaxFecha_SIRI" = max(fecha_efectos_juridicos)) , by = .(entidad_sancionado)]
contratosSIC <- contratosSIC[, .(Identificacion,`Falta que origina la sancion`, `Multa Inicial`, `Fecha de Radicacion`)]
secop1_2020 <- secop1_2020[, .("contratosSecop_I_20"=.N
                               , "promContrato_Secop_I_20" = mean(as.numeric(VALOR_CONTRATO)))
                           , by = .(NIT_ENTIDAD)]
secop1_2019 <- secop1_2019[, .("contratosSecop_I_19"=.N
                               , "promContrato_Secop_I_19" = mean(as.numeric(VALOR_CONTRATO)))
                           , by = .(NIT_ENTIDAD)]
secop2 <- secop2[, .("contratosSecop_II"=.N
                               , "promContrato_Secop_II" = mean(as.numeric(VALOR_CONTRATO)))
                           , by = .(NIT_ENTIDAD)]

#Corregir nombre de columnas
setnames(x = multasSecop
         , old = c("nombre_entidad", "valor_sancion")
         , new = c("entidad", "valor_sancion_secop"))

setnames(x = antecedSiri
         , old = c("entidad_sancionado")
         , new = c("entidad"))

setnames(x = contratosSIC
         , old = c("Identificacion","Falta que origina la sancion", "Multa Inicial", "Fecha de Radicacion")
         , new = c("nit_entidad", "Falta_SIC", "Multa_SIC", "Fecha_SIC"))

setnames(x = secop1_2020
         , old = c("NIT_ENTIDAD")
         , new = c("nit_entidad"))

setnames(x = secop1_2019
         , old = c("NIT_ENTIDAD")
         , new = c("nit_entidad"))

setnames(x = secop2
         , old = c("NIT_ENTIDAD")
         , new = c("nit_entidad"))

#unir fuentes de datos
entidades_ <- merge(x = entidades_, y = desemInstitucional, by = "entidad", all.x=TRUE)
entidades_ <- merge(x = entidades_, y = multasSecop, by = "entidad", all.x = T)
entidades_ <- merge(x = entidades_, y = antecedSiri, by = "entidad", all.x = T)
entidades_ <- merge(x = entidades_, y = contratosSIC, by = "nit_entidad", all.x = T)
entidades_ <- merge(x = entidades_, y = secop1_2020, by = "nit_entidad", all.x = T)
entidades_ <- merge(x = entidades_, y = secop1_2019, by = "nit_entidad", all.x = T)
entidades_ <- merge(x = entidades_, y = secop2, by = "nit_entidad", all.x = T)

entidades_ <- as.data.table(entidades_)
entidades_ <- entidades_[order(contratos, decreasing = T)]

sheet_overwrite(data = entidades_
                , ss = driveUrl
                , sheet = "Entidades")
