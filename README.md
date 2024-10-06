# INSTALACIÓN DE LOS PAQUETES Y LIBRERIAS NECESARIAS
install.packages("DBI")
install.packages("odbc")
install.packages("sqldf")
install.packages("write.xlsx2")
install.packages("randomForest")
library(randomForest)
library(sqldf)
library(odbc)
library(dplyr)
library(rvest)
library(lattice)
library(rpart)
library(rpart.plot)
library(caret)
library(DBI)
library(lubridate)
library(readxl)

# CONEXIÓN A LA BASE DE DATOS
sort(unique(odbcListDrivers()[[1]]))
con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "sqlprod16", 
                 Database = "DWkPLS", 
                 user = "wp" ,
                 Trusted_Connection = "True")

con
dbListFields(con,"D_ACFMAS_")
result <- dbSendQuery(con, "SELECT cod_Usuario_Investigador FROM D_ACFMAS_")

# CREACIÓN DE LOS DATAS FRAME
result
tabla1 <- tbl(con,"D_ACFMAS_")
tabla1
tabla1_df <- collect(tabla1)

count_query <- "SELECT Usuario_investigador COUNT 
                FROM D_ACFMAS_
                GROUP BY Usuario_investigador"

dbGetQuery(con, count_query)

rawdata <- tabla1_df

TRX <- data.frame(rawdata$Cod_MCC,rawdata$MCC)

MCC  <- data.frame(unique(MCC))

# CREACIÓN DE LOS DATAS FRAME
datos <- data.frame(Fecha_trx=rawdata$Fecha_TRX, Horario = rawdata$Grupo_Horario, Fecha_cierre=rawdata$Fecha_Cierre_Caso, Hora2=rawdata$Hora_Fin_Caso, 
                    Analista = rawdata$Usuario_investigador, 
                    Resultado =as.numeric(rawdata$Tipo_Result_Caso_Inv), 
                    Monto_Moneda_Local= rawdata$Monto_Moneda_Local, 
                    Monto_Dollar= rawdata$Monto_Dollar, Cod_Bin= rawdata$Cod_Bin,Bin= rawdata$Bin,
                    Cod_Moneda_Trx=rawdata$Cod_Moneda_Trx, Moneda= rawdata$Moneda_Trx, MCC= rawdata$MCC, Cod_MCC= rawdata$Cod_MCC,
                    Modo_Entrada= rawdata$Modo_Entrada, Cod_modo_entrada= rawdata$Cod_modo_entrada,Cod_Respuesta= rawdata$Cod_Respuesta
                    ,Localidad = rawdata$Localidad_Comercio, Pais = rawdata$Pais_Origen, CodTRX= rawdata$Cod_TRX , DescripcionTrx= rawdata$Trx, step=rawdata$Hora_sin_Minutos_TRX
                    ,balance= rawdata$Saldo_Disp_Moneda_Trx, tarjetas = rawdata$Tarjeta_registro_750, Marca= rawdata$Marca_Franquicia, Autorizacion = rawdata$Autorizacion, codComercio= rawdata$Id_Comercio, Reverso = rawdata$Reverso)

# AJUTAR LOS FORMATOS EN LAS FECHAS

year <- substring(datos$Fecha_trx, 1,4)
month <-substring(datos$Fecha_trx, 5,6)
day <- substring(datos$Fecha_trx, 7,8)
fecha_real <- paste(day,month,year,sep = "/")
year1 <- substring(datos$Fecha_Cierre, 1,4)
month1 <-substring(datos$Fecha_Cierre, 5,6)
day1 <- substring(datos$Fecha_Cierre, 7,8)
fecha_cierre <- paste(day,month,year,sep = "/")

# CREACIÓN DE LOS DATAS FRAME

datos <- data.frame(fecha_real, Horario = rawdata$Grupo_Horario,Hora1= rawdata$Hora_TRX, fecha_cierre, Hora2=rawdata$Hora_Fin_Caso,
                    Analista = rawdata$Usuario_investigador, 
                    Resultado =as.numeric(rawdata$Tipo_Result_Caso_Inv), 
                    Monto_Moneda_Local= rawdata$Monto_Moneda_Local, 
                    Monto_Dollar= rawdata$Monto_Dollar, Cod_Bin= rawdata$Cod_Bin,Bin= rawdata$Bin,
                    Cod_Moneda_Trx=rawdata$Cod_Moneda_Trx,Moneda= rawdata$Moneda_Trx, MCC= rawdata$MCC, Cod_MCC= rawdata$Cod_MCC,
                    Modo_Entrada= rawdata$Modo_Entrada, Cod_modo_entrada= rawdata$Cod_modo_entrada,Cod_Respuesta= rawdata$Cod_Respuesta
                    , Localidad = rawdata$Localidad_Comercio, Pais = rawdata$Pais_Origen, CodTRX= rawdata$Cod_TRX , DescripcionTrx= rawdata$Trx
                    ,  tarjetas = rawdata$Tarjeta_registro_750, comercio = rawdata$Nombre_Localizacion_Cio, cedula =rawdata$Id_Cliente,Marca= rawdata$Marca_Franquicia,Autorizacion = rawdata$Autorizacion, codComercio= rawdata$Id_Comercio, Reverso = rawdata$Reverso)

# CREACIÓN DE LOS DATAS FRAME
datos$fecha_real <- parse_date_time(datos$fecha_real, orders = c("dmy"))
datos$fecha_cierre <- parse_date_time(datos$fecha_cierre, orders = c("dmy"))

# DIVISIÓN DEL CONJUNTO DE DATOS EN CATEGORÍAS 
fraudes <-datos%>%
  filter(Resultado == 1,Cod_Respuesta =="00", Monto_Moneda_Local > 0, Monto_Dollar > 0, ) %>% 
  group_by(Resultado)
buenas <- datos %>% 
  filter(Resultado == 2, Cod_Respuesta =="00",  Monto_Moneda_Local > 0, Monto_Dollar > 0) %>% 
  group_by(Resultado) 
descartada <- datos %>% 
  filter(Resultado == 3, Cod_Respuesta =="00",  Monto_Moneda_Local > 0, Monto_Dollar > 0) %>% 
  group_by(Resultado) 
pendiente <- datos %>% 
  filter(Resultado == 4, Cod_Respuesta =="00",  Monto_Moneda_Local > 0, Monto_Dollar > 0) %>% 
  group_by(Resultado) 
  
# CREACIÓN DE LOS DATAS FRAME SEGÚN LAS CATEGORÍAS

dataset1 <- full_join(fraudes,buenas)
dataset2 <- full_join(descartada,pendiente)
dataset1 <- full_join(dataset1,dataset2)  
dataset1$Resultado <- factor(dataset1$Resultado,levels = c(1,2,3,4),labels = c("Fraude", "Buenas", "Descartada","Pendiente"))

# PESO PARA CREACIÓN DEL PUNTAJE

# PESO SEGÚN EL TIPO DE TARJETA VISA/MASTERCARD Y EL MONTO
dataset1$`Monto real` <- apply(dataset1, 1, function(x) {
  if ((x['Cod_Bin'] == "12345" | x['Cod_Bin'] == "45678" | x['Cod_Bin'] == "76578") & x['Cod_Moneda_Trx'] != "214") {
    return(x['Monto_Dollar'])
  } else {
    return(x['Monto_Moneda_Local'])
  }
})
dataset1$`Monto real`<-as.double(dataset1$`Monto real`)

# PESO SEGÚN EL TIPO DE HORARIO DE LA TRANSACCIÓN
dataset1$PesoHorario <- ifelse(dataset1$Horario == "Madrugada",10,
                         ifelse(dataset1$Horario =="Comercial PM",10,
                                ifelse(dataset1$Horario== "Nocturno", 10,
                                       ifelse(dataset1$Horario == "Alborada",8,
                                              ifelse(dataset1$Horario=="Laboral PM",8,
                                                     ifelse(dataset1$Horario=="Almuerzo", 5,
                                                            ifelse(dataset1$Horario =="Desayuno",3,
                                                                   ifelse(dataset1$Horario == "Laboral AM",2,0))))))))
                                                     
# PESO SEGÚN LA CLASIFICACIÓN DE LA TARJETA 
dataset1$PesoBIN <-ifelse(dataset1$Bin=="Visa Infinite",15,
                          ifelse(dataset1$Bin=="Visa Gold",10,
                                 ifelse(dataset1$Bin=="MasterCard Platinum",15,
                                        ifelse(dataset1$Bin=="Mastercard Black",15,
                                               ifelse(dataset1$Bin=="MasterCard Internacional",10,
                                                      ifelse(dataset1$Bin=="Visa Platinum",15,
                                                             ifelse(dataset1$Bin=="Visa Familiar",15,
                                                                    ifelse(dataset1$Bin=="Visa Clásica Internacional",10,
                                                                           ifelse(dataset1$Bin=="MasterCard Local",5,
                                                                                  ifelse(dataset1$Bin=="MasterCard Gold",5,
                                                                                         ifelse(dataset1$Bin=="Visa Joven",5,
                                                                                                ifelse(dataset1$Bin=="Visa Clásica Local",5,2))))))))))))

dataset1$PesoBinTRX <- ifelse(dataset1$Bin=="Visa Clásica Internacional",5,
                              ifelse(dataset1$Bin=="MasterCard Internacional",5,
                                     ifelse(dataset1$Bin=="Visa Gold",3,
                                            ifelse(dataset1$Bin=="Visa Familiar",3,
                                                   ifelse(dataset1$Bin=="Visa Joven",2,
                                                          ifelse(dataset1$Bin=="Visa Platinum",2,
                                                                 ifelse(dataset1$Bin=="Visa Infinite",2,
                                                                        ifelse(dataset1$Bin=="MasterCard Gold",2,
                                                                               ifelse(dataset1$Bin=="MasterCard Platinum",2,
                                                                                      ifelse(dataset1$Bin=="Mastercard Black",2,
                                                                                             ifelse(dataset1$Bin=="Visa Clásica Local",2,
                                                                                                    ifelse(dataset1$Bin=="MasterCard Local",2,3
                                                                                                    )
)))))))))))

# PESO SEGÚN EL TIPO DE MONEDA DE LA TRANSACCIÓN
dataset1$PesoMoneda <-ifelse(dataset1$Moneda=="USD dólar estadounidense",5,
ifelse(dataset1$Moneda=="RD pesos dominicanos",5,
ifelse(dataset1$Moneda=="EUR Euro",10,
ifelse(dataset1$Moneda=="TRY Lira turca",10,
ifelse(dataset1$Moneda=="GBP Libra esterlina",10,
ifelse(dataset1$Moneda=="SAR Riyal saudí",10,
ifelse(dataset1$Moneda=="COP Peso colombiano",10,
ifelse(dataset1$Moneda=="BGN Lev búlgaro",10,
ifelse(dataset1$Moneda=="INR Rupia india",10,
ifelse(dataset1$Moneda=="MXN Peso mexicano",10,
ifelse(dataset1$Moneda=="PHP Peso filipino",10,
ifelse(dataset1$Moneda=="CAD Dólar canadiense",10,
ifelse(dataset1$Moneda=="AED Dirham de los Emiratos Árabes Unidos",10,
ifelse(dataset1$Moneda=="JPY Yen japonés",10,
ifelse(dataset1$Moneda=="PLN zloty polaco",10,
ifelse(dataset1$Moneda=="KZT Tenge kazajo",10,
ifelse(dataset1$Moneda=="NZD Dólar neozelandés",10,
ifelse(dataset1$Moneda=="EGP Libra egipcia",10,
ifelse(dataset1$Moneda=="PEN Nuevo sol peruano",10,
ifelse(dataset1$Moneda=="UYU Peso uruguayo",10,
ifelse(dataset1$Moneda=="BRL Real brasileño",10,
ifelse(dataset1$Moneda=="BDT Taka bangladesí",10,
ifelse(dataset1$Moneda=="PKR Rupia pakistaní",10,
ifelse(dataset1$Moneda=="KES Chelín keniata",10,10))))))))))))))))))))))))


dataset1$Pesopais <- ifelse(dataset1$Pais=="US",3,
ifelse(dataset1$Pais=="DO",2,
ifelse(dataset1$Pais=="GB",5,
ifelse(dataset1$Pais=="ES",5,
ifelse(dataset1$Pais=="IE",5,
ifelse(dataset1$Pais=="NL",5,
ifelse(dataset1$Pais=="SG",5,
ifelse(dataset1$Pais=="LU",5,
ifelse(dataset1$Pais=="HK",5,
ifelse(dataset1$Pais=="LT",5,
ifelse(dataset1$Pais=="CY",5,
ifelse(dataset1$Pais=="GR",5,
ifelse(dataset1$Pais=="TR",5,
ifelse(dataset1$Pais=="CA",5,
ifelse(dataset1$Pais=="CZ",5,
ifelse(dataset1$Pais=="FR",5,
ifelse(dataset1$Pais=="PA",5,
ifelse(dataset1$Pais=="CO",5,
ifelse(dataset1$Pais=="KZ",5,
ifelse(dataset1$Pais=="DE",5,
ifelse(dataset1$Pais=="MX",5,
ifelse(dataset1$Pais=="MT",5,
ifelse(dataset1$Pais=="MY",5,
ifelse(dataset1$Pais=="SE",5,
ifelse(dataset1$Pais=="PL",5,
ifelse(dataset1$Pais=="GI",5,
ifelse(dataset1$Pais=="CH",5,
ifelse(dataset1$Pais=="SK",5,
ifelse(dataset1$Pais=="UY",5,
ifelse(dataset1$Pais=="LB",5,
ifelse(dataset1$Pais=="JP",5,
ifelse(dataset1$Pais=="HU",5,
ifelse(dataset1$Pais=="PK",5,5)))))))))))))))))))))))))))))))))

# PESO SEGÚN EL TIPO DE COMERCIO O ACTIVIDAD COMERCIAL DONDE SUCEDA LA TRANSACCIÓN

MCC10 <-data.frame(MCC10=c("4814",	"NA",	"5411",	"5311",	"5816",	"7273",	"5310",	"6051",	"5691",	"4121",	"5964",	"5541",	"5812",	"5967",	"5817",	"4722",	"4899",	"7311",	"5999",	"6010",	"5399",	"6012",	"7372",	"7922",	"5942",	"5251"
),Peso=c(10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10,	10
))

MCC8 <-data.frame(MCC8=c("5651",	"5921",	"5621",	"5734",	"5968",	"5732",	"7299",	"8220",	"5735",	"5977",	"8398",	"7699",	"4829"),
                  Peso=c(8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8,	8
))

MCC5 <-data.frame(MCC5=c("7995",	"5992",	"5814",	"7542",	"5331",	"5300",	"5947",	"5813",	"7392",	"5944",	"5137",	"5719",	"8299"),
                  Peso=c(5,	5,	5,	5,	5,	5,	5,	5,	5,	5,	5,	5,	5
))


MCC3 <-data.frame(MCC3=c("8999",	"6211",	"4816",	"7399",	"5941",	"7277",	"5970",	"5533",	"3127",	"5811",	"5051",	"4215",	"5122",	"5699",	"5192",	"9399",	"5661",	"4411",	"6300",	"6011",	"7011",	"5969",	"4511",	"5045",	"5099",	"8021",	"7298",	"5912",	"5712",	"8641",	"5521",	"9311",	"3015",	"1711",	"5611",	"5983"),
                  Peso=c(3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3,	3))

dataset1$PesoMCC <- ifelse(!is.na(match(dataset1$Cod_MCC,MCC10$MCC10)),10,
                           ifelse(!is.na(match(dataset1$Cod_MCC,MCC8$MCC8)),8,
                                  ifelse(!is.na(match(dataset1$Cod_MCC,MCC5$MCC5)),5,
                                         ifelse(!is.na(match(dataset1$Cod_MCC,MCC3$MCC3)),3,3))))


MCC_10 <- data.frame(MCC10=c(	"4722",	"5411",	"6051",	"5732",	"NA",	"5521",	"5311",	"5051",	"7995",	"5691",	"5310",	"5331",	"7922",	"5621",	"5300",	"5942",	"5399",	"6012",	"5813",	"4829",	"7311",	"5944",	"4814",	"5816")
                    )


MCC_8 <- data.frame(MCC8=c("5651",	"5817",	"3127",	"5541",	"8220",	"5734",	"5964",	"9311",	"7273",	"5921"
))


MCC_5 <- data.frame(MCC5=c("6010","5977",	"3015",	"1711",	"5992",	"5999",	"5712",	"4121",	"5812",	"8299",	"5967",	"5122",	"5968",	"5533",	"4411",	"7299"
))


MCC_3 <- data.frame(MCC3=c("4511",	"7372",	"5912",	"7392",	"5814",	"5045",	"7277",	"5969",	"5947",	"4899",	"5251",	"5735",	"7542",	"8641",	"6011",	"5611",	"7011",	"5941",	"7699",	"6211",	"5661",	"7399",	"5137",	"4215",	"7298",	"5099",	"4816",	"5699",	"5970",	"5719",	"5811",	"8398",	"8999",	"5983",	"8021",	"9399",	"5192",	"6300"
))

dataset1$PesoMCC2 <- ifelse(!is.na(match(dataset1$Cod_MCC,MCC_10$MCC10)),10,
                           ifelse(!is.na(match(dataset1$Cod_MCC,MCC_8$MCC8)),8,
                                  ifelse(!is.na(match(dataset1$Cod_MCC,MCC_5$MCC5)),5,
                                         ifelse(!is.na(match(dataset1$Cod_MCC,MCC_3$MCC3)),3,3))))


# PESO SEGÚN LA FRECUENCIA DE LA TRANSACCIÓN
 
velocidad <- dataset1 %>%
  group_by(cedula,tarjetas,Horario,fecha_real)%>%
  count(tarjetas) 

combinar <- merge(dataset1, velocidad, by = c("tarjetas","fecha_real","cedula","Horario"))

combinar$velocidad <- ifelse(combinar$n <=1,5,
                             ifelse(combinar$n <=2,10,
                                    ifelse(combinar$n >= 3,20,0))) 

combinar$score <- rowSums(combinar[,c("PesoBIN","PesoHorario","PesoBinTRX","PesoMCC",
                                   "PesoMCC2","PesoMoneda","Pesopais","PesoTRX", "velocidad", "MDE")])

  
 # RESULTADO FINAL 

library(openxlsx)
write.xlsx(fraudes, "data2.xlsx", sheetName = "Sheet1")


unique(rawdata$Cod_grupo_horario,rawdata$Grupo_Horario)
