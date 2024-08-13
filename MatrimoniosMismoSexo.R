 ####################################################
 #SÍ, QUIERO. ANÁLISIS DE MATRIMONIOS DEL MISMO SEXO#
 ####################################################

##### PREPARACIONES INICIALES

#Preparamos el directorio con los datos
setwd("C:/Users/usuario/Documents/MatrimoniosMismoSexo/Datos")

#Nos aseguramos de que el en efecto es el directorio correcto
getwd()

#Incluimos las bibliotecas con las que vamos a trabajar

## BÁSICAS PARA EL MANEJO DE DATOS
#install.packages("plyr")       
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("data.table")

## LECTURA DE DATOS
#install.packages("readr")      Lectura de ficheros a gran velocidad
#install.packages("openxlsx")   Lectura de ficheros .xlsx (el de municipios)

## CREACIÓN DE TABLAS
#install.packages("remotes")    Esta línea y la siguiente es para crear tablas con el paquete "reactable" (se necesita la última versión disponible en Github)
#remotes::install_github("glin/reactable")

## ANÁLISIS ESTADÍSTICO AVANZADO
#install.packages("spatstat")           Para hallar mediana, cuantiles y varianzas de distribuciones de datos con frecuencias
#install.packages("rriskDistributions") Para usar función fit.cont
#install.packages("fitdistrplus")       Para usar el resto de funciones de ajuste de distribuciones
#install.packages("tseries")            Para el tratamiento de los errores de las rectas de regresión
#install.packages("lmtest")             Para el tratamiento de los errores de las rectas de regresión

## REPRESENTACIÓN GRÁFICA
#install.packages("ggplot2")
#install.packages("ggpubr")     Para hacer representaciones de múltiples gráficas 
#install.packages("plotly")     Para hacer representaciones interactivas
#install.packages("RJSplot")    Para hacer representaciones interactivas
#install.packages("rgdal")      Para las representaciones de los mapas de España
#install.packages("broom")      Para las representaciones de los mapas de España
#install.packages("viridis")    Para crear degradados de alta calidad en las escalas gráficas 


## Incluimos las bibliotecas

library("plyr")
library("dplyr")
library("tidyverse")
library("data.table")
library("readr")
library("openxlsx")
library("reactable")
library("spatstat")
library("rriskDistributions")
library("fitdistrplus")
library("tseries")
library("ggplot2")
library("ggpubr")
library("plotly")
library("RJSplot")
library("lmtest")
library("rgdal")
library("broom")
library("viridis")    


## Modificamos el formato e idioma de las tablas creadas con el paquete reactable
options(reactable.language = reactableLang(searchPlaceholder = "Buscar...",
  noData = "No se ha encontrado ninguna entrada",
  pageInfo = "Entradas desde la fila {rowStart} a la fila {rowEnd} ({rows} filas)"))

## Copiamos el comando para representar las rectas de regresión
ggplotRegression <- function (lmObject,Title,xTitle,yTitle) {
  real<-lmObject$model[,2]
  predicted<-lmObject$model[,1]
  regression<-data.frame(real,predicted)
  coeff<-paste(names(lmObject$coefficients), "=", round(as.vector(lmObject$coefficients),8), "( P.value =", round(as.vector(summary(lmObject)$coefficients[,"Pr(>|t|)"]),3),")")
  coeffSubtitle<-c()
  for(i in 1:length(coeff)){coeffSubtitle<-paste(coeffSubtitle,coeff[i],sep="\n")}
  gg<-ggplot(regression, aes_string(x = real, y = predicted)) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = Title , caption = bquote("Adj"~R^2 == .(round(summary(lmObject)$adj.r.squared,4))),subtitle = coeffSubtitle,x=xTitle,y=yTitle) 
  gg+theme_bw()+
    theme(plot.title = element_text(size = 9),plot.subtitle = element_text(size = 7),plot.caption = element_text(face = "bold")) 
}

#Esta opción la tenemos para que evitar el uso de notación científica en las gráficas ya que es poco clarificador
options(scipen=10000)



#### 0- IMPORTACIÓN Y LIMPIEZA DE LOS DATOS CON LOS QUE VAMOS A TRABAJAR

#Vamos a ser sistemáticos:
#De ahora en adelante usaremos las abreviaturas: DS=distinto sexo, MS=mismo sexo, HMS=hombres mismo sexo, MMS=mujeres mismo sexo para referirnos a las parejas con las que trabajemos

### 0.1- IMPORTACIÓN Y LIMPIEZA DE LOS DATOS OBTENIDOS MEDIANTE LAS SERIES DE MICRODATOS DE MATRIMONIOS DEL INE

## En primer lugar, vamos a exportar los datos más sencillos que poseemos, que son los de la población de las provincias. Estos se encuentran en la web del INE, en formato .csv.
#Datos poblacionales de las provincias (2005-2018)
poblacion<-read.table(file="Poblacion.txt",sep="\t",header=TRUE)[,c("Provincias","Periodo","Total")]

#Los datos del extranjero están disponibles en un fichero adicional y además solamente están disponibles a partir del año 2009
poblacionextranjero<-transpose(read.table(file="PoblacionExtranjero.txt",sep="\t"))
poblacionextranjero<-cbind(c("Extranjero","Extranjero","Extranjero","Extranjero","Extranjero","Extranjero","Extranjero","Extranjero","Extranjero","Extranjero"),poblacionextranjero)
colnames(poblacionextranjero)<-c("Provincias","Periodo","Total")
poblacion<-rbind(poblacion,poblacionextranjero)

#Los datos disponibles en el INE en formato .csv son bastante limitados (y más aún en el caso de las parejas del mismo sexo). Es por esto que he decidido recurrir a los microdatos.
#Estos recogen los datos de todos los matrimonios celebrados en España anonimizados y codificados en formato fixed width files.
#Para analizar los datos hay que consultar la información del INE para ver los caracteres de cada línea del archico están reservados para cada columna.
#Con esta información se crea (manualmente, ya que no hay otra opción) un vector que recoge las longitudes variables de las categorías que se estudian (por ejemplo, para codificar sexo, se utiliza un solo caracter 1 o 6, mientras que se utilizan 2 para codificar edad, 4 para año...)
#Para complicar el tratamiento de los datos, además en 2007 se realizó un cambio en la codificación de los microdatos de matrimonio, de modo que hay que analizarlos aparte. Hasta 2007 se recogían 41 datos de cada matrimonio y a partir de 2007 se pasaron a recoger nada más y nada menos que 78.
#Para poder recoger las categorías, he tenido que estudiar a fondo los archivos que determinan la anonimización de las categorías, para ver qué características que se recogían eran comunes a ambas series y modificar los nombres de estas para poder trabajar con ellas.

#Este es el proceso de lectura de datos. Para ello se ha empleado la función read_fwf específica para los archivos de anchura constante.
#Esta función es parte del paquete readr especial para big data, ya que de no utilizar este paquete, el tiempo de lectura de los archivos (cada serie anual son 25 Mb en formato numérico) se multiplicaba hasta por 60, al usar la función read.fwf
vectorlongitud1=c(3,2,2,4,1,3,2,2,4,1,1,2,3,2,2,4,1,1,2,3,2,3,3,1,1,2,4,2,4,1,1,1,1,1,1,1,1,1,1,2,2)
colnames1=c("CMUNI","CPROI","MESCM","AÑOCM","TIPOC","CMUMA","CPROMA","MESNCA","AÑONCA","SEXOCA","ECIVCA","SPROF1","CMUNNCA","CPRONCA","MESNCB","AÑONCB","SEXOCB","ECIVCB","SPROF2","CMUNNCB","CPRONCB","PAISNACCA","PAISNACCB","SNORM1","SNORM2","MESDCA","AÑODCA","MESDCB","AÑODCB","TMUNIN","TMUNRMA","TMUNRA","TMUNRB","CARIN","CBRIN","MARIN","CARMR","CBRMR","MARMR","EDADCA","EDADCB")

datos2005<-read_fwf(file="Matrimonios.a2005.txt", fwf_widths(vectorlongitud1,col_names=colnames1))
datos2006<-read_fwf(file="Matrimonios.a2006.txt", fwf_widths(vectorlongitud1,col_names=colnames1))
datos2007<-read_fwf(file="Matrimonios.a2007.txt", fwf_widths(vectorlongitud1,col_names=colnames1))

vectorlongitud2=c(2,3,2,4,1,2,3,3,2,4,1,1,3,1,2,3,3,1,1,2,4,1,2,4,1,2,3,3,2,2,2,4,1,1,3,1,2,3,3,1,1,2,4,1,2,4,1,2,3,3,2,2,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,1,1,1,1,1,1,1,1,1,12)
colnames2=c("CPROI","CMUNI","MESCM","AÑOCM","TIPOC","CPROMA","CMUMA","CPAISRXMA","MESNCA","AÑONCA","NACIOECA","NACIOXCA","PAISNACCA","CUANNACCA","CPRONCA","CMUNNCA","PAISNXCA","SEXOCA","ECIVCA","MESFACONANTCA","AÑOFACONANTCA","NORMVCA","MESDCA","AÑODCA","NORDMDCA","CPRORECA","CMUNRECA","PAISREXCA","ESTUDIOCA","CAUTOCA","MESNCB","AÑONCB","NACIOECB","NACIOXCB","PAISNACCB","CUANNACCB","CPRONCB","CMUNNCB","PAISNXCB","SEXOCB","ECIVCB","MESFACONANTCB","AÑOFACONANTCB","NORMVCB","MESDCB","AÑODCB","NORDMDCB","CPRORECB","CMUNRECB","PAISREXCB","ESTUDIOCB","CAUTOCB","TMUNIN","TMUNRMA","TMUNNA","TMUNNB","TMUNRA","TMUNRB","CARIN","CBRIN","MARIN","CARMR","CBRMR","MARMR","EDADCA","EDADCB","TPAISNACIMIENTOCA","TPAISNACIMIENTOCB","TPAISNACIONALIDADCA","TPAISNACIONALIDADCB","TPAISRMA","TPAISRCA","TPAISRCB","NACIDOECA","NACIDOXCA","NACIDOECB","NACIDOXCB","BLANCOS")

#Como curiosidad, leer estas 10 tablas con el comando read_fwf tarda 5 veces menos que leer una sola de ellas con read.fwf. De este experimento es de donde saco la conclusión anterior.
start <- Sys.time()
datos2008<-read_fwf(file="Matrimonios.a2008.txt", fwf_widths(vectorlongitud2,col_names=colnames2))
datos2009<-read_fwf(file="Matrimonios.a2009.txt", fwf_widths(vectorlongitud2,col_names=colnames2))
datos2010<-read_fwf(file="Matrimonios.a2010.txt", fwf_widths(vectorlongitud2,col_names=colnames2))
datos2011<-read_fwf(file="Matrimonios.a2011.txt", fwf_widths(vectorlongitud2,col_names=colnames2))
datos2012<-read_fwf(file="Matrimonios.a2012.txt", fwf_widths(vectorlongitud2,col_names=colnames2))
datos2013<-read_fwf(file="Matrimonios.a2013.txt", fwf_widths(vectorlongitud2,col_names=colnames2))
datos2014<-read_fwf(file="Matrimonios.a2014.txt", fwf_widths(vectorlongitud2,col_names=colnames2))
datos2015<-read_fwf(file="Matrimonios.a2015.txt", fwf_widths(vectorlongitud2,col_names=colnames2))
datos2016<-read_fwf(file="Matrimonios.a2016.txt", fwf_widths(vectorlongitud2,col_names=colnames2))
datos2017<-read_fwf(file="Matrimonios.a2017.txt", fwf_widths(vectorlongitud2,col_names=colnames2))
datos2018<-read_fwf(file="Matrimonios.a2018.txt", fwf_widths(vectorlongitud2,col_names=colnames2))
end <- Sys.time()
end-start

#Seleccionamos ahora las variables comunes a ambas codificaciones de datos, que son aquellas con las que podemos trabajar (aunque solo vamos a analizar una pequeña parte de ellas, son 37)
#Estas variables están disponibles para futuros análisis en estos datos.
#La codificación empleada facilita añadir las series anuales que salgan en el futuro.

colintersection<-intersect(colnames1,colnames2)

#Seleccionamos las columnas comunes y unimos las series de datos con la función rbind
datos0507<-rbind(datos2005[,colintersection],datos2006[,colintersection],datos2007[,colintersection])
datos0818<-rbind(datos2008[,colintersection],datos2009[,colintersection],datos2010[,colintersection],datos2011[,colintersection],datos2012[,colintersection],datos2013[,colintersection],datos2014[,colintersection],datos2015[,colintersection],datos2016[,colintersection],datos2017[,colintersection],datos2018[,colintersection])
datostotal<-rbind(datos0507,datos0818)

#En 2005-2007 se codificaba el matrimonio en el extranjero con el codigo de provincia 66 y luego se cambió a <NA> en los siguientes años, así que para ser coherentes, vamos a cambiar todos los años a 66.
datostotal$CPROMA[is.na(datostotal$CPROMA)]<-"66"
datostotal$CMUMA[is.na(datostotal$CMUMA)]<-"000"
datostotal<-datostotal%>%mutate("CMUMA"=revalue(CMUMA,c("0" = "000")))

#Separaremos los matrimonios de las variables entre matrimonios de distinto sexo, de hombres de mismo sexo y de mujeres de mismo sexo; y, aparte, por las categorías que estudiaremos posteriormente en el trabajo. 
#Se ha comprobado que si la pareja es de distinto sexo, el primer registrado siempre será el hombre y la segunda la mujer.

## 0.1.1- DISTRIBUCIÓN GEOGRÁFICA DEL MATRIMONIO

# Separamos los datos de matrimonios por municipios de residencia. Lo hacemos esto lo primero porque nos interesan los códigos de municipio que constan de código de provincia (dos números, en el caso de Salamanca 37) y tres números de municipio
datostotal%>% filter(SEXOCA==1,SEXOCB==6) %>% group_by(CPROMA)%>%count(CMUMA)->munDS
datostotal%>% filter(SEXOCA==1,SEXOCB==1) %>% group_by(CPROMA)%>%count(CMUMA)->munHMS
datostotal%>% filter(SEXOCA==6,SEXOCB==6) %>% group_by(CPROMA)%>%count(CMUMA)->munMMS

#Los volvemos a unir por columnas
datosmunicipios<-rename(full_join(full_join(munDS,munHMS,by=c("CMUMA","CPROMA")), munMMS,by=c("CMUMA","CPROMA")),DS=n.x,HMS=n.y,MMS=n)

#Hay en municipios en los que no ha habido matrimonios del mismo sexo, así que al unir las columnas aparece NA. Cambiamos esto por 0
datosmunicipios$HMS[is.na(datosmunicipios$HMS)]<-0
datosmunicipios$MMS[is.na(datosmunicipios$MMS)]<-0

#Leemos ahora con la función read.xlsx el archivo de Excel con la corrspondencia de los datos del municipio y
correspmunicipios<-read.xlsx("CodigosMunicipios.xlsx")
correspmunicipios<-correspmunicipios%>%transmute(CMUMA=CMUMA,CPROMA=CPROMA,Municipio=Nombre)
datosmunicipios<-merge(datosmunicipios,correspmunicipios)

#Ahora que ya hemos recogido los datos de muncicipios, sustituimos los códigos de provincia. Utilizamos para esto la función revalue.
CP=c("01" = "Álava", "02" = "Albacete", "03" = "Alicante", "04" = "Almería", "05" = "Ávila", "06" = "Badajoz", "07" = "Islas Baleares", "08" = "Barcelona", "09" = "Burgos", "10" = "Cáceres", "11" = "Cádiz", "12" = "Castellón", "13" = "Ciudad Real", "14" = "Córdoba", "15" = "La Coruña", "16" = "Cuenca", "17" = "Gerona", "18" = "Granada", "19" = "Guadalajara", "20" = "Guipúzcoa", "21" = "Huelva", "22" = "Huesca", "23" = "Jaén", "24" = "León", "25" = "Lérida", "26" = "La Rioja", "27" = "Lugo", "28" = "Madrid", "29" = "Málaga", "30" = "Murcia", "31" = "Navarra", "32" = "Orense", "33" = "Asturias", "34" = "Palencia", "35" = "Las Palmas", "36" = "Pontevedra", "37" = "Salamanca", "38" = "Santa Cruz de Tenerife", "39" = "Cantabria", "40" = "Segovia", "41" = "Sevilla", "42" = "Soria", "43" = "Tarragona", "44" = "Teruel", "45" = "Toledo", "46" = "Valencia", "47" = "Valladolid", "48" = "Vizcaya", "49" = "Zamora", "50" = "Zaragoza", "51" = "Ceuta", "52" = "Melilla", "66" = "Extranjero")
datostotal<-datostotal%>% mutate("CPROI"=revalue(CPROI,CP),"CPROMA"=revalue(CPROMA,CP),"CPRONCA"=revalue(CPRONCA,CP),"CPRONCB"=revalue(CPRONCB,CP))

#Filtramos por tipo de matrimonio
datosDS<-datostotal%>% filter(SEXOCA==1,SEXOCB==6)
datosHMS<-datostotal%>% filter(SEXOCA==1,SEXOCB==1)
datosMMS<-datostotal%>% filter(SEXOCA==6,SEXOCB==6)

#Número de matrimonios por provincia de residencia y año

datosDS%>%group_by(AÑOCM)%>%count(CPROMA)->provDS
datosHMS%>%group_by(AÑOCM)%>%count(CPROMA)->provHMS
datosMMS%>%group_by(AÑOCM)%>%count(CPROMA)->provMMS
datosprovincias<-rename(full_join(full_join(provDS,provHMS,by=c("AÑOCM","CPROMA")), provMMS,by=c("AÑOCM","CPROMA")),DS=n.x,HMS=n.y,MMS=n)

#Para poder operar con los datos cambiamos todos los NA restantes por 0.
datosprovincias[is.na(datosprovincias)]<-0

#Agregamos datos de comunidades autónomas y hacemos una correspondencia entre comunidades autónomas y provincias
provdesordenadas=sapply(poblacion,levels)$Provincias
provordenadas=c("Álava", "Albacete", "Alicante", "Almería", "Ávila", "Badajoz", "Islas Baleares", "Barcelona", "Burgos", "Cáceres", "Cádiz", "Castellón", "Ciudad Real", "Córdoba", "La Coruña", "Cuenca", "Gerona", "Granada", "Guadalajara", "Guipúzcoa", "Huelva", "Huesca", "Jaén", "León", "Lérida", "La Rioja", "Lugo", "Madrid", "Málaga", "Murcia", "Navarra", "Orense", "Asturias", "Palencia", "Las Palmas", "Pontevedra", "Salamanca", "Santa Cruz de Tenerife", "Cantabria", "Segovia", "Sevilla", "Soria", "Tarragona", "Teruel", "Toledo", "Valencia", "Valladolid", "Vizcaya", "Zamora", "Zaragoza", "Ceuta", "Melilla", "Extranjero")
comunidades<-c("País Vasco","Castilla - La Mancha","Comunidad Valenciana","Andalucía","Castilla y León","Extremadura","Islas Baleares","Cataluña","Castilla y León","Extremadura","Andalucía","Comunidad Valenciana","Castilla - La Mancha","Andalucía","Galicia","Castilla - La Mancha", "Cataluña", "Andalucía", "Castilla - La Mancha", "País Vasco", "Andalucía","Aragón", "Andalucía", "Castilla y León","Cataluña","La Rioja","Galicia","Comunidad de Madrid","Andalucía", "Región de Murcia","Comunidad Foral de Navarra", "Galicia", "Principado de Asturias","Castilla y León","Islas Canarias","Galicia", "Castilla y León","Islas Canarias", "Cantabria","Castilla y León","Andalucía", "Castilla y León","Cataluña","Aragón", "Castilla - La Mancha", "Comunidad Valenciana","Castilla y León","País Vasco","Castilla y León","Aragón","Ceuta", "Melilla", "Extranjero")
correspondencia<-data.frame(provordenadas,comunidades)
colnames(correspondencia)<-c("CPROMA","CCAA")
poblacion<-poblacion %>% mutate(Provincias=mapvalues(Provincias,from = provdesordenadas,to=provordenadas),Periodo=as.numeric(Periodo),Total=as.numeric(Total))
colnames(poblacion)<-c("CPROMA","AÑOCM","Población")
poblacionCCAA<-merge(poblacion,correspondencia)%>%group_by(AÑOCM,CCAA)%>%summarise(Población=sum(Población))
datosCCAA<-merge(datosprovincias,correspondencia)


## 0.1.2- EDADES

datosDS%>%group_by(EDADCA)%>%count(EDADCB)->edadesDS

#Como dato curioso hay una mujer que se casó con 100 años de edad en nuestro data set (lo sabemos porque nació en 1905 y se casó en 2005), pero como los creadores de la codificación no se plantearon que ninguno de los cónyuges llegaría al altar con una edad de tres cifras, nos sale que tiene 0 años de edad. Hay que cambiarlo:
edadesDS[3690,"EDADCB"]<-100

datosHMS%>%group_by(EDADCA)%>%count(EDADCB)->edadesHMS
datosMMS%>%group_by(EDADCA)%>%count(EDADCB)->edadesMMS

#La elección de quién queda registrado como el cónyuge 1 y quién es el cónyuge 2 es completamente arbitraria en el caso de las parejas del mismo sexo (en las de distinto sexo, 1 es el hombre y 2 es la mujer)
#Para eliminar esta arbitrariedad, vamos a simetrizar los datos:
simedadesHMS<-edadesHMS
colnames(simedadesHMS)<-c("EDADCB","EDADCA","n")
edadesHMS<-full_join(edadesHMS,simedadesHMS,by=c("EDADCB","EDADCA"))
edadesHMS[is.na(edadesHMS)]<-0
edadesHMS<-edadesHMS%>%transmute(EDADCB,n=(n.x+n.y)/2)

simedadesMMS<-edadesMMS
colnames(simedadesMMS)<-c("EDADCB","EDADCA","n")
edadesMMS<-full_join(edadesMMS,simedadesMMS,by=c("EDADCB","EDADCA"))
edadesMMS[is.na(edadesMMS)]<-0
edadesMMS<-edadesMMS%>%transmute(EDADCB,n=(n.x+n.y)/2)

## 0.1.3- NACIONALIDADES DE LOS CONTRAYENTES

#Separamos los matrimonios por la nacionalidad de los cónyuges:
datosDS%>%group_by(PAISNACCA)%>%count(PAISNACCB)->paisDS
datosHMS%>%group_by(PAISNACCA)%>%count(PAISNACCB)->paisHMS
datosMMS%>%group_by(PAISNACCA)%>%count(PAISNACCB)->paisMMS

#Nos interesa saber con qué países estamos trabajando así que empleamos el Excel que recoge los códigos de países

paises<-read.xlsx("CodigosPaises.xlsx",colNames=TRUE)[,c(2,3)]
paisDS<-paisDS%>%transmute(PAISA=mapvalues(PAISNACCA,from=paises$Código.de.país,to=paises$Literal.de.País),PAISB=mapvalues(PAISNACCB,from=paises$Código.de.país,to=paises$Literal.de.País),n=n)
paisDS<-paisDS[,-1]
paisDS<-paisDS[complete.cases(paisDS),] #Eliminamos los datos corruptos en los que no se ha especificado el país
paisDS<-paisDS%>%filter(PAISA!=390 & PAISA!=449 & PAISB!=390) #Eliminamos estos datos que contienen los países 390 y 449 que no están en el diccionario geográfico

paisHMS<-paisHMS%>%transmute(PAISA=mapvalues(PAISNACCA,from=paises$Código.de.país,to=paises$Literal.de.País),PAISB=mapvalues(PAISNACCB,from=paises$Código.de.país,to=paises$Literal.de.País),n=n)
paisHMS<-paisHMS[,-1]
paisHMS<-paisHMS[complete.cases(paisHMS),] 

paisMMS<-paisMMS%>%transmute(PAISA=mapvalues(PAISNACCA,from=paises$Código.de.país,to=paises$Literal.de.País),PAISB=mapvalues(PAISNACCB,from=paises$Código.de.país,to=paises$Literal.de.País),n=n)
paisMMS<-paisMMS[,-1]
paisMMS<-paisMMS[complete.cases(paisMMS),] 


### 0.2- DATOS OBTENIDOS DE LAS SERIES DE DIVORCIOS
#Me apetecía también incluir este apartado en el trabajo, pero quería centrarme principalmente en l
#Procedemos a incluir las variables de divorcios por tipo de divorcio:
divtipoDS<-read.table(file="DS_DivorciosTipo.csv",sep="\t",header=TRUE)
divtipoMS<-read.table(file="MS_DivorciosTipo.csv",sep="\t",header=TRUE)

#La primera columna de divtipoDS es irrelevante, ya que solo pone "Total Nacional" así que la quitamos:
divtipoDS<-divtipoDS[,-1]

#Unimos todas las variables en una única tabla
divtipo<-full_join(divtipoDS,filter(divtipoMS,Sexo=="Total")[,-2],by=c("Tipo.de.divorcio","Periodo"))
divtipo<-full_join(divtipo,filter(divtipoMS,Sexo=="Hombres")[,-2],by=c("Tipo.de.divorcio","Periodo"))
divtipo<-full_join(divtipo,filter(divtipoMS,Sexo=="Mujeres")[,-2],by=c("Tipo.de.divorcio","Periodo"))

#Procedemos a incluir las variables de divorcios por duración:
divduracionDS<-read.table(file="DS_DivorciosDuracion.csv",sep="\t",header=TRUE)
divduracionMS<-read.table(file="MS_DivorciosDuracion.csv",sep="\t",header=TRUE)

#La segunda columna de divduracionDS es irrelevante ya que solo pone Total, así que la quitamos. Unificamos estos datos a las categorías de los datos de matrimonios del mismo sexo
divduracionDS<-divduracionDS[,-2]

#Unimos todas las variables del mismo sexo en una única tabla
divduracionMS<-full_join(full_join(filter(divduracionMS,Sexo=="Total")[,-2],filter(divduracionMS,Sexo=="Hombres")[,-2],by=c("Duración.del.matrimonio","Periodo")),filter(divduracionMS,Sexo=="Mujeres")[,-2],by=c("Duración.del.matrimonio","Periodo"))




#### 1. ANÁLISIS Y REPRESENTACIÓN GRÁFICA DE LA DISTRIBUCIÓN DE LOS MATRIMONIOS POR MUNICIPIOS, PROVINCIAS Y COMUNIDADES AUTÓNOMAS EN ESPAÑA

### 1.1- TABULACIÓN Y ANÁLISIS ESTADÍSTICO BÁSICO DE LOS DATOS

## 1.1.0- NACIONAL

#Hallamos el número total de matrimonios entre los años 2005-2018 y los representamos en tabla
matraño<-datosprovincias%>%group_by(AÑOCM)%>%summarise(suma_DS=sum(DS),suma_HMS=sum(HMS),suma_MMS=sum(MMS),suma_MS=sum(HMS)+sum(MMS),media_DS=mean(DS),media_HMS=mean(HMS),media_MMS=mean(MMS),media_MS=mean(HMS)+mean(MMS))
tmatraño<-matraño[,c(1,2,3,4,5)]
colnames(tmatraño)<-c("Año","Matrimonios de distinto sexo","Matrimonios de hombres","Matrimonios de mujeres","Matrimonios del mismo sexo")
reactable(tmatraño,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#FFDAC1")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

#Hallamos el porcentaje de matrimonios de hombres y mujeres entre los matrimonios del mismo sexo
porHMaño<-datosprovincias%>%group_by(AÑOCM)%>%summarise(porH=100*sum(HMS)/(sum(HMS)+sum(MMS)),porM=100*sum(MMS)/(sum(HMS)+sum(MMS)))
tporHMaño<-porHMaño
colnames(tporHMaño)<-c("Año","Porcentaje de matrimonios de hombres del total de matrimonios del mismo sexo","Porcentaje de matrimonios de mujeres del total de matrimonios del mismo sexo")
reactable(porHMaño,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#FF9AA2")),
  filterable = TRUE, searchable=TRUE, highlight = TRUE, rownames = FALSE, compact=TRUE,pagination = FALSE)

#Hallamos el porcentaje de matrimonios respecto al total que se ofician por año
pormatraño<-datosprovincias%>%group_by(AÑOCM)%>%summarise(porcentaje_DS=100*sum(DS)/(sum(DS)+sum(HMS)+sum(MMS)),porcentaje_HMS=100*sum(HMS)/(sum(DS)+sum(HMS)+sum(MMS)),porcentaje_MMS=100*sum(MMS)/(sum(DS)+sum(HMS)+sum(MMS)),porcentaje_MS=100*(sum(MMS)+sum(HMS))/(sum(DS)+sum(HMS)+sum(MMS)))
tpormatraño<-pormatraño
colnames(tpormatraño)<-c("Año","Porcentaje de matrimonios de distinto sexo","Porcentaje de matrimonios de hombres","Porcentaje de matrimonios de mujeres","Porcentaje de matrimonios del mismo sexo")
reactable(tpormatraño,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#FFDAC1")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)


## 1.1.1- MUNICIPIOS

#Separamos los datos por porcentaje
tpormunicipios<-datosmunicipios%>%transmute(Municipio,"Provincia"=revalue(CPROMA,CP),"Porcentaje de matrimonios de distinto sexo"=100*DS/(DS+HMS+MMS),"Porcentaje de matrimonios de hombres"=100*HMS/(DS+HMS+MMS),"Porcentaje de matrimonios de mujeres"=100*MMS/(DS+HMS+MMS),"Porcentaje de matrimonios del mismo sexo"=100*(HMS+MMS)/(DS+HMS+MMS))

#Representamos los datos en una tabla
tdatosmunicipios<-datosmunicipios%>%transmute("Municipio"=Municipio,"Provincia"=revalue(CPROMA,CP),"Matrimonios de distinto sexo"=DS,"Matrimonios de hombres"=HMS,"Matrimonios de mujeres"=MMS,"Matrimonios del mismo sexo"=HMS+MMS)

#Unimos los datos anteriores en una tabla
reactable(full_join(tdatosmunicipios,tpormunicipios,by=c("Municipio","Provincia")),defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "lightblue")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

## 1.1.2- PROVINCIAS

#Hallamos lo mismo que hallamos para el territorio nacional por año, pero para las provincias esta vez
matrprovincia<-datosprovincias%>%group_by(CPROMA)%>%summarise(suma_DS=sum(DS),suma_HMS=sum(HMS),suma_MMS=sum(MMS),suma_MS=sum(HMS)+sum(MMS),media_DS=mean(DS),media_HMS=mean(HMS),media_MMS=mean(MMS),media_MS=mean(HMS)+mean(MMS))
tmatrprovincia<-matrprovincia
colnames(tmatrprovincia)<-c("Provincia","Matrimonios de distinto sexo","Matrimonios de hombres","Matrimonios de mujeres","Matrimonios del mismo sexo","Media de matrimonios de distinto sexo","Media de matrimonios de hombres","Media de matrimonios de mujeres","Media de matrimonios del mismo sexo")
reactable(tmatrprovincia,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#FFFFD8")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

#Hallamos los porcentajes de hombres y mujeres respecto al total de matrimonios del mismo sexo por provincia
porHMpro<-datosprovincias%>%group_by(CPROMA)%>%summarise(porH=100*sum(HMS)/(sum(HMS)+sum(MMS)),porM=100*sum(MMS)/(sum(HMS)+sum(MMS)))
tporHMpro<-porHMpro
colnames(tporHMpro)<-c("Provincia","Porcentaje de matrimonios de hombres del total de matrimonios del mismo sexo","Porcentaje de matrimonios de mujeres del total de matrimonios del mismo sexo")
reactable(tporHMpro,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#FF9AA2")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

#Hallamos los porcentajes de matrimonios por año de cada tipo
pordatosprovincias<-datosprovincias%>%transmute(CPROMA,porcentaje_DS=100*DS/(DS+HMS+MMS),porcentaje_HMS=100*HMS/(DS+HMS+MMS),porcentaje_MMS=100*MMS/(DS+HMS+MMS),porcentaje_MS=100*(MMS+HMS)/(DS+MMS+HMS))
tpordatosprovincias<-pordatosprovincias
colnames(tpordatosprovincias)<-c("Año","Provincia","Porcentaje de matrimonios de distinto sexo","Porcentaje de matrimonios de hombres","Porcentaje de matrimonios de mujeres","Porcentaje de matrimonios del mismo sexo")
reactable(tpordatosprovincias,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#FFFFD8")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

#Hallamos los porcentajes de matrimonios de cada tipo
pormatrprovincia<-datosprovincias%>%group_by(CPROMA)%>%summarise(porcentaje_DS=100*sum(DS)/(sum(DS)+sum(HMS)+sum(MMS)),porcentaje_HMS=100*sum(HMS)/(sum(DS)+sum(HMS)+sum(MMS)),porcentaje_MMS=100*sum(MMS)/(sum(DS)+sum(HMS)+sum(MMS)),porcentaje_MS=100*(sum(MMS)+sum(HMS))/(sum(DS)+sum(HMS)+sum(MMS)))
tpormatrprovincia<-pormatrprovincia
colnames(tpormatrprovincia)<-c("Provincia","Porcentaje de matrimonios de distinto sexo","Porcentaje de matrimonios de hombres","Porcentaje de matrimonios de mujeres","Porcentaje de matrimonios del mismo sexo")
reactable(tpormatrprovincia,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#FFFFD8")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)


#Vemos que Las Palmas, Santa Cruz de Tenerife y Barcelona son las provincias que mayor porcentaje de matrimonios del mismo sexo ofician, mientras que Ceuta, Teruel y Zamora son las que menos


## 1.1.3 - COMUNIDADES AUTÓNOMAS

#Hallamos los datos por CCAA
matrCCAA<-datosCCAA%>%group_by(CCAA)%>%summarise(suma_DS=sum(DS),suma_HMS=sum(HMS),suma_MMS=sum(MMS),suma_MS=sum(HMS)+sum(MMS),media_DS=mean(DS),media_HMS=mean(HMS),media_MMS=mean(MMS),media_MS=mean(HMS)+mean(MMS))
tmatrCCAA<-matrCCAA
colnames(tmatrCCAA)<-c("Comunidad Autónoma","Matrimonios de distinto sexo","Matrimonios de hombres","Matrimonios de mujeres","Matrimonios del mismo sexo","Media de matrimonios de distinto sexo","Media de matrimonios de hombres","Media de matrimonios de mujeres","Media de matrimonios del mismo sexo")
reactable(tmatrCCAA,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#B5EAD7")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

#Hallamos los porcentajes de hombres y mujeres respecto al total de matrimonios del mismo sexo por provincia
porHMCCAA<-datosCCAA%>%group_by(CCAA)%>%summarise(porH=100*sum(HMS)/(sum(HMS)+sum(MMS)),porM=100*sum(MMS)/(sum(HMS)+sum(MMS)))
tporHMCCAA<-porHMCCAA
colnames(tporHMCCAA)<-c("Comunidad Autónoma","Porcentaje de matrimonios de hombres del total de matrimonios del mismo sexo","Porcentaje de matrimonios de mujeres del total de matrimonios del mismo sexo")
reactable(tporHMCCAA,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#FF9AA2")),
  filterable = TRUE, searchable=TRUE, highlight = TRUE, rownames = FALSE, compact=TRUE,pagination = FALSE)

#Hallamos los porcentajes de matrimonios de cada tipo
pormatrCCAA<-datosCCAA%>%group_by(CCAA)%>%summarise(porcentaje_DS=100*sum(DS)/(sum(DS)+sum(HMS)+sum(MMS)),porcentaje_HMS=100*sum(HMS)/(sum(DS)+sum(HMS)+sum(MMS)),porcentaje_MMS=100*sum(MMS)/(sum(DS)+sum(HMS)+sum(MMS)),porcentaje_MS=100*(sum(MMS)+sum(HMS))/(sum(DS)+sum(HMS)+sum(MMS)))
tpormatrCCAA<-pormatrCCAA
colnames(tpormatrCCAA)<-c("Comunidad Autónoma","Porcentaje de matrimonios de distinto sexo","Porcentaje de matrimonios de hombres","Porcentaje de matrimonios de mujeres","Porcentaje de matrimonios del mismo sexo")
reactable(tpormatrCCAA,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#B5EAD7")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

#Vemos que las Islas Canarias y Baleares son las comunidades que mayor porcentaje de matrimonios del mismo sexo ofician, mientras que Castilla y León y Extremadura son las que menos


## 1.1.4 - TASAS DE NUPCIALIDAD

#Calculemos ahora la tasa de nupcialidad, es decir, el cociente entre los matrimonios oficiados en un año y la población ese mismo año
#Hagámoslo en primer lugar por provincias
nupcial<-right_join(datosprovincias, poblacion)%>%mutate(TBNupDS=10000*DS / Población,TBNupHMS=10000*HMS / Población,TBNupMMS=10000*MMS / Población,TBNupMS=10000*(HMS+MMS) / Población)
tnupcial<-nupcial[,-c(3,4,5,6)]
colnames(tnupcial)<-c("Año","Provincia","Tasa de nupcialidad bruta de matrimonios de distinto sexo","Tasa de nupcialidad bruta de matrimonios de hombres","Tasa de nupcialidad bruta de matrimonios de mujeres","Tasa de nupcialidad bruta de matrimonios del mismo sexo")
reactable(tnupcial,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#E0BBE4")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

#Repetimos el proceso para las comunidades autónomas
nupcialCCAA<-full_join(datosCCAA, poblacionCCAA)%>%transmute(CCAA=CCAA,AÑOCM=AÑOCM,TBNupDS=10000*DS / Población,TBNupHMS=10000*HMS / Población,TBNupMMS=10000*MMS / Población,TBNupMS=10000*(HMS+MMS) / Población)
tnupcialCCAA<-nupcialCCAA
colnames(tnupcialCCAA)<-c("Comunidad Autónoma","Año","Tasa de nupcialidad bruta de matrimonios de distinto sexo","Tasa de nupcialidad bruta de matrimonios de hombres","Tasa de nupcialidad bruta de matrimonios de mujeres","Tasa de nupcialidad bruta de matrimonios del mismo sexo")
reactable(tnupcialCCAA,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#E0BBE4")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

#Finalmente para la población nacional
nupcialnacional<-full_join(datosprovincias, poblacion,by="AÑOCM")%>%group_by(AÑOCM)%>%summarise(Población=sum(Población),DS=sum(DS),HMS=sum(HMS),MMS=sum(MMS),TBNupDS=10000*sum(DS) / sum(Población),TBNupHMS=10000*sum(HMS) / sum(Población),TBNupMMS=10000*sum(MMS) / sum(Población),TBNupMS=10000*(sum(HMS)+sum(MMS)) / sum(Población))
tnupcialnacional<-nupcialnacional[,-c(2,3,4,5)]
colnames(tnupcialnacional)<-c("Año","Tasa de nupcialidad bruta de matrimonios de distinto sexo","Tasa de nupcialidad bruta de matrimonios de hombres","Tasa de nupcialidad bruta de matrimonios de mujeres","Tasa de nupcialidad bruta de matrimonios del mismo sexo")
reactable(tnupcialnacional,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#E0BBE4")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

### 1.2 - TÉCNICAS ESTADÍSTICAS AVANZADAS

## CONTRASTE DE LA PROPORCIÓN

#Constraste de la proporción entre el número de matrimonios de distinto sexo y de hombres del mismo sexo en las CCAA.
prop.test(matrix(c(matrCCAA$suma_DS,matrCCAA$suma_HMS),nrow=20,ncol=2),alternative = "two.sided")
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones entre las comunidades no son equivalentes.

#Constraste de la proporción entre el número de matrimonios de distinto sexo y de mujeres del mismo sexo en las CCAA.
prop.test(matrix(c(matrCCAA$suma_DS,matrCCAA$suma_MMS),nrow=20,ncol=2),alternative = "two.sided")
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones entre las comunidades no son equivalentes.

#Constraste de la proporción entre el total número de matrimonios del mismo sexo y de hombres del mismo sexo en las CCAA.
prop.test(matrix(c(matrCCAA$suma_MS,matrCCAA$suma_HMS),nrow=20,ncol=2),alternative = "two.sided")
#Puesto que 3.035e-15<0.05, rechazamos la hipótesis nula por lo que las proporciones entre las comunidades no son equivalentes.

#Constraste de la proporción entre el total número de matrimonios del mismo sexo y de mujeres del mismo sexo en las CCAA.
prop.test(matrix(c(matrCCAA$suma_MS,matrCCAA$suma_MMS),nrow=20,ncol=2),alternative = "two.sided")
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones entre las comunidades no son equivalentes.

#Constraste de la proporción entre el total número de matrimonios de hombres y de mujeres del mismo sexo en las CCAA.
prop.test(matrix(c(matrCCAA$suma_HMS,matrCCAA$suma_MMS),nrow=20,ncol=2),alternative = "two.sided")
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones entre las comunidades no son equivalentes.

#Luego todos nuestros contrastes han fallado probando la enorme diferencias que hay entre las comunidades en materia de matrimonios

## CONTRASTE DE LA VARIANZA

#Contraste de la varianza entre el número de matrimonios de distinto sexo y de hombres del mismo sexo en las CCAA.
var.test(matrCCAA$suma_DS,matrCCAA$suma_HMS)
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones entre las comunidades no son equivalentes.

#Contraste de la varianza entre el número de matrimonios de distinto sexo y de mujeres del mismo sexo en las CCAA.
var.test(matrCCAA$suma_DS,matrCCAA$suma_MMS)
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones entre las comunidades no son equivalentes.

#Constraste de la varianza entre el total número de matrimonios del mismo sexo y de hombres del mismo sexo en las CCAA.
var.test(matrCCAA$suma_MS,matrCCAA$suma_HMS)
#Puesto que 0.03442<0.05, rechazamos la hipótesis nula por lo que las proporciones entre las comunidades no son equivalentes.

#Constraste de la varianza entre el total número de matrimonios del mismo sexo y de mujeres del mismo sexo en las CCAA.
var.test(matrCCAA$suma_MS,matrCCAA$suma_MMS)
#Puesto que 0.0003845<0.05, rechazamos la hipótesis nula por lo que las proporciones entre las comunidades no son equivalentes.

#Constraste de la varianza entre el total número de matrimonios de hombres y de mujeres del mismo sexo en las CCAA.
var.test(matrCCAA$suma_HMS,matrCCAA$suma_MMS)
#Puesto que 0.113>0.05, podemos considerar que la varianza de los matrimonios de hombres y mujeres del mismo sexo es la misma en las distintas comunidades autónomas.

# CONTRASTE DE LA MEDIA

#Hagamos ahora un contraste de la media entre el número de matrimonios de hombres y el número de matrimonios de mujeres por comunidad autónoma
#Veamos a ver si los matrimonios por CCAA siguen una distibución normal
shapiro.test(matrCCAA$suma_HMS)
shapiro.test(matrCCAA$suma_MMS)
#En ambos casos el p-valor es muy inferior a 0.05, así que ninguna de las dos variables es normal.
#Tendremos que emplear entonces el test de Wilcoxon. Las variables se consideran dependientes porque están medidas en los mismos territorios (las comunidades autónomas en este caso)
wilcox.test(matrCCAA$suma_HMS,matrCCAA$suma_MMS,paired = TRUE)
#Puesto que 0.001411<0.05, rechazamos la hipótesis nula por lo que la media de estas variables no es la misma.

#ANÁLISIS DE LA CORRELACIÓN Y REGRESIÓN

#Correlación entre matrimonios matrimonios de distinto sexo y del mismo sexo por provincia
cor.test(matrprovincia$suma_DS,matrprovincia$suma_MS,method = "spearman")
#El p-valor es muy inferior a 0.05 y hay una fuerte correlación (0.96) entre ambas variables.

modeloMSDS<-lm(matrprovincia$suma_MS~matrprovincia$suma_DS)
summary(modeloMSDS)
#Vemos que se trata de un modelo bastante preciso
#La recta de regresión sería la siguiente
ggplotRegression(lmObject=modeloMSDS, Title="Matrimonios del mismo sexo en función de matrimonios de distinto sexo", xTitle="Matrimonios de distinto sexo", yTitle="Matrimonios del mismo sexo")

#Veamos a ver si existen posibles sesgos en la creación de este modelo.
# Comprobamos la normalidad:
ks.test(modeloMSDS$residuals,pnorm, mean(modeloMSDS$residuals), sd(modeloMSDS$residuals))
#Se cumple la normalidad

# Comprobamos la aleatoriedad:
# Creamos un factor con los signos de los residuos
signo<-as.factor(ifelse(modeloMSDS$residuals>0,"+","-"))
runs.test(signo)
#Se cumple la aleatoriedad

# Comprobamos la autocorrelación:
dwtest(matrprovincia$suma_MS~matrprovincia$suma_DS)
#Los residuos no están correlacionados.

# Representamos los residuos:
plot(matrprovincia$suma_DS, modeloMSDS$residuals, xlab="Predictora: Matrimonios de distinto sexo", ylab="Residuos", pch=20)
abline(h=0, lty=2, col="orangered2")
#Muestra las virtudes de este modelo

#Correlación entre población y matrimonios de distinto sexo por provincia y año
cor(nupcial$DS,nupcial$Población,method = "spearman")
#El p-valor no nos deja calcularlo, pero se ve que hay una fuerte correlación (0.96) entre ambas variables.

modeloPDS<-lm(nupcial$DS~nupcial$Población)
summary(modeloPDS)
#Vemos que se trata de un modelo bastante preciso
ggplotRegression(lmObject=modeloPDS, Title="Matrimonios de distinto sexo en función de la población", xTitle="Población", yTitle="Matrimonios de distinto sexo")
#Los outliers de este modelo se corresponden a los matrimonios en el extranjero, que son mucho menores en proporción que en las provincias españolas

#Veamos a ver si existen posibles sesgos en la creación de este modelo.
# Comprobamos la normalidad:
ks.test(modeloPDS$residuals,pnorm, mean(modeloPDS$residuals), sd(modeloPDS$residuals))
#No se cumple la normalidad
# Comprobamos la aleatoriedad:
# Creamos un factor con los signos de los residuos
signo<-as.factor(ifelse(modeloPDS$residuals>0,"+","-"))
runs.test(signo)
#No se cumple la aleatoriedad
# Comprobamos la autocorrelación:
dwtest(nupcial$DS~nupcial$Población)
#Los residuos están correlacionados.
# Representamos los residuos:
plot(nupcial$Población, modeloPDS$residuals, xlab="Predictora: Población", ylab="Residuos", pch=20)
abline(h=0, lty=2, col="orangered2")
#Muestra en efecto los fallos de este modelo

#Correlación entre población y matrimonios de hombres por provincia y año

cor(nupcial$HMS,nupcial$Población,method = "spearman")
#El p-valor es muy inferior a 0.05 y hay una fuerte correlación (0.925) entre ambas variables.
modeloPHMS<-lm(nupcial$HMS~nupcial$Población)
summary(modeloPHMS)
#Vemos que se trata de un modelo bastante preciso
ggplotRegression(lmObject=modeloPHMS, Title="Matrimonios de hombres del mismo sexo en función de la población", xTitle="Población", yTitle="Matrimonios de hombres")
#Los outliers de este modelo se corresponden a los matrimonios del 2005, que son mucho menores en proporción ya que el matrimonio se legalizó a mitades de ese año y los matrimonios del 2006, que fueron el boom del año posterior, así que vamos a eliminarlos.
nupcialcor<-nupcial%>%filter(AÑOCM>2006)
modeloPHMScor<-lm(nupcialcor$HMS~nupcialcor$Población)
ggplotRegression(lmObject=modeloPHMScor, Title="Matrimonios de hombres del mismo sexo en función de la población", xTitle="Población", yTitle="Matrimonios de hombres")
#Vemos que el R^2 ha subido hasta 0.92 al eliminar estos datos

#Veamos a ver si existen posibles sesgos en la creación de este modelo.
# Comprobamos la normalidad:
ks.test(modeloPHMScor$residuals,pnorm, mean(modeloPHMScor$residuals), sd(modeloPHMScor$residuals))
#No se cumple la normalidad
# Comprobamos la aleatoriedad:
# Creamos un factor con los signos de los residuos
signo<-as.factor(ifelse(modeloPHMScor$residuals>0,"+","-"))
runs.test(signo)
#No se cumple la aleatoriedad
# Comprobamos la autocorrelación:
dwtest(nupcialcor$HMS~nupcialcor$Población)
#Los residuos están correlacionados.
# Representamos los residuos:
plot(nupcialcor$Población, modeloPHMScor$residuals, xlab="Predictora: Población", ylab="Residuos", pch=20)
abline(h=0, lty=2, col="orangered2")
#Muestra en efecto los fallos de este modelo


#Correlación entre población y matrimonios de mujeres por provincia y año

cor(nupcial$MMS,nupcial$Población,method = "spearman")
#El p-valor no nos deja calcularlo, pero hay correlación (0.871) entre ambas variables.
modeloPMMS<-lm(nupcial$MMS~nupcial$Población)
summary(modeloPHMS)
#Vemos que se trata de un modelo bastante preciso
ggplotRegression(lmObject=modeloPMMS, Title="Matrimonios de mujeres del mismo sexo en función de la población en cada provincia y en cada año", xTitle="Población", yTitle="Matrimonios de mujeres")

#Esta regresión presenta las mismas deficiencias que el anterior, así que vamos a corregirlas
modeloPMMScor<-lm(nupcialcor$MMS~nupcialcor$Población)
ggplotRegression(lmObject=modeloPMMScor, Title="Matrimonios de mujeres del mismo sexo en función de la población", xTitle="Población", yTitle="Matrimonios de mujeres")
#Vemos que el R^2 ha subido hasta 0.84 al eliminar estos datos

#Veamos a ver si existen posibles sesgos en la creación de este modelo.
# Comprobamos la normalidad:
ks.test(modeloPMMScor$residuals,pnorm, mean(modeloPMMScor$residuals), sd(modeloPMMScor$residuals))
#Como dos residuos son iguales el test falla, pero veremos al representar los residuos, que claramente no son normales
# Comprobamos la aleatoriedad:
# Creamos un factor con los signos de los residuos
signo<-as.factor(ifelse(modeloPMMScor$residuals>0,"+","-"))
runs.test(signo)
#No se cumple la aleatoriedad
# Comprobamos la autocorrelación:
dwtest(nupcialcor$MMS~nupcialcor$Población)
#Los residuos están correlacionados.
# Representamos los residuos:
plot(nupcialcor$Población, modeloPMMScor$residuals, xlab="Predictora: Población", ylab="Residuos", pch=20)
abline(h=0, lty=2, col="orangered2")
#Muestra en efecto los fallos de este modelo

#Es fácil observar la deficiencia de los modelos predictores en que sabemos que la tasa de nupcialidad, que es el cociente entre matrimonios y población va variando con el año. Así pues, es natural plantearse que se puede hacer un modelo de mejor ajuste incluyendo el año en la regresión
#Eso es, en efecto, lo que haremos a continuación

#Para el caso del matrimonio de distinto sexo:
modeloPADS<-lm(DS~Población+AÑOCM,data=nupcial)
summary(modeloPADS)
modeloPADS$coefficients
#El valor R^2 ha subido a 0.96 en este modelo.
#Podemos observar que el valor del coeficiente correspondiente al año es negativo, lo que implica que (si la población se mantiene constante respecto al tiempo) el número de matrimonios entre personas de distinto sexo baja. 
#Veamos los residuales del modelo
plot(nupcial$Población, modeloPADS$residuals, xlab="Predictora: Población", ylab="Residuos", pch=20)
abline(h=0, lty=2, col="orangered2")
plot(nupcial$AÑOCM, modeloPADS$residuals, xlab="Predictora: Año", ylab="Residuos", pch=20)
abline(h=0, lty=2, col="orangered2")


#Para el caso del matrimonio entre hombres:
modeloPAHMS<-lm(HMS~Población+AÑOCM,data=nupcialcor)
summary(modeloPAHMS)
modeloPAHMS$coefficients
#El valor R^2 ha subido a 0.92 en este modelo.
#Podemos observar que el valor del coeficiente correspondiente al año es positivo, lo que implica que (si la población se mantiene constante respecto al tiempo) el número de matrimonios entre hombres sube. 
plot(nupcialcor$Población, modeloPAHMS$residuals, xlab="Predictora: Población", ylab="Residuos", pch=20)
abline(h=0, lty=2, col="orangered2")
plot(nupcialcor$AÑOCM, modeloPAHMS$residuals, xlab="Predictora: Año", ylab="Residuos", pch=20)
abline(h=0, lty=2, col="orangered2")

#Para el caso del matrimonio entre mujeres:
modeloPAMMS<-lm(MMS~Población+AÑOCM,data=nupcialcor)
summary(modeloPAMMS)
modeloPAMMS$coefficients
#El valor R^2 ha subido a 0.86 en este modelo.
#Podemos observar que el valor del coeficiente correspondiente al año es positivo, lo que implica que (si la población se mantiene constante respecto al tiempo) el número de matrimonios entre hombres sube. 
plot(nupcialcor$Población, modeloPAMMS$residuals, xlab="Predictora: Población", ylab="Residuos", pch=20)
abline(h=0, lty=2, col="orangered2")
plot(nupcialcor$AÑOCM, modeloPAMMS$residuals, xlab="Predictora: Año", ylab="Residuos", pch=20)
abline(h=0, lty=2, col="orangered2")

#Estos modelos estudiados nos permitirían hacer una previsión anticipada de los datos de matrimonios en un cierto año a partir de la estadística de población (que suele salir a la luz unos cuantos meses antes)
#Para probar la eficacia de estos modelos, vamos a repetir el proceso con los datos disponibles hasta 2017 y comparar la eficacia de los datos conocidos con los datos de 2018:
nupcial2017<-nupcial%>%filter(AÑOCM<2018)
nupcialcor2017<-nupcialcor%>%filter(AÑOCM<2018)
nupcial2018<-nupcial%>%filter(AÑOCM==2018)

modelo2017DS<-lm(DS~Población+AÑOCM,data=nupcial2017)
modelo2017HMS<-lm(HMS~Población+AÑOCM,data=nupcialcor2017)
modelo2017MMS<-lm(MMS~Población+AÑOCM,data=nupcialcor2017)

nupcial2018<-nupcial2018%>%transmute(AÑOCM=AÑOCM,CPROMA=CPROMA,Población=Población,RealDS=DS,PredDS=modelo2017DS$coefficients[1]+modelo2017DS$coefficients[2]*Población+modelo2017DS$coefficients[3]*AÑOCM,RealHMS=HMS,PredHMS=modelo2017HMS$coefficients[1]+modelo2017HMS$coefficients[2]*Población+modelo2017HMS$coefficients[3]*AÑOCM,RealMMS=MMS,PredMMS=modelo2017MMS$coefficients[1]+modelo2017MMS$coefficients[2]*Población+modelo2017MMS$coefficients[3]*AÑOCM)
#Este experimento pone en relieve la dificultad de emplear modelos lineales para modelizar este tipo de datos de modo que se ajuste a las diferencias de cada región. En el caso de los matrimonios del mismo sexo, los resultados obtenidos no están tan alejados de la realidad (para las provincias de más de 200000 habitantes.)

#Repitamos el mismo procedimiento pero para los resultados nacionales

#Correlación entre población y matrimonios de distinto sexo nacional
cor.test(nupcialnacional$DS,nupcialnacional$Población,method = "spearman")
#El p-valor es menor que 0.05 así que los resultados son significativos, pero parece haber una leve correlación inversa entre la población y el número de matrimonios de distinto sexo, lo cual aunque parece un poco contraintuitivo, no lo es porque la población y matrimonios están correlacionadas con el año de modo que la población aumenta con el tiempo y los matrimonios de distinto sexo disminuyen.
modelonacPDS<-lm(nupcialnacional$DS~nupcialnacional$Población)
summary(modelonacPDS)
#Representémoslo
ggplotRegression(lmObject=modelonacPDS, Title="Matrimonios de distinto sexo en función de la población", xTitle="Población", yTitle="Matrimonios de distinto sexo")

nupcialnacionalcor<-nupcialnacional%>%filter(AÑOCM>2006)

#Correlación entre población y matrimonios entre hombres nacional
cor.test(nupcialnacionalcor$HMS,nupcialnacionalcor$Población,method = "spearman")
#El p-valor es muy superior a 0.05 así que los resultados no son muy significativos, pero el test indica una correlación directa muy ligera entre la población nacional y el número de matrimonios de hombres.
#Veámosla representada
modelonacPHMS<-lm(nupcialnacional$HMS~nupcialnacional$Población)
summary(modelonacPDS)
ggplotRegression(lmObject=modelonacPHMS, Title="Matrimonios de hombres del mismo sexo en función de la población", xTitle="Población", yTitle="Matrimonios de distinto sexo")


#Correlación entre población y matrimonios entre mujeres nacional
cor.test(nupcialnacionalcor$MMS,nupcialnacionalcor$Población,method = "spearman")
#El p-valor es muy superior a 0.05 así que los resultados no son muy significativos, pero el test indica una fuerte correlación entre la población nacional y el número de matrimonios de hombres.
#Al representarla, vemos que esta correlación es dudosamente lineal
modelonacPMMS<-lm(nupcialnacional$MMS~nupcialnacional$Población)
summary(modelonacPMMS)
ggplotRegression(lmObject=modelonacPMMS, Title="Matrimonios de mujeres del mismo sexo en función de la población", xTitle="Población", yTitle="Matrimonios de distinto sexo")

#Así pues, aunque al separar las diversas provincias obteníamos una correlación entre población nacional y matrimonios, esa correlación se pierde en la totalidad del territorio nacional
#Sin embargo, sigue habiendo correlación entre los matrimonios en España y el año

#Correlación entre año y matrimonios de distinto sexo nacional
cor.test(nupcialnacional$DS,nupcialnacional$AÑOCM,method = "spearman")
#El p-valor es menor que 0.05 así que los resultados son significativos, pero parece haber una leve correlación inversa entre la población y el número de matrimonios de distinto sexo, lo cual indica una disminución en el número de matrimonios de distinto sexo con el tiempo.
modelonacADS<-lm(nupcialnacional$DS~nupcialnacional$AÑOCM)
summary(modelonacADS)
#Representémoslo
ggplotRegression(lmObject=modelonacADS, Title="Matrimonios del distinto sexo en función del año", xTitle="Año", yTitle="Matrimonios de distinto sexo")

#Correlación entre año y matrimonios entre hombres nacional
cor.test(nupcialnacionalcor$HMS,nupcialnacionalcor$AÑOCM,method = "spearman")
#El p-valor es muy superior a 0.05 así que los resultados no son muy significativos, pero el test indica que hay una ligera correlación directa entre el año y el número de matrimonios de hombres.
#Veámosla representada
modelonacAHMS<-lm(nupcialnacionalcor$HMS~nupcialnacionalcor$AÑOCM)
summary(modelonacAHMS)
ggplotRegression(lmObject=modelonacAHMS, Title="Matrimonios de hombres del mismo sexo en función del año", xTitle="Año", yTitle="Matrimonios de hombres")

#Correlación entre año y matrimonios entre mujeres nacional
cor.test(nupcialnacionalcor$MMS,nupcialnacionalcor$AÑOCM,method = "spearman")
#El p-valor es inferior a 0.05 así que los resultados son significativos, hay una fuerte correlación directa entre el año y el número de matrimonios de mujeres.
#Veámosla representada
modelonacAMMS<-lm(nupcialnacionalcor$MMS~nupcialnacionalcor$AÑOCM)
summary(modelonacAMMS)
ggplotRegression(lmObject=modelonacAMMS, Title="Matrimonios de mujeres del mismo sexo en función del año", xTitle="Año", yTitle="Matrimonios de mujeres")


### 1.3- REPRESENTACIONES GRÁFICAS

#Trabajaremos con los siguientes paquetes:

## 1.3.1- PAQUETE PLOTLY

#Representamos proporciones mediante gráficos de barras

grporHMaño <- plot_ly(porHMaño, x = ~AÑOCM, y = ~porH, type = 'bar', name = 'Porcentaje Hombres',marker=list(color="blue"))
grporHMaño <- grporHMaño %>% add_trace(y = ~porM, name = 'Porcentaje Mujeres',marker=list(color="red"))
grporHMaño <- grporHMaño %>% layout(title = "Matrimonios entre parejas del mismo sexo por año",xaxis = list(title ="Año"),yaxis = list(title = 'Porcentaje total'), barmode = 'stack',margin(b=500))
grporHMaño

grporHMpro <- plot_ly(porHMpro, x = ~CPROMA, y = ~porH, type = 'bar', name = 'Porcentaje Hombres',marker=list(color="blue"))
grporHMpro <- grporHMpro %>% add_trace(y = ~porM, name = 'Porcentaje Mujeres',marker=list(color="red"))
grporHMpro <- grporHMpro %>% layout(title = "Matrimonios entre parejas del mismo sexo por provincias", xaxis = list(title ="Provincia"),yaxis = list(title = 'Porcentaje total'), barmode = 'stack',margin(b=500))
grporHMpro

grporHMCCAA <- plot_ly(porHMCCAA, x = ~CCAA, y = ~porH, type = 'bar', name = 'Porcentaje Hombres',marker=list(color="blue"))
grporHMCCAA <- grporHMCCAA %>% add_trace(y = ~porM, name = 'Porcentaje Mujeres',marker=list(color="red"))
grporHMCCAA <- grporHMCCAA %>% layout(title = "Matrimonios entre parejas del mismo sexo por comunidades autónomas", xaxis = list(title ="Comundidad Autónoma"),yaxis = list(title = 'Porcentaje total'), barmode = 'stack',margin(b=500))
grporHMCCAA

#Hacemos diagramas de marcadores

pormatrprovincia<-pormatrprovincia%>%arrange(porcentaje_MS)
grportpro <- plot_ly(pormatrprovincia, x = ~CPROMA, y = ~porcentaje_MMS, name = "Mujeres", type = 'scatter', mode = "markers", marker = list(color = "red"))
grportpro <- grportpro %>% add_trace(x =~CPROMA,  y = ~porcentaje_HMS, name = "Hombres",type = 'scatter',mode = "markers", marker = list(color = "blue"))
grportpro <- grportpro %>% add_trace(x =~CPROMA,  y = ~porcentaje_MS, name = "Total",type = 'scatter',mode = "markers", marker = list(color = "purple"))
grportpro <- grportpro %>% layout(title = "Porcentaje de los matrimonios que son entre parejas del mismo sexo entre los años 2005-2018", xaxis = list(title ="Provincia", categoryorder = "array",categoryarray=~CPROMA), yaxis=list(title="Matrimonios entre 2005-2018"))
grportpro

pormatrCCAA<-pormatrCCAA%>%arrange(porcentaje_MS)
grportCCAA <- plot_ly(pormatrCCAA, x = ~CCAA, y = ~porcentaje_MMS, name = "Mujeres", type = 'scatter', mode = "markers", marker = list(color = "red"))
grportCCAA <- grportCCAA %>% add_trace(x =~CCAA,  y = ~porcentaje_HMS, name = "Hombres",type = 'scatter',mode = "markers", marker = list(color = "blue"))
grportCCAA <- grportCCAA %>% add_trace(x =~CCAA,  y = ~porcentaje_MS, name = "Total",type = 'scatter',mode = "markers", marker = list(color = "purple"))
grportCCAA <- grportCCAA %>% layout(title = "Porcentaje de los matrimonios que son entre parejas del mismo sexo entre los años 2005-2018", xaxis = list(title ="Comunidad Autónoma", categoryorder = "array",categoryarray=~CCAA), yaxis=list(title="Porcentaje del total"))
grportCCAA

matrprovincia<-matrprovincia%>%arrange(suma_MS)
grmatpro <- plot_ly(matrprovincia, x = ~CPROMA, y = ~suma_MMS, name = "Mujeres", type = 'scatter', mode = "markers", marker = list(color = "red"))
grmatpro <- grmatpro %>% add_trace(x =~CPROMA,  y = ~suma_HMS, name = "Hombres",type = 'scatter',mode = "markers", marker = list(color = "blue"))
grmatpro <- grmatpro %>% add_trace(x =~CPROMA,  y = ~suma_MS, name = "Total",type = 'scatter',mode = "markers", marker = list(color = "purple"))
grmatpro <- grmatpro %>% layout(title = "Matrimonios del mismo sexo", xaxis = list(title ="Provincia", categoryorder = "array",categoryarray=~CPROMA), yaxis=list(title="Matrimonios entre 2005-2018"))
grmatpro

matrCCAA<-matrCCAA%>%arrange(suma_MS)
grmatCCAA <- plot_ly(matrCCAA, x = ~CCAA, y = ~suma_MMS, name = "Mujeres", type = 'scatter', mode = "markers", marker = list(color = "red"))
grmatCCAA <- grmatCCAA %>% add_trace(x =~CCAA,  y = ~suma_HMS, name = "Hombres",type = 'scatter',mode = "markers", marker = list(color = "blue"))
grmatCCAA <- grmatCCAA %>% add_trace(x =~CCAA,  y = ~suma_MS, name = "Total",type = 'scatter',mode = "markers", marker = list(color = "purple"))
grmatCCAA <- grmatCCAA %>% layout(title = "Matrimonios del mismo sexo", xaxis = list(title ="Comunidades Autónomas", categoryorder = "array",categoryarray=~CCAA), yaxis=list(title="Matrimonios entre 2005-2018"))
grmatCCAA

dnupc<-plot_ly(nupcialnacional, x= ~AÑOCM, y= ~TBNupMS, type='scatter',name='Total',mode = 'lines', line=list(shape='splinter',color="purple"))
dnupc<-dnupc%>%add_trace(y= ~TBNupHMS, type='scatter',name='Hombres',mode = 'lines', line=list(shape='splinter',color="blue"))
dnupc<-dnupc%>%add_trace(y= ~TBNupMMS, type='scatter',name='Mujeres',mode = 'lines', line=list(shape='splinter',color="red"))
dnupc<-dnupc%>%layout(title = "Tasa de nupcialidad nacional (Bodas por año y por cada 10.000 habitantes)", xaxis = list(title ="Año"), yaxis=list(title="Tasa de nupcialidad"))
dnupc


## 1.3.2- PAQUETE RJSPLOT

#Hacemos diagramas de burbujas (este paquete no acepta acentos y por eso hemos quitado los acentos de los nombres de todas las provincias)

tporHMpro<-tporHMpro%>%arrange(Provincia)
dat<-as.data.frame(tporHMpro[,-1])
rownames(dat)<-chartr('Ááéíóúñ','Aaeioun',tporHMpro$Provincia)
matrprovincia<-matrprovincia%>%arrange(Provincia)
pesos<-as.vector(matrprovincia[[5]])
bubbles_rjs(dat,size=pesos)

tporHMCCAA<-tporHMCCAA%>%arrange("Comunidad Autónoma")
dat<-as.data.frame(tporHMCCAA[,-1])
rownames(dat)<-chartr('Ááéíóúñ','Aaeioun',tporHMCCAA$"Comunidad Autónoma")
tmatrCCAA<-tmatrCCAA%>%arrange("Comunidad Autónoma")
pesos<-as.vector(tmatrCCAA[[5]])
bubbles_rjs(dat,size=pesos)


## 1.3.3- MAPAS DE ESPAÑA

## MAPAS DE COMUNIDADES
# Guardamos el archivo shapefile
shapefileCCAA<-readOGR("Comunidades_Autonomas_ETRS89_30N.shp")

# Para convertir el archivo shapefile en un dataframe utilizamos la función tidy()
mapaCCAA<-tidy(shapefileCCAA)

#Cambiamos los nombres
nombresCCAA <- data.frame(shapefileCCAA$Texto)
nombresCCAA$id <- as.character(seq(0, nrow(nombresCCAA)-1))
nombresCCAA$shapefileCCAA.Texto<-gsub('Ã³','ó',nombresCCAA$shapefileCCAA.Texto)
nombresCCAA$shapefileCCAA.Texto<-gsub('Ã±','ñ',nombresCCAA$shapefileCCAA.Texto)
nombresCCAA$shapefileCCAA.Texto<-gsub('Ã','í',nombresCCAA$shapefileCCAA.Texto)
nombresCCAA$shapefileCCAA.Texto<-gsub('Canarias','Islas Canarias',nombresCCAA$shapefileCCAA.Texto)
nombresCCAA$shapefileCCAA.Texto[1]<-"Andalucía"
nombresCCAA$shapefileCCAA.Texto[16]<-"País Vasco"

colnames(nombresCCAA)<-c("CCAA","id")
mapaCCAA<- left_join(mapaCCAA, nombresCCAA, by = "id")
mapaCCAA$CCAA<-as.factor(mapaCCAA$CCAA)

mapaCCAADS <- mapaCCAA %>%left_join(matrCCAA[,c(1,2)], by= "CCAA")
mapaCCAAHMS <- mapaCCAA %>%left_join(matrCCAA[,c(1,3)], by= "CCAA")
mapaCCAAMMS <- mapaCCAA %>%left_join(matrCCAA[,c(1,4)], by= "CCAA")
mapaCCAAMS <- mapaCCAA %>%left_join(matrCCAA[,c(1,5)], by= "CCAA")

mapaCCAADS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=suma_DS), color= "black", size = 0.2) +
  labs( title = "Matrimonios de distinto sexo por comunidades autónomas",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(option="plasma",direction = -1)

mapaCCAAHMS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=suma_HMS), color= "black", size = 0.2) +
  labs( title = "Matrimonios entre hombres del mismo sexo por comunidades autónomas",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(option="plasma",direction = -1)

mapaCCAAMMS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=suma_MMS), color= "black", size = 0.2) +
  labs( title = "Matrimonios entre mujeres del mismo sexo por comunidades autónomas",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(option="plasma",direction = -1)

mapaCCAAMS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=suma_MS), color= "black", size = 0.2) +
  labs( title = "Matrimonios del mismo sexo por comunidades autónomas",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(option="plasma",direction = -1)


## MAPAS POR PROVINCIAS

#Guardamos el mapa
shapefileprovincias <- readOGR("Provincias_ETRS89_30N.shp")

# Para convertir el archivo shapefile en un dataframe utilizamos la función tidy()
mapaprovincias<-tidy(shapefileprovincias)

#Cambiamos los nombres
nombresprovincias <- data.frame(shapefileprovincias$Texto)
nombresprovincias$id <- as.character(seq(0, nrow(nombresprovincias)-1))
nombresprovincias$shapefileprovincias.Texto<-gsub('Ã³','ó',nombresprovincias$shapefileprovincias.Texto)
nombresprovincias$shapefileprovincias.Texto<-gsub('Ã±','ñ',nombresprovincias$shapefileprovincias.Texto)
nombresprovincias$shapefileprovincias.Texto<-gsub('Ã¡','á',nombresprovincias$shapefileprovincias.Texto)
nombresprovincias$shapefileprovincias.Texto<-gsub('Ãº','ú',nombresprovincias$shapefileprovincias.Texto)
nombresprovincias$shapefileprovincias.Texto<-gsub('Ã©','é',nombresprovincias$shapefileprovincias.Texto)
nombresprovincias$shapefileprovincias.Texto[1]<-"Álava"
nombresprovincias$shapefileprovincias.Texto[4]<-"Almería"
nombresprovincias$shapefileprovincias.Texto[5]<-"Ávila"
nombresprovincias$shapefileprovincias.Texto[25]<-"Lérida"

colnames(nombresprovincias)<-c("CPROMA","id")
mapaprovincias<- left_join(mapaprovincias, nombresprovincias, by = "id")
mapaprovincias$CPROMA<-as.factor(mapaprovincias$CPROMA)

mapaprovinciasDS <- mapaprovincias %>%left_join(matrprovincia[,c(1,2)], by= "CPROMA")
mapaprovinciasHMS <- mapaprovincias %>%left_join(matrprovincia[,c(1,3)], by= "CPROMA")
mapaprovinciasMMS <- mapaprovincias %>%left_join(matrprovincia[,c(1,4)], by= "CPROMA")
mapaprovinciasMS <- mapaprovincias %>%left_join(matrprovincia[,c(1,5)], by= "CPROMA")

nupcialmedia<-nupcial%>%group_by(CPROMA)%>%summarise(TBNupDS=mean(TBNupDS),TBNupHMS=mean(TBNupHMS),TBNupMMS=mean(TBNupMMS),TBNupMS=mean(TBNupMS))

mapanupcialproDS <- mapaprovincias %>%left_join(nupcialmedia[,c(1,2)], by= "CPROMA")
mapanupcialproHMS <- mapaprovincias %>%left_join(nupcialmedia[,c(1,3)], by= "CPROMA")
mapanupcialproMMS <- mapaprovincias %>%left_join(nupcialmedia[,c(1,4)], by= "CPROMA")
mapanupcialproMS <- mapaprovincias %>%left_join(nupcialmedia[,c(1,5)], by= "CPROMA")

mapaprovinciasDS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=suma_DS), color= "black", size = 0.2) +
  labs( title = "Matrimonios de distinto sexo por provincias",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill= "white", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(direction = -1)

mapaprovinciasHMS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=suma_HMS), color= "black", size = 0.2) +
  labs( title = "Matrimonios entre hombres del mismo sexo por provincias",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(direction = -1)

mapaprovinciasMMS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=suma_MMS), color= "black", size = 0.2) +
  labs( title = "Matrimonios entre mujeres del mismo sexo por provincias",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(direction = -1)

mapaprovinciasMS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=suma_MS), color= "black", size = 0.2) +
  labs( title = "Matrimonios del mismo sexo por comunidades autónomas",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(direction = -1)

mapanupcialproDS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=TBNupDS), color= "black", size = 0.2) +
  labs( title = "Tasa de nupcialidad de matrimonios de distinto sexo por provincias",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(option="magma",direction =-1)

mapanupcialproHMS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=TBNupHMS), color= "black", size = 0.2) +
  labs( title = "Tasa de nupcialidad de matrimonios entre hombres del mismo sexo por provincias",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(option="magma",direction =-1)

mapanupcialproMMS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=TBNupMMS), color= "black", size = 0.2) +
  labs( title = "Tasa de nupcialidad de matrimonios entre mujeres del mismo sexo por provincias",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(option="magma",direction =-1)

mapanupcialproMS %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=TBNupMS), color= "black", size = 0.2) +
  labs( title = "Tasa de nupcialidad de matrimonios del mismo sexo por comunidades autónomas",
        caption = "Fuente: INE",
        fill = "Número de matrimonios") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm"))+
  scale_fill_viridis(option="magma",direction =-1)




#### 2- ANÁLISIS Y REPRESENTACIÓN GRÁFICA DE LOS DATOS DE EDAD

### 2.1- TABULACIÓN Y ANÁLISIS ESTADÍSTICO BÁSICO DE LOS DATOS

## 2.1.1- DATOS DE EDAD

#Creamos tablas con los datos
tedadesDS<-edadesDS
colnames(tedadesDS)<-c("Edad del hombre", "Edad de la mujer","Número de parejas")
reactable(tedadesDS,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#bdecb6")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

tedadesHMS<-edadesHMS
colnames(tedadesHMS)<-c("Edad de uno de los hombres", "Edad del otro hombre","Número de parejas")
reactable(tedadesHMS,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value),
  minWidth = 70,
  headerStyle = list(background = "#bdecb6")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

tedadesMMS<-edadesMMS
colnames(tedadesMMS)<-c("Edad de una de las mujeres", "Edad de la otra mujer","Número de parejas")
reactable(tedadesMMS,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value),
  minWidth = 70,
  headerStyle = list(background = "#bdecb6")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

#Calculamos ahora los momentos estadísticos

estedadHDS<-data.frame(edadesDS)%>%summarise(Media=weighted.mean(EDADCA,n), Mediana=weighted.median(EDADCA,n),Varianza=weighted.var(EDADCA,n),"Desviación típica "=sqrt(weighted.var(EDADCA,n)))
estedadMDS<-data.frame(edadesDS)%>%summarise(Media=weighted.mean(EDADCB,n), Mediana=weighted.median(EDADCB,n),Varianza=weighted.var(EDADCB,n),"Desviación típica "=sqrt(weighted.var(EDADCB,n)))
cuantedadHDS<-as.data.frame(weighted.quantile(edadesDS$EDADCA,edadesDS$n))
colnames(cuantedadHDS)<-"Cuantiles Hombres DS"
cuantedadMDS<-as.data.frame(weighted.quantile(edadesDS$EDADCB,edadesDS$n))
colnames(cuantedadMDS)<-"Cuantiles Mujeres DS"
decilesedadHDS<-as.data.frame(weighted.quantile(edadesDS$EDADCA,edadesDS$n,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
colnames(decilesedadHDS)<-"Deciles Hombres DS"
decilesedadMDS<-as.data.frame(weighted.quantile(edadesDS$EDADCB,edadesDS$n,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
colnames(decilesedadMDS)<-"Deciles Mujeres DS"

#Haber simetrizado los datos nos permite calcular estos momentos más fácilmente, ya que sólo hay que trabajar con una columna
estedadHMS<-data.frame(edadesHMS)%>%summarise(Media=weighted.mean(EDADCA,n), Mediana=weighted.median(EDADCA,n),Varianza=weighted.var(EDADCA,n),"Desviación típica "=sqrt(weighted.var(EDADCA,n)))
cuantedadHMS<-as.data.frame(weighted.quantile(edadesHMS$EDADCA,edadesHMS$n))
colnames(cuantedadHMS)<-"Cuantiles Hombres MS"
decilesedadHMS<-as.data.frame(weighted.quantile(edadesHMS$EDADCA,edadesHMS$n,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
colnames(decilesedadHMS)<-"Deciles Hombres MS"

estedadMMS<-data.frame(edadesMMS)%>%summarise(Media=weighted.mean(EDADCA,n), Mediana=weighted.median(EDADCA,n),Varianza=weighted.var(EDADCA,n),"Desviación típica "=sqrt(weighted.var(EDADCA,n)))
cuantedadMMS<-as.data.frame(weighted.quantile(edadesMMS$EDADCA,edadesMMS$n))
colnames(cuantedadMMS)<-"Cuantiles Mujeres MS"
decilesedadMMS<-as.data.frame(weighted.quantile(edadesMMS$EDADCA,edadesMMS$n,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
colnames(decilesedadMMS)<-"Deciles Mujeres MS"

#Unimos todos los datos

estedad<-rbind(estedadHDS,estedadHMS,estedadMDS,estedadMMS)
rownames(estedad)<-c("Hombres de parejas de distinto sexo", "Hombres de parejas del mismo sexo", "Mujeres de parejas de distinto sexo", "Mujeres de parejas del mismo sexo")
cuantilesedad<-data.frame(cuantedadHDS,cuantedadHMS,cuantedadMDS,cuantedadMMS)
decilesedad<-data.frame(decilesedadHDS,decilesedadHMS,decilesedadMDS,decilesedadMMS)
colnames(cuantilesedad)<-c("Hombres de parejas de distinto sexo", "Hombres de parejas del mismo sexo", "Mujeres de parejas de distinto sexo", "Mujeres de parejas del mismo sexo")
colnames(decilesedad)<-c("Hombres de parejas de distinto sexo", "Hombres de parejas del mismo sexo", "Mujeres de parejas de distinto sexo", "Mujeres de parejas del mismo sexo")

reactable(estedad,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#fba3ff")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = TRUE,compact=TRUE,pagination = FALSE)

reactable(cuantilesedad,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#fba3ff")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = TRUE,compact=TRUE,pagination = FALSE)

reactable(decilesedad,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#fba3ff")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = TRUE,compact=TRUE,pagination = FALSE)

#Elaboramos las tablas de contingencia por edades
edcontDS<-xtabs(n~EDADCA+EDADCB,data=edadesDS)
edcontHMS<-xtabs(n~EDADCA+EDADCB,data=edadesHMS)
edcontMMS<-xtabs(n~EDADCA+EDADCB,data=edadesMMS)

#Cambiamos los números a formato numérico y separamos por grupos de edad
edDS<-edadesDS%>%transmute(nEDADCA=as.numeric(EDADCA),nEDADCB=as.numeric(EDADCB),n=n)
edDS<-data.frame(edDS,data.frame(GEDADCA=cut(as.numeric(edDS$nEDADCA), breaks =c(0,19,24,29,34,39,44,49,54,59,100)),GEDADCB=cut(as.numeric(edDS$nEDADCB), breaks =c(0,19,24,29,34,39,44,49,54,59,100))))
gredadesDS<-edDS%>%group_by(GEDADCA,GEDADCB)%>%summarise(n=sum(n))

edHMS<-edadesHMS%>%transmute(nEDADCA=as.numeric(EDADCA),nEDADCB=as.numeric(EDADCB),n=n)
edHMS<-data.frame(edHMS,data.frame(GEDADCA=cut(as.numeric(edHMS$nEDADCA), breaks =c(0,19,24,29,34,39,44,49,54,59,100)),GEDADCB=cut(as.numeric(edHMS$nEDADCB), breaks =c(0,19,24,29,34,39,44,49,54,59,100))))
gredadesHMS<-edHMS%>%group_by(GEDADCA,GEDADCB)%>%summarise(n=sum(n))

edMMS<-edadesMMS%>%transmute(nEDADCA=as.numeric(EDADCA),nEDADCB=as.numeric(EDADCB),n=n)
edMMS<-data.frame(edMMS,data.frame(GEDADCA=cut(as.numeric(edMMS$nEDADCA), breaks =c(0,19,24,29,34,39,44,49,54,59,100)),GEDADCB=cut(as.numeric(edMMS$nEDADCB), breaks =c(0,19,24,29,34,39,44,49,54,59,100))))
gredadesMMS<-edMMS%>%group_by(GEDADCA,GEDADCB)%>%summarise(n=sum(n))


#Elaboramos las tablas de contingencia por grupos de edades
gredcontDS<-xtabs(n~GEDADCA+GEDADCB,data=gredadesDS)
gredcontHMS<-xtabs(n~GEDADCA+GEDADCB,data=gredadesHMS)
gredcontMMS<-xtabs(n~GEDADCA+GEDADCB,data=gredadesMMS)


## 2.1.1- DATOS DE DIFERENCIA DE EDAD

#Vamos ahora a contabilizar la diferencia de edad
datosDS%>%group_by(MESNCA,AÑONCA,MESNCB)%>%count(AÑONCB)->nacDS
datosHMS%>%group_by(MESNCA,AÑONCA,MESNCB)%>%count(AÑONCB)->nacHMS
datosMMS%>%group_by(MESNCA,AÑONCA,MESNCB)%>%count(AÑONCB)->nacMMS

#Cambiamos los años a formato numérico para poder operar con ellos
nacDS<-nacDS%>%mutate(nMESNCA=as.numeric(MESNCA),nAÑONCA=as.numeric(AÑONCA),nMESNCB=as.numeric(MESNCB),nAÑONCB=as.numeric(AÑONCB))
nacHMS<-nacHMS%>%mutate(nMESNCA=as.numeric(MESNCA),nAÑONCA=as.numeric(AÑONCA),nMESNCB=as.numeric(MESNCB),nAÑONCB=as.numeric(AÑONCB))
nacMMS<-nacMMS%>%mutate(nMESNCA=as.numeric(MESNCA),nAÑONCA=as.numeric(AÑONCA),nMESNCB=as.numeric(MESNCB),nAÑONCB=as.numeric(AÑONCB))

#Veamos la diferencia de edad entre las parejas. 
nacDS<-nacDS%>%transmute(AÑONCB=AÑONCB,n=n,DEDAD=(nMESNCB - nMESNCA)/12+nAÑONCB - nAÑONCA,DEDADabs=abs((nMESNCB - nMESNCA)/12+nAÑONCB - nAÑONCA))
nacHMS<-nacHMS%>%transmute(AÑONCB=AÑONCB,n=n,DEDAD=(nMESNCB - nMESNCA)/12+nAÑONCB - nAÑONCA,DEDADabs=abs((nMESNCB - nMESNCA)/12+nAÑONCB - nAÑONCA))
nacMMS<-nacMMS%>%transmute(AÑONCB=AÑONCB,n=n,DEDAD=(nMESNCB - nMESNCA)/12+nAÑONCB - nAÑONCA,DEDADabs=abs((nMESNCB - nMESNCA)/12+nAÑONCB - nAÑONCA))

difedadDS<-data.frame(nacDS$n,nacDS$DEDAD)%>%group_by(nacDS.DEDAD)%>%summarise(N=sum(nacDS.n))
difedadabsDS<-data.frame(nacDS$n,nacDS$DEDADabs)%>%group_by(nacDS.DEDADabs)%>%summarise(N=sum(nacDS.n))
difedadHMS<-data.frame(nacHMS$n,nacHMS$DEDADabs)%>%group_by(nacHMS.DEDADabs)%>%summarise(N=sum(nacHMS.n))
difedadMMS<-data.frame(nacMMS$n,nacMMS$DEDADabs)%>%group_by(nacMMS.DEDADabs)%>%summarise(N=sum(nacMMS.n))

#Para hacernos mejor idea de los datos, los pasaremos a años y calcularemos sus momentos
estdifedadDS<-difedadDS%>%summarise(Media=weighted.mean(nacDS.DEDAD,N),Mediana=weighted.median(nacDS.DEDAD,N),Varianza=weighted.var(nacDS.DEDAD,N),"Desviación típica"=sqrt(weighted.var(nacDS.DEDAD,N)))
cuantdifedadDS<-as.data.frame(weighted.quantile(difedadDS$nacDS.DEDAD,difedadDS$N))
colnames(cuantdifedadDS)<-"Cuantiles"
decilesdifedadDS<-as.data.frame(weighted.quantile(difedadDS$nacDS.DEDAD,difedadDS$N,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
colnames(decilesdifedadDS)<-"Deciles"

estdifedadabsDS<-difedadabsDS%>%summarise(Media=weighted.mean(nacDS.DEDADabs,N),Mediana=weighted.median(nacDS.DEDADabs,N),Varianza=weighted.var(nacDS.DEDADabs,N),"Desviación típica"=sqrt(weighted.var(nacDS.DEDADabs,N)))
cuantdifedadabsDS<-as.data.frame(weighted.quantile(difedadabsDS$nacDS.DEDADabs,difedadabsDS$N))
colnames(cuantdifedadabsDS)<-"Cuantiles"
decilesdifedadabsDS<-as.data.frame(weighted.quantile(difedadabsDS$nacDS.DEDADabs,difedadabsDS$N,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
colnames(decilesdifedadabsDS)<-"Deciles"

estdifedadHMS<-difedadHMS%>%summarise(Media=weighted.mean(nacHMS.DEDADabs,N),Mediana=weighted.median(nacHMS.DEDADabs,N),Varianza=weighted.var(nacHMS.DEDADabs,N),"Desviación típica"=sqrt(weighted.var(nacHMS.DEDADabs,N)))
cuantdifedadHMS<-as.data.frame(weighted.quantile(difedadHMS$nacHMS.DEDADabs,difedadHMS$N))
colnames(cuantdifedadHMS)<-"Cuantiles"
decilesdifedadHMS<-as.data.frame(weighted.quantile(difedadHMS$nacHMS.DEDADabs,difedadHMS$N,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
colnames(decilesdifedadHMS)<-"Deciles"

estdifedadMMS<-difedadMMS%>%summarise(Media=weighted.mean(nacMMS.DEDADabs,N),Mediana=weighted.median(nacMMS.DEDADabs,N),Varianza=weighted.var(nacMMS.DEDADabs,N),"Desviación típica"=sqrt(weighted.var(nacMMS.DEDADabs,N)))
cuantdifedadMMS<-as.data.frame(weighted.quantile(difedadMMS$nacMMS.DEDADabs,difedadMMS$N))
colnames(cuantdifedadMMS)<-"Cuantiles"
decilesdifedadMMS<-as.data.frame(weighted.quantile(difedadMMS$nacMMS.DEDADabs,difedadMMS$N,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
colnames(decilesdifedadMMS)<-"Deciles"

cuantiles<-data.frame(cuantdifedadabsDS,cuantdifedadHMS,cuantdifedadMMS)
colnames(cuantiles)<-c("Cuantiles de la diferencia de edad de parejas de distinto sexo","Cuantiles de la diferencia de edad de hombres del mismo sexo","Cuantiles de la diferencia de edad de mujeres del mismo sexo")

reactable(cuantiles,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#fba3ff")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = TRUE,compact=TRUE,pagination = FALSE)

deciles<-data.frame(decilesdifedadabsDS,decilesdifedadHMS,decilesdifedadMMS)
colnames(deciles)<-c("Deciles de la diferencia de edad de parejas de distinto sexo","Deciles de la diferencia de edad de hombres del mismo sexo","Deciles de la diferencia de edad de mujeres del mismo sexo")

reactable(deciles,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#fba3ff")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = TRUE,compact=TRUE,pagination = FALSE)


### 2.2 - TÉCNICAS ESTADÍSTICAS AVANZADAS

## AJUSTE A DISTRIBUCIONES ESTUDIADAS

#Intentamos ver la distribución de estas variables con la función fit.cont, pero en la mayoría de los casos no se llega a ejecutar y provoca un error en la consola

#Distribución matrimonios de hombres
gHMS<-rep(difedadHMS$nacHMS.DEDADabs+0.0001,difedadHMS$N) #Añadimos ese 0.0001 para poder hacer ajustes con log-normal y F de Senector, que requiere que todas las observaciones sean estrictamente positivas
#Hacemos los ajustes por el método de ajuste de momentos excepto para la log-normal, que se recomienda en la documentación ajustar con el método de máxima verosimilitud
#tests<-fit.cont(f)
descdist(gHMS,boot=1000) #De este gráfico deducimos que las únicas distribuciones que se ajustan bien en cuanto a asimetría y curtosis de las que trabajan en este paquete serían la gamma y la exponencial (recordemos que la beta exige que los valores estén acotados)
ajHMS1<-fitdist(gHMS,"gamma",method="mme")
ajHMS2<-fitdist(gHMS,"exp",method="mme")
ajHMS3<-fitdist(gHMS,"lnorm",method="mle")

gHMS1<-plotdist(gHMS,"gamma",para=list(shape=ajHMS1$estimate[1],rate=ajHMS1$estimate[2]))
gHMS2<-plotdist(gHMS,"exp",para = list(rate=ajHMS2$estimate))
gHMS3<-plotdist(gHMS,"lnorm",para=list(meanlog=ajHMS3$estimate[1],sdlog=ajHMS3$estimate[2]))

gofstat(list(ajHMS1,ajHMS2,ajHMS3),fitnames = c("Gamma","Exponencial","Log-normal"))
#Del test de bondad de ajuste, se deduce que la distribución de datos no se ajusta bien a ninguna de las distribuciones estudiadas
#Lo podemos ver por ejemplo con el criterio de Kolmogorov-Smirnov.
#tests<-fit.cont(f)

gMMS<-rep(difedadMMS$nacMMS.DEDADabs+0.0001,difedadMMS$N)

ajMMS1<-fitdist(gMMS,"gamma",method="mme")
ajMMS2<-fitdist(gMMS,"exp",method="mme")
ajMMS3<-fitdist(gMMS,"lnorm",method="mle")

gMMS1<-plotdist(gMMS,"gamma",para=list(shape=ajMMS1$estimate[1],rate=ajMMS1$estimate[2]))
gMMS2<-plotdist(gMMS,"exp",para = list(rate=ajMMS2$estimate))
gMMS3<-plotdist(gMMS,"lnorm",para=list(meanlog=ajMMS3$estimate[1],sdlog=ajMMS3$estimate[2]))

gofstat(list(ajMMS1,ajMMS2,ajMMS3),fitnames = c("Gamma","Exponencial","Log-normal"))

### 1.3- REPRESENTACIONES GRÁFICAS

## PAQUETE PLOTLY

#En primer lugar vamos a representar las funciones de densidad de las variables estudiadas

densedadHDS<-plot_ly(edadesDS%>%group_by(EDADCA)%>%summarise(sumaH=sum(n)), x= ~EDADCA, y= ~sumaH, type='scatter',mode = 'lines', line=list(shape='splinter',color="darkturquoise"))
densedadHDS<-densedadHDS%>%layout(title = "Edad del hombre en matrimonios de distinto sexo", xaxis = list(title ="Años edad"), yaxis=list(title="Número de hombres"))
densedadHDS

densedadMDS<-plot_ly(edadesDS%>%group_by(EDADCB)%>%summarise(sumaM=sum(n)), x= ~EDADCB, y= ~sumaM, type='scatter',mode = 'lines', line=list(shape='splinter',color="deeppink"))
densedadMDS<-densedadMDS%>%layout(title = "Edad de la mujer en matrimonios de distinto sexo", xaxis = list(title ="Años edad"), yaxis=list(title="Número de mujeres"))
densedadMDS

densedadHMS<-plot_ly(edadesHMS%>%group_by(EDADCA)%>%summarise(sumaH=2*sum(n)), x= ~EDADCA, y= ~sumaH, type='scatter',mode = 'lines', line=list(shape='splinter',color="blue"))
densedadHMS<-densedadHMS%>%layout(title = "Edad del hombre en matrimonios del mismo sexo", xaxis = list(title ="Años edad"), yaxis=list(title="Número de hombres"))
densedadHMS

densedadMMS<-plot_ly(edadesMMS%>%group_by(EDADCA)%>%summarise(sumaM=2*sum(n)), x= ~EDADCA, y= ~sumaM, type='scatter',mode = 'lines', line=list(shape='splinter',color="red"))
densedadMMS<-densedadMMS%>%layout(title = "Edad de la mujer en matrimonios del mismo sexo", xaxis = list(title ="Años edad"), yaxis=list(title="Número de mujeres"))
densedadMMS

#Vamos a normalizar estas tres últimas gráficas y representarlas conjuntamente
pedadHDS<-edadesDS%>%group_by(EDADCA)%>%summarise(sumaH=sum(n))%>%transmute(edad=EDADCA,nHDS=sumaH/sum(edadesDS$n))
pedadMDS<-edadesDS%>%group_by(EDADCB)%>%summarise(sumaM=sum(n))%>%transmute(edad=EDADCB,nMDS=sumaM/sum(edadesDS$n))
pedadHMS<-edadesHMS%>%group_by(EDADCA)%>%summarise(sumaH=2*sum(n))%>%transmute(edad=EDADCA,nHMS=sumaH/sum(edadesHMS$n))
pedadMMS<-edadesMMS%>%group_by(EDADCA)%>%summarise(sumaM=2*sum(n))%>%transmute(edad=EDADCA,nMMS=sumaM/sum(edadesMMS$n))
ptotal<-full_join(full_join(full_join(pedadHDS,pedadMDS),pedadHMS),pedadMMS)

densconjunto<-plot_ly(ptotal, x= ~edad, y= ~nHDS, type='scatter',name='Hombres casados con mujeres',mode = 'lines', line=list(shape='splinter',color="darkturquoise"))
densconjunto<-densconjunto%>%add_trace(y= ~nMDS, type='scatter',name='Mujeres casadas con hombres',mode = 'lines', line=list(shape='splinter',color="deeppink"))
densconjunto<-densconjunto%>%add_trace(y= ~nHMS, type='scatter',name='Hombres casados con hombres',mode = 'lines', line=list(shape='splinter',color="blue"))
densconjunto<-densconjunto%>%add_trace(y= ~nMMS, type='scatter',name='Mujeres casadas con mujeres',mode = 'lines', line=list(shape='splinter',color="red"))
densconjunto<-densconjunto%>%layout(title = "Función de densidad normalizada de edad", xaxis = list(title ="Edad"), yaxis=list(title="Porcentaje"))
densconjunto

#Procedamos a analizar las diferencias de edad
densdifedadDS<-plot_ly(difedadDS, x= ~nacDS.DEDAD, y= ~N, type='scatter',mode = 'lines', line=list(shape='splinter',color="green"))
densdifedadDS<-densdifedadDS%>%layout(title = "Diferencia de edad entre la edad del hombre y la edad de la mujer en parejas de matrimonio", xaxis = list(title ="Años diferencia edad"), yaxis=list(title="Número de casos"))
densdifedadDS

densdifedadabsDS<-plot_ly(difedadabsDS, x= ~nacDS.DEDADabs, y= ~N, type='scatter',mode = 'lines', line=list(shape='splinter',color="orange"))
densdifedadabsDS<-densdifedadabsDS%>%layout(title = "Diferencia absoluta de edad entre la edad del hombre y la edad de la mujer en parejas de matrimonio", xaxis = list(title ="Años diferencia edad"), yaxis=list(title="Número de casos"))
densdifedadabsDS

densdifedadHMS<-plot_ly(difedadHMS, x= ~nacHMS.DEDADabs, y= ~N, type='scatter',mode = 'lines', line=list(shape='splinter',color="blue"))
densdifedadHMS<-densdifedadHMS%>%layout(title = "Diferencia de edad entre la edad de los cónyuges en matrimonios de hombres", xaxis = list(title ="Años diferencia edad"), yaxis=list(title="Número de casos"))
densdifedadHMS

densdifedadMMS<-plot_ly(difedadMMS, x= ~nacMMS.DEDADabs, y= ~N, type='scatter',mode = 'lines', line=list(shape='splinter',color="red"))
densdifedadMMS<-densdifedadMMS%>%layout(title = "Diferencia de edad entre la edad de los cónyuges en matrimonios de hombres", xaxis = list(title ="Años diferencia edad"), yaxis=list(title="Número de casos"))
densdifedadMMS

#Vamos a normalizar estas tres últimas gráficas y representarlas conjuntamente
pdifedadabsDS<-difedadabsDS%>%transmute(dif=nacDS.DEDADabs,nDS=N/sum(difedadabsDS$N))
pdifedadHMS<-difedadHMS%>%transmute(dif=nacHMS.DEDADabs,nHMS=N/sum(difedadHMS$N))
pdifedadMMS<-difedadMMS%>%transmute(dif=nacMMS.DEDADabs,nMMS=N/sum(difedadMMS$N))
ptotal<-full_join(full_join(pdifedadabsDS,pdifedadHMS),pdifedadMMS)

densconjunto<-plot_ly(ptotal, x= ~dif, y= ~nDS, type='scatter',name='Parejas de distinto sexo',mode = 'lines', line=list(shape='splinter',color="orange"))
densconjunto<-densconjunto%>%add_trace(y= ~nHMS, type='scatter',name='Parejas de hombres',mode = 'lines', line=list(shape='splinter',color="blue"))
densconjunto<-densconjunto%>%add_trace(y= ~nMMS, type='scatter',name='Parejas de mujeres',mode = 'lines', line=list(shape='splinter',color="red"))
densconjunto<-densconjunto%>%layout(title = "Función de densidad normalizada de la diferencia de edad", xaxis = list(title ="Años diferencia de edad"), yaxis=list(title="Porcentaje de parejas respecto al total"))
densconjunto

#Una vez hecho esto vamos a recurrir a otro tipo de gráfico muy interesante que es el mapa de calor para ver la frecuencia con las que se dan las parejas
#Vamos a hacer esta representación para edades y para grupos de edades

cat=list("[0,19)","[19,24)","[24,29)","[29,34)","[34,39)","[39,44)","[44,49)","[49,54)","[54,59)","[60,100]")
num=list(0,1,2,3,4,5,6,7,8,9)

hmedadDS<-plot_ly(z=edcontDS,type = "heatmap")
hmedadDS<-hmedadDS%>%layout(title = "Enlaces por edad de matrimonios de distinto sexo", xaxis = list(title ="Edad del hombre"), yaxis=list(title="Edad de la mujer"))%>%
hmedadDS

hmgredadDS<-plot_ly(z=gredcontDS,type = "heatmap")
hmgredadDS<-hmgredadDS%>%layout(title = "Enlaces por grupos de edad de matrimonios de distinto sexo", xaxis = list(title ="Grupo de edad del hombre",ticktext=cat,tickvals=num,tickmode="array"),yaxis=list(title="Grupo de edad de la mujer",ticktext=cat,tickvals=num,tickmode="array"))
hmgredadDS

hmedadHMS<-plot_ly(z=edcontHMS,type = "heatmap")
hmedadHMS<-hmedadHMS%>%layout(title = "Enlaces por edad de matrimonios de hombres del mismo sexo", xaxis = list(title ="Edad del cónyuge 1"), yaxis=list(title="Edad del cónyuge 2"))
hmedadHMS

hmgredadHMS<-plot_ly(z=gredcontHMS, type = "heatmap")
hmgredadHMS<-hmgredadHMS%>%layout(title = "Enlaces por grupos de edad de matrimonios de hombres", xaxis = list(title ="Grupo de edad del cónyuge 1",ticktext=cat,tickvals=num,tickmode="array"),yaxis=list(title="Grupo de edad del cónyuge 2",ticktext=cat,tickvals=num,tickmode="array"))
hmgredadHMS

hmedadMMS<-plot_ly(z=edcontMMS,type = "heatmap")
hmedadMMS<-hmedadMMS%>%layout(title = "Enlaces por edad de matrimonios de mujeres del mismo sexo", xaxis = list(title ="Edad del cónyuge 1"), yaxis=list(title="Edad del cónyuge 2"))
hmedadMMS

hmgredadMMS<-plot_ly(z=gredcontMMS,type = "heatmap")
hmgredadMMS<-hmgredadMMS%>%layout(title = "Enlaces por grupos de edad de matrimonios de mujeres", xaxis = list(title ="Grupo de edad del cónyuge 1",ticktext=cat,tickvals=num,tickmode="array"),yaxis=list(title="Grupo de edad del cónyuge 2",ticktext=cat,tickvals=num,tickmode="array"))
hmgredadMMS

#### 3- ANÁLISIS Y REPRESENTACIÓN GRÁFICA DE LOS DATOS DE NACIONALIDAD

### 3.1- TABULACIÓN Y ANÁLISIS ESTADÍSTICO BÁSICO DE LOS DATOS

#Separamos ahora las parejas en las que solo uno de los cónyuges tiene la nacionalidad española y en las que ninguno es de nacionalidad española
espextDS<-paisDS%>%filter(PAISA=="ESPAÑA" & PAISB!="ESPAÑA")
extespDS<-paisDS%>%filter(PAISA!="ESPAÑA" & PAISB=="ESPAÑA")
extextDS<-paisDS%>%filter(PAISA!="ESPAÑA" & PAISB!="ESPAÑA")

espextHMS<-paisHMS%>%filter((PAISA=="ESPAÑA" & PAISB!="ESPAÑA")|(PAISA!="ESPAÑA" & PAISB=="ESPAÑA"))
extextHMS<-paisHMS%>%filter(PAISA!="ESPAÑA" & PAISB!="ESPAÑA")

espextMMS<-paisMMS%>%filter((PAISA=="ESPAÑA" & PAISB!="ESPAÑA")|(PAISA!="ESPAÑA" & PAISB=="ESPAÑA"))
extextMMS<-paisMMS%>%filter(PAISA!="ESPAÑA" & PAISB!="ESPAÑA")

nacionalidadmatr<-data.frame(c(filter(paisDS,PAISA=="ESPAÑA" & PAISB=="ESPAÑA")$n,filter(paisHMS,PAISA=="ESPAÑA" & PAISB=="ESPAÑA")$n,filter(paisMMS,PAISA=="ESPAÑA" & PAISB=="ESPAÑA")$n),c(sum(espextDS$n)+sum(extespDS$n),sum(espextHMS$n),sum(espextMMS$n)),c(sum(extextDS$n),sum(extextHMS$n),sum(extextMMS$n)))
colnames(nacionalidadmatr)<-c("Ambos cónyuges son de nacionalidad española", "Solo un conyuge tiene la nacionalidad española","Ninguno de los conyuges tiene la nacionalidad española")
rownames(nacionalidadmatr)<-c("Parejas de distinto sexo","Parejas de hombres","Parejas de mujeres")

pornacionalidad<-transpose(nacionalidadmatr)%>%transmute(DS=V1/sum(paisDS$n),HMS=V2/sum(paisHMS$n),MMS=V3/sum(paisMMS$n))
pornacionalidad<-transpose(pornacionalidad)
colnames(pornacionalidad)<-c("Ambos cónyuges son de nacionalidad española", "Solo un conyuge tiene la nacionalidad española","Ninguno de los conyuges tiene la nacionalidad española")
rownames(pornacionalidad)<-c("Parejas de distinto sexo","Parejas de hombres","Parejas de mujeres")


#Vamos a contar ahora el número de personas de distinta nacionalidad a la española que se han casado en nuestro país

#Casados con españoles
xDS1<-espextDS[-1]
colnames(xDS1)<-c("PAIS","DS")
xDS2<-extespDS[-2]
colnames(xDS2)<-c("PAIS","DS")
cespDS<-rbind(xDS1,xDS2)%>%group_by(PAIS)%>%summarise(DS=sum(DS))

xHMS1<-filter(espextHMS,PAISB!="ESPAÑA")[-1]
colnames(xHMS1)<-c("PAIS","HMS")
xHMS2<-filter(espextHMS,PAISA!="ESPAÑA")[-2]
colnames(xHMS2)<-c("PAIS","HMS")
cespHMS<-rbind(xHMS1,xHMS2)%>%group_by(PAIS)%>%summarise(HMS=sum(HMS))

xMMS1<-filter(espextMMS,PAISB!="ESPAÑA")[-1]
colnames(xMMS1)<-c("PAIS","MMS")
xMMS2<-filter(espextMMS,PAISA!="ESPAÑA")[-2]
colnames(xMMS2)<-c("PAIS","MMS")
cespMMS<-rbind(xMMS1,xMMS2)%>%group_by(PAIS)%>%summarise(MMS=sum(MMS))

cesp<-full_join(full_join(cespDS,cespHMS),cespMMS)
cesp[is.na(cesp)]<-0


#Casados con personas con otra nacionalidad
xDS3<-extextDS[-1]
colnames(xDS3)<-c("PAIS","n")
xDS4<-extextDS[-2]
colnames(xDS4)<-c("PAIS","n")
cextDS<-rbind(xDS3,xDS4)%>%group_by(PAIS)%>%summarise(DS=sum(n))

xHMS3<-extextHMS[-1]
colnames(xHMS3)<-c("PAIS","n")
xHMS4<-extextHMS[-2]
colnames(xHMS4)<-c("PAIS","n")
cextHMS<-rbind(xHMS3,xHMS4)%>%group_by(PAIS)%>%summarise(HMS=sum(n))

xMMS3<-extextMMS[-1]
colnames(xMMS3)<-c("PAIS","n")
xMMS4<-extextMMS[-2]
colnames(xMMS4)<-c("PAIS","n")
cextMMS<-rbind(xMMS3,xMMS4)%>%group_by(PAIS)%>%summarise(MMS=sum(n))

cext<-full_join(full_join(cextDS,cextHMS),cextMMS)
cext[is.na(cext)]<-0


ctotal<-full_join(cesp,cext,by="PAIS")
ctotal[is.na(ctotal)]<-0
ctotal<-ctotal%>%mutate(DS.z=DS.x+DS.y,HMS.z=HMS.x+HMS.y,MMS.z=MMS.x+MMS.y)
colnames(ctotal)<-c("Nacionalidad","Personas casadas con una persona española de distinto sexo","Hombres casados con un hombre español","Mujeres casadas con una mujer española", "Personas casadas con otra persona extranjera de distinto sexo","Hombres casados con otro extranjero","Mujeres casadas con otra extranjera","Total distinto sexo","Total hombres mismo sexo","Total mujeres mismo sexo")

reactable(ctotal,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#D291BC")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)


cesp<-cesp%>%mutate(MS=HMS+MMS)
cext<-cext%>%mutate(MS=HMS+MMS)


### 3.2 - TÉCNICAS ESTADÍSTICAS AVANZADAS

#CONTRASTES DE LA PROPORCIÓN

#Contraste de la proporción de las nacionalidades de las parejas de distinto sexo y de hombres del mismo sexo
prop.test(matrix(c(transpose(nacionalidadmatr)$V1,transpose(nacionalidadmatr)$V2),nrow=3,ncol=2),alternative = "two.sided")
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.

#Contraste de la proporción de las nacionalidades de las parejas de distinto sexo y de mujeres del mismo sexo
prop.test(matrix(c(transpose(nacionalidadmatr)$V1,transpose(nacionalidadmatr)$V3),nrow=3,ncol=2),alternative = "two.sided")
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.

#Contraste de la proporción de las nacionalidades de las parejas de distinto sexo y de mujeres del mismo sexo
prop.test(matrix(c(transpose(nacionalidadmatr)$V2,transpose(nacionalidadmatr)$V3),nrow=3,ncol=2),alternative = "two.sided")
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.

#Al computar cextHMS, se ve claramente que los países de donde son las parejas de hombres extranjeros que se casan en España difieren de los países de las parejas de distinto sexo.
#Esto podría deberse a que España fue uno de los países de Europa que antes legalizó los matrimonios homosexuales.
#Utilicemos el contraste de proporción para determinar si de verdad España ha sido polo de atracción de matrimonios homosexuales de Italia, Reino Unido, Brasil, Alemania y Francia
#Para eso compararemos esas proporciones con las de cextDS, utilizando prop.test (podríamos utilizar chi cuadrado que es equivalente, pero prop.test nos da proporciones además)

#Italia 
It<-as.matrix(rbind(filter(cext,PAIS=="ITALIA")[,c(2,3)],c(sum(cextDS$DS),sum(cextHMS$HMS))))
prop.test(It)
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.

#Reino Unido
Re<-as.matrix(rbind(filter(cext,PAIS=="REINO UNIDO")[,c(2,3)],c(sum(cextDS$DS),sum(cextHMS$HMS))))
prop.test(Re)
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.

#Brasil
Br<-as.matrix(rbind(filter(cext,PAIS=="BRASIL")[,c(2,3)],c(sum(cextDS$DS),sum(cextHMS$HMS))))
prop.test(Br)
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.

#Alemania
Al<-as.matrix(rbind(filter(cext,PAIS=="ALEMANIA")[,c(2,3)],c(sum(cextDS$DS),sum(cextHMS$HMS))))
prop.test(Al)
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.

#Francia
Fr<-as.matrix(rbind(filter(cext,PAIS=="FRANCIA")[,c(2,3)],c(sum(cextDS$DS),sum(cextHMS$HMS))))
prop.test(Fr)
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.

#Repitamos el mismo proceso para las mujeres

#Italia 
It<-as.matrix(rbind(filter(cext,PAIS=="ITALIA")[,c(2,4)],c(sum(cextDS$DS),sum(cextMMS$MMS))))
prop.test(It)
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.

#Reino Unido
Re<-as.matrix(rbind(filter(cext,PAIS=="REINO UNIDO")[,c(2,4)],c(sum(cextDS$DS),sum(cextMMS$MMS))))
prop.test(Re)
#Puesto que 0.025<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.

#Brasil
Br<-as.matrix(rbind(filter(cext,PAIS=="BRASIL")[,c(2,4)],c(sum(cextDS$DS),sum(cextMMS$MMS))))
prop.test(Br)
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.

#Alemania
Al<-as.matrix(rbind(filter(cext,PAIS=="ALEMANIA")[,c(2,4)],c(sum(cextDS$DS),sum(cextMMS$MMS))))
prop.test(Al)
#Puesto que 0.2209>0.05, aceptamos la hipótesis nula de donde deducimos que la proporción entre matrimomios entre alemanes de distinto sexo y matrimonios de alemanas son las mismas

#Francia
Fr<-as.matrix(rbind(filter(cext,PAIS=="FRANCIA")[,c(2,4)],c(sum(cextDS$DS),sum(cextMMS$MMS))))
prop.test(Fr)
#Puesto que 9.004e-06<0.05, rechazamos la hipótesis nula por lo que las proporciones no son equivalentes.



### 3.3- REPRESENTACIONES GRÁFICAS

#Escogemos los países con el mayor número de ciudadanos casados con españoles del mismo sexo:

hcesp<-head(arrange(cesp,desc(MS)),20)
hcesp$PAIS <- factor(hcesp$PAIS, levels = hcesp[["PAIS"]])
gresp <- plot_ly(hcesp, x = ~PAIS, y = ~MS, type = 'bar', name = 'Total parejas mismo sexo', marker = list(color = 'purple'))
gresp <- gresp %>% add_trace(y = ~HMS, name = 'Hombres', marker = list(color = 'blue'))
gresp <- gresp %>% add_trace(y = ~MMS, name = 'Mujeres', marker = list(color = 'red'))
gresp <- gresp %>% layout(title="Nacionalidades con mayor número de ciudadanos casados con españoles de su mismo sexo",xaxis = list(title = "Nacionalidad", tickangle = -45),
                                    yaxis = list(title = "Número de matrimonios"),
                                    margin = list(b = 100),
                                    barmode = 'group')
gresp

#Hagamos lo mismo para los países con el mayor número de ciudadanos casados con otra persona de su mismo sexo en España:
hcext<-head(arrange(cext,desc(MS)),20)
hcext$PAIS <- factor(hcext$PAIS, levels = hcext[["PAIS"]])
grext <- plot_ly(hcext, x = ~PAIS, y = ~MS, type = 'bar', name = 'Total parejas mismo sexo', marker = list(color = 'purple'))
grext <- grext %>% add_trace(y = ~HMS, name = 'Hombres', marker = list(color = 'blue'))
grext <- grext %>% add_trace(y = ~MMS, name = 'Mujeres', marker = list(color = 'red'))
grext <- grext %>% layout(title="Nacionalidades con mayor número de ciudadanos casados en España con extranjeros de su mismo sexo",xaxis = list(title = "Nacionalidad", tickangle = -45),
                          yaxis = list(title = "Número de matrimonios"),
                          margin = list(b = 100),
                          barmode = 'group')
grext

#### 4- ANÁLISIS Y REPRESENTACIÓN GRÁFICA DE LOS DATOS DE DIVORCIOS

### 4.1- TABULACIÓN Y ANÁLISIS ESTADÍSTICO BÁSICO DE LOS DATOS

#Tabulamos los resultados obtenidos anteriormente

tdivtipo<-divtipo
colnames(tdivtipo)<-c("Tipo de divorcio","Año","Distinto sexo","Total mismo sexo","Hombres mismo sexo","Mujeres mismo sexo")
reactable(tdivtipo,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#E0D595")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

tdivduracionDS<-divduracionDS
colnames(tdivduracionDS)<-c("Duración del matrimonio","Año","Número de divorcios")
reactable(tdivduracionDS,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#E0D595")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

tdivduracionMS<-divduracionMS
colnames(tdivduracionMS)<-c("Duracion del matrimonio","Periodo","Total mismo sexo","Hombres mismo sexo","Mujeres mismo sexo")
reactable(tdivduracionMS,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#E0D595")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

#Juntamos las variables anteriores por tipo para obtener los resultados totales en el periodo 2013-2018

divtipototal<-divtipo%>%group_by(Tipo.de.divorcio)%>%summarise(DS=sum(Total.x),MS=sum(Total.y),HMS=sum(Total.x.x),MMS=sum(Total.y.y))
tdivtipototal<-divtipototal
colnames(tdivtipototal)<-c("Tipo de divorcio","Distinto sexo","Total mismo sexo","Hombres mismo sexo","Mujeres mismo sexo")
reactable(tdivtipototal,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#E0D595")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

divdurDStotal<-divduracionDS%>%group_by(Duración.del.matrimonio)%>%summarise(DS=sum(Total))
tdivdurDStotal<-divdurDStotal
colnames(tdivdurDStotal)<-c("Duracion del matrimonio","Total distinto sexo")
reactable(tdivdurDStotal,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#E0D595")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)

divdurMStotal<-divduracionMS%>%group_by(Duración.del.matrimonio)%>%summarise(MS=sum(Total.x),HMS=sum(Total.y),MMS=sum(Total))
tdivdurMStotal<-divdurMStotal
colnames(tdivdurMStotal)<-c("Duracion del matrimonio","Total mismo sexo","Hombres mismo sexo","Mujeres mismo sexo")
reactable(tdivdurMStotal,defaultColDef = colDef(
  align = "center",
  cell = function(value) format(value, nbig = 3),
  minWidth = 70,
  headerStyle = list(background = "#E0D595")),
  filterable = TRUE,searchable=TRUE,highlight = TRUE,rownames = FALSE,compact=TRUE,pagination = FALSE)


### 4.2- TÉCNICAS ESTADÍSTICAS AVANZADAS

## CONTRASTE DE PROPORCIÓN

#Contraste de la proporción por tipo de los divorcios entre personas de distinto sexo y los divorcios entre personas del mismo sexo.
chisq.test(cbind(divtipototal[-3,]$DS,divtipototal[-3,]$MS))
#Puesto que 2.2e-16<0.05, rechazamos la hipótesis nula por lo que deducimos que las proporciones entre tipos de divorcio no son las mismas (las parejas del mismo sexo recurren más al divorcio de mutuo acuerdo)

#Contraste de la proporción por tipo de los divorcios entre parejas de hombres y de mujeres del mismo sexo.
chisq.test(cbind(divtipototal[-3,]$HMS,divtipototal[-3,]$MMS))
#Puesto que 0.63<0.05, aceptamos la hipótesis nula por lo que los matrimonios entre personas del mismo sexo recurren en la misma proporción a cada tipo de divorcio.


### 4.3- REPRESENTACIONES GRÁFICAS

grdivtipo <- plot_ly(divtipototal[-3,], x = ~Tipo.de.divorcio, y = ~MS, type = 'bar', name = 'Total parejas mismo sexo', marker = list(color = 'purple'))
grdivtipo <- grdivtipo %>% add_trace(y = ~HMS, name = 'Hombres', marker = list(color = 'blue'))
grdivtipo <- grdivtipo %>% add_trace(y = ~MMS, name = 'Mujeres', marker = list(color = 'red'))
grdivtipo <- grdivtipo %>% layout(title="Divorcios de parejas del mismo sexo por tipo de divorcio",xaxis = list(title = "", tickangle = -45),
                      yaxis = list(title = "Número de divorcios"),
                      margin = list(b = 100),
                      barmode = 'group')
grdivtipo


divdurMStotal$Duración.del.matrimonio<-factor(divdurMStotal$Duración.del.matrimonio,levels=c("Total","Menos de 2 años","De 2 a 4 años","5 y más años"))
grdivdurMS <- plot_ly(divdurMStotal[-4,], x = ~Duración.del.matrimonio, y = ~MS, type = 'bar', name = 'Total parejas mismo sexo', marker = list(color = 'purple'))
grdivdurMS <- grdivdurMS %>% add_trace(y = ~HMS, name = 'Hombres', marker = list(color = 'blue'))
grdivdurMS <- grdivdurMS %>% add_trace(y = ~MMS, name = 'Mujeres', marker = list(color = 'red'))
grdivdurMS <- grdivdurMS %>% layout(title="Divorcios de parejas del mismo sexo por duración del matrimonio",xaxis = list(title = "", tickangle = -45),
                                  yaxis = list(title = "Número de divorcios"),
                                  margin = list(b = 100),
                                  barmode = 'group')
grdivdurMS


divdurDStotal$Duración.del.matrimonio<-factor(divdurDStotal$Duración.del.matrimonio,levels=c("Total","Menos de 1 año","De 1 año","De 2 a 4 años","De 5 a 9 años","De 10 a 14 años","De 15 a 19 años","20 y más años"))
divdurDStotal<-divdurDStotal[-8,]
grdivdurDS <- plot_ly(divdurDStotal, x = ~Duración.del.matrimonio, y = ~DS, type = 'bar', name = 'Total parejas mismo sexo', marker = list(color = 'orange'))
grdivdurDS <- grdivdurDS %>% layout(title="Divorcios de parejas de distinto sexo por duración del matrimonio",xaxis = list(title = "", tickangle = -45),
                                    yaxis = list(title = "Número de divorcios"),
                                    margin = list(b = 100),
                                    barmode = 'group')
grdivdurDS
