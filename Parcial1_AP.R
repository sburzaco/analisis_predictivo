library(readxl)
library(dplyr)
library(tibble)
library(hms)
library(stringr)
library(tidyverse)
library(plyr)
library("mlbench")
library("caret")
library(rpart)
library(rpart.plot)
library(rfm)
library(ggplot2)
#IMPORTAR Y PREPARA VARIABLES
#importar datos --> formato tbl_df
original=read_excel("/Users/sofiaburzaco/Downloads/clase_eda/online_retail_II.xlsx")
df=read_excel("/Users/sofiaburzaco/Downloads/clase_eda/online_retail_II.xlsx")

#separar date y time
df <- tidyr::separate(df, `InvoiceDate`, c("Date", "Time"), sep = " ")

#cambiar tipo de variable de date
df$Date <- lubridate::ymd(df$Date)

#cambiar tipo de variable de time
df$Time <- as_hms(df$Time)

#crear variable booleana con cancelacion
df$Cancellation<-factor(ifelse(substr(df$Invoice,1,1)=="C","Yes","No"))

#crear variable weekday
df$Weekday <- lubridate::wday(df$Date, label=TRUE)


#ANALIZAR NAs,MISSINGS 
summary(df)
#cosas llamativas
#customer ids con NA --> los dejo o saco dependiendo del analisis, si quiero calcular algo por producto lo dejo por ej
map_dbl(df, function(x) mean(is.na(x)) * 100) %>% 
  sort(decreasing=T) %>% 
  as.data.frame()

#paso customer id de numeric a cateorica
df$`Customer ID`=as.factor(df$`Customer ID`)

#price y quantity tienen negativos:
#quantity negativo --> todos los cancelados tienen quantity negativo, esto se debe a que se devuelven(?) con estos ver q hago
#tambien hay algunos registros que tienen quntity negativo pero no son una cancelacion esos, ademas todos estos tienen customer id na son registros poco claros los borraria 
df %>% filter(Quantity>1, Cancellation==1, !is.na(`Customer ID`))
df %>% filter(Quantity<1, Cancellation==0, Price>0)
df %>% filter(Cancellation==1, Quantity> -15)
df %>% filter(Price==0,Quantity>0, Cancellation==1)
df %>% filter(is.na(`Customer ID`))
df %>% filter(is.na(`Customer ID`), Country !="United Kingdom")
df  %>% summarise(min=min(Date), max=max(Date))

#probando filters me di cuenta que tambien hay nas en description, pero que en summary no aparecen y en la tabla de nas tampoco

#price negativo --> son 3 transacciones tituladas Adjust bad debt que no tienen customer id, como no son una venta las borraria
df %>% filter(Price<0)

#eliminacion de las entradas inconsistentes o que no aportan al analisis
df = df[df$Price > 0,] #elimina los casos price negativo y price igual a 0 (este ultimo incluye los de quantity negativa que no son cancelacion)

#paso quantity a valores positivos
df$Quantity<-ifelse(df$Quantity<0,df$Quantity*(-1),df$Quantity)

#crear variable amount
df$Amount <- (df$Quantity * df$Price)

#verificar consistencia interna de la base, que no haya una transaccion que tenga ciertos productos cancelados y otros no
res=ddply(df, .(Invoice), transform, status = Cancellation == Cancellation)
res %>% filter(status=FALSE)

#dentro de la transaccion se encuentra como item el postage (cargo de envio, cuando es fuera de UK es muy alto, esto genera valores atipicos)
df %>% filter(StockCode=="POST", Country == "United Kingdom")

#CANCELACION
#creo una variable que represente el cargo de envio
dc <- df %>% mutate(Postage= case_when(StockCode=="POST" ~ Price*Quantity,
                                       StockCode=="DOT"~ Price*Quantity,
                                       TRUE ~ 0))

#indico el la cantidad de cargos y el precio de envio como 0, ya que los voy a sumarizar a parte
dc$Quantity<-ifelse(dc$StockCode=="POST",0,dc$Quantity)
dc$Quantity<-ifelse(dc$StockCode=="DOT",0,dc$Quantity)
dc$Price<-ifelse(dc$StockCode=="POST",0,dc$Price)
dc$Price<-ifelse(dc$StockCode=="DOT",0,dc$Price)

dc <- subset(dc, select = c("Invoice", "Quantity", "Date", "Time", "Price", "Customer ID", "Country", "Cancellation", "Weekday", "Amount", "Postage"))
dc <- dc %>% dplyr::group_by (Invoice, Date, Time, `Customer ID`,Country, Cancellation, Weekday,) %>% dplyr::summarise(Quantity=sum(Quantity), Price=mean(Price), Amount=sum(Amount),Postage=sum(Postage))
dc = dc[dc$Price > 0,]
summary(dc)

#EDA
#outliers --> todas las distribuciones presentan grandes desigualdades, ver si se separa en B2c y b2b si se reduce
boxplot(dc$Price,main = "Boxplot Price")
boxplot(dc$Quantity,main = "Boxplot Quantity")
boxplot(dc$Postage,main = "Boxplot Postage")
boxplot(dc$Amount,main = "Boxplot Amount")

#separar wholesale de retail
retail <-dc %>% filter(Quantity<=12)
summary(retail)
wholes <-dc %>% filter(Quantity>12)

boxplot(retail$Price,main = "Boxplot Retail Price")
boxplot(retail$Quantity,main = "Boxplot Retail Quantity") #este no presenta outliers (se limito con el filtro)
boxplot(retail$Postage,main = "Boxplot Retail Postage")
boxplot(retail$Amount,main = "Boxplot Retail Amount")
boxplot(wholes$Price,main = "Boxplot Wholesale Price")
boxplot(wholes$Quantity,main = "Boxplot Wholesale Quantity")
boxplot(wholes$Postage,main = "Boxplot Wholesale Postage")
boxplot(wholes$Amount,main = "Boxplot Wholesale Amount")
#no cambia si son ventas minoristas o mayoristas de todas formas hay mucha disparidad en los datos

#se puede observar que la clasificacion de venta si influye en en la distribucion de la relacion entre las variables
ggplot(dc, aes(x=Amount, y=Price, color=Cancellation)) +geom_point(size=2, alpha=0.2)+ggtitle("Amount vs Price")#hay una relacion
ggplot(retail, aes(x=Amount, y=Price, color=Cancellation)) +geom_point(size=2, alpha=0.2)+ ggtitle("Amount vs Price Retail") #relacion lineal
ggplot(wholes, aes(x=Amount, y=Price, color=Cancellation)) +geom_point(size=2, alpha=0.2)+ ggtitle("Amount vs Price Wholesale") #hay que agregarle logaritmo
ggplot(wholes, aes(x=Amount, y=Price, color=Cancellation)) +geom_point(size=2, alpha=0.2)+scale_y_log10()+scale_x_log10() + ggtitle("Amountvs Price Wholesale")

ggplot(dc, aes(x=Amount, y=Quantity, color=Cancellation)) +geom_point(size=2, alpha=0.2)+scale_y_log10()+scale_x_log10()+ggtitle("Amount vs Quantity (log)") #se detecta tendencia
ggplot(retail, aes(x=Amount, y=Quantity, color=Cancellation)) +geom_point(size=2, alpha=0.2) +scale_y_log10()+scale_x_log10()+ggtitle("Amount vs Quantity Retail (log)") #tendencia 
ggplot(wholes, aes(x=Amount, y=Quantity, color=Cancellation)) +geom_point(size=2, alpha=0.2)+scale_y_log10()+scale_x_log10()+ggtitle("Amount vs Quantity Wholesale (log)") #tendencia

ggplot(dc, aes(x=Price, y=Quantity, color=Cancellation)) +geom_point(size=2, alpha=0.2)+scale_y_log10()+scale_x_log10()+ggtitle("Price vs Quantity (log)") 
ggplot(retail, aes(x=Price, y=Quantity, color=Cancellation)) +geom_point(size=2, alpha=0.2) +scale_y_log10()+scale_x_log10()+ggtitle("Price vs Quantity Retail (log)") #tendencia 
ggplot(wholes, aes(x=Price, y=Quantity, color=Cancellation)) +geom_point(size=2, alpha=0.2)+scale_y_log10()+scale_x_log10()+ggtitle("Price vs Quantity Wholesale (log)")

#antes de hacer correlacion normalizar y balancear variables

#densidad por variable
ggplot(dc_num) +geom_density(aes(x=Price), fill="red", alpha=0.2) +scale_y_log10()+scale_x_log10()+theme_minimal()
ggplot(dc_num) +geom_density(aes(x=Quantity), fill="red", alpha=0.2) +scale_y_log10()+scale_x_log10() +theme_minimal()
ggplot(dc_num) +geom_density(aes(x=Amount), fill="red", alpha=0.2)+scale_y_log10()+scale_x_log10() +theme_minimal() 
ggplot(dc_num) +geom_density(aes(x=Postage), fill="red", alpha=0.2) +scale_y_log10()+scale_x_log10() +theme_minimal()

#amout gastada segun el dia de la semana
ggplot(data = retail, mapping = aes(x = Amount)) + geom_freqpoly(mapping = aes(colour = Weekday), binwidth = 500, alpha=0.8)+theme_minimal()
#correlacion
#separa variables numericas y categoricas
dc_num = dc %>% select_if(is.numeric)
dc_cat = dc %>% select_if(function(x) !is.numeric(x))

#correlacion con pearson
GGally::ggcorr(
  dc_num, method=c("pairwise","pearson"), 
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)

#correlacion con spearman --> este tiene mas sentido
GGally::ggcorr(
  dc_num, method=c("pairwise","spearman"),  
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)

#pruebo correlacion separando por retail y wholesale
dc_numr=dc_num %>% filter(Quantity<=12)
dc_numw=dc_num %>% filter(Quantity>12)

GGally::ggcorr(
  dc_numr, method=c("pairwise","spearman"),  
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)+ ggtitle("Correlacion Retail")

GGally::ggcorr(
  dc_numw, method=c("pairwise","spearman"),  
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)+ ggtitle("Correlacion Wholesale")


#cantidad de cancelaciones vs no
ggplot(data = dc) +
  geom_bar(mapping = aes(x = Cancellation, fill=Cancellation))

summary(dc$Cancellation)

#cantidad de cancelados y no a lo largo del tiempo
dc %>% ggplot(mapping = aes(Date)) + 
  geom_freqpoly(mapping = aes(colour = Cancellation), binwidth = 1/4)

#cantidad de transacciones por dia de la semana
table(dc_cat$Weekday)

#proporcion de cancelados y no por dia de la semana
prop.table(table(dc$Weekday, dc$Cancellation))

#proporcion de cancelados y no por pais
prop.table(table(dc$Country, dc$Cancellation))

#Cantidad de transacciones por pais
trans_pais=table(dc$Country)
is.matrix(trans_pais)
trans_pais=as.data.frame(trans_pais)
#lista ordenada
trans_pais %>% arrange(trans_pais$Freq)

#PARTICION EN TRAIN Y TEST
entreno=dc[dc$Date<="2010-09-10",]
testeo=dc[dc$Date>"2010-09-10",]

#comparar el porcentaje asigando a cada conjunto,
summary(dc$Cancellation)
summary(entreno$Cancellation)
summary(testeo$Cancellation)

#Arbol de Decision
#crear arbol de decision
arbol=rpart(Cancellation~ Time +Weekday + Quantity + Price + Amount ,entreno, method="class")
summary(arbol)
#dibujar el Árbol de Decisión
rpart.plot(arbol, extra=1, type=5) #al dibujar el arbol de decision se nota que la variable quantity es la que mas influye, probar un analisis separando venta mayorista de minorista
#probar el arbol de decision
set.seed(21);particion=createDataPartition(y=testeo$Cancellation, p=0.5, list=FALSE)
test=testeo[particion,]
validation=testeo[-particion,]
#probar arbol de decision
pred=predict(arbol,test, type="class") 
confusionMatrix(pred, test$Cancellation)

#PRUEBA MISMO ANALISIS SEPARANDO MAYORISTA Y MINORISTA
p1 <- dc%>% dplyr::filter(Quantity<12)
p2 <-dc %>% dplyr::filter(Quantity>12)
#12 como cantidad que separa ya que p ej 12 platos es un juego

#mayorista
#separar train y test
entreno2=p2[p2$Date<="2010-09-10",]
testeo2=p2[p2$Date>"2010-09-10",]
#comparar el porcentaje asigando a cada conjunto,
summary(p2$Cancellation)
summary(entreno2$Cancellation)
summary(testeo2$Cancellation)
#Arbol de Decision
#crear arbol de decision
arbol2=rpart(Cancellation~ Time +Weekday + Quantity + Price + Amount ,entreno2, method="class")
summary(arbol2)
#dibujar el Árbol de Decisión
rpart.plot(arbol2, extra=1, type=5) #al dibujar el arbol de decision se nota que se utilizan mas variables especficas para los mayoristas
#separar testeo en testeo y validacion

pred2=predict(arbol2,testeo2, type="class") 
confusionMatrix(pred2, testeo2$Cancellation)

#minorista
#separar train y test
entreno1=p1[p1$Date<="2010-09-10",]
testeo1=p1[p1$Date>"2010-09-10",]
#comparar el porcentaje asigando a cada conjunto,
summary(p1$Cancellation)
summary(entreno1$Cancellation)
summary(testeo1$Cancellation) # la particion para yes queda 50-50 no queda muy bien repartida por el tiempo
#Arbol de Decision
#crear arbol de decision
arbol1=rpart(Cancellation~ Time +Weekday + Quantity + Price + Amount ,entreno1, method="class")
summary(arbol1)
#dibujar el Árbol de Decisión
rpart.plot(arbol1, extra=1, type=5) #al dibujar el arbol de decision se nota que se utilizan mas variables especficas para los mayoristas
pred1=predict(arbol1,testeo1, type="class") 
confusionMatrix(pred1, testeo1$Cancellation) #el modelo es peor para predecir, tiene una sensibilidad muy baja, probablemente se deba a la reparticion de los casos cancelados para entrenamiento y testeo
#se podria probar alargando el periodo de entrenamiento para esta seccion

#EXPORTAR DATA SET CREADO DE CANCELACION A UN CSV
write.csv(dc,'Cancelacion.csv')





------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #RFM
  dr <- subset(df, select = c("Invoice","StockCode", "Date", "Customer ID","Amount")) #separo variables necesarias
#eliminamos las entrasdas de cargo de envio
dr = dr[dr$StockCode !="DOT",]
dr = dr[dr$StockCode !="POST",]
#eliminamos la variable Stock code 
dr <- subset(dr, select=-StockCode)
#eliminamos los Customer IDs con NA
dr = dr[!is.na(dr$`Customer ID`),]
#agrupamos por transaccion
dr <- dr %>% dplyr::group_by (Invoice, Date, `Customer ID`) %>% dplyr::summarise(Amount=sum(Amount)) 
#tomamos los ultimos 90 dias
dr <- dr %>% dplyr::filter(Date>"2010-09-10")

#creamos modelo
rfm <-rfm_table_order(data=dr, customer_id =`Customer ID`, revenue=Amount, order_date = Date, analysis_date = as.Date("2010/12/10"))

#barchart recency-frequency-monetary
dev.off()
rfm_bar_chart(rfm) 

#hetamap
rfm_heatmap(rfm) 

#scatterplots
rfm_rm_plot(rfm) #recency vs monetary
rfm_fm_plot(rfm)+scale_y_log10()+scale_x_log10()+theme_light() 
rfm_rf_plot(rfm)+scale_y_log10()+scale_x_log10()+theme_light()

        
