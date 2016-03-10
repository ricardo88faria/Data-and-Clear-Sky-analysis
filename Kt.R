#!/usr/bin/env Rscript

#packages:
library(plyr)
library(oce)
library(mondate)
library(date)
library(sirad)
library(solaR)
library(ggplot2)
library(xlsx)

#limpeza ambiente e objetos:
rm(list=ls())
cat("\014")

#####################################
print("Programado por Ricardo Faria")
#####################################

system("mkdir output")
source("config.txt")

#rececao do ficheiro(dados):
file_data <- Sys.glob(paste0(paste0("input/data/*", name,"*")))

dados <- read.csv(file_data[1],
                  skip = 0,
                  #nrows=length(data) - 7,
                  header = TRUE, 
                  stringsAsFactors=FALSE,
                  #colClasses=c("Data", "Hora", "Valor", NA),
                  sep= ";"
                  #quote = "\"")
)


#conversao dados para zero/NA durante a noite e passar para Watt
dados$rad <- as.numeric(as.character(dados$rad)) /3.5
dados$rad[dados$rad <= 4] <- 0
dados$rad[dados$rad >= 1700] <- NA
dados[dados == -99.9] <- NA


dados$Data <- paste(dados$Data, dados$Hora)
dados <- dados[!duplicated(dados$Data), ]
dados$Data <- as.POSIXct(dados$Data, format="%d/%m/%y %H:%M:%S")
dados$Data <- round(dados$Data, "hours")
dados$Data <- as.POSIXct(dados$Data)
dados$Hora = NULL


#definicao de intervalo de tempo:
data_i <- as.POSIXct(min(dados$Data, na.rm = TRUE))
data_f <- as.POSIXct(max(dados$Data, na.rm = TRUE))
#data_i <- readline(prompt = "Data inicial para analise de dados no formato (AAAA-MM-DD): ")
#data_f <- readline(prompt = "Data final para analise de dados no formato (AAAA-MM-DD): ")

#hora solar:
#int_temp = format(seq(ISOdate(format(as.POSIXlt(data_i), "%Y"),format(as.POSIXlt(data_i), "%m"),format(as.POSIXlt(data_i), "%d"),format(as.POSIXlt(data_i), "%H"),format(as.POSIXlt(data_i), "%M"),format(as.POSIXlt(data_i), "%S")),
#                      ISOdate(format(as.POSIXlt(data_f), "%Y"),format(as.POSIXlt(data_f), "%m"),format(as.POSIXlt(data_f), "%d"),format(as.POSIXlt(data_f), "%H"),format(as.POSIXlt(data_f), "%M"),format(as.POSIXlt(data_f), "%S")), "hour"), "%Y-%m-%d %H:%M:%S")
#int_temp_day = format(seq(ISOdate(format(as.POSIXlt(data_i), "%Y"),format(as.POSIXlt(data_i), "%m"),format(as.POSIXlt(data_i), "%d")),
#                      ISOdate(format(as.POSIXlt(data_f), "%Y"),format(as.POSIXlt(data_f), "%m"),format(as.POSIXlt(data_f), "%d")), "day"), "%Y-%m-%d")

int_temp_day <- as.POSIXct(seq(data_i, data_f, by = "day"), format="%d/%m/%y")
int_temp <- as.POSIXct(seq(data_i, data_f, by = "hour"), format="%d/%m/%y %H:%M:%S")


int_data <- data.frame(Data=int_temp)
#int_data = int_data[-1,]
int_data <- data.frame(Data=int_data)

dados_temp <- dados

dados <- dados_temp[,1:2]

#juncao sequencia de tempo hora a hora aos medidos:
#dados <- merge(data.frame(int_dados), data.frame(int_data), fill = NA, all = T) 
#dados <- dados[-nrow(dados),] 

int_data <- data.frame(Data=int_temp)

merged <- merge(int_data, dados, by.x = "Data", by.y = "Data", sort = T, fill = NA, all.x = T)
#ddplyed <- ddply(merged, .(Data))
#dados <- merged[unique(merged$Data),]
#dados <- dados[unique(dados$Data),]
dados <- merged

#dia juliano
D_Juliano <- as.numeric(format(as.Date(int_temp_day), "%j")) 
D_Juliano_h <- julian(as.POSIXct(int_temp))
dados$D_Juliano <- as.numeric(format(as.Date(dados$Data), "%j")) 
dados$D_Juliano_h <- julian(as.POSIXct(dados$Data))

#Gh = rad. global horizontal
Gh = dados$rad

#lat em rad:
#Degrees <- readline(prompt = "Indicar a latitude em Graus Decimais: ")
lat = lat_degrees*(pi/180)


#I0 = extraterrestrial radiation constante solar (W/m^2) usar package sirad funcao extraT
I0jd = extrat(D_Juliano, lat)
#dados$Rad_extat_h <- I0jd$ExtraTerrestrialSolarRadiationHourly

I0_mat <- matrix(I0jd$ExtraTerrestrialSolarRadiationHourly, nrow=length(I0jd$ExtraTerrestrialSolarRadiationHourly)/24, ncol=24, byrow=F)

I0_tab <- c(t(I0_mat))
# transforma as unidade MJ para Kw e acerta uma hora para frente na table dados
# devido a transformacao da matriz para tabela e adia 2 horas para acerto de hora local 
# e do relogio do piranometro que grava as horas no fim da hora
dados$Rad_extat_h <- I0_tab*1000/3.5 
#lag(dados$Rad_extat_h, k = 1, na.pad = TRUE)
dados$Rad_extat_h[3:length(dados$Rad_extat_h)] <- I0_tab[1:(length(I0_tab)-2)]*1000/3.5 
dados$Rad_extat_h[dados$Rad_extat_h < 0] <- 0
#dados$Rad_extat_h[is.infinite(dados$Rad_extat_h)] <- 1 


#E = ecentricidade (Solar radiation model - L.T. Wong, W.K. Chow)
#r = anglo do dia em radianos em que N=nr do dia do ano(dia juliano 1-365)
r = 2*pi*((200-1)/365) 
E = 1.00011 + 0.034221*cos(r) + 0.00128*sin(r) + 0.000719*cos(2*r) + 0.000077*sin(2*r)

# r'$\alpha = angulo da altitude do sol
time_d <- as.POSIXct(paste(dados$Data), format="%Y-%m-%d %H:%M:%S", tz = "UTC")
#time_d <- strptime(paste(dados$Data, dados$Hora), format="%Y-%m-%d %H:%M:%S")
sun_angle = sunAngle(time_d, lon_degrees, lat_degrees) #fazer prompt para utilizador inserir coordenadas
alfa = 90 - sun_angle$altitude
dados$ang_solar <- sun_angle$altitude
dados$ang_solar[2:length(dados$ang_solar)] <- sun_angle$altitude[1:(length(I0_tab)-1)]

dados$ang_solar[dados$ang_solar <=0 ] = 0

#############calculated rad estrat. [w/m2*h] and sun angle in dados table

dados$kt <- dados$rad/dados$Rad_extat
dados$kt[dados$kt == Inf] = NA 
dados$kt[dados$kt >= 1] = 1 
#dados$kt[ is.infinite(dados$kt) ] <- NA

dados_ano <- aggregate(dados[,c(2,5,6,7)], by=list(dados$D_Juliano), FUN=mean, na.rm=TRUE, na.action=NULL)
names(dados_ano) <- c("Dia", names(dados_ano[2:length(dados_ano)]))

write.table(dados_ano, file = paste0("output/rad_", name,".csv"), row.names = F, sep = ";")

png(paste0("output/rad_", name,".png"), width = 6950, height = 4500, units = "px", res = 500)

par(mfrow = c(2, 2))

plot(x = dados_ano$Dia, y = dados_ano$Rad_extat_h, ylim = range(c(0,500)), grid(), col ="blue", pch = 16, lwd = 2, xlab = "Dia do ano", ylab = "Rad [w/m^2]", main = "Rad. extraterrestre vs medida", xaxt="n")#, yaxt="n")
axis(1, at = seq(1,366,7))
points(x = dados_ano$Dia, y = dados_ano$rad, col ="red", type = "h", lwd = 2)
#axis(2, at = seq(round(min(dados_ano$rad), digits = -1), round(max(dados_ano$Rad_extat_h), digits = -1), by = 50))

plot(x = dados_ano$Dia, y = dados_ano$rad, grid(), col ="red", type = "p", lwd = 2, xlab = "Dia do ano", ylab = "Rad [w/m^2]", main = "Rad. medida", xaxt="n")
axis(1, at = seq(1,366,7))

plot(x = dados_ano$Dia, y = dados_ano$kt, grid(), type = "h", lwd = 2, col = ifelse(dados_ano$kt > 0.5, "blue", "gray"), xlab = "Dia do ano", ylab = "kt []", main = "Ìndice de Claridade", xaxt="n")
axis(1, at = seq(1,366,7))
#col=brewer.pal(4, "Blues")

plot(x = dados_ano$Dia, y = dados_ano$ang_solar, grid(), col ="yellow", type = "b", lwd = 2, xlab = "Dia do ano", ylab = "Ang. Solar [º]", main = "Altura média solar", xaxt="n")
axis(1, at = seq(1,366,7))

dev.off()

plot(x = dados$ang_solar, y = dados$kt, col = ifelse(dados$kt > 0.5, "blue", "gray"), type = "p", lwd = 0.5, xlab = "altura solar", ylab = "kt []", main = "Altura solar VS kt")

plot(x = dados$Data, y = dados$rad, grid(), col = ifelse(dados$kt > 0.5, "blue", "gray"), type = "h", lwd = 0.2, xlab = "tempo", ylab = "Rad [w/m^2]", main = "Valores de radiacao medidos")
