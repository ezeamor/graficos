# graficos

####################################################

# GRAFICOS.

####################################################

graphics.off() # Elimina configuracion de graficos previos 
setwd("/home/ezequiel.amor")

# Librerias a usar en este programa.

library(ggplot2) # Para graficar
library(zoo) # Para promedios moviles
library(patchwork) # Para unir graficos de ggplot2

####################################################

# DIAGRAMA DE CONFIABILIDAD: RELIABILITY PLOT.

####################################################

# Para poder graficar los diagramas necesito usar en el eje x los datos de 
# "y.i", mientras que en el eje y se usan los datos de "obar.i" guardados en A,
# B, C y D con verify en el programa "Indices_Verif_Prono".
# El eje x es el mismo para todos los graficos, solo varian los obar.i.

# Primero guardo toda la informacion en un dataframe para luego poder usar 
# ggplot.

datos_reliability <- data.frame("y.i"=A$y.i,"obar.i_1mm"=A$obar.i,
                                "obar.i_20mm"=B$obar.i,"obar.i_50mm"=C$obar.i,
                                "obar.i_100mm"=D$obar.i)

# Solo habria que modificar el titulo en funcion de la estacion, epoca del anio,
# etc. con la que se este trabajando.

gr <- ggplot(datos_reliability,aes(x=y.i)) + 
     geom_line(aes(x=y.i,y=obar.i_1mm,color="1mm"),linetype=1,size=0.5) + 
     geom_point(aes(y=obar.i_1mm),size=2,color="darkgreen") + 
     geom_line(aes(y=obar.i_20mm,color="20mm"),linetype=1,size=0.5) + 
     geom_point(aes(y=obar.i_20mm),size=2,color="blue") + 
     geom_line(aes(y=obar.i_50mm,color="50mm"),linetype=1,size=0.5) + 
     geom_point(aes(y=obar.i_50mm),size=2,color="darkviolet") + 
     geom_line(aes(y=obar.i_100mm,color="100mm"),linetype=1,size=0.5) + 
     geom_point(aes(y=obar.i_100mm),size=2,color="red") + 
     scale_x_continuous(breaks=seq(0,1,0.1)) + 
     scale_y_continuous(breaks=seq(0,1,0.1)) + 
     geom_abline(intercept=0, slope=1) + 
     labs(title="Reliability Plot para estación Tres Horquetas ROLON durante período frío (abr-sep)",
          x="Probabilidad de Pronóstico (y.i)",
          y="Frecuencia Relativa Observada (obar.i)",colour="Acumulado") + 
    theme_bw() + 
    coord_cartesian(ylim=c(0,1)) 
gr

####################################################

# GRAFICO DE BARRAS DEL RELIABILITY PLOT.

####################################################

# En el eje x van los valores de "y.i". Pero en el eje y en vez de poner los 
# datos de "prob.y", calculo la valores absolutos, es decir, cuantos datos 
# caen dentro de cada caja de probabilidad (cuantos valores de pronostico caen 
# en la caja de 0-10%, cuantos en 10-20%, etc).

val_abs_1mm   <- c()
val_abs_20mm  <- c()
val_abs_50mm  <- c()
val_abs_100mm <- c()

for(i in 1:10) {
  if(i == 1) {
    val_abs_1mm[i] <- length(which(Probabilidad$Prob_1mm >= 0 & 
                                     Probabilidad$Prob_1mm <= 0.1)) 
  } else {
    val_abs_1mm[i] <- length(which(Probabilidad$Prob_1mm > (0.1*(i-1)) & 
                                     Probabilidad$Prob_1mm <= (0.1+(0.1*(i-1)))))
  }
}

for(i in 1:10) {
  if(i == 1) {
    val_abs_20mm[i] <- length(which(Probabilidad$Prob_20mm >= 0 & 
                                      Probabilidad$Prob_20mm <= 0.1)) 
  } else {
    val_abs_20mm[i] <- length(which(Probabilidad$Prob_20mm > (0.1*(i-1)) & 
                                      Probabilidad$Prob_20mm <= (0.1+(0.1*(i-1)))))
  }
}

for(i in 1:10) {
  if(i == 1) {
    val_abs_50mm[i] <- length(which(Probabilidad$Prob_50mm >= 0 & 
                                      Probabilidad$Prob_50mm <= 0.1)) 
  } else {
    val_abs_50mm[i] <- length(which(Probabilidad$Prob_50mm > (0.1*(i-1)) & 
                                      Probabilidad$Prob_50mm <= (0.1+(0.1*(i-1)))))
  }
}

for(i in 1:10) {
  if(i == 1) {
    val_abs_100mm[i] <- length(which(Probabilidad$Prob_100mm >= 0 & 
                                       Probabilidad$Prob_100mm <= 0.1)) 
  } else {
    val_abs_100mm[i] <- length(which(Probabilidad$Prob_100mm > (0.1*(i-1)) & 
                                       Probabilidad$Prob_100mm <= (0.1+(0.1*(i-1)))))
  }
}

# Ahora armo los gráficos de barras, donde en el eje x va "y.i" y en el eje y 
# van los "val_abs". Primero armo el dataframe que va a leer ggplot.

datos_barras <- data.frame("y.i"=A$y.i,"val_abs_1mm"=val_abs_1mm,
                           "val_abs_20mm"=val_abs_20mm, 
                           "val_abs_50mm"=val_abs_50mm,
                           "val_abs_100mm"=val_abs_100mm)

g <- ggplot(datos_barras,aes(x=y.i,y=val_abs_1mm)) + 
  geom_bar(stat = "identity",fill="green",color="darkgreen") +
  scale_x_continuous(breaks=seq(0,1,0.1)) + 
  scale_y_continuous(breaks=seq(0,250,10)) + 
  labs(title="Frecuencia de Probabilidades de Pronósticos para 1 mm",
       x="Probabilidad de Pronóstico (y.i)",
       y="Cantidad de Valores de Probabilidad") +
  theme_bw()
g

g1 <- ggplot(datos_barras,aes(x=y.i,y=val_abs_20mm)) + 
  geom_bar(stat = "identity",fill="blue",color="darkblue") +
  scale_x_continuous(breaks=seq(0,1,0.1)) + 
  scale_y_continuous(breaks=seq(0,115,5)) + 
  labs(title="Frecuencia de Probabilidades de Pronósticos para 20 mm",
       x="Probabilidad de Pronóstico (y.i)",
       y="Cantidad de Valores de Probabilidad") +
  theme_bw()
g1

g2 <- ggplot(datos_barras,aes(x=y.i,y=val_abs_50mm)) + 
  geom_bar(stat = "identity",fill="violet",color="darkviolet") +
  scale_x_continuous(breaks=seq(0,1,0.1)) + 
  scale_y_continuous(breaks=seq(0,205,10)) + 
  labs(title="Frecuencia de Probabilidades de Pronósticos para 50 mm",
       x="Probabilidad de Pronóstico (y.i)",
       y="Cantidad de Valores de Probabilidad") +
  theme_bw()
g2

g3 <- ggplot(datos_barras,aes(x=y.i,y=val_abs_100mm)) + 
  geom_bar(stat = "identity",fill="red",color="darkred") +
  scale_x_continuous(breaks=seq(0,1,0.1)) + 
  scale_y_continuous(breaks=seq(0,430,15)) + 
  labs(title="Frecuencia de Probabilidades de Pronósticos para 100 mm",
       x="Probabilidad de Pronóstico (y.i)",
       y="Cantidad de Valores de Probabilidad") +
  theme_bw()
g3

# Uno los graficos

g + g1 + g2 + g3

####################################################

# GRAFICOS DE CLIMATOLOGIA.

####################################################

# Primero calculo la climatología completa, no solo para las semanas 2 que esta-
# mos estudiando. Los datos climatologicos de PP ya están en "datos_clima", 
# calculados en el programa "datos_PP_Prono".

# Elimino los dias 29 de febrero.

datos_clima <- datos_clima[-which(substr(datos_clima$Fechas,6,10) == "02-29"),]

# Calculo las probabilidades climatológicas.

acum_clima_total    <- list()
acum_S2_clima_total <- c()

for(i in 1:365) {
  for(j in 1:30) {
    if(j>=1 | j<=4) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(365*(j-1))):((7+(i-1))+(365*(j-1)))],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total 
    } else if(j>=5 | j<=8) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(365*(j-1))+1):((7+(i-1))+(365*(j-1))+1)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total 
    } else if(j>=9 | j<=12) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(365*(j-1))+2):((7+(i-1))+(365*(j-1))+2)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total 
    } else if(j>=13 | j<=16) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(365*(j-1))+3):((7+(i-1))+(365*(j-1))+3)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total
    } else if(j>=17 | j<=20) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(365*(j-1))+4):((7+(i-1))+(365*(j-1))+4)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total
    } else if(j>=21 | j<=24) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(375*(j-1))+5):((7+(i-1))+(375*(j-1))+5)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total
    } else if(j>=25 | j<=28) {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(375*(j-1))+6):((7+(i-1))+(375*(j-1))+6)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total
    } else {
      acum_S2_clima_total[j] <- sum(datos_clima$PP[((1+(i-1))+(375*(j-1))+7):((7+(i-1))+(375*(j-1))+7)],na.rm = T)
      acum_clima_total[[i]]  <- acum_S2_clima_total
    }
  }  
}

Probabilidad_1_clima_total   <- c()
Probabilidad_20_clima_total  <- c()
Probabilidad_50_clima_total  <- c()
Probabilidad_100_clima_total <- c()

for(i in 1:365) {
  Probabilidad_1_clima_total[i]   <- (length(acum_clima_total[[i]][which(acum_clima_total[[i]] >= 1)])/30)
  Probabilidad_20_clima_total[i]  <- (length(acum_clima_total[[i]][which(acum_clima_total[[i]] >= 20)])/30)
  Probabilidad_50_clima_total[i]  <- (length(acum_clima_total[[i]][which(acum_clima_total[[i]] >= 50)])/30)
  Probabilidad_100_clima_total[i] <- (length(acum_clima_total[[i]][which(acum_clima_total[[i]] >= 100)])/30)
}

# Guardo todo en un dataframe.

clima_total <- data.frame("Semana"=c(1:365),"Proba_1mm"=Probabilidad_1_clima_total,
                          "Proba_20mm"=Probabilidad_20_clima_total,
                          "Proba_50mm"=Probabilidad_50_clima_total,
                          "Proba_100mm"=Probabilidad_100_clima_total)

# Realizo el grafico.

g_clima <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_1mm,color="1mm"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=Proba_20mm,color="20mm"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=Proba_50mm,color="50mm"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=Proba_100mm,color="100mm"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018)",x="Semana 2",
       y="Probabilidad",colour="Acumulado") + 
  theme_bw()

g_clima

################ PROMEDIOS MOVILES #################

# Hago el mismo grafico pero aplicando promedios moviles. Para los promedios uso
# ventana de 5, 7 y 10 dias.

# Calculo los promedios moviles.

prom_5d_1mm   <- rollmean(clima_total$Proba_1mm,5,fill=NA)
prom_5d_20mm  <- rollmean(clima_total$Proba_20mm,5,fill=NA)
prom_5d_50mm  <- rollmean(clima_total$Proba_50mm,5,fill=NA)
prom_5d_100mm <- rollmean(clima_total$Proba_100mm,5,fill=NA)

prom_7d_1mm   <- rollmean(clima_total$Proba_1mm,7,fill=NA)
prom_7d_20mm  <- rollmean(clima_total$Proba_20mm,7,fill=NA)
prom_7d_50mm  <- rollmean(clima_total$Proba_50mm,7,fill=NA)
prom_7d_100mm <- rollmean(clima_total$Proba_100mm,7,fill=NA)

prom_10d_1mm   <- rollmean(clima_total$Proba_1mm,10,fill=NA)
prom_10d_20mm  <- rollmean(clima_total$Proba_20mm,10,fill=NA)
prom_10d_50mm  <- rollmean(clima_total$Proba_50mm,10,fill=NA)
prom_10d_100mm <- rollmean(clima_total$Proba_100mm,10,fill=NA)

# Los agrego al dataframe "clima_total" para poder graficar.

clima_total$prom_5d_1mm   <- prom_5d_1mm
clima_total$prom_5d_20mm  <- prom_5d_20mm
clima_total$prom_5d_50mm  <- prom_5d_50mm
clima_total$prom_5d_100mm <- prom_5d_100mm

clima_total$prom_7d_1mm   <- prom_7d_1mm
clima_total$prom_7d_20mm  <- prom_7d_20mm
clima_total$prom_7d_50mm  <- prom_7d_50mm
clima_total$prom_7d_100mm <- prom_7d_100mm

clima_total$prom_10d_1mm   <- prom_10d_1mm
clima_total$prom_10d_20mm  <- prom_10d_20mm
clima_total$prom_10d_50mm  <- prom_10d_50mm
clima_total$prom_10d_100mm <- prom_10d_100mm

# Graficos categoria 1 mm.

g_clima1_5d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_1mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_5d_1mm,color="PM 5 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 1 mm y Promedio Móvil (PM) de 5 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("green","black")) +
  theme_bw()

g_clima1_5d

g_clima1_7d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_1mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_7d_1mm,color="PM 7 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 1 mm y Promedio Móvil (PM) de 7 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") +
  scale_color_manual(values = c("green","black")) +
  theme_bw()

g_clima1_7d

g_clima1_10d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_1mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_10d_1mm,color="PM 10 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 1 mm y Promedio Móvil (PM) de 10 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") +
  scale_color_manual(values = c("green","black")) +
  theme_bw()
  
g_clima1_10d

# Graficos categoria 20 mm.

g_clima20_5d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_20mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_5d_20mm,color="PM 5 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 20 mm y Promedios Móviles (PM) de 5 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("turquoise","black")) +
    theme_bw()

g_clima20_5d

g_clima20_7d<- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_20mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_7d_20mm,color="PM 7 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 20 mm y Promedios Móviles (PM) de 7 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("turquoise","black")) +
  theme_bw()

g_clima20_7d

g_clima20_10d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_20mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_10d_20mm,color="PM 10 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 20 mm y Promedios Móviles (PM) de 10 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("turquoise","black")) +
  theme_bw()

g_clima20_10d

# Graficos categoria 50 mm.

g_clima50_5d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_50mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_5d_50mm,color="PM 5 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 50 mm y Promedios Móviles (PM) de 5 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("violet","black")) +
  theme_bw()

g_clima50_5d

g_clima50_7d<- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_50mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_7d_50mm,color="PM 7 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 50 mm y Promedios Móviles (PM) de 7 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("violet","black")) +
  theme_bw()

g_clima50_7d

g_clima50_10d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_50mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_10d_50mm,color="PM 10 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 50 mm y Promedios Móviles (PM) de 10 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("violet","black")) +
  theme_bw()

g_clima50_10d

# Graficos categoria 100 mm.

g_clima100_5d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_100mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_5d_100mm,color="PM 5 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 100 mm y Promedios Móviles (PM) de 5 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("red","black")) +
  theme_bw()

g_clima100_5d

g_clima100_7d<- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_100mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_7d_100mm,color="PM 7 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 100 mm y Promedios Móviles (PM) de 7 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("red","black")) +
  theme_bw()

g_clima100_7d

g_clima100_10d <- ggplot(clima_total,aes(x=Semana)) +
  geom_line(aes(x=Semana,y=Proba_100mm,color="Original"),linetype=1,size=0.5) +
  geom_line(aes(x=Semana,y=prom_10d_100mm,color="PM 10 días"),linetype=1,size=0.5) +
  scale_x_continuous(breaks=seq(0,365,30)) + 
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Probabilidad Climatológica (1989-2018) para 100 mm y Promedios Móviles (PM) de 10 días",
       x="Semana 2",
       y="Probabilidad",colour="Series") + 
  scale_color_manual(values = c("red","black")) +
  theme_bw()

g_clima100_10d

#######################################################

# GRAFICOS DE DATOS MENSUALES DE DISTINTAS ESTACIONES.

#######################################################

# Grafico las precipitaciones mensuales de las estaciones de la RCB y de Resistencia
# Aero para ver como es el comportamiento espacial de la precipitacion y si es
# posible comparar lo que pasa en Resistencia Aero con las otras estaciones.
# Los datos mensuales se calcularon en el programa "datos_estaciones".

# Solo habria que modificar en el titulo el rango de meses en el que se tiene datos.

g_est <- ggplot(PP_mensual,aes(x=mes)) + 
  geom_line(aes(x=mes,y=Resistencia,color="Resistencia"),linetype=1,size=1.2) + 
  geom_line(aes(y=Moncholo,color="Moncholo"),linetype=1,size=0.5) + 
  geom_line(aes(y=GralVedia,color="Gral Vedia"),linetype=1,size=0.5) + 
  geom_line(aes(y=Lote16,color="Lote 16"),linetype=1,size=0.5) + 
  geom_line(aes(y=PtoBermejokm90,color="Puerto Bermejo KM 90"),linetype=1,size=0.5) + 
  geom_line(aes(y=TresHorquetasRolon,color="Tres Horquetas ROLON"),linetype=1,size=0.5) + 
  scale_x_continuous(breaks=seq(1,30,1)) + 
  scale_y_continuous(breaks=seq(0,350,10)) + 
  labs(title="Precipitaciones mensuales para estaciones de la RCB y Resistencia entre Noviembre 2020 y Abril 2023",
       x="Datos (Mes)",
       y="Precipitación Mensual (mm)",colour="Estaciones") + 
  theme_bw() +
  scale_color_manual(values=c("red1", "darkorange","green3","turquoise","black","darkviolet"))

g_est

####################################################

# CURVAS ROC.

####################################################

# Grafico todas las curvas ROC en una unica figura. Uso los datos guardados en 
# el programa "Indices_Verif_Prono" en Z1, Z2, Z3 y Z4.

# No habría que modificar nada más que el titulo del grafico. Cambiar "main" segun
# cual sea la estacion con la que se este trabajando, el periodo, etc.

plot(x=Z1$plot.data[,3,1],y=Z1$plot.data[,2,1], 
     main="Curvas ROC para el período frío (abr-sep) de la estación Tres Horquetas ROLON",
     type="b",col="green3",lwd=1.5,xlab="False Alarm Rate",ylab="Hit Rate")
lines(Z2$plot.data[,3,1],Z2$plot.data[,2,1],type="b",col="blue",lwd=1.5)
lines(Z3$plot.data[,3,1],Z3$plot.data[,2,1],type="b",col="darkviolet",lwd=1.5)
lines(Z4$plot.data[,3,1],Z4$plot.data[,2,1],type="b",col="red",lwd=1.5)
abline(a=0,b=1,lwd=1.5)
legend("bottomright", legend = c("1 mm","20 mm","50 mm","100 mm"),lwd = 2, 
       col = c("green3", "blue","darkviolet","red"),title="Categoría de Acumulado")

# Recordar que el area bajo la curva se calculo en "Indices_Verif_Prono".
