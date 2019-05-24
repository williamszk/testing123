#190522 test bandwidth size GWR


library(gstat)
library(sp)
library(spdep)
library(maptools)
library(ggplot2)
library(rgdal)
library(readxl)  
library(foreign)
library(Hmisc)
library(gdata)
library(stargazer)
library(AER)
library(xlsx)
library(stats)
library(tmap)
library(rgeos)
library(sphet)
library(Matrix)
library(tseries)
library(magrittr)
library(scales)
library(raster)
library(matrixcalc)
library(tidyverse)
library(spgwr)


#regressions for Brazil ######################################
data <- exbase@data
attach(data)
#mybase have all the spatial variables
#nb.br <- poly2nb(exbase, queen=T) 
W <- nb2listw(nb.br, glist=NULL, style ="W") #normlize weights to 1

iqim <- iqim
gold <- ouro_200
sugar <- acucar_200
port <- distportkm #distance to portugal

idhm <- IDHM #human development index for the city

port_gold <- gold*port
port_sugar <- sugar*port

s1 <-lm(iqim ~ port+gold+sugar+port_gold+port_sugar); summary(s1)
iqim.h <- fitted.values(s1)


#################################################################
#build a data frame with 20 columns showing statistics for each city
d <- dim(exbase)[1] #d is 5502, number of cities
coords <- coordinates(exbase) 
dist1 <-as.matrix(dist(coords,method="euclidean",diag = FALSE, upper = FALSE, p = 2)) #distance matrix

names_coef1  <- c("k")
length_coef1 <- length(names_coef1)

matrix1 <- matrix(rep(0,d*length_coef1),nrow=d) #a holder matrix for future use
coef1 <- data.frame(matrix1) #transform the matrix in data frame
coef1$cod_ibge <- as.character(data$cod_ibge)
names(coef1) <- c(names_coef1,'cod_ibge')

sizes <- 30:500
d_sizes <- length(sizes)
coef2 <- data.frame(sizes,rep(0,d_sizes)); names(coef2)<-c('k size','value')

i <- 1 #just a test if the loop is ok
#k <- 10
for (i in 1:d) {
  for (k in sizes) {
    x1 <- data.frame(dist1[i,],1:d) #build a temporary dataframe
    row.names(x1) <- 1:d #
    names(x1) <- c("distance","position")
    x2 <- x1[order(x1$distance),]
    x3 <- head(x2,k)
    x4 <- x3$position #vector with municipality position that are the k nearest
    x5 <- x4[-1]
    iqim.h2 <- iqim.h[x5] #just pick the municipalities in the neighboring sample
    iqim.h3 <- iqim.h[x4][1]
    idhm2 <- idhm[x5] #you can put another explained variables here
    idhm3 <- idhm[x4][1]
    model <- summary(lm(idhm2 ~ iqim.h2)) #model OLS
    coefs <- model$coefficients
    pos1 <- which(k==sizes)
    coef2[pos1,2] <- (idhm3-(coefs[1,1]+ coefs[2,1]*iqim.h3))^2  #the usual cross-validation
  }
  coef2_1 <- coef2[order(coef2$value),] 
  coef2_2 <- coef2_1[1,]
  #head(coef2_1)
  #plot(coef2$`k size`,coef2$value,type='l')
  coef1[i,1] <- coef2_2[1]
  #coef1[i,]
  print(c(paste(i),paste(Sys.time())))
}


plot(density(k_values$k))
hist(k_values$k,breaks=471)

k_values <- coef1



gdata::keep(exbase, 
            nb.br,
            k_values, 
            sure=TRUE)

