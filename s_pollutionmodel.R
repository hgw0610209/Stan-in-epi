library("rstan")
library("shapefiles")
library("sp")
library("CARBayesST")
library("rgdal")
library("spdep") 
library("raster") 
library("here") 



# get those data to fit pollution model -----------------------------------

data <- read.csv(file=here::here("R_paper","4pollutionmodel","Monitoring data_whole.csv"))


## Combine site types and delete the duplicate rows with the same coordinates
site<-rep(NA,nrow(data))
site[data$sitetype=="Airport"] <- "Background"
site[data$sitetype=="Urban Background"] <- "Background"
site[data$sitetype=="Suburban"] <- "Background"
site[data$sitetype=="Roadside"] <- "Roadside"
site[data$sitetype=="Rural"] <- "Rural"
site[data$sitetype=="Kerbside"] <- "Kerbside"

data <- data.frame(data, site=site)
data<-subset(data, !duplicated(data[,4:5]) )
data<-data[-which(is.na(data$site)),] ##delete the site type of Urban Industrial ##delete the site type of Urban Industrial
data<-data[-which(is.na(data$NO2modelled2006)),] ##this row, the modelled data for 2006 is NA
data<-data[-which(data$MISSINGALL==0),]

#take the log for observations and also modelled grid data
data$NO2annualmean2006<-log(data$NO2annualmean2006)
data$NO2annualmean2007<-log(data$NO2annualmean2007)
data$NO2annualmean2008<-log(data$NO2annualmean2008)
data$NO2annualmean2009<-log(data$NO2annualmean2009)
data$NO2annualmean2010<-log(data$NO2annualmean2010)
data$NO2modelled2006<-log(data$NO2modelled2006)
data$NO2modelled2007<-log(data$NO2modelled2007)
data$NO2modelled2008<-log(data$NO2modelled2008)
data$NO2modelled2009<-log(data$NO2modelled2009)
data$NO2modelled2010<-log(data$NO2modelled2010)

#take the log for observations and also modelled grid data
data$PM10annualmean2006<-log(data$PM10annualmean2006)
data$PM10annualmean2007<-log(data$PM10annualmean2007)
data$PM10annualmean2008<-log(data$PM10annualmean2008)
data$PM10annualmean2009<-log(data$PM10annualmean2009)
data$PM10annualmean2010<-log(data$PM10annualmean2010)
data$PM10modelled2006<-log(data$PM10modelled2006)
data$PM10modelled2007<-log(data$PM10modelled2007)
data$PM10modelled2008<-log(data$PM10modelled2008)
data$PM10modelled2009<-log(data$PM10modelled2009)
data$PM10modelled2010<-log(data$PM10modelled2010)

## Fit the model and save the coefficients
NO2model2006 <- lm(NO2annualmean2006~factor(site)+NO2modelled2006+tem2006,data=data)
NO2model2007 <- lm(NO2annualmean2007~factor(site)+NO2modelled2007+tem2007,data=data)
NO2model2008 <- lm(NO2annualmean2008~factor(site)+NO2modelled2008+tem2008,data=data)
NO2model2009 <- lm(NO2annualmean2009~factor(site)+NO2modelled2009+tem2009,data=data)
NO2model2010 <- lm(NO2annualmean2010~factor(site)+NO2modelled2010+tem2010,data=data)

## Extract response and design matrix
frame <- try(suppressWarnings(model.frame(NO2model2006, data=data, na.action=na.pass)), silent=TRUE)
Y1_2006<-model.response(frame)

frame <- try(suppressWarnings(model.frame(NO2model2007, data=data, na.action=na.pass)), silent=TRUE)
Y1_2007<-model.response(frame)

frame <- try(suppressWarnings(model.frame(NO2model2008, data=data, na.action=na.pass)), silent=TRUE)
Y1_2008<-model.response(frame)

frame <- try(suppressWarnings(model.frame(NO2model2009, data=data, na.action=na.pass)), silent=TRUE)
Y1_2009<-model.response(frame)

frame <- try(suppressWarnings(model.frame(NO2model2010, data=data, na.action=na.pass)), silent=TRUE)
Y1_2010<-model.response(frame)

## Fit the model and save the coefficients
PM10model2006 <- lm(PM10annualmean2006~factor(site)+PM10modelled2006+tem2006,data=data)
PM10model2007 <- lm(PM10annualmean2007~factor(site)+PM10modelled2007+tem2007,data=data)
PM10model2008 <- lm(PM10annualmean2008~factor(site)+PM10modelled2008+tem2008,data=data)
PM10model2009 <- lm(PM10annualmean2009~factor(site)+PM10modelled2009+tem2009,data=data)
PM10model2010 <- lm(PM10annualmean2010~factor(site)+PM10modelled2010+tem2010,data=data)

## Extract response and design matrix
frame <- try(suppressWarnings(model.frame(PM10model2006, data=data, na.action=na.pass)), silent=TRUE)
Y2_2006<-model.response(frame)

frame <- try(suppressWarnings(model.frame(PM10model2007, data=data, na.action=na.pass)), silent=TRUE)
Y2_2007<-model.response(frame)

frame <- try(suppressWarnings(model.frame(PM10model2008, data=data, na.action=na.pass)), silent=TRUE)
Y2_2008<-model.response(frame)

frame <- try(suppressWarnings(model.frame(PM10model2009, data=data, na.action=na.pass)), silent=TRUE)
Y2_2009<-model.response(frame)

frame <- try(suppressWarnings(model.frame(PM10model2010, data=data, na.action=na.pass)), silent=TRUE)
Y2_2010<-model.response(frame)

# the following is used for multipollutant model 

X1_2006<-model.matrix(NO2model2006,na.action=na.pass)
X1_2007<-model.matrix(NO2model2007,na.action=na.pass)
X1_2008<-model.matrix(NO2model2008,na.action=na.pass)
X1_2009<-model.matrix(NO2model2009,na.action=na.pass)
X1_2010<-model.matrix(NO2model2010,na.action=na.pass)


X2_2006<-model.matrix(PM10model2006,na.action=na.pass)
X2_2007<-model.matrix(PM10model2007,na.action=na.pass)
X2_2008<-model.matrix(PM10model2008,na.action=na.pass)
X2_2009<-model.matrix(PM10model2009,na.action=na.pass)
X2_2010<-model.matrix(PM10model2010,na.action=na.pass)


data_obs <- cbind(c(Y1_2006,Y1_2007,Y1_2008,Y1_2009,Y1_2010), c(Y2_2006,Y2_2007,Y2_2008,Y2_2009,Y2_2010))
data_cov <- array(NA, c(ncol(data_obs),nrow(data_obs),ncol(X1_2006)))
data_cov[1,,] <- rbind(X1_2006,X1_2007,X1_2008,X1_2009,X1_2010)
data_cov[2,,] <- rbind(X2_2006,X2_2007,X2_2008,X2_2009,X2_2010)

# Extract the missing values into a VECTOR
dat_complete <- data_obs[!is.na(data_obs)]

# Extract the missing and present values as MATRICES
ind_pres <- which(!is.na(data_obs), arr.ind = TRUE)
ind_miss <- which(is.na(data_obs), arr.ind = TRUE)



# run the stan pollution model --------------------------------------------

mod.data <- list(K=ncol(X1_2006),
                 Nrow = nrow(data_obs),
                 Ncol = ncol(data_obs),
                 Ncomp = length(dat_complete),
                 Nmiss = sum(is.na(data_obs)),
                 dat_complete = dat_complete,
                 ind_pres = ind_pres,
                 ind_miss = ind_miss
                 ,data_cov=data_cov
                 )

fit <- stan(file = here::here("R_paper","4pollutionmodel","s_pollutionmodel.stan"), data = mod.data,seed = 110,
            iter = 20000, chains = 1,thin = 5, verbose=TRUE)
save(fit, file = "fit.RData")


#############################
# 
# result <- rstan::extract(fit)
# 
# names(result)
# dim(result$betas)
# 
# table_matrix <- NULL
# table_matrix <- rbind(table_matrix, c(apply(result$betas[,1,],2,mean), apply(result$betas[,2,],2,mean)))
# 












