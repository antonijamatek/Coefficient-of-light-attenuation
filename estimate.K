#set the working directory
setwd("C:/Users/Antonija/OneDrive - Prirodoslovno-matematički fakultet/ISLAND/Primarna proizvodnja/Modeliranje/Svjetlost")

#1. install packages
install.packages("tidyverse")
install.packages("tibble")
install.packages("readr")
install.packages("dichromat")
install.packages("dplyr")

#2. load packages
library(tidyverse)
library(tibble)
library(readr)
library(dichromat)
library(patchwork)
library(dplyr)

#3. import data
#light intensity at the surface
l0 <- readr::read_delim("light0.csv", delim =",", show_col_types = FALSE) 
#light intensity at 5 meters
l5 <- readr::read_delim("light5.csv", delim =",", show_col_types = FALSE) 
#l. int. at 10 m
l10 <- readr::read_delim("light10.csv", delim =",", show_col_types = FALSE)
#l. int. at 20 m
l20 <- readr::read_delim("light20.csv", delim =",", show_col_types = FALSE)
#l. int. at 30 m
l30 <- readr::read_delim("light30.csv", delim =",", show_col_types = FALSE)

#4. organize data 
#a filter the data using time filter for all zeros (exclude the night)
l0 <- l0 %>% filter(`Light intensity(Lux)`> 0)
l5 <- l5 %>% filter(`Light intensity(Lux)`> 0)
l10 <- l10 %>% filter(`Light intensity(Lux)`> 0)
l20 <- l20 %>% filter(`Light intensity(Lux)`> 0)
l30 <- l30 %>% filter(`Light intensity(Lux)`> 0)

#plot raw data
dev.off() 
par(mfrow=c(2,3))
plot(l0$No, l0$`Light intensity(Lux)`)
plot(l5$No, l5$`Light intensity(Lux)`)
plot(l10$No, l10$`Light intensity(Lux)`)
plot(l20$No, l20$`Light intensity(Lux)`)
plot(l30$No, l30$`Light intensity(Lux)`)

#b filter the timeseries for values at solar noon (16 points per depth)
nl0 <- l0[grep("01:00:00 PM", l0$`Date Time(GMT+02:00)`),] 
nl5 <- l5[grep("01:00:00 PM", l5$`Date Time(GMT+02:00)`),]
nl10 <- l10[grep("01:00:00 PM", l10$`Date Time(GMT+02:00)`),]
nl20 <- l20[grep("01:00:00 PM", l20$`Date Time(GMT+02:00)`),]
nl30 <- l30[grep("01:00:00 PM", l30$`Date Time(GMT+02:00)`),]

#plot raw data
dev.off()
par(mfrow=c(5,1)) 
plot(nl0$No, nl0$`Light intensity(Lux)`)
plot(nl5$No, nl5$`Light intensity(Lux)`)
plot(nl10$No, nl10$`Light intensity(Lux)`)
plot(nl20$No, nl20$`Light intensity(Lux)`)
plot(nl30$No, nl30$`Light intensity(Lux)`)

#c filter the timeseries to leave out outliers due to cloud coverage
nl0 <- nl0 %>% filter(`Light intensity(Lux)` > 100000)
nl5 <- nl5 %>% filter(`Date Time(GMT+02:00)` != "07/19/23 01:00:00 PM")
nl10 <- nl10 %>% filter(`Date Time(GMT+02:00)` != "07/19/23 01:00:00 PM")
nl20 <- nl20 %>% filter(`Date Time(GMT+02:00)` != "07/19/23 01:00:00 PM")
nl30 <- nl30 %>% filter(`Date Time(GMT+02:00)` != "07/19/23 01:00:00 PM")

#d make a list
v0 <- nl0[4] #vector for the data set
v5 <- nl5[4]
v10 <- nl10[4]
v20 <- nl20[4]
v30 <- nl30[4]
l <- bind_rows( #binding data
  mutate(v0, Depth = 0, Date = nl0$`Date Time(GMT+02:00)`),
  mutate(v5, Depth = 5, Date = nl5$`Date Time(GMT+02:00)`),
  mutate(v10, Depth = 10, Date = nl10$`Date Time(GMT+02:00)`),
  mutate(v20, Depth = 20, Date = nl20$`Date Time(GMT+02:00)`),
  mutate(v30, Depth = 30, Date = nl30$`Date Time(GMT+02:00)`)) 
l <- as.data.frame(l)

#plot raw data
dev.off()
plot(l$`Light intensity(Lux)`, -l$Depth)

#5. define function for non linear squares
I ~ I0 * e^(-K*z)
K <- numeric() #empty vector to store K estimate
e <- exp(1) #euler's number
I <- l$`Light intensity(Lux)`
I0 <- l %>% filter(Depth == as.numeric(0))
I0 <- I0$`Light intensity(Lux)` #surface light intensity vector
z <- l$Depth #defining depth vector

#6. saving K using nls (nonlinear squares) function
set.seed(1)
fit <- nls(I ~ I0 * e^(-K*z), start= list(K=0.1)) #fit data to the model using nonlinear squares
K <- coef(fit)["K"] #store the estimated K to the empty vector 
summary(fit)

#7. check my work with a plot of light with depth
depth <- c(-0,-5,-10,-20,-30)
x1 <- median(nl0$`Light intensity(Lux)`)
x2 <- median(nl5$`Light intensity(Lux)`)
x3 <- median(nl10$`Light intensity(Lux)`)
x4 <- median(nl20$`Light intensity(Lux)`)
x5 <- median(nl30$`Light intensity(Lux)`)
I. <- c(x1, x2, x3, x4, x5) #intensity at each depth at noon
K #estimate I got by fitting the data
Iest <- I.[1]*e^(K*depth) #fitting the depth, light intensity at surface at the noon, and K estimate 

#plotting the measured data vs the function of the light decay with depth using estimated K from the measured data
dev.off()
par(mfrow=c(1,1))
plot(I., depth, type="b")
lines(Iest, depth, col="red", type="b")

#8. If estimate is a bit off (underestimating)- normalizing to 5 m
#a parameters
K <- numeric() #empty vector to store K estimate
e <- exp(1) #euler's number
I <- l$`Light intensity(Lux)`
I5 <- l %>% filter(Depth == as.numeric(5))
I5 <- I5$`Light intensity(Lux)`  #surface light intensity vector
z <- l$Depth #defining depth vector

#b fitting to nls
set.seed(1)
fit <- nls(I ~ I5 * e^(-K*z), start= list(K=0.1)) #fit data to the model using nonlinear squares
K <- coef(fit)["K"] #store the estimated K 
summary(fit)

#c plot
depth <- c(-5,-10,-20,-30)
x2 <- median(nl5$`Light intensity(Lux)`)
x3 <- median(nl10$`Light intensity(Lux)`)
x4 <- median(nl20$`Light intensity(Lux)`)
x5 <- median(nl30$`Light intensity(Lux)`)
I.. <- c(x2, x3, x4, x5) #intensity at each depth at noon
K #estimate I got by fitting the data
Iest. <- I..[1]*e^(K*depth) #fitting the depth, light intensity at surface at the noon, and K estimate 
dev.off()
par(mfrow=c(1,1))#plotting the measured data vs the function of the light decay with depth using estimated K from the measured data
plot(I.., depth, type="b")
lines(Iest., depth, col="red", type="b")

#9 Estimating K using nls (nonlinear squares) function for each day
fit_summary <- list() #store summary for fit of each iteration
n <-unique(l$Date)
N <- as.numeric(length(n))
set.seed(1)
for (i in 1:N){ #loop through each day 
day_data <- l[l$Date == n[i],] #subset the data for the specific day
I <- day_data$`Light intensity(Lux)`#extract relevant values for the specific day
I0 <- day_data %>% filter(Depth == as.numeric(0))
I0 <- I0$`Light intensity(Lux)`
z <- day_data$Depth
fit <- (nls(I ~ I0 * e^(-K*z), start= list(K=0.1))) #fit data for each specified day (i) to the model using nonlinear squares
K[i] <- (coef(fit))["K"] #store the estimated K(i)
fit_summary[[i]] <- summary(fit) #store the summary of nls fit for every iteration in the list
}

K
fit_summary

#10 Check distribution of K estimates for each day at solar noon 
hist(K, main = "Distribution of K estimates", xlab = "K estimates")

#11 Plotting the measured data vs the function of the light decay with depth using estimated K for each day
depth <- c(-0,-5,-10,-20,-30)
x1 <- median(nl0$`Light intensity(Lux)`)
x2 <- median(nl5$`Light intensity(Lux)`)
x3 <- median(nl10$`Light intensity(Lux)`)
x4 <- median(nl20$`Light intensity(Lux)`)
x5 <- median(nl30$`Light intensity(Lux)`)
I. <- c(x1, x2, x3, x4, x5) #intensity at each depth at noon
color_palette <- colorRampPalette(c("green","blue"))(length(K))

dev.off()
par(mfrow=c(1,1)) #adjust the plot layout
plot(1, type="n", xlim=c(0, max(I.)), ylim=range(depth), xlab="Light Intensity", ylab="Depth") #define margins of the plot
for (i in 1:length(K)) #loop through each day's K estimate and plot the light decay with depth 
  {
  Iest <- I.[1]*e^(K[i]*depth) #calculate light decay for each day using the estimated K(i)
  lines(Iest, depth, type="b", col=color_palette[i], lwd=1)  #plot light decay for each day using the estimated K (i)
  }
lines(I., depth, type="b", col="red", lwd=2) #plot the measured light intensity data
legend("topright", legend=paste("Day", 1:length(K)), col=color_palette)
