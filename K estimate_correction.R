setwd("C:/Users/Antonija/OneDrive - Prirodoslovno-matematički fakultet/ISLAND/Primarna proizvodnja/Modeliranje/Svjetlost")

#1. install packages
install.packages("tidyverse")
install.packages("tibble")
install.packages("readr")
install.packages("dichromat")
install.packages("dplyr")
install.packages("SciViews")

#2. load packages
library(tidyverse)
library(tibble)
library(readr)
library(dichromat)
library(patchwork)
library(dplyr)
library(SciViews)

#3. import data
#Light intensity
l0 <- readr::read_delim("light0.csv", delim =",", show_col_types = FALSE) #light intensity at surface
l5 <- readr::read_delim("light5.csv", delim =",", show_col_types = FALSE) #light intensity at 5 meters
l10 <- readr::read_delim("light10.csv", delim =",", show_col_types = FALSE) #l. int. at 10 m
l20 <- readr::read_delim("light20.csv", delim =",", show_col_types = FALSE) #l. int. at 20 m
l30 <- readr::read_delim("light30.csv", delim =",", show_col_types = FALSE) #l. int. at 30 m

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

#d create a list
v0 <- nl0[4] #vectors for the data set
v5 <- nl5[4]
v10 <- nl10[4]
v20 <- nl20[4]
v30 <- nl30[4]

#e filter the timeseries for noon to separate the days

l <- bind_rows( #binding data
  mutate(v0, Depth = 0, Date = nl0$`Date Time(GMT+02:00)`),
  mutate(v5, Depth = 5, Date = nl5$`Date Time(GMT+02:00)`),
  mutate(v10, Depth = 10, Date = nl10$`Date Time(GMT+02:00)`),
  mutate(v20, Depth = 20, Date = nl20$`Date Time(GMT+02:00)`),
  mutate(v30, Depth = 30, Date = nl30$`Date Time(GMT+02:00)`)) 
l <- as.data.frame(l)
#plot
dev.off()
plot(l$`Light intensity(Lux)`, -l$Depth)

#5. define function for non linear squares
I ~ I0 * e^(-K*z)
e <- exp(1) #euler's number
fit_summary <- list() #store summary for fit of each iteration
n <-unique(l$Date)
N <- as.numeric(length(n))
K <- numeric(length(N))

#6 Estimating K using nls (nonlinear squares) analysis for each day
set.seed(1)
#loop through each day 
for (i in 1:N){ 
  day_data <- l[l$Date == n[i],] #subset the data for the specific day
  I <- day_data$`Light intensity(Lux)`#extract relevant values for the specific day
  I0 <- day_data %>% filter(Depth == as.numeric(0))
  I0 <- I0$`Light intensity(Lux)`
  z <- day_data$Depth
  fit <- (nls(I ~ I0 * e^(-K*z), start= list(K=0.1))) #fit data for each specified day (i) to the model using nonlinear squares
  K[i] <- (coef(fit))["K"] #store the estimated K(i)
  fit_summary[[i]] <- summary(fit) #store the summary of nls fit for every iteration in the list
}
#print values
K
fit_summary

#10 Check distribution of K estimates for each day at solar noon 
hist(K, main = "Distribution of K estimates (solar noon)", xlab = "K estimates")

#11 Plotting the measured data vs the function of the light decay with depth using estimated K for each day
depth <- c(-0,-5,-10,-20,-30)
color_palette <- colorRampPalette(c("green","blue"))(length(K))

dev.off()
par(mfrow=c(1,1)) #adjust the plot layout
plot(1, type="n", xlim=c(0, max(subset_data$`Light intensity(Lux)`)), ylim=range(depth), xlab="Light Intensity at solar noon", ylab="Depth") #define margins of the plot
legend("topright", legend=paste("Day", 1:length(K)), col=color_palette, lwd=2)
for (i in 1:length(K)) #loop through each day's K estimate and plot the light decay with depth 
{
  Iest <- I[1]*e^(K[i]*depth) #calculate light decay for each day using the estimated K(i)
  lines(Iest, depth, type="b", col=color_palette[i], lwd=1)  #plot light decay for each day using the estimated K (i)
}
for (i in 1:length(n)) 
{ subset_data <- l[l$Date == n[i],] #subset the data for the current date
lines(subset_data$`Light intensity(Lux)`, depth, lty=2, lwd=2, col=color_palette[i])  #plot the measured light intensity data for the current date
}

#CORRECTION- 5 METERS AS A SURFACE VALUE 

#4. organize data 
l1 <- bind_rows( #binding data
  mutate(v5, Depth = 5, Date = nl5$`Date Time(GMT+02:00)`),
  mutate(v10, Depth = 10, Date = nl10$`Date Time(GMT+02:00)`),
  mutate(v20, Depth = 20, Date = nl20$`Date Time(GMT+02:00)`),
  mutate(v30, Depth = 30, Date = nl30$`Date Time(GMT+02:00)`)) 
l1 <- as.data.frame(l1)
#plot
dev.off()
plot(l1$`Light intensity(Lux)`, -l1$Depth)

#5. define function for non linear squares
I ~ I0 * e^(-K*z)
e <- exp(1) #euler's number
fit_summary1 <- list() #store summary for fit of each iteration
n1 <-unique(l1$Date)
N1 <- as.numeric(length(n1))
K1 <- numeric(length(N1))

#6 Estimating K using nls (nonlinear squares) function for each day
set.seed(1)
#loop through each day 
for (i in 1:N1){ 
  day_data1 <- l1[l1$Date == n1[i],] #subset the data for the specific day
  I1 <- day_data1$`Light intensity(Lux)`#extract relevant values for the specific day
  I01 <- day_data1 %>% filter(Depth == as.numeric(5))
  I01 <- I01$`Light intensity(Lux)`
  z1 <- day_data1$Depth
  fit <- (nls(I1 ~ I01 * e^(-K1*z1), start= list(K1=0.1))) #fit data for each specified day (i) to the model using nonlinear squares
  K1[i] <- (coef(fit))["K1"] #store the estimated K(i)
  fit_summary1[[i]] <- summary(fit) #store the summary of nls fit for every iteration in the list
}
#print values
K1
fit_summary1

#10 Check distribution of K estimates for each day at solar noon 
hist(K1, main = "Distribution of K estimates (solar noon)", xlab = "K estimates")

#11 Plotting the measured data vs the function of the light decay with depth using estimated K for each day
depth1 <- c(-5,-10,-20,-30)
color_palette <- colorRampPalette(c("green","blue"))(length(K1))

dev.off()
par(mfrow=c(1,1)) #adjust the plot layout
plot(1, type="n", xlim=c(0, max(l1$`Light intensity(Lux)`)), ylim=range(depth1), xlab="Light Intensity at solar noon", ylab="Depth") #define margins of the plot
legend("topright", legend=paste("Day", 1:length(K1)), col=color_palette, lwd=2)
for (i in 1:length(K1)) #loop through each day's K estimate and plot the light decay with depth 
{
  Iest <- I1[1]*e^(K1[i]*depth1) #calculate light decay for each day using the estimated K(i)
  lines(Iest, depth1, type="b", col=color_palette[i], lwd=1)  #plot light decay for each day using the estimated K (i)
}
for (i in 1:length(n1)) 
{ subset_data1 <- l1[l1$Date == n1[i],] #subset the data for the current date
lines(subset_data1$`Light intensity(Lux)`, depth1, lty=2, lwd=2, col=color_palette[i])  #plot the measured light intensity data for the current date
}

#STORING MODEL STATISTICS
#initialize empty vectors to store the values
K_estimate <- numeric()
std_error <- numeric()
t_value <- numeric()
p_value <- numeric()
residual_std_errors <- numeric()
#iterate over fit_summary and extract the values
for (i in 1:length(fit_summary)) {
  summary <- fit_summary[[i]]
  K_estimate <- c(K_estimate, summary$parameters["K", "Estimate"])
  std_error <- c(std_error, summary$parameters["K", "Std. Error"])
  t_value <- c(t_value, summary$parameters["K", "t value"])
  p_value <- c(p_value, summary$parameters["K", "Pr(>|t|)"])
  residual_std_errors[i] <- fit_summary[[i]]$sigma
}
#create a data frame
fit_summary_df <- data.frame(
  Day = n,
  K_Estimate = K_estimate,
  Std_Error = std_error,
  t_value = t_value,
  p_value = p_value,
  Residual_Std_Error = residual_std_errors)
#save the data frame in table 
writexl::write_xlsx(fit_summary_df, "fit_summary-solar noon.xlsx")

#initialize empty vectors to store the values
K_estimate <- numeric()
std_error <- numeric()
t_value <- numeric()
p_value <- numeric()
residual_std_errors <- numeric()
#iterate over fit_summary and extract the values
for (i in 1:length(fit_summary1)) {
  summary <- fit_summary1[[i]]
  K_estimate <- c(K_estimate, summary$parameters["K1", "Estimate"])
  std_error <- c(std_error, summary$parameters["K1", "Std. Error"])
  t_value <- c(t_value, summary$parameters["K1", "t value"])
  p_value <- c(p_value, summary$parameters["K1", "Pr(>|t|)"])
  residual_std_errors[i] <- fit_summary.1[[i]]$sigma
}
#create a data frame
fit_summary1_df <- data.frame(
  Day = n,
  K_Estimate = K_estimate,
  Std_Error = std_error,
  t_value = t_value,
  p_value = p_value,
  Residual_Std_Error = residual_std_errors)
#save the data frame in table 
writexl::write_xlsx(fit_summary1_df, "fit_summary1-solar noon.xlsx")

n
#ESTIMATE WITHOUT NLS
l2<- l %>% filter(Date == "07/06/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/07/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/08/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/09/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/10/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth

Kd

l2<- l %>% filter(Date == "07/11/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/12/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/13/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/14/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/15/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/16/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/17/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/18/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2 <- l %>% filter(Date == "07/19/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/20/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

l2<- l %>% filter(Date == "07/21/23 01:00:00 PM")
I <- l2 %>% filter(Depth == as.numeric(10))
I <- I$`Light intensity(Lux)`
I0 <- l2 %>% filter(Depth == as.numeric(5))
I0 <- I0$`Light intensity(Lux)` 
z <- l2 %>% filter(Depth == as.numeric(5))
z <- z$Depth
Kd <- ln(I/I0) * -(1/z) #fit data

Kd

#07/06/23 01:00:00 PM - Kd = 0.17
#07/07/23 01:00:00 PM - Kd = 0.14
#07/08/23 01:00:00 PM - Kd = 0.25
#07/09/23 01:00:00 PM - Kd = -0.02
#07/10/23 01:00:00 PM - Kd = -0.02
#07/11/23 01:00:00 PM - Kd = 0.06
#07/12/23 01:00:00 PM - Kd = 0.08
#07/13/23 01:00:00 PM - Kd = 0.09
#07/14/23 01:00:00 PM - Kd = 0.05
#07/15/23 01:00:00 PM - Kd = 0.02
#07/16/23 01:00:00 PM - Kd = 0.08
#07/17/23 01:00:00 PM - Kd = 0.03
#07/18/23 01:00:00 PM - Kd = 0
#07/20/23 01:00:00 PM - Kd = 0.14
#07/21/23 01:00:00 PM - Kd = 0.05