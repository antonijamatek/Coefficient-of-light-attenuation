# Coefficient of light attenuation (K)
This repository containts scripts for estimating coefficient of light attenuation (K) by fitting in situ measured light intensity data to daily light function (I=I0*e^(-K *z)) using nonlinear least squares method (determine the nonlinear (weighted) least-squares estimates of the parameters of a nonlinear model).

In situ light intensity data is retreaved from data loggers that were mounted at the cliffs of Lastovo Island in South Adriatic Sea. They were deployed at surface (0 m), 5 m, 10 m, 20 m and 30 m. and measuring in the period from 6th to 21st July. 

This is a process of inspecting K values at South Adriatic Sea using in situ light intensity measurements, and to choose single value that will be most representative for usage in further models (specifically primary productivity models)

# Script estimate.K
Data filtered for solar noon was used to estimate K for each day of light measurements, estimated K were used to model light profiles for each day,
and they were plotted versus measured light profile (median light intensity of all days for each depth was taken) 

# Script estimate.K- normalized data
Measured light data was normalized to 1 for better comparison of different K estimated for each day at solar noon.
The same process as described above was implemented. It was repeated for data filtered for 10:00AM and 04:30PM.

# Script K distribution - mean and median
Distribution of K estimates is looked at for each time, and the mean and median was calculated. 
Light profile is modeled using single K values for each time (morning, solar noon, and afternoon), and plotted versus in situ measured light profile (median of light intensity data for all days at each time period was taken as representative)

# Script storing K fit statistics summary
Storing the model fit statistics to the table - standard error, t value, p value, residual standard error in order to examine if the model was a good fit to the data, and to test statistical significance of K estimates. 

# Script storing K estimate_correction.R
Workflow without normalizing dataset, and using solar noon dataset only. 
