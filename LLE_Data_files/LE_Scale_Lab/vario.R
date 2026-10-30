
# Scale Detection Using Semivariograms and Autocorrelograms 
# M.W. Palmer, D.J. McGlinn
# this R code is written for the provided data on vegetation height (see Option 2 in Chapter exercise)

################################################################################
# DATA COLLECTION
################################################################################

# Option 2: Using Provided Data Sets
# Set R's working directory to the folder where you have stored the relevant data files

  setwd("/Users/gallington/Desktop/LE_Scale_Lab")

# In a Windows OS environment the path address starts with "C:\?". 
# Note that R does not accept single forward slashes in a path address.
# Therefore, you need to write the path address either with forward slashes (e.g. "C:/"), or   
# with double backward slashes (e.g. "C:\\...)

# read in the .csv files you have prepared in the working directory
# this dataframe contains veg height data from three sites
# for the third site there are data from five different pastures within that site.
  veght<-read.table("veght_Chp5_data.csv",sep=',',header=T)
  names(veght) #this gives the names (column headings) of the veght data.frame
  ##############################
  #Column descriptions for veght
  #"Pos": Transect position (m)
  #"TP": Tallgrass Prairie (Osage County, Oklahoma)
  #"MF": Mowed field (Oklahoma State Univ. Campus)
  #"Pasture.A to Pasture.E: Five savanna pastures in Payne County, Oklahoma
  ##############################

################################################################################
# DATA EXPLORATION
################################################################################
# begin with some exploration of the data 
# NOTE: the following data exploration is not described in the chapter text
# but is recommended when starting with any new dataset

  str(veght) # examine the structure of the data
 
  head(veght) # the first six rows of the data

# Examine the data visually for one of the transects (e.g., Tallgrass Prairie (TP) dataset)
  plot(veght$Pos,veght$TP,
       xlab='Transect Position (m)',
       ylab='Vegetation Height (cm)',
       main='Tallgrass Prairie',type='o')

  dev.off() # close graphical device

# compute a variogram cloud for the Tallgrass Prairie 
  lag <- dist(veght$Pos) # 'dist' computes the Euclidean distances as the default
  varTP <- dist(veght$TP)^2*.5
  plot(lag,varTP,xlab='Spatial lag (m)',ylab='Semivariance of Veg. Ht. (cm^2)')
# add a smoothing line to examine the average pattern of semivariance as a function of distance
  lines(lowess(lag,varTP),col='red',lwd=2)
# a rule of thumb is to not interpret beyond 1/2 of the spatial extent of the dataset
  abline(v=max(lag)/2,lwd=2,lty=2,col='dodgerblue')
# add a legend to the topleft corner of the plot
  legend("topleft",c('lowess line','half max extent'),lty=c(1,2),col=c('red','dodgerblue'),bty='n',lwd=2) 

  dev.off()

# examine the variogram clouds and smoothers for all the datasets simultaneously
  windows(width=7*4,height=7*2) # this command is not necessary but makes the plots more readable
  par(mfrow=c(2,4)) # set up a graphical matrix with 2 rows and 4 columns
  for(i in 2:8){ # this is a 'for loop'
   # for these we'll just compute semivariance on the fly rather than assign it to a particular object, with larger datasets this may not be optimal
   plot(lag,dist(veght[,i])^2*.5,xlab='Spatial lag (m)',ylab='Semivariance of Veg. Ht. (cm^2)',main=paste(names(veght)[i]))
   lines(lowess(lag,dist(veght[,i])^2*.5),col='red',lwd=2)
   abline(v=max(lag)/2,lwd=2,col='dodgerblue',lty=2)
   if(i == 2) 
    legend("topleft",c('lowess line','half max extent'),lty=c(1,2),col=c('red','dodgerblue'),bty='n',lwd=2)
  }
# already we can see some pretty striking differences between the different fields

  dev.off()

################################################################################
# DATA ANALYSIS 
################################################################################

# now let's compute a variogram for the Tallgrass Prairie veg. hts.
#the variogram is simply the average of the variogram cloud for particular spatial lags
  lag <- dist(veght$Pos) 
  varTP <- dist(veght$TP)^2*.5
# split the seminvariance values up depending on what spatial lag they occur at    
  TP.split<-split(varTP,lag)        
# compute the mean semivariance at each spatial lag
  TP.gamma <- sapply(TP.split,mean) 
# examine TP.gamma, the indexing names are the spatial lag at which that semivariance value applies to
  TP.gamma[1:5]
# extract the values of the unique spatial lags, in this case our lags are simply 1:199 but in other contexts (not considered in this example)
# the spatial lags may be more complex and this step will be more relevant
  lag.uni<- as.numeric(names(TP.gamma))

# visually examine the variogram for the Tallgrass Prairie (NOTE: not in chapter text)
  plot(lag.uni,TP.gamma,xlab='Spatial Lag (m)',ylab='Semivariance of Veg. Ht. (cm^2)',type='o') 
  abline(v=max(lag)/2,lwd=2,col='dodgerblue',lty=2)

  dev.off()

# compute Autocorrelogram for the Tallgrass Prairie
# first compute the autocorrelation at a lag of 1 for this dataset, note that veght$TP has 200 samples
  cor(veght$TP[1:199],veght$TP[2:200])
# now at a lag of 2
  cor(veght$TP[1:198],veght$TP[3:200])
# now we can continue to compute the autorcorrelation for each lag as such or we can write a function
# that will accomplish for us, below is such a function:

  a.cor<-function(x){
   ## This function computes autocorrelation of the variable x.
   ## It is assumed that x is sampled along a linear, uniformly-spaced transect.
   ## This function is not appropriate for computing spatial autocorrelation for 
   ## more complex spatial arrangements of data.
   len<-length(x) # first compute the total size of x  
   r<-rep(NA,len-2) # create an empty output variable
   for(h in 1:(len-2)){ # loop through all the possible spatial lags (h)
     r[h] <- cor(x[1:(len-h)],x[(1+h):len]) # compute the autocorrelation at each lag
   }
   r # print the output variable
  }
 
# now that we have our function we can compute the autorcorrelation for all possible lags
  correl<-a.cor(veght$TP)

# plot the autocorrelogram for the Tallgrass Prairie (NOTE: not in chapter text)
  plot(1:198,correl,xlab='Spatial lag (m)',ylab='Autocorrelation',main='Tallgrass Prairie',type='o')
  abline(v=100,col='dodgerblue',lty=2,lwd=2)
  abline(h=0)

 dev.off()

################################################################################
# RESULTS
################################################################################

# plot side-by-side plots of vegetation height as a function of spatial position 
# and the semivariogram and correlogram as a function of spatial lag
# because we only are interested in spatial lags less than half of the max distance we
# will only plot lags from 1 to 100 m

  par(mfrow=c(3,7))
  for(i in 2:8)
   plot(1:200,veght[,i],xlab='Transect Position (m)',ylab='Veg. Ht. (cm)',main=paste(names(veght)[i]),type='o')
  for(i in 2:8){
   X.split<-split(dist(veght[,i])^2*.5,lag) # split the seminvariance values up depending on what spatial lag they occur at        
   X.gamma <- sapply(X.split,mean) # compute the mean semivariance at each spatial lag
   plot(1:100,X.gamma[1:100],xlab='Spatial Lag (m)',ylab='Semivariance',type='o')
  }
  for(i in 2:8){
   plot(1:100,a.cor(veght[,i])[1:100],xlab='Spatial Lag (m)',ylab='Autocorrelation',type='o')
   abline(h=0)
  }
# ignore the warnings from R that the standard deviation is zero

  dev.off()

################################################################################
# OPTIONAL EXERCISES
################################################################################
# EXERCISE 1
# create a .csv file of the hypothetical variables and read them into R
  hyp.dat<-read.table("hypo_ex.csv",sep=",",header=TRUE)
# examine structure of the data
  str(hyp.dat)
# attach the dataset for easier manipulation
  attach(hyp.dat) 

# Question 1: How do these variables differ?
  z1[1:10]
  z2[1:10]
  summary(z1)
  summary(z2)
  par(mfrow=c(1,3))
   boxplot(cbind(z1,z2))
   plot(position,z1,type='o')
   plot(position,z2,type='o')

# Question 2. How do you expect their semivariograms and autocorrelograms to differ?

# Question 3. Examine the semivariogram and autocorrelogram. Now repeat with the second variable.

  lag<-dist(position)
  var.z1<-dist(z1)^2*.5
  var.z2<-dist(z2)^2*.5
  gamma.z1<-sapply(split(var.z1,lag),mean)
  gamma.z2<-sapply(split(var.z2,lag),mean)
  correl.z1<-a.cor(z1)
  correl.z2<-a.cor(z2) 
  par(mfrow=c(2,2))
   plot(1:100,gamma.z1[1:100],xlab='lag',type='o')
   plot(1:100,gamma.z2[1:100],xlab='lag',type='o')
   plot(1:100,correl.z1[1:100],xlab='lag',type='o')
   plot(1:100,correl.z2[1:100],xlab='lag',type='o')
  
# Were you right in your answer to the previous question?
  dev.off()

 #EXERCISE 2
  #Plot your semivariogram on a double logarithmic scale.
  #there are two simple ways to do this
   par(mfrow=c(1,2))
   #1) transform the scaling of the axes
    plot(1:100,gamma.z1[1:100],log='xy',xlab='lag',type='o')
   #2) or transform the scaling of the variables
    plot(log10(1:100),log10(gamma.z1[1:100]),type='o')  
   #both methods give identical looking patterns

# Question 1. Is the semivariogram a straight line? 

# Question 2. Are there multiple plateaus? 

# Question 3. Would you predict that most spatial patterns in nature are self-similar, hierarchical, or neither?

 dev.off()
