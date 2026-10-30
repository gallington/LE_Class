# script to project a matrix of classes in to the future...

setwd("./MarkovLab")  

# load the tally data
# 200 sample points that have been examined and their classifications confirmed.
pnw.data <- read.csv("sample200.csv")  
names(pnw.data)
dim(pnw.data)
sample.n <- dim(pnw.data)[[1]] # 1st dimension, nrow, = N

# set initial conditions:
  # tally for 1972
t1972 <- table(as.factor(pnw.data$X1972)) 
  # proportions of each class in 1972
p1972 <- t1972/sample.n   
  # tally for 1984
t1984 <- table(as.factor(pnw.data$X1984))
  # proportions of each class in 1984
p1984 <- t1984/sample.n
  # etc.
t1991 <- table(as.factor(pnw.data$X1991))
p1991 <- t1991/sample.n
  # creates a vector of the initial proportion of each class
inits <- as.vector(p1972)

# look at the data over time ...
years <- c(1972, 1984, 1991)
pnw.d2 <- rbind(as.vector(p1972), as.vector(p1984), as.vector(p1991))
pnw.d2 <- data.frame(cbind(years, pnw.d2))
  # name the columns, year and class number
########## CHANGE THIS TO BE ACTUAL CLASS NAMES FOR EASE OF INTERPRETATION
names(pnw.d2) <- c("Year","ClearCut","EarlySeral","MidSeral","Mature","OldGrowth")  #NOT TIDY

attach(pnw.d2)
plot(Year,ClearCut, xlim=c(1970,1995), xlab="Year", ylim=c(0,1), ylab="Proportion", type="b", pch=19, col="brown", lwd=2)
lines(Year,EarlySeral,type="b",pch=19,col="gold",lwd=2)
lines(Year,MidSeral,type="b",pch=19,col="light green",lwd=2)
lines(Year,Mature,type="b",pch=19,col="green",lwd=2)
lines(Year,OldGrowth,type="b",pch=19,col="dark green",lwd=2)
legend(x="topright",legend=c("ClearCut","EarlySeral","MidSeral","Mature","OldGrowth"), lwd=2, pch=19, col=c("brown","gold","light green","green","dark green"), bty="n")

# build the transition matrix ...
t.72.84 <- table(pnw.data$X1972,pnw.data$X1984)
t.72.84 # look at it:  the transition tally

# relativize by row sum:
source("rel.R")  # This loads a function contained in a diff script
  # this uses that function
  # need to specify which transition matrix you want, and whether you want to cal by row or column
tr.72.84 <- rel(t.72.84, byrow=TRUE, bycol=FALSE) 

# this is for a 12-yr time step, but we need annual steps ...
  # so load this script as well
source("fix_tm.R")
  # and then use this function, which generates probability of trans
  # again you need to specify which transition matrix you want
  # and also how many years
  # use the one w relativized values.
P <- fix.steps(tr.72.84, 12)

# the other time period, for comparison:
t.84.91 <- table(pnw.data$X1984,pnw.data$X1991)
t.84.91
tr.84.91 <- rel(t.84.91, byrow=TRUE, bycol=FALSE)
P.84.91 <- fix.steps(tr.84.91, 7)
  # annual probability of transition to other class
P.84.91


# project ...
ntypes <- length(inits) # 5 types, in this case

# create some empty data frames for the output 
ntimes <- 999
t <- vector(length=ntimes)
transient <- matrix(nrow=ntimes, ncol=ntypes)
  # let's explore them so we know what we've done:
str(t)
str(transient)

tmp <- inits # copy these to simplify the projections

t[1] <- 1972   # set the start values -- start year &
transient[1,] <- tmp    # start proportions

t1 <- 1972
# the actual projection:
for (i in 1:ntimes) {
	t[i]<-t1+i
	transient[i,]<-tmp %*% P # matrix multiplication in R lingo
	tmp<-transient[i,]
}

# some cosmetics ...
# add the "date" column for convenience:
out <- data.frame(cbind(t, transient)) 
 
# create labels for the output columns:
#names(out)<-c("Year",paste("C",as.character(1:ntypes),sep=""))
names(out)<-c("Year","ClearCut","EarlySeral","MidSeral","Mature","OldGrowth")

# dump the output to a CSV, in case we want to open it in Excel:
write.csv(out,"markov_out.csv")

# plot the output, first for just the initial few years:
plot(out[,1],out[,2],type="l",col="brown", lwd=2, xlim=c(1970,1995),ylim=c(0,0.5),xlab="Year",ylab="Proportion")
lines(out[,1],out[,3],type="l", col="gold", lwd=2)
lines(out[,1],out[,4],type="l", col="light green", lwd=2)
lines(out[,1],out[,5],type="l", col="green", lwd=2)
lines(out[,1],out[,6],type="l", col="dark green", lwd=2)
# put the actual data on top of the model projections:
points(rep(years[1],ntypes), p1972, pch=19, col=c("brown","gold","light green","green","dark green"))
points(rep(years[2],ntypes), p1984, pch=19, col=c("brown","gold","light green","green","dark green"))
points(rep(years[3],ntypes), p1991, pch=19, col=c("brown","gold","light green","green","dark green"))
legend(x="topright",legend=names(out[2:6]), lwd=2, col=c("brown","gold","light green","green","dark green"),pch=19)
title("Model verification and validation")

# plot the entire simulation, so we can see the trend:
plot(out[,1],out[,2],type="l",col="brown", lwd=2, xlim=c(1970,2600),ylim=c(0,0.5),xlab="Year",ylab="Proportion")
lines(out[,1],out[,3],type="l", col="gold", lwd=2)
lines(out[,1],out[,4],type="l", col="light green", lwd=2)
lines(out[,1],out[,5],type="l", col="green", lwd=2)
lines(out[,1],out[,6],type="l", col="dark green", lwd=2)
legend(x="topright",legend=names(out[2:6]),col=c("brown","gold","light green","green","dark green"),lwd=2)
title("Baseline case, projected 1000 yr")

# to save this, use the "File -> Save as ..." menu and dump this as a 
# EMF (for Word) or JPG (for PPT) file.

# - - - - - the rest of this just recycles stuff from above,
# assuming you hand-edit the transition matrices outside of R ...

# try adjusting the harvest rates, by cutting them in half ...
P2 <- P
P[4,1]   # this is the cell for transition from mature forest to clear cut.
h41 <- P[4,1]/2  # divide it in half
P2[4,1] <- h41   # and replace it in the matrix
P2[4,4] <- P[4,4]+h41  # add the remaining to stay mature forest
P[5,1]    # this is the cell for transition from old growth forest to clear cut.
h51 <- P[5,1]/2   # divide it in half
P2[5,1] <- h51   # and replace it in the matrix
P2[5,5] <- P2[5,5]+h51  # add the remaining to stay old growth forest

# RERUN THE PROJECTION W THESE NEW RATES
trans2 <- matrix(nrow=ntimes, ncol=ntypes)
t1<-1972
trans2[1,]<-inits
tmp<-inits
# the projection:
for (i in 1:ntimes) {
	t[i]<-t1+i
	trans2[i,]<-tmp %*% P2 # matrix multiplication in R lingo
	tmp<-trans2[i,]
}
out2 <- data.frame(cbind(t, trans2))
out2<- rbind(c(t1,inits),out2)
names(out2)<-c("time",paste("C",as.character(1:ntypes),sep=""))

# plot the entire simulation:
plot(out2[,1],out2[,2],type="l",col=1, lwd=2, xlim=c(1970,2975),ylim=c(0,0.5),xlab="Year",ylab="Proportion")
lines(out2[,1],out2[,3],type="l", col=2, lwd=2)
lines(out2[,1],out2[,4],type="l", col=3, lwd=2)
lines(out2[,1],out2[,5],type="l", col=4, lwd=2)
lines(out2[,1],out2[,6],type="l", col=5, lwd=2)
legend(x="topright",legend=names(out[2:6]),col=1:5,pch=19)
title("Projection under half the harvesting rate")

# and by cutting them again ... (reuse the code above)

P3 <- P2
# adjust the rates P3[4,1], P3[4,4], P3[5,1], and P3[5,5] as needed

trans3 <- matrix(nrow=ntimes, ncol=ntypes)
t1<-1972
trans3[1,]<-inits
tmp<-inits
# the projection:
for (i in 1:ntimes) {
	t[i]<-t1+i
	trans3[i,]<-tmp %*% P3 # matric multiplication in R lingo
	tmp<-trans3[i,]
}
out3 <- data.frame(cbind(t, trans3))
out3 <- rbind(c(t1,inits),out3)
names(out)<-c("time",paste("C",as.character(1:ntypes),sep=""))

# plot the entire simulation, so we can see the trend:
plot(out3[,1],out3[,2],type="l",col=1, lwd=2, xlim=c(1970,2975),ylim=c(0,0.5),xlab="Year",ylab="Proportion")
lines(out3[,1],out3[,3],type="l", col=2, lwd=2)
lines(out3[,1],out3[,4],type="l", col=3, lwd=2)
lines(out3[,1],out3[,5],type="l", col=4, lwd=2)
lines(out3[,1],out3[,6],type="l", col=5, lwd=2)
legend(x="topright",legend=names(out[2:6]),col=1:5,pch=19)
title("Projection under 1/4th the harvesting rate")

# look more closely at old-growth when you're satisfied ...
plot(out[,1],out[,6],type="l",col=1, lwd=2, xlim=c(1970,2020),ylim=c(0,0.5),xlab="Year",ylab="Proportion Old Growth")
lines(out2[,1],out2[,6],type="l",col=2, lwd=2)
lines(out3[,1],out3[,6],type="l",col=3, lwd=2)
legend(x="bottomleft",legend=c("1972-84 rate","50% rate","25% rate"),col=1:3,lwd=2)
title("Old growth sustainability?")

# end of markov 

