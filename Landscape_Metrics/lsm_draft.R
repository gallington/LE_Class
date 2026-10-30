check_landscape()
list_lsm()
calculate_lsm()

calculate_lsm(
  mad1.R,
  level = c("class","landscape"),
  metric = c(
    "pland",
    "lpi",
    "contag",
    "enn",
    "edge",
    "shape"
  )
)

# to combine results from multiple landscapes:
results <- bind_rows(
  calc_land1,
  calc_land2,
  calc_land3,
  calc_land4
)

ggplot(results,
       aes(metric, value, fill = landscape)) +
  geom_col(position="dodge")

#### Generating all the steps the students need to do so I can check their outputs:

###
# Madison 1 Landscape
###
mad2.R<-terra::rast("data/mad2.tif") #load the terra::rast
mad2.R #check some info on it
terra::plot(mad2.R, #this should match the figure in the LEE book
            type = "classes")
terra::plot(mad2.R, type = "classes")
#Contagion
cont.mad2<-lsm_l_contag(mad2.R, verbose = TRUE) 
cont.mad2 #note, the value returned is percent

#patch density
pd.mad2<-lsm_l_pd(mad2.R, direction=8) #units = number per 100 ha
pd.mad2

#edge density
ed.mad2<-lsm_l_ed(mad2.R) #meters per ha
ed.mad2

#landscape shape index 
lsi.mad2<-lsm_l_lsi(mad2.R) #no units
lsi.mad2

#largest patch index  
lpi.mad2<-lsm_l_lpi(mad2.R, direction=8) #units percentage
lpi.mad2

#patch richness  
# what is 'patch richness', you ask? you can learn more like this:
?lsm_l_pr()

pr.mad2<-lsm_l_pr(mad2.R)
pr.mad2

#wrap the variables above into a table that we'll use later
mad2.DF<-rbind(cont.mad2, pd.mad2, ed.mad2, lsi.mad2, lpi.mad2, pr.mad2)
#rename the value column to mad2 - so we can delineate
mad2.DF<-mad2.DF %>% 
  rename(mad2 = value) 
#round columns to include 2 sig digits for clarity
mad2.DF$mad2<-round(mad2.DF$mad2, digits=2) 

######
# Mad2 Landscape:
#############
mad2.R<-terra::rast("data/mad2.tif") #load the terra::rast
mad2.R #check some info on it
terra::plot(mad2.R, type = "classes") #this should match the figure in the LEE book

#Contagion
cont.mad2<-lsm_l_contag(mad2.R, verbose = TRUE) 
cont.mad2 #note, the value returned is percent

#patch density
pd.mad2<-lsm_l_pd(mad2.R, direction=8) #units = number per 100 ha
pd.mad2

#edge density
ed.mad2<-lsm_l_ed(mad2.R) #meters per ha
ed.mad2

#landscape shape index 
lsi.mad2<-lsm_l_lsi(mad2.R) #no units
lsi.mad2

#largest patch index  
lpi.mad2<-lsm_l_lpi(mad2.R, direction=8) #units percentage
lpi.mad2

#patch richness  
# what is 'patch richness', you ask? you can learn more like this:
?lsm_l_pr()

pr.mad2<-lsm_l_pr(mad2.R)
pr.mad2

#wrap the variables above into a table that we'll use later
mad2.DF<-rbind(cont.mad2, pd.mad2, ed.mad2, lsi.mad2, lpi.mad2, pr.mad2)
#rename the value column to mad2 - so we can delineate
mad2.DF<-mad2.DF %>% 
  rename(mad2 = value) 
#round columns to include 2 sig digits for clarity
mad2.DF$mad2<-round(mad2.DF$mad2, digits=2) 

##################
# NE LANDSCAPE 1:
################
ne1.R<-terra::rast("data/ne1.tif") #load the terra::rast
ne1.R #check some info on it
terra::plot(ne1.R, type = "classes") #this should match the figure in the LEE book

#Contagion
cont.ne1<-lsm_l_contag(ne1.R, verbose = TRUE) 
cont.ne1 #note, the value returned is percent

#patch density
pd.ne1<-lsm_l_pd(ne1.R, direction=8) #units = number per 100 ha
pd.ne1

#edge density
ed.ne1<-lsm_l_ed(ne1.R) #meters per ha
ed.ne1

#landscape shape index 
lsi.ne1<-lsm_l_lsi(ne1.R) #no units
lsi.ne1

#largest patch index  
lpi.ne1<-lsm_l_lpi(ne1.R, direction=8) #units percentage
lpi.ne1

#patch richness  
# what is 'patch richness', you ask? you can learn more like this:
?lsm_l_pr()

pr.ne1<-lsm_l_pr(ne1.R)
pr.ne1

#wrap the variables above into a table that we'll use later
ne1.DF<-rbind(cont.ne1, pd.ne1, ed.ne1, lsi.ne1, lpi.ne1, pr.ne1)
#rename the value column to ne1 - so we can delineate
ne1.DF<-ne1.DF %>% 
  rename(ne1 = value) 
#round columns to include 2 sig digits for clarity
ne1.DF$ne1<-round(ne1.DF$ne1, digits=2) 

############
# NE 2
##########
ne2.R<-terra::rast("data/ne2.tif") #load the terra::rast
ne2.R #check some info on it
terra::plot(ne2.R, type = "classes") #this should match the figure in the LEE book

#Contagion
cont.ne2<-lsm_l_contag(ne2.R, verbose = TRUE) 
cont.ne2 #note, the value returned is percent

#patch density
pd.ne2<-lsm_l_pd(ne2.R, direction=8) #units = number per 100 ha
pd.ne2

#edge density
ed.ne2<-lsm_l_ed(ne2.R) #meters per ha
ed.ne2

#landscape shape index 
lsi.ne2<-lsm_l_lsi(ne2.R) #no units
lsi.ne2

#largest patch index  
lpi.ne2<-lsm_l_lpi(ne2.R, direction=8) #units percentage
lpi.ne2

#patch richness  
# what is 'patch richness', you ask? you can learn more like this:
?lsm_l_pr()

pr.ne2<-lsm_l_pr(ne2.R)
pr.ne2

#wrap the variables above into a table that we'll use later
ne2.DF<-rbind(cont.ne2, pd.ne2, ed.ne2, lsi.ne2, lpi.ne2, pr.ne2)
#rename the value column to ne2 - so we can delineate
ne2.DF<-ne2.DF %>% 
  rename(ne2 = value) 
#round columns to include 2 sig digits for clarity
ne2.DF$ne2<-round(ne2.DF$ne2, digits=2) 

#######
# NE3
#######
ne3.R<-terra::rast("data/ne3.tif") #load the terra::rast
ne3.R #check some info on it
terra::plot(ne3.R, type = "classes") #this should match the figure in the LEE book

#Contagion
cont.ne3<-lsm_l_contag(ne3.R, verbose = TRUE) 
cont.ne3 #note, the value returned is percent

#patch density
pd.ne3<-lsm_l_pd(ne3.R, direction=8) #units = number per 100 ha
pd.ne3

#edge density
ed.ne3<-lsm_l_ed(ne3.R) #meters per ha
ed.ne3

#landscape shape index 
lsi.ne3<-lsm_l_lsi(ne3.R) #no units
lsi.ne3

#largest patch index  
lpi.ne3<-lsm_l_lpi(ne3.R, direction=8) #units percentage
lpi.ne3

#patch richness  
# what is 'patch richness', you ask? you can learn more like this:
?lsm_l_pr()

pr.ne3<-lsm_l_pr(ne3.R)
pr.ne3

#wrap the variables above into a table that we'll use later
ne3.DF<-rbind(cont.ne3, pd.ne3, ed.ne3, lsi.ne3, lpi.ne3, pr.ne3)
#rename the value column to ne3 - so we can delineate
ne3.DF<-ne3.DF %>% 
  rename(ne3 = value) 
#round columns to include 2 sig digits for clarity
ne3.DF$ne3<-round(ne3.DF$ne3, digits=2) 




# calcs at diff scales:
 lsm_c_ai(ne1.R)
 lsm_l_ai(ne1.R) 
 