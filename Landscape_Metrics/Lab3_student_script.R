# Title:  Lab3_script.R
# 17 Sept 2026
# 
# code to needed to run the lab in the second half of LLE Ch.4 
# The original is designed to run in a program called Fragstats in the LEE textbook
# This version is converted to use the *landscapemetrics* package instead.
# Updated from script originally by Tim Assal

# I suggest you make a copy of this script so you'll have the original 
# after you make changes


#load the packages you'll need
# library(raster) # replaced by terra
library(landscapemetrics)
library(tidyverse)
library(terra)

### 
# !!! Be sure to set up an .RProj file for Lab3 (I gave you one)
# once you do that, your working directory should be set to Lab3
# This is KEY, along with proper data management
getwd() #check the location of your working dir
# if you can't remmeber how to change your working directory, look at your script from last time.

############
# Exploring the package
##########

# Go to Packages/landscapemetrics and select
# This will open the Help page
# from the index on teh HElp page you can scroll down to see all of the functions

# We can also look at it in the console 
# list all available metrics
list_lsm()

# but it's a lot... so sometimes easier to preview in chunks.

# list only aggregation metrics at landscape level 
list_lsm(level = "landscape",            # change this to the level you want: patch/class/landscape
         type = "aggregation metric")    # change to 

# you could, if you wish, run all of them at once: 
# calculate all metrics on patch level
# *****CAUTION: If you have a big landscape this will take a long time 
# so don't do it right now! *********
calculate_lsm(landscape, level = "patch") # just specify which level you want

# From the Help page you can also select:
# Help/User_guides


# Ok, now on to the lab:

# First we'll recreate what we did last Thursday

#################################
##  LLE Ch.4 Part 3    ##
#################################

########
# Landscape Metrics using the 4-neighbor rule
########

###
# Early-settlement landscape
###

# Load the raster of the early settlement landscape
esett.R <- terra::rast("data/esett.tif")
esett.R #check some info on it
esett.R <- as.factor(esett.R)
terra::plot(esett.R) #this should match the figure in the LEE book, just with different colors

## Calculate landscape metrics
# we'll calculate the same ones we did by hand:
# # of patches, mean patch size, SHEI and contagion

#Note: we'll write each of the values to an object, so we can call them later to wrap into a table

##First use the 4 neighbor rule##
#Number of patches (note: .E4 indicates early settlement, 4 neighbors)

np.E4 <- lsm_l_np(esett.R,      # early settlement landscape file   
                  directions=4) # 4-neighbor rule
np.E4 #show the value


# Is it any different if we specify class scale instead of landscape?
np_c.E4 <- lsm_c_np(esett.R, directions=4)
np_c.E4 # show the results

#if you'd like to get a visual of the patches by cover type
show_patches(esett.R, class = "all", directions=4, labels = TRUE) 

# how does this vary if we change the neighbor rule?
np.E8 <- lsm_l_np(esett.R, directions=8)
np.E8 #show the value
show_patches(esett.R, class = "all", directions=8, labels = TRUE) 

####### Now the rest of the calculations:
#Mean patch size - across all cover types
mps.E4<-lsm_l_area_mn(esett.R, directions = 4) #note: output is in hectares
mps.E4

#Shannon's eveness index
shei.E4<-lsm_l_shei(esett.R) 
shei.E4

#Contagion
cont.E4<-lsm_l_contag(esett.R, verbose = TRUE) 
cont.E4 #note, the value returned is percent

# NOTE:
# for help on any function, simply type
?lsm_l_contag #or whichever one you want to know more about

#Compare these calcs with the hand calcs you made earlier. 
#note: the contagion value is a little different than the hand calculation 

#wrap the variables above into a table that we'll use later
early4.DF<-rbind(np.E4, mps.E4, shei.E4, cont.E4)
#rename the value column to EarlyPeriod - so we can delineate
early4.DF<-early4.DF %>% 
  rename(EarlyPeriod = value) 

#have a look at your new table:
view(early4.DF)

###
#End Early-settlement landscape
### 

###
#Post-settlement landscape
###
#load the raster
psett.R <- terra::rast("data/psett.tif")
psett.R #check some info on it
psett.R <- as.factor(psett.R)
terra::plot(psett.R) #this should match the figure in the LEE book

##Calculate landscape metrics
# we'll calculate the # of patches, mean patch size, SHEI and contagion

##First use the 4 neighbor rule##
#Number of patches (note: .P4 indicates post settlement, 4 neighbors)
np.P4<-lsm_l_np(psett.R, directions=4)
np.P4 #show the value
#if you'd like to get a visual of the patches by cover type
show_patches(psett.R, class = c(1:3),directions=4, labels = TRUE) 

#Mean patch size - across all cover types
mps.P4<-lsm_l_area_mn(psett.R, directions = 4) #note: output is in hectares
mps.P4

#Shannon's eveness index
shei.P4<-lsm_l_shei(psett.R) 
shei.P4

#Contagion
cont.P4<-lsm_l_contag(psett.R, verbose = TRUE) 
cont.P4 #note, the value returned is percent

#wrap the variables above into a table that we'll use later
post4.DF<-rbind(np.P4, mps.P4, shei.P4, cont.P4)
#rename the value column to PostPeriod - so we can delineate
post4.DF<-post4.DF %>% 
  rename(PostPeriod = value) 

###
#End Post-settlement landscape
### 

#Now join the early and late dataframes to create one 4-neighbor dataframe
full4.DF<-early4.DF %>% 
  full_join(post4.DF) %>% 
  mutate(NeighborRule= 4)

#round EarlyPeriod and PostPeriod columns to include 3 sig digits for clarity
full4.DF$EarlyPeriod<-round(full4.DF$EarlyPeriod, digits=3) 
full4.DF$PostPeriod<-round(full4.DF$PostPeriod, digits=3) 

#have a look:
view(full4.DF)

#let's drop columns we don't need
# Note: you only don't need these now bc you calculated this at the landscape scale!
# If you used a class scale you would get totals for each class
full4.DF<-full4.DF %>% 
  select(-layer, -class, -id)

#have a look now, better
view(full4.DF)

#this is how you would export the dataframe to a table (csv)
# not totally necessary right now, but giving you this so you know how to save results!
# you would need to specify the folder you want to save it to
write_csv(full4.DF, "outputs/part3_4neighbors.csv")
###

########
# End Landscape Metrics using the 4-neighbor rule
########

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!! Write code here to calculate the same metrics as above using the 8-neighbor rule
# HINT: copy/paste the entire code block, then change the notation, but take your time!
# HINT: don't forget to change the object names 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#################
##  END Part 3  ##
#################


#################
##  Part 4  ##
#################
# Calc Landscape Metrics for real landscapes 

# See p. 59-60 of the LEE text for details
# We will load 5 landscapes (two classifications of Madison, WI from different analysts
# and three different NE landscapes). See slides for maps of the landscapes

# We'll calculate 5 metrics using the 8-neighbor rule:
# • Contagion
# • Patch Density (the average number of patches per 100 ha)
# • Edge Density (an expression of edge:area relationships)
# • Landscape Shape Index (a measure of shape complexity)
# • Largest Patch Index (an indicator of connectivity)
# • Patch Richness (the number of patch types)


########### 
# First we'll set a few things to make it nicer to view outputs

###
# Color palettes for categorical raster plotting
###

# Madison landscapes palette (mad1, mad2)
# Classes: 2=Evergreen, 7=Water, 11=Other Vegetation, 17=Alfalfa,
#          20=Deciduous, 28=Corn, 29=Impervious
madison.pal <- data.frame(
  value = c(2,   7,         11,        17,        20,        28,        29),
  label = c("Evergreen", "Water", "Other Vegetation", "Alfalfa",
            "Deciduous", "Corn", "Impervious"),
  color = c("#1B2A3B",   # 2  Evergreen     - dark navy
            "#ADD8E6",   # 7  Water         - light blue
            "#2D6A0A",   # 11 Other Veg     - dark green
            "#E8220A",   # 17 Alfalfa       - red
            "#7DBD7A",   # 20 Deciduous     - sage green
            "#F5C97A",   # 28 Corn          - pale gold
            "#8B1010")   # 29 Impervious    - dark red
)

# NE landscapes palette (ne1, ne2, ne3)
# NLCD-style classes
ne.pal <- data.frame(
  value = c(11,   12,              21,                  22,                    23,                      24,                    31,           41,                 42,                43,             52,            71,                     81,            82,               90,               95),
  label = c("Open Water", "Perennial Ice/Snow", "Developed, Open Space", "Developed, Low Intensity",
            "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land",
            "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrub/Scrub",
            "Grassland/Herbaceous", "Pasture Hay", "Cultivated Crops",
            "Woody Wetlands", "Emergent Herbaceous Wetlands"),
  color = c("#4B6EA8",   # 11 Open Water                   - steel blue
            "#F5F5F5",   # 12 Perennial Ice/Snow            - near white
            "#E8D5C4",   # 21 Developed, Open Space         - light tan
            "#D4A882",   # 22 Developed, Low Intensity      - tan/peach
            "#E8220A",   # 23 Developed, Medium Intensity   - red
            "#A01010",   # 24 Developed, High Intensity     - dark red
            "#B8B09A",   # 31 Barren Land                   - grey-tan
            "#7DBD5A",   # 41 Deciduous Forest              - medium green
            "#2D6A0A",   # 42 Evergreen Forest              - dark green
            "#C8D48A",   # 43 Mixed Forest                  - yellow-green
            "#C8B87A",   # 52 Shrub/Scrub                   - tan
            "#D4CC8A",   # 71 Grassland/Herbaceous          - pale yellow-green
            "#E8E050",   # 81 Pasture Hay                   - yellow
            "#9B7535",   # 82 Cultivated Crops              - brown
            "#C8DCD8",   # 90 Woody Wetlands                - pale blue-grey
            "#9AB8C8")   # 95 Emergent Herbaceous Wetlands  - medium blue-grey
)

# Helper function to apply a color table to a terra raster and plot it
plot_categorical <- function(r, pal) {
  ct <- data.frame(value = pal$value,
                   label = pal$label,
                   col   = pal$color)
  levels(r) <- ct[ , c("value", "label")]
  terra::coltab(r) <- ct[ , c("value", "col")]
  terra::plot(r)
}

###
# Madison 1 Landscape
###
mad1.R <- terra::rast("data/mad1.tif") #load the raster
mad1.R #check some info on it
plot_categorical(mad1.R, madison.pal) #plot with categorical Madison legend

#C ontagion
cont.mad1<-lsm_l_contag(mad1.R, verbose = TRUE) 
cont.mad1 #note, the value returned is percent

# patch density
pd.mad1<-lsm_l_pd(mad1.R, direction=8) #units = number per 100 ha
pd.mad1

# edge density
ed.mad1<-lsm_l_ed(mad1.R) #meters per ha
ed.mad1

# landscape shape index 
lsi.mad1<-lsm_l_lsi(mad1.R) #no units
lsi.mad1

#largest patch index  
lpi.mad1<-lsm_l_lpi(mad1.R, direction=8) #units percentage
lpi.mad1

#patch richness  
# what is 'patch richness', you ask? you can learn more like this:
?lsm_l_pr()

pr.mad1<-lsm_l_pr(mad1.R)
pr.mad1

#wrap the variables above into a table that we'll use later
mad1.DF<-rbind(cont.mad1, pd.mad1, ed.mad1, lsi.mad1, lpi.mad1, pr.mad1)
#rename the value column to mad1 - so we can delineate
mad1.DF<-mad1.DF %>% 
  rename(mad1 = value) 
#round columns to include 2 sig digits for clarity
mad1.DF$mad1<-round(mad1.DF$mad1, digits=2) 

###
# End Mad1 Landscape
###

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!! Write code here to calculate the same metrics as above for the other 4 landscapes
# be sure to use the prefix: mad2, ne1, ne2, ne3 - then you can continue on to the code below
# HINT: copy/paste each code block, then change the notation, but take your time!
# HINT: for plotting use plot_categorical(mad2.R, madison.pal) for Madison landscapes
#       and plot_categorical(ne1.R, ne.pal) for NE landscapes (ne1, ne2, ne3)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#######
# Join dataframes into one master table
#######
#Now join all dataframes into one
part4.DF<-mad1.DF %>% 
  full_join(mad2.DF) %>% 
  full_join(ne1.DF) %>% 
  full_join(ne2.DF) %>% 
  full_join(ne3.DF) 

# check it out:
part4.DF
# why is the class column empty for all rows?? 

#export the dataframe to a table (csv)
write_csv(part4.DF, "outputs/part4.csv")

############
# Let's visualize the data to make easy comparisons
############

#first we need to reshape the data so we can visualize it in a clear way

#convert the data table from wide to long
part4.DF_gather <- part4.DF %>%
  gather(key = landscape, value = value, mad1, mad2, ne1, ne2, ne3)

#have a look at the two dataframes to see how we changed them
head(part4.DF)
head(part4.DF_gather)

#create a basic bar plot that is faceted by landscape metric
ggplot(part4.DF_gather, aes(x=landscape, y=value)) + 
  geom_bar(stat = "identity")+
  facet_wrap(facets = vars(metric))

#not bad, but let's make it look nicer
good.plot<-ggplot(part4.DF_gather, aes(x=landscape, y=value)) + 
  geom_bar(stat = "identity", fill="dark green")+
  facet_wrap(facets = vars(metric))+
  labs(title = "Landscape Metrics on 'Real' Landscapes",
       subtitle = "!!! put your name here !!!",
       caption = "#youcanmakenoteshere", 
       x = "Landscape", 
       y = "Metric Value") +
  theme_bw()
good.plot #since we wrote it an object, we need to call it to plot it

#that looks decent, but the y-axis has all the same values and some of the metrics are 
#hard to see; let's modify and create a free y-axis (note: values will be different)
better.plot<-ggplot(part4.DF_gather, aes(x=landscape, y=value)) + 
  geom_bar(stat = "identity", fill="dark green")+
  facet_wrap(. ~ metric, scales = "free_y")+ #note the diff code here
  labs(title = "Landscape Metrics on 'Real' Landscapes",
       subtitle = "!!! put your name here !!!",
       caption = "#youcanmakenoteshere", 
       x = "Landscape", 
       y = "Metric Value") +
  theme_bw()
better.plot #since we wrote it an object, we need to call it to plot it
#Note: y-axis are now different for each metric

#this is better, but let's set each landscape to the same color
# so it will be easier to interpret
#let's use a custom package
library(wesanderson) #load the Wes Anderson Color package; I like Wes Anderson movies


names(wes_palettes) #have a look at the palletes
wes_palette("IsleofDogs1") #I've never seen this movie, but the colors are nice

#now plot; note there are a few diffs in the code
best.plot<-ggplot(part4.DF_gather, aes(x=landscape, y=value, fill=landscape)) + 
  geom_bar(stat = "identity", alpha=1)+
  #geom_bar(position = "dodge", stat = "identity", alpha=1) +
  facet_wrap(. ~ metric, scales = "free_y")+ 
  scale_fill_manual(values=wes_palette("IsleofDogs1"))+ 
  labs(title = "Landscape Metrics on 'Real' Landscapes",
       subtitle = "!!! put your name here !!!",
       caption = "#wesandersoncolors", 
       x = "Landscape", 
       y = "Metric Value") +
  theme_bw()
best.plot #since we wrote it an object, we need to call it to plot it
#Note: y-axis are now different for each metric; each landscape has it's own color - easy interp

#save the plot- now it can be brought into your lab word document
ggsave("outputs/Part4_landscape_metrics.jpg", best.plot, width = 7.5, height = 4)
# or you can take a screenshot and paste that into the doc

#############
### Assess correlation of metrics
############
#we want to compare the metrics for each landscape against each other to assess correlation strength

#convert the data table from wide to long
#part4.DF_gather <- part4.DF %>%
# gather(key = landscape, value = value, mad1, mad2, ne1, ne2, ne3)
#have a look at the two dataframes to see how we changed them

#head(part4.DF_gather)


#first create a subset of variables that we want to assess
head(part4.DF) #rememebr what we have

#subset the data and drop columns that are not needed
part4.DF.sub<-part4.DF%>%
  select(metric:ne3)  # this just pulls out the columns that we want, from metric through ne3

head(part4.DF.sub)

# We need to transpose the data frame
library(data.table) #load the package; remember to install if first!
t.part4.DF.sub <- transpose(part4.DF.sub)  #this flips it around
#promote first row to column names
names(t.part4.DF.sub) <- t.part4.DF.sub[1,] 
#now delete the extra first row
t.part4.DF.sub <- t.part4.DF.sub[-1,]
head(t.part4.DF.sub) #looks good
str(t.part4.DF.sub) #for some reason, the columns are character
#convert all to numeric
t.part4.DF.sub <- as.data.frame(sapply( t.part4.DF.sub, as.numeric ))


library(GGally) #load the package; remember to install if first!
cor1 <- ggpairs(t.part4.DF.sub)
cor1  

#this is a Pearson's correlation coefficient. 

#What does this table tell us?
#Simply crosswalk two variables to get the correlation value. 

#Note: this line from the paper we read (Assal et al. 2015):
#Multicollinearity among all potential explanatory variables was assessed 
#prior to model calibration using the Pearson's correlation coefficient. 
#Variables with a correlation coefficient greater than 0.8 or less than -0.8 
#were removed from consideration within the same model.

#With that in mind, now many of these metrics would we include as model variables?

#############
### End Assess correlation of metrics
############

###############
### CHALLENGE
###############

# all of the metrics above were calculated at the scale of the landscape
# try calculating some patch and class-level metrics for these same regions
# insert your code below:

# For instance you could calculate the aggregation index: 
# at diff scales for NE1:
lsm_c_ai(ne1.R)
lsm_l_ai(ne1.R) 


# Remember that you can use (most of) the same functions, just need to specify a diff scale of analysis
# for instance, you can calculate the number of patches for the enitre landscape or by class
# lsm_l_np versus lsm_c_np
# and if you start typing lsm_c or lsm_l and then hit /tab/ it will list the options!
