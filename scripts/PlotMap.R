#### Prepared for D-lab workshop series: Mapping in R
#### Instructors: Patty Frontiera & Shinhye Choi
#### contact: schoi [at] berkeley.edu for suggestions
#### 
#### New topics: lines starting with "###"

rm(list = ls())

### installing spatial data packages

# Here is some code to identify all needed packages, install if not installed, then load with library()
req.pkg <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", 
             "sp", "ggplot2", "scales", "Cairo", "maps", "RColorBrewer")
pkgs.not.installed <- req.pkg[!sapply(req.pkg, function(p) require(p, character.only=T))]
install.packages(pkgs.not.installed, dependencies=TRUE)
lapply(req.pkg, library, character.only = TRUE)         # you could library them all at once.

### Topic: map plotting with built-in map data
### required package: ggplot2, maps

setwd("~/Dropbox/Dlab/SpatialPlot")          # set your own working directory

library(ggplot2)
library(maps)

# retrieve a map from the "maps" package 
# ?map_data
world <- map_data("world")                        
ggplot(world, aes(x=long, y=lat)) +   
              geom_path(aes(group=group)) 

# let's talk about the object 
# group, order - what do they mean? 
# group: grouping for each polygon.
head(world)

# a country may have multiple polygons:
# which country do you think should have more data points? why?
length(which(world$region=="India"))               
length(which(world$region=="Canada"))

# Plot one region: India
p1 <- ggplot(world[world$region=="India",], aes(x=long, y=lat)) +   
              geom_path(aes(group=group))   # what doesnt look right? Compare to this:

p2 <- ggplot(world[world$region=="India",], aes(x=long, y=lat)) +   
              geom_path(aes(group=group)) + coord_map("mercator") 
multiplot(p1, p2, cols=2)

# Map projectons & coord ref systems CRS: 
# Geographic CRS - coordinates specify geographic locations on a 3d sphere. Look weird in 2d
# Projected CRS (also called map projections) specify geographic location on 2d plane
# All projections introduce distortion in shape, area, distance or direction. Or some combo

# there are multiple ways to map the same regions 
## "ways" in terms of style (color) and feature type (point line or polygon), 
#   or subset/region selection or Coord Ref System (CRS)

# subset/region selection
north_a_countries <- map_data("world", region=c("USA", "Canada"))
north_a <- subset(north_a_countries, long < -50 & lat > 20)
# So if CA between +32 and +48 lat and =114 and -122 long try subsetting that region

# we want to plot a better map of North America than this
ggplot(north_a_countries, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group)) 

# with different prj
p1 <- ggplot(north_a, aes(x=long, y=lat)) +   
          geom_path(aes(group=group))

p2 <- ggplot(north_a, aes(x=long, y=lat)) +   
          geom_path(aes(group=group)) + 
          coord_map("mercator")

multiplot(p1, p2, cols=2)

# Points, Lines, & Polygons
ggplot(north_a, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group)) + coord_map("mercator")

ggplot(north_a, aes(x=long, y=lat)) +
  geom_point(aes(group=group)) + coord_map("mercator")

ggplot(north_a, aes(x=long, y=lat)) +
  geom_path(aes(group=group)) + coord_map("mercator")

# Symbolizing Paths (or Lines)
ggplot(north_a, aes(x=long, y=lat)) +
  geom_path(aes(group=group), color="red") + coord_map("mercator")

# symbolizing polygons
ggplot(north_a, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=region)) + coord_map("mercator")

ggplot(north_a, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=region), color="blue") + coord_map("mercator")

# Using Color Palettes
# Save the plot created by the code above and then add a color palette to fill the regions.
p1 <- ggplot(north_a, aes(x=long, y=lat)) +
       geom_polygon(aes(group=group, fill=region), color="blue") + coord_map("mercator")

p1 +  scale_fill_brewer(palette="Set3")              # what just happened?

# ColorBrewer Package
# Read the package help doc on the ColorBrewer Package See aslo: colorbrewer.org
library(RColorBrewer)
?ColorBrewer
display.brewer.pal(3,"Set3")    # 3 different levels (3 is min)/ types of palettes

# types of palettes
display.brewer.all(type="qual")
display.brewer.all(type="seq")
display.brewer.all(type="div")

# still another possibiilty
states <- map_data("state")  # ??map_data for available regions
ggplot(states, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group,  fill=region), colour="black", fill="white")   # fill="white"

# explain
## why important: points define location, connected points define lines, closed lines define polygons
## colour/color is for the geom data (point, line, polygon), 
# fill="color" is an attribute of region - no meaning for points or paths/lines


### Topic: Creating choropleth maps
# important! combine the attribute data with spatial data
# match based on state id or state name

data(state)
state_att <- state.x77     # rm(state.x77)

unique(states$region)
unique(row.names(state_att))   # Alaska/DC not in both data sets, but it's okay. why?
# tolower state names
state_att <- as.data.frame(state_att[-which(row.names(state_att)=="Alaska"),])  # not necessary. why?
state_att$name <- tolower(row.names(state_att))

# now merge the two data sets: spatial + attribute data
# look for the common values!
states_dat <- merge(states, state_att, by.x="region", by.y="name")
ggplot(states_dat, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=Population), colour="black")     

# wait, is something wrong with the data?

# need to rearrange the data by group and order. WHY?
# arrange function in the plyr package
library(plyr)
states_dat <- arrange(states_dat, group, order)
ggplot(states_dat, aes(x=long, y=lat, group=group, fill=Population)) +
  geom_polygon(colour="black") +
  coord_map("mercator") +                         
  labs(title = "Population by State", fill = "")   # add a title

# Now, remove the grid lines - what code addtion makes that happen?
library(ggmap)
ggplot(states_dat, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=Population), colour="NA") +
  coord_map("mercator") +      
  labs(title = "Population by State", fill = "") +
  theme_nothing(legend=TRUE)

# now change those colors - too dark
# scale_fill_brewer() scale for discrete color scales, 
# distill" them into continuous scales with scale_fill_distiller
?scale_fill_distiller
p1 <- ggplot(states_dat, aes(x=long, y=lat)) +
         geom_polygon(aes(group=group, fill=Population), colour="NA") +
         scale_fill_distiller(palette="Reds") +
         coord_map("mercator") +      
         labs(title = "Population by State", fill = "") +
         theme_nothing(legend=TRUE)

# and reverse the legend
p2 <- ggplot(states_dat, aes(x=long, y=lat)) +
       geom_polygon(aes(group=group, fill=Population), colour="NA") +
       scale_fill_distiller(palette = "Reds") +
       coord_map("mercator") +      
       labs(title = "Population by State", fill = "") +
       theme_nothing(legend=TRUE) +
       guides(fill = guide_legend(reverse = TRUE))

multiplot(p1, p2, cols=2)
# now save it to a file
ggsave("p2.png", width = 9, height = 9) # save figure

# Let's take a break!
# Exercise - subset California
# Look back to the previous code where we subset North American Countries using coordinates values.

# Subset CA between +32 and +42.5 degrees latitude and 114 and -124 longitude.
# Plot CA with and without + coord_map("mercator").
# Add some custom color symbology using a qualitative palette and white outlines
# Add a title
# use a different ggplot theme (eg theme_bw or theme_minimal). See ?ggtheme for help.
# Here's one approach

cal <- subset(states, (long < -114 & long > -124) & (lat > 32 & lat < 42.5))
ggplot(cal, aes(x=long, y=lat)) +
    geom_polygon(aes(group=group, fill=region), color="white") + coord_map("mercator") +
    scale_fill_brewer(palette="Set2") +
    theme_bw() +
    labs(title="this is where it is at")


### Topic 3: Plotting Spatial Points onto a Map retreived from the Goolge Maps API
# required package: ggmap (retrieving a map from the Google Maps API)
# https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

# Geocoding
library(ggmap)
geocode("Berkeley, CA", source="google")
berkeley_loc <- as.numeric(geocode("Berkeley, CA",source="google"))  
                # What's the difference?

# Now, add the point for Berkeley to the map of CA. 
# Note the syntax used to layer a point on top of a polygon.
# Assign basic map to mymap
mymap <- ggplot(cal, aes(x=long, y=lat)) +
              geom_polygon(aes(group=group, fill=region)) + 
              coord_map("mercator")
# add point
mymap <- mymap + geom_point(x=berkeley_loc[1],y=berkeley_loc[2], col="red", size=10)

# label the point
mymap <- mymap + geom_text(data = NULL,x=berkeley_loc[1]+1,y=berkeley_loc[2], label = "Berkeley")
         # Why did we add a +1 to the x coordinate for the text lable Berkeley?

# Creating maps with ggmap
qmap(location="uc berkeley", zoom=10) # what about changing that zoom level? 
qmap(location="uc berkeley", zoom=15, maptype = "toner-lite", source = "stamen") #change the background

# Center a qmap around a point
sather_gate_location = c(lon = -122.2616814, lat = 37.8702548) #from search in maps.google.com
qmap(location=sather_gate_location, zoom=15, maptype = "toner-lite", source = "stamen") 


# Geocode an Address
# Let's use the geocode() function to get the coordinates for an address. 
# We can then repeat the above process of centering the map at that location and adding the point to the map.

# address of my party location
geocode("2315 Durant Ave, Berkeley, CA 94704", source="google") #run on command line - but we need different format
party_loc = as.numeric(geocode("2315 Durant Ave, Berkeley, CA 94704", source="google"))

#n ow add a point so we know where we going
qmap(location=party_loc, zoom=14) + 
     geom_point(x=party_loc[1],y=party_loc[2], col="orange",size=6)
###############################
# Another style - http://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/
# usa_center = as.numeric(geocode("United States", source="google"))
# ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="normal") 


# Point Maps 
# The most common types of point maps are:
# dot maps: plot point locations using a single color or icon
# categorical dot maps: color code the points by category
# proportional symbol maps: scale the symbol size based on an attribute value.
data(us.cities)
str(us.cities)

# subset cities - just bay area - get lat lon from
bayarea_map <-get_map('Berkeley',zoom=8)
ggmap(bayarea_map)

bayarea_cities <- subset(us.cities, (long < -120 & long > -124) & (lat > 36 & lat <40)) # Patty: explain what and why
dim(bayarea_cities)

# now plot the cities in the bay area
ggmap(bayarea_map) +
  geom_point(data=bayarea_cities, aes(x=long, y=lat))

# change col or color or colour
ggmap(bayarea_map) +
  geom_point(data=bayarea_cities, aes(x=long, y=lat), col="orange") 

# change the point size
ggmap(bayarea_map) +
  geom_point(data=bayarea_cities, aes(x=long, y=lat), col="orange", size=4)

# change shape so we can specify fill color and outline color
ggmap(bayarea_map) +
  geom_point(data=bayarea_cities, aes(x=long, y=lat), 
                     col="white", fill="orange", size=4, shape=21)

# scale size by population
ggmap(bayarea_map) +
  geom_point(data=bayarea_cities, aes(x=long, y=lat, size=pop)) 

#### Topic: Plotting your data!!
# Thus far we have created maps using data from the R maps and ggmaps packages.
# lets add local point data

# Make sure you have csv files in your working directory.
# data: cafes where I go often. let's not argue about my personal preferences.
cafe <- read.csv("cafe.csv")
head(cafe)
str(cafe)

map <- get_map("Berkeley", zoom=14)                 # retrieve a map from Gmap API

# Let's create a cateogry dot map (or pin map)
ggmap(map) +
  geom_point(aes(x = long, y = lat, size = 4, colour = I(name)), 
             data = cafe, alpha = .9)                # geom_points!
# Note: In the above code the I() sets the a color for each unique value of name.
# If the column values are numbers not strings you need to use factor() instead.


# reate a proportional symbol map
g <- ggmap(map) +
         geom_point(aes(x = long, y = lat, size = price), 
                    colour = "red", data = cafe, alpha = 1)                # geom_points!
#let's scale the symbol size by the price but make it a bit bigger by adding 2


# add a title, remember how?  # + labs(title = "Coffee Prices in Berkeley") 
# and remove axis, remember how?
g <- g + labs(title = "Coffee Prices in Berkeley") 
multiplot(g, g + theme_nothing(legend=TRUE), cols=2)


### Topic: Mapping Spatial Data Objects with the Base Plotting System
# Thus far we have mapped data where the geographic locations are 
# stored in a data frame as numbers in columns labeled lat and lon. 
# We will now work with explicit spatial data which store 
# geographic locations aspoints, lines and polygons objects. 
# FYI here is a really nice cheat sheet for working with spatial data in R:
# http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html

library(sandwich)
library(classInt)
library(RColorBrewer)
library(sp)
library(maptools)
library(rgeos)
library(rgdal)  # allows for datum transformation and projection 

# library(sp): different classes of spatial data objects in the sp package
getClass("Spatial")
getClass("SpatialPoints")

#### spatialpoints
# create a Spatial Points object from the cafe dat data frame. 
# First, extract the coordinates (long, lat) from the data frame.
cafe_coords <- cbind(cafe$long, cafe$lat)
row.names(cafe_coords) <- cafe$name
str(cafe_coords)

# create a spatialpoints object
cafe_sp <- SpatialPoints(cafe_coords)

# Define the CRS for the SpatialPoints object
proj4string(cafe_sp) <- CRS("+proj=longlat + ellps=WGS84")

# Note we could do those two steps in one line of code - as the documentation shows us
cafe_sp <- SpatialPoints(cafe_coords, proj4string=CRS("+proj=longlat + ellps=WGS84"))
# But it is good to know you can assign a CRS to a spatial object at any time

# Take a look at the Objectclass(cafe_sp)
str(cafe_sp) 
summary(cafe_sp)

#How many and what slots are in the SpatialPoints Object cafe_sp?
slot(cafe_sp, "coords")
bbox(cafe_sp)
proj4string(cafe_sp)

# subset the data: I want to know the location of Strada..
coordinates(cafe_sp)[which(row.names(cafe_sp)=="strada"),]

# Create Maps with R Base Plotting System
# basic plot
plot(cafe_sp)
points(coordinates(cafe_sp)[which(row.names(cafe_sp)=="strada"),1],
       coordinates(cafe_sp)[which(row.names(cafe_sp)=="strada"),2],col="red", pch=20)


# Create a SpatialPointsDataFrame Object
# A SpatialPointsDataFrame stores the data frame in the @data slot of the object. 
# See ?SpatialPointsDataFrame for details on this class of object.

cafe_spdf <- SpatialPointsDataFrame(cbind(cafe$long,cafe$lat),
                                    data=cafe,
                                    proj4string=CRS("+proj=longlat + ellps=WGS84"))
#examine it
class(cafe_spdf)
str(cafe_spdf)

# map it
plot(cafe_spdf)
points(cafe_spdf[which(cafe_spdf$name=="strada"),], col="red", pch=20)

# Explore Spatial data with rgdal package
# crs and projection
# ### codes for plots included in the slides
library(rgdal)

setwd("~/Dropbox/Dlab/DlabSpatialPlot")
ogrInfo(".", "uc_bldgs")
uc_buildings <- readOGR(dsn=".", layer="uc_bldgs") 
summary(uc_buildings)

# load other data
ogrInfo(".", "uc_bnd")
uc_boundary <- readOGR(dsn=".", layer="uc_bnd") 

ogrInfo(".", "censusblk")
census <- readOGR(dsn=".", layer="censusblk") 

# Map overlays
# make a map of all of our data
plot(uc_boundary, border="blue")
plot(census, add=T)
plot(uc_buildings, col="red", add=T)
points(cafe_spdf, col="green")    # why can't wee our cafe points?

# CRS should match!
proj4string(cafe_spdf) == proj4string(uc_buildings)  # What does this tell us?

# transforming CRS
# spatial data must have the same coordinate systems
# spTransform to transform or reproject spatial data from one CRS to another
uc_buildingsT  <- spTransform(uc_buildings, CRS("+proj=longlat +ellaps=WGS84"))
censusT <- spTransform(census, CRS("+proj=longlat +ellaps=WGS84"))
uc_boundaryT  <- spTransform(uc_boundary, CRS(proj4string(censusT)))

# now you should see all points on our map
plot(uc_boundaryT, border="blue")
plot(censusT, add=T)
plot(uc_buildingsT, col="red", add=T)
points(cafe_spdf, col="orange", pch=20)

# more maps!
# look into the data
plot(uc_buildings)
plot(uc_buildings[which(uc_buildings$NAME2=="Barrows Hall"),], add=TRUE, col="blue")
plot(uc_buildings[which(uc_buildings$NAME2=="Lawrence Hall of Science"),], add=TRUE, col="red")
plot(uc_buildings[which(uc_buildings$NAME2=="Sproul Hall"),], add=TRUE, col="red")

# plot all maps
plot(uc_buildings)
plot(uc_boundary, add=T, border="blue", lwd=2)
plot(census, add=T, border="grey")

# Map Census Data for Berkeley
# let's look at the census attribute data
# plot white pop density for each block
library(classInt)
cls <- classIntervals(census$WHITE, 5, style = "quantile")
colpal<- findColours(cls, brewer.pal(5, "BuGn"))
plot(census, border="grey", col=colpal)
plot(uc_boundary, add=T, border="blue", lwd=2)
plot(uc_buildings, add=T)

# pop density 
#Patty - don't use class as a var name as it is reserved word
cls <- classIntervals(census$POP_DENS, 5, style = "quantile")
colpal<- findColours(cls, brewer.pal(5, "OrRd"))
plot(census, border="grey", col=colpal)
plot(uc_boundary, add=T, border="blue", lwd=2)
plot(uc_buildings, add=T)

# rent
cls <- classIntervals(census$RENT, 5, style = "quantile")
colpal<- findColours(cls, brewer.pal(5, "PuRd"))
plot(census, border="grey", col=colpal)
plot(uc_boundary, add=T, border="blue", lwd=2)
plot(uc_buildings, add=T)

#Patty: let's bring this full circle
# First add a unique id and coordinates to the census SPDF
censusT$geoid <- with(censusT@data, paste0(STATE, COUNTY, TRACT, BLOCK))
censusT$X <- coordinates(censusT)[,1]
censusT$Y <- coordinates(censusT)[,2]

# Use fortify() to make the census data an object that ggplot can map
plotData <- fortify(censusT, data=censusT@data, region="geoid")
head(plotData) # take a look at the result of the fortify command

#now map it
map <- get_map("Berkeley", zoom=13)  
ggmap(map) + geom_polygon(data=plotData, aes(x=long, y=lat, group=group))

# Add transparency to better see reference basemap
ggmap(map) +
  geom_polygon(data=plotData, aes(x=long, y=lat, group=group), alpha=0.5)

#WORKS: http://www.kevjohnson.org/making-maps-in-r-part-2/
plotData <- merge(plotData,censusT@data, by.x="id", by.y="geoid")
head(plotData) # now you can see the attribute data re-joined to the geographic data

#map it - color regions by census variable
ggmap(map) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group, 
                                    fill = POP_DENS), color = "black", size = 0.25) +
  coord_map()

# Too dark - try this
myplot <- ggmap(map) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = POP_DENS)) +
  coord_map() +
  scale_fill_distiller(palette = "Reds",
                       breaks = pretty_breaks(n = 8)) +
  guides(fill = guide_legend(reverse = TRUE))

myplot


#--- WORKS END -----
#### UC spatial data available at
#### http://ced.berkeley.edu/faculty/ratt/downloads/CCanyon/lab4Proj_files.zip 
#### Disclaimer: I do not own the UC spatial data. Anyone interested in the data
#### for other purposes should contact the owner directly.
#### Reference book: Applied Spatial Data Analysis with R, 2nd edition
################################################



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
