#### Prepared for D-lab workshop series: Mapping in R
#### Instructors: Patty Frontiera & Shinhye Choi
#### contact: schoi [at] berkeley.edu for suggestions
#### 
#### New topics: lines starting with "###"

#Patty: start with clean slate
rm(list = ls())

### installing spatial data packages
#x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", 
       "sp", "ggplot2", "scales", "Cairo", "maps")
# install.packages(x)                             # remember, you only install a package once!
#lapply(x, library, character.only = TRUE)         # you could library them all at once.
       
#Patty: Here is some code to identify all needed packages, install if not installed, then load with library()
req.pkg <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", 
             "sp", "ggplot2", "scales", "Cairo", "maps")
pkgs.not.installed <- req.pkg[!sapply(req.pkg, function(p) require(p, character.only=T))]
install.packages(pkgs.not.installed, dependencies=TRUE)
lapply(req.pkg, library, character.only = TRUE)         # you could library them all at once.

### Topic: map plotting with built-in map data
### required package: ggplot2, maps

#setwd("~/Dropbox/Dlab/SpatialPlot")          # set your own working directory
setwd("~/Documents/Dlab/dlab_workshops/geoplotting/DlabSpatialPlot")

library(ggplot2)
library(maps)

# retrieve a map from the "maps" package 
#Patty: use data() to see all data available and data(package="maps")
world <- map_data("world")                        
ggplot(world, aes(x=long, y=lat, group=group)) +   
  geom_path() + coord_map("mercator")        # will talk more about coord_map later.
#Patty: why start with coord_map as it is not needed for this first map?
#then add it after plotting India or another region without it to show its value/meaning           

# let's talk about the object 
# group, order - what do they mean? #Patty - wy order matters - think connect the dots!
# group: grouping for each polygon.
head(world)

# a country may have multiple polygons:
# which country do you think should have more data points? why?
length(which(world$region=="India"))               
length(which(world$region=="Canada"))

# Plot one region
ggplot(world[which(world$region=="India"),], aes(x=long, y=lat, group=group)) +   
  geom_path() 
# what doesnt look right? Compare to this:
#
ggplot(world[which(world$region=="India"),], aes(x=long, y=lat, group=group)) +   
  geom_path() + coord_map("mercator") 

#Map projectons & coord ref systems CRS: 
#Geographic CRS - coordinates specify geographic locations on a 3d sphere. Look weird in 2d
#Projected CRS (also called map projections) specify geographic location on 2d plane
#All projections introduce distortion in shape, area, distance or direction. Or some combo
#
ggplot(world[which(world$region=="Canada"),], aes(x=long, y=lat, group=group)) +   
  geom_path() + coord_map("mercator")


# there are multiple ways to map the same regions 
## Patty - "ways" in terms of style (color) and feature type (point line or polygon), or subset/region selection or Coord Ref System (CRS)
## Patty - Slow it down - add one change at a time

#1. what's different here (bc geom_polygon we get fill)
north_a <- map_data("world", region=c("USA", "Canada"))
ggplot(north_a, aes(x=long, y=lat, group=group)) +
  geom_polygon()  

#2. what's different here - bc region specified we fill by region
north_a <- map_data("world", region=c("USA", "Canada"))
ggplot(north_a, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon()

#3. whats diff here (specify geom color)
# Patty: ask what happens if you change colour from black to white or colour to color!
north_a <- map_data("world", region=c("USA", "Canada"))
ggplot(north_a, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon(colour="black") +
  scale_fill_brewer(palette="Set3")              # what just happened?
                                                 # your options are limited, but there are bettter ways to do the same task.


# another way to subset by region: extracting from the world map
#Patty: explain lat ranges from +-90 N&s of equator and long from +-180 e & west of prime meridean (just outside of london)

north_a <- subset(north_a, long < -50 & lat > 20) # Patty: explain what and why
ggplot(north_a, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon(color="pink") +
  scale_fill_brewer(palette="Set3") 
#So if CA between +32 and +48 lat and =114 and -122 long try subsetting that region

# still another possibiilty
#patty: ask them to look at data(package="maps)
states <- map_data("state")  # ??map_data for available regions
ggplot(states, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon(colour="black", fill="white") 
#what happens if you don't specify fill="white"
#how is that different from:
ggplot(states, aes(x=long, y=lat, group=group)) +
  geom_path(colour="black") 

## Patty: ask what happens when you change geom_path / geom_polygon to geom_point in first example:

ggplot(states, aes(x=long, y=lat, group=group)) +
  geom_point(colour="black") 
## why important: points define location, connected points define lines, closed lines define polygons
## colour/color is for the geom data (point, line, polygon), fill="color" is an attribute of region - no meaning for points or paths/lines


### Topic: Creating choropleth maps
# important! combine the attribute data with spatial data
# match based on state id or state name
#Patty: do objects()
data(state)
#Patty: do objects() to show what got loaded
state_att <- state.x77     # rm(state.x77)

unique(states$region)
unique(row.names(state_att))   # Alaska/DC not in both data sets, but it's okay. why?
                               # tolower state names
state_att <- as.data.frame(state_att[-which(row.names(state_att)=="Alaska"),])  # not necessary. why?
state_att$name <- tolower(row.names(state_att))

# now merge the two data sets: spatial + attribute data
states_dat <- merge(states, state_att, by.x="region", by.y="name")
ggplot(states_dat, aes(x=long, y=lat, group=group, fill=Population)) +
  geom_polygon(colour="black")     
                                 
                                      # wait, is something wrong with the data?

# need to rearrange the data by group and order. WHY?
# arrange function in the plyr package
states_dat <- arrange(states_dat, group, order)
ggplot(states_dat, aes(x=long, y=lat, group=group, fill=Population)) +
  geom_polygon(colour="black") +
  coord_map("mercator") +                         
  labs(title = "Population by State", fill = "")

#Patty: now get rid of the ticks and change color to NA
ggplot(states_dat, aes(x=long, y=lat, group=group, fill=Population)) +
  geom_polygon(colour="NA") +
  coord_map("mercator") +      
  labs(title = "Population by State", fill = "") +
  theme_nothing(legend=TRUE)

#Patty: now change those colors - too dark
ggplot(states_dat, aes(x=long, y=lat, group=group, fill=Population)) +
  geom_polygon(colour="NA") +
  scale_fill_distiller(palette = "Reds") +
  coord_map("mercator") +      
  labs(title = "Population by State", fill = "") +
  theme_nothing(legend=TRUE)

#Patty - and reverse the legend
ggplot(states_dat, aes(x=long, y=lat, group=group, fill=Population)) +
  geom_polygon(colour="NA") +
  scale_fill_distiller(palette = "Reds") +
  coord_map("mercator") +      
  labs(title = "Population by State", fill = "") +
  theme_nothing(legend=TRUE) +
guides(fill = guide_legend(reverse = TRUE))

#Patty: now save it to a file
# ggsave("usa_pop.png", width = 9, height = 9) # save figure

#Patty: try changing the fill variable (eg Illiteracy, Murder)




### Topic: Plotting Spatial Points onto a Map retreived from the Goolge Maps API
# required package: ggmap (retrieving a map from the Google Maps API)
library(ggmap)



# patty: make a quick map using qmap wrapper around get_map
# see ?qmap for details
qmap(location="uc berkeley", zoom=10) # what about changing that zoom level? what value gets you a better map of ucb
qmap(location="uc berkeley", zoom=15, maptype = "toner-lite", source = "stamen") #change the background
sather_gate_location = c(lon = -122.2616814, lat = 37.8702548) #from search in maps.google.com
qmap(location=sather_gate_location, zoom=14, maptype = "toner-lite", source = "stamen") 
# but what about a map of my party location
geocode("Berkeley City Club, Berkeley, CA") #run on command line - but we need different format
city_club_loc = as.numeric(geocode("Berkeley City Club, Berkeley, CA"))
#now add a point so we know where we going
qmap(location=city_club_loc, zoom=15) + geom_point(x=city_club_loc[1],y=city_club_loc[2], col="orange",size=10)
#try a street address as the loc

#Another style - http://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/
usa_center = as.numeric(geocode("United States"))
USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="normal") 


#Patty: add points to map
#from us.cities
objects()
data(us.cities)
objects()
str(us.cities)
#Patty: subset cities - just bay area - get lat lon from
bayarea_map <-get_map('Berkeley',zoom=8)
ggmap(bayarea_map)

bayarea_cities <- subset(us.cities, (long < -120 & long > -124) & (lat > 36 & lat <40)) # Patty: explain what and why
nrow(bayarea_cities)

#now plot cities
ggmap(bayarea_map) +
  geom_point(data=bayarea_cities, aes(x=long, y=lat))

#change col or color or colour
ggmap(bayarea_map) +
  geom_point(data=bayarea_cities, aes(x=long, y=lat) col="orange")) 

#change size
ggmap(bayarea_map) +
  geom_point(data=bayarea_cities, aes(x=long, y=lat), col="orange", size=4)

#change shape so we can specify fill color and outline color
ggmap(bayarea_map) +
  geom_point(data=bayarea_cities, aes(x=long, y=lat), col="white", fill="orange", size=4, shape=21)

#scale size by population

ggmap(bayarea_map) +
  geom_point(data=bayarea_cities, aes(x=long, y=lat, size=pop)) 


ggmap(bayarea_map) +
  geom_point(data=bayarea_cities, aes(x=long, y=lat, size=pop), col="orange")

#Let's change the basemap
?get_map - see maptype option
bayareamap2 <- (get_map('Berkeley',zoom=8, maptype="your_selection"))

#Patty: Takeaways- we mapped using data from R packages
#That's great for making context maps
#Or if you can merge your data tables with the data from packages
# For example, data on crimes by city with the us.cities data
# Now let's try adding your data

# lets add local point data

# data: cafes where I go often. let's not argue about my personal preferences.
dat <- read.csv("cafe.csv")
head(dat)
str(dat)

map <- get_map("Berkeley", zoom=14)                 # retrieve a map from Gmap API
#Patty: just ggmap(map) for the warmup

# Let's create a cateogry dot map (or pin map)
ggmap(map) +
  geom_point(aes(x = long, y = lat, size = 4, colour = I(name)), 
             data = dat, alpha = .9)                # geom_points!

#Let's create a proportional symbol map
#let's scale the symbol size by the price but make it a bit bigger by adding 2
ggmap(map) +
  geom_point(aes(x = long, y = lat, size = price), colour = "red", 
             data = dat, alpha = 1)                # geom_points!

#Patty - add title
g <- ggmap(map) +
  geom_point(aes(x = long, y = lat, size = price), colour = "red", 
             data = dat, alpha = 1)
#add a title
g + labs(title = "Coffee Prices in Berkeley") 
#save it BACK TO PLOT
g <- g + labs(title = "Coffee Prices in Berkeley") 

#Remove axis - see above
#g + ??
#g + theme_nothing(legend=TRUE)

### Topic: Mapping from your own spatial data
library(sandwich)
library(classInt)
library(RColorBrewer)
library(sp)
library(maptools)
library(rgeos)
library(rgdal)  # allows for datum transformation and projection 

# library(sp)
getClass("Spatial")
getClass("SpatialPoints")

#### spatialpoints
# take out coordinates (long, lat)
dat_coords <- cbind(dat$long, dat$lat)
row.names(dat_coords) <- dat$name
str(dat_coords)

# create a spatialpoints object
dat_sp <- SpatialPoints(dat_coords, 
                        proj4string=CRS("+proj=longlat + ellps=WGS84"))
class(dat_sp)
str(dat_sp) 
summary(dat_sp)

slot(dat_sp, "coords")
bbox(dat_sp)
proj4string(dat_sp)

# subset the data: I want to know the location of Strada..
coordinates(dat_sp)[which(row.names(dat_sp)=="strada"),]

# basic plot
plot(dat_sp)
points(coordinates(dat_sp)[which(row.names(dat_sp)=="strada"),1],
       coordinates(dat_sp)[which(row.names(dat_sp)=="strada"),2],col="red", pch=20)

# crs and projection
# ### codes for plots included in the slides
#setwd("~/Dropbox/Dlab/SpatialPlot/lab")
ogrInfo(".", "uc_bldgs")
uc_bd <- readOGR(dsn=".", layer="uc_bldgs") 
summary(uc_bd)

# load other data
ogrInfo(".", "uc_bnd")
uc_bnd <- readOGR(dsn=".", layer="uc_bnd") 

ogrInfo(".", "censusblk")
census <- readOGR(dsn=".", layer="censusblk") 

uc_bdT  <- spTransform(uc_bd, CRS("+proj=longlat +ellaps=WGS84"))
censusT <- spTransform(census, CRS("+proj=longlat +ellaps=WGS84"))

pdf(file = paste("types", Sys.Date(), ".pdf", sep = ""),
    height = 6, width = 8)
plot(dat$long, dat$lat, col="red", pch=16, cex=2, axes=F)
dev.off()

pdf(file = paste("types1", Sys.Date(), ".pdf", sep = ""),
    height = 6, width = 8)
plot(uc_bdT)
plot(censusT, add=T)
points(-122.258016, 37.870151, col="blue", pch=16, cex=2)
points(dat$long, dat$lat, col="red", pch=16, cex=2)
dev.off()

pdf(file = paste("types2", Sys.Date(), ".pdf", sep = ""),
    height = 6, width = 8)
plot(uc_bdT)
plot(censusT, add=T)
points(-122.258016, 37.870151, col="blue", pch=16, cex=2)
points(dat$long, dat$lat, col="red", pch=16, cex=2)
lines(dat$long, dat$lat, col="red", lwd=2)
dev.off()


# SpatialPointsDataFrame: matching on ID names
all.equal(row.names(dat), row.names(dat_sp))
data <- SpatialPointsDataFrame(dat_sp[match(row.names(dat), row.names(dat_sp)),],
                               dat, 
                               proj4string = CRS(dat_sp),
                               match.ID =TRUE) 
row.names(dat) <- dat$name
all.equal(row.names(dat), row.names(dat_sp))
data <- SpatialPointsDataFrame(dat_sp[match(row.names(dat), row.names(dat_sp)),],
                               dat, 
                               proj4string = CRS(dat_sp),
                               match.ID =TRUE) 
class(data)

# SpatialLines
getClass("Line")
getClass("Lines")
getClass("SpatialLines")

# SpatialPolygons data
getClass("SpatialPolygons")

ogrInfo(".", "uc_bldgs")
uc_bd <- readOGR(dsn=".", layer="uc_bldgs") Patty -make var names self explanatory, eg uc_buildings
summary(uc_bd)

# load other data - Patty -make var names self explanatory, eg uc_boundary
ogrInfo(".", "uc_bnd")
uc_bnd <- readOGR(dsn=".", layer="uc_bnd") 

ogrInfo(".", "censusblk")
census <- readOGR(dsn=".", layer="censusblk") 

# look into the data
plot(uc_bd)
plot(uc_bd[which(uc_bd$NAME2=="Barrows Hall"),], add=TRUE, col="blue")
plot(uc_bd[which(uc_bd$NAME2=="Lawrence Hall of Science"),], add=TRUE, col="red")
plot(uc_bd[which(uc_bd$NAME2=="Sproul Hall"),], add=TRUE, col="red")

# plot all maps
plot(uc_bd)
plot(uc_bnd, add=T, border="blue", lwd=2)
plot(census, add=T, border="grey")

# let's look at the census attribute data
# plot white pop density for each block
class <- classIntervals(census$WHITE, 5, style = "quantile")
colpal<- findColours(class, brewer.pal(5, "BuGn"))
plot(census, border="grey", col=colpal)
plot(uc_bnd, add=T, border="blue", lwd=2)
plot(uc_bd, add=T)



# pop density 
#Patty - don't use class as a var name as it is reserved word
class <- classIntervals(census$POP_DENS, 5, style = "quantile")
colpal<- findColours(class, brewer.pal(5, "OrRd"))
plot(census, border="grey", col=colpal)
plot(uc_bnd, add=T, border="blue", lwd=2)
plot(uc_bd, add=T)

# rent
class <- classIntervals(census$RENT, 5, style = "quantile")
colpal<- findColours(class, brewer.pal(5, "PuRd"))
plot(census, border="grey", col=colpal)
plot(uc_bnd, add=T, border="blue", lwd=2)
plot(uc_bd, add=T)

#Patty: let's bring this full circle
census$geoid <- with(census@data, paste0(STATE, COUNTY, TRACT, BLOCK))
census$X <- coordinates(census)[,1]
census$Y <- coordinates(census)[,2]
plotData <- fortify(census, data=census@data, region="geoid")
library(dplyr)
plotData <- left_join(plotData,census@data, by.x="id", by.y="geoid")

ggmap(map) +
  geom_polygon(data=census, aes(x=long, y=lat, group=group))

census2<-spTransform(census, CRS("+proj=longlat + ellps=WGS84"))

ggmap(map) +
  geom_polygon(data=census2, aes(x=long, y=lat, group=group), alpha=0.5)

cblocks <- tract <- fortify(census2, region="geoid")
data <- census2@data[,c("geoid", "POP_DENS")]
colnames(data) <- c("id", "popdens")
plotData <- left_join(cblocks, data)
p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = popdens), color = "black", size = 0.25)
p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = popdens), color = "black", size = 0.25) +
  coord_map()
p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = popdens)) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", labels = percent,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE))


#--- 
#WORKS: http://www.kevjohnson.org/making-maps-in-r-part-2/
map <- get_map("Berkeley", zoom=13)    
ggmap(map) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = popdens),alpha = 0.65) +
  coord_map() +
  scale_fill_distiller(palette = "Reds", #labels = percent,
                       #breaks = pretty_breaks(n = 5)) +
                       breaks = y$brks) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend=TRUE) +
  labs(title = "Population Density by Block Group", fill = "")

#NOW try that with RENT!

#--- WORKS END -----


map <- get_map("Berkeley", zoom=14)    
p <- ggmap(map) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group, 
                                    fill = popdens), colour = NA, alpha = 0.5) +
  scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), 
                       labels = popdens) +
  labs(fill = "") +
  theme_nothing(legend = TRUE) +
  guides(fill = guide_legend(reverse = TRUE, override.aes = 
                               list(alpha = 1)))
#=====
library(scales)
ggplot() +
  geom_polygon(data = census2, aes(x = long, y = lat, group = group,
                                    fill = census2@data$HISPANIC)) +
  geom_polygon(data = census2, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", labels = percent,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE))
#====
ggplot() +
  geom_polygon(data=census2, aes(x=long, y=lat, color=census2$HISPANIC), alpha=0.5) +
  scale_colour_gradientn(colours=c( "#f9f3c2","#660000")) 

  ggplot() +
  geom_polygon(data = plotData2, aes(x = X, y = Y, group = geoid, 
                                    fill = HISPANIC), colour = NA, alpha = 0.5) +
  scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), 
                       labels = HISPANIC) +
  labs(fill = "") +
  theme_nothing(legend = TRUE) +
  guides(fill = guide_legend(reverse = TRUE, override.aes = 
                               list(alpha = 1))) geom_polygon(data = plotData, aes(x = long, y = lat, group = group, 
        fill = HISPANIC), colour = NA, alpha = 0.5) +
    scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), 
        labels = HISPANIC) +
    labs(fill = "") +
    theme_nothing(legend = TRUE) +
    guides(fill = guide_legend(reverse = TRUE, override.aes = 
        list(alpha = 1)))  

# references:
# https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
# First Created: OCT 25 2015


#### UC spatial data available at
#### http://ced.berkeley.edu/faculty/ratt/downloads/CCanyon/lab4Proj_files.zip 
#### Disclaimer: I do not own the UC spatial data. Anyone interested in the data
#### for other purposes should contact the owner directly.
#### Reference book: Applied Spatial Data Analysis with R, 2nd edition
################################################