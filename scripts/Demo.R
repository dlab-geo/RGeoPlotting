#### Prepared for D-lab workshop series: Mapping in R
#### Instructors: Patty Frontiera & Shinhye Choi
#### contact: schoi [at] berkeley.edu for suggestions
#### 
#### New topics: lines starting with "###"

### installing spatial data packages
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", 
       "sp", "ggplot2", "scales", "Cairo", "maps")
# install.packages(x)                             # remember, you only install a package once!
lapply(x, library, character.only = TRUE)         # you could library them all at once.


### Topic: map plotting with built-in map data
### required package: ggplot2, maps

setwd("~/Dropbox/Dlab/SpatialPlot")          # set your own working directory
library(ggplot2)
library(maps)

# retrieve a map from the "maps" package 
world <- map_data("world")                        
ggplot(world, aes(x=long, y=lat, group=group)) +   
  geom_path() + coord_map("mercator")        # will talk more about coord_map later.

# let's talk about the object 
# group, order - what do they mean?
# group: grouping for each polygon.
head(world)

# a country may have multiple polygons:
# which country do you think should have more data points? why?
length(which(world$region=="India"))               
length(which(world$region=="Canada"))

ggplot(world[which(world$region=="India"),], aes(x=long, y=lat, group=group)) +   
  geom_path() + coord_map("mercator")  

ggplot(world[which(world$region=="Canada"),], aes(x=long, y=lat, group=group)) +   
  geom_path() + coord_map("mercator")

# there are multiple ways to map the same regions
north_a <- map_data("world", region=c("USA", "Canada"))
ggplot(north_a, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon(colour="black") +
  scale_fill_brewer(palette="Set3")              # what just happened?
                                                 # your options are limited, but there are bettter ways to do the same task.
# another possibility: extracting from the world map
north_a <- subset(north_a, long < -50 & lat > 20)
ggplot(north_a, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon(colour="black") +
  scale_fill_brewer(palette="Set3") 

# still another possibiilty
states <- map_data("state")  # ??map_data for available regions
ggplot(states, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon(colour="black", fill="white") 

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


### Topic: Plotting Spatial Points onto a Map retreived from the Goolge Maps API
# required package: ggmap (retrieving a map from the Google Maps API)
library(ggmap)

# data: cafes where I go often. let's not argue about my personal preferences.
dat <- read.csv("cafe.csv")
head(dat)
str(dat)

map <- get_map("Berkeley", zoom=14)                 # retrieve a map from Gmap API
ggmap(map) +
  geom_point(aes(x = long, y = lat, size = 4, colour = I(name)), 
             data = dat, alpha = .9)                # geom_points!

ggmap(map) +
  geom_point(aes(x = long, y = lat, size = price+2, colour = "red"), 
             data = dat, alpha = 1)                # geom_points!

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
setwd("~/Dropbox/Dlab/SpatialPlot/lab")
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
uc_bd <- readOGR(dsn=".", layer="uc_bldgs") 
summary(uc_bd)

# load other data
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


# references:
# https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
# First Created: OCT 25 2015


#### UC spatial data available at
#### http://ced.berkeley.edu/faculty/ratt/downloads/CCanyon/lab4Proj_files.zip 
#### Disclaimer: I do not own the UC spatial data. Anyone interested in the data
#### for other purposes should contact the owner directly.
#### Reference book: Applied Spatial Data Analysis with R, 2nd edition
################################################