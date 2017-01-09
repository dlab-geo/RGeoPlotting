# R geoplotting code

rm(list = ls()) # clean envi
setwd("~/Documents/Dlab/dlab_workshops/RGeoPlotting/data") # set working directory

# Make a vector of the packages used by this tutorial
required.pkg <- c("maps", "ggplot2", "ggmap", "maptools", "rgdal", "sp", "dplyr", "tidyr", "scales", "RColorBrewer", "classInt")

# Identify any uninstalled packages
pkgs.not.installed <- required.pkg[!sapply(required.pkg, function(p) require(p, character.only=T))]

# Install any needed packages that are not currentlty installed
if(length(pkgs.not.installed) > 0) {
  install.packages(pkgs.not.installed, dependencies=TRUE)
} else {
  print("All required packages installed.")
}

# Load all libraries them all at once.
lapply(required.pkg, library, character.only = TRUE)   

# geographic not geospatial data
fruit_data <- read.csv("fruit_consumption.csv")
head(fruit_data)

# geospatial ?
cafe <- read.csv("cafe.csv")
head(cafe)

?sp
getClass("Spatial") 

# make cafe a spatialPointsDataFrame (SPDF)
coordinates(cafe) <- c("long","lat") # or ~long+lat
head(cafe)
str(cafe)

#what is the crs
cafe@proj4string
proj4string(cafe) <- CRS("+proj=longlat + ellps=WGS84")
cafe@proj4string

#map the cafes
plot(cafe, pch=21, col="black", bg='palegreen') # all cafes  
plot(cafe[cafe$taste > 6,], pch=21, col="black", bg="darkgreen", add=T) # What does this line do?

great_cafes <- subset(cafe, taste > 6) 
plot(cafe, pch=21, col="black", bg='palegreen') # all cafes  
plot(great_cafes, pch=21, col="black", bg="red", add=T)

ogrInfo(".", "uc_bldgs")  # You can look at the data contents without reading in the data
uc_buildings <- readOGR(dsn=".", layer="uc_bldgs") # read data into R

summary(uc_buildings)
str(uc_buildings@data)
class(uc_buildings)
nrow(uc_buildings)

uc_boundary <- readOGR(dsn=".", layer="uc_bnd") # ucb boundary within Berkeley
census_blks <- readOGR(dsn=".", layer="censusblk") # Berkeley 2010 census blocks


plot(uc_boundary, border="blue")
plot(uc_buildings, col="red", add=T)
plot(census_blks, add=T)
points(cafe, col="green", pch=20)

proj4string(cafe) == proj4string(uc_buildings)
cafe@proj4string

uc_buildings@proj4string

cafe_sp3 <- spTransform(cafe, CRS("+proj=lcc +lat_1=37.06666666666667 +lat_2=38.43333333333333 +lat_0=36.5 +lon_0=-120.5 +x_0=2000000 +y_0=500000.0000000001 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# or more simply
cafe_sp3 <- spTransform(cafe, CRS(proj4string(uc_boundary)))

cafe_sp3@coords
cafe@coords


plot(uc_boundary, border="blue")
plot(census_blks, add=T)
plot(uc_buildings, col="red", add=T)
points(cafe_sp3, col="green", pch=20)

#
?ColorBrewer # access help pages
display.brewer.pal(3,"Set3") # view a color palette

display.brewer.all(type="qual")
display.brewer.all(type="seq")
display.brewer.all(type="div")

head(census_blks@data)
str(census_blks@data)
summary(census_blks$POP_DENS)
hist(census_blks$POP_DENS)
summary(census_blks$WHITE)
hist(census_blks$WHITE)
summary(census_blks$ONE_PARENT)
hist(census_blks$ONE_PARENT)

myclass <- classIntervals(census_blks$WHITE, 5, style = "quantile")
myclass
myclass2 <- classIntervals(census_blks$WHITE, 5, style = "equal")
myclass2
colpal  <- findColours(myclass, brewer.pal(5, "BuGn"))
colpal

data(package="maps")
plot(census_blks, border="grey", col=colpal)
plot(uc_boundary, add=T, border="blue", lwd=2)
plot(uc_buildings, add=T)

world <- map_data("world")
ggplot() + geom_path(data=world, aes(x=long, y=lat, group=group))  

head(world) # Take a look at the first few rows of data
tail(world) # Take a look at the last few rows of data
dim(world) # How many rows and columns
str(world) # Take a look at the data types

# Sum the number of rows for India
sum(world$region == "India")

# Compare
sum(world$region == "India") > sum(world$region == "Canada")

ggplot(world[world$region=="India",], aes(x=long, y=lat)) +   
  geom_path(aes(group=group)) + coord_map("mercator") 

# Compare
sum(world$region == "India") > sum(world$region == "Canada")

ggplot(world[world$region=="USA",], aes(x=long, y=lat)) +   
  geom_path(aes(group=group)) + coord_map("mercator") 

north_a_countries <- map_data("world", region=c("USA", "Canada"))
north_a <- subset(north_a_countries, long < -50 & lat > 20)

ggplot(north_a_countries, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group))

ggplot(north_a, aes(x=long, y=lat)) +   
  geom_path(aes(group=group))

na_plot <- ggplot(north_a, aes(x=long, y=lat)) +   
  geom_path(aes(group=group))

# Add a plot option to a plot variable!
na_plot + coord_map("mercator")

# and display it
na_plot

ggplot(north_a, aes(x=long, y=lat)) +
  geom_point(aes(group=group)) + coord_map("mercator")

states <- map_data("state")  #  See ?map_data for details
head(states)
ggplot(states, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=region), colour="black", fill="white")  #note: color, colour, and col all work!

state_att <- state.x77     # rename it

head(state_att)
str(state_att)

unique(states$region)
unique(row.names(state_att))   # Alaska/DC not in both data sets, but it's okay. why?
# tolower state names
#convert to a data frame
state_att <- as.data.frame(state_att[-which(row.names(state_att)=="Alaska"),])  # not necessary. why?

state_att$name <- tolower(row.names(state_att)) # why is this necessary?
states_dat <- merge(states, state_att, by.x="region", by.y="name")

head(states_dat)

states_dat <- arrange(states_dat, group, order)
ggplot(states_dat, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=Population), colour="black") +
  coord_map("mercator")  

ggplot(states_dat, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=Population), colour="NA") +
  coord_map("mercator") +
  labs(title = "Population by State") +
  theme_nothing(legend=TRUE)

ggplot(states_dat, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=Population), colour="NA") +
  scale_fill_distiller() +
  coord_map("mercator") +
  labs(title = "Population by State") +
  theme_nothing(legend=TRUE)

ggplot(states_dat, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=Population), colour="NA") +
  scale_fill_distiller(palette = "Reds", trans="reverse") +
  coord_map("mercator") +
  labs(title = "Population by State") +
  theme_nothing(legend=TRUE) +
  guides(fill = guide_legend(reverse = TRUE))