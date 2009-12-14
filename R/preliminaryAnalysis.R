library(ggplot2)
library(RMySQL)
library(gplots)
library(RColorBrewer)

# Connect to the database. This line assumes that the data has been loaded in a local instance of a MySQL Database
# Username: ClimateChange
# Table: ClimateChange
con <- dbConnect(MySQL(), user="ClimateChange", dbname="ClimateChange", host="localhost")

# Obtain the list of stations (id, name, country, latitude and longitude)
stations <- dbGetQuery(con, "select id, name, country, lat, lon from stations")

hist(stations$lon)
hist(stations$lat)

# Plot a 2D histogram of the stations in lon,lat coordinates. Hide the default axes, add a title, and relabel the axes
# Longitude has to be multiplied by -1 in order to display the map following the convention: negative: west, zero: center, positive: east
# TODO: Use spatial classes to hold this data and be able to reproject, overlay other spatial objects, select polygons, export results to other formats (eg. KML, Shape)
png(file="stations_distribution.png", width=500, bg="grey")
hist2d((-1)*stations$lon, stations$lat, xlab="Longitude", ylab="Latitude", main = "Station distribution", axes=FALSE)
axis(2,axTicks(2),lab=seq(-50,50,50))
axis(1,axTicks(1),lab=seq(150,-150,-50))
dev.off()


# Obtain the readings per year and the delta variation between years
readings_per_year <- dbGetQuery(con, "select theyear, count(*) from readings group by theyear")
names(readings_per_year) <- c("year", "count")
readings_per_year <- cbind(readings_per_year, c(0, diff(readings_per_year$count)))
names(readings_per_year) <- c("year", "count", "delta")

# Plot the readings per year
png(file="readings_per_year.png", width=500, bg="grey")
d <- ggplot(readings_per_year, aes(x=year)) + geom_point(aes(y=count))
d <- d + opts(title = "Number of readings per year") + xlab("Years") + ylab("Number of readings")
print(d)
dev.off()

# Plot the delta variation in readings per year
ggplot(readings_per_year, aes(x=year)) + geom_point(aes(y=delta))

# Define the regions we are dividing the world in. 
# Rounded the values available on wikipedia http://en.wikipedia.org/wiki/Latitude#Important_named_circles_of_latitude
artic <- c(66.3, 90.0)
north <- c(23.3, 66.3)
tropic <- c(-23.3, 23.3)
south <- c(-66.3, -23.3)
antarctic <- c(-90.0, -66.3) 

# Define function to return the stations of the dataset that fall between the latitudes latMin and latMax
stationsPerLatitude <- function(latMin, latMax) {
	stations <- dbGetQuery(con, paste("select id, name, country, lat, lon, height, first_good_year from stations where lat>=", latMin, " and lat<=", latMax ))
	stations
}

# Define function to return the stations of the dataset that fall inside one of the previously defined bands or regions
stationsPerBand <- function(band) {
	stations <- stationsPerLatitude(min(band), max(band))
	stations
}

# Define function to return all the measurements in the dataset for a given station after a given year
measurementsPerStation <- function(id, first_good_year) {
	measurements <- dbGetQuery(con, paste("select theyear, themonth, val from readings where theyear>=",first_good_year," and id=", id))
	names(measurements) <- c("Year", "Month", "Temperature")
	measurements
}

# Define a function that returns all the measurements that fall between the latitudes latMin and latMax
measurementsPerLatitude <- function(latMin, latMax) {
	sql <- paste("select lat, lon, height, theyear, themonth, val from (select id, name, lat, lon, height, first_good_year from stations where lat>=", latMin,
	 " and lat <=", latMax, ") as sStations inner join readings on sStations.id = readings.station_id where theyear>=first_good_year");
	measurements <- dbGetQuery(con, sql)
	names(measurements) <- c("Latitude", "Longitude", "Altitude", "Year", "Month", "Temperature")
	measurements
}

# Define function to return all the measurements that fall inside one of the previously defined bands or regions
measurementsPerBand <- function(band) {
	measurements <- measurementsPerLatitude(min(band), max(band))
	measurements
}

# Obtain the stations that fall in each band
antarctic.stations <- stationsPerBand(antarctic)
north.stations <- stationsPerBand(north)
south.stations <- stationsPerBand(south)
artic.stations <- stationsPerBand(artic)
tropic.stations <- stationsPerBand(tropic)

# Count for each year from 1700 to 2009 how many stations were active and with reliable data per year
antarctic.count <- sapply(seq(1700,2009, 1), function(x) sum(antarctic.stations$first_good_year<=x))
north.count <- sapply(seq(1700,2009, 1), function(x) sum(north.stations$first_good_year<=x))
south.count <- sapply(seq(1700,2009, 1), function(x) sum(south.stations$first_good_year<=x))
artic.count <- sapply(seq(1700,2009, 1), function(x) sum(artic.stations$first_good_year<=x))
tropic.count <- sapply(seq(1700,2009, 1), function(x) sum(tropic.stations$first_good_year<=x))

# Put all the data in a single dataframe in order to make more compact the ggplot call
station_count <- data.frame(year=seq(1700,2009,1), antarctic.count, artic.count, north.count, south.count, tropic.count)

# Define a colour palette from Color Brewer http://colorbrewer2.org/
catColor <- brewer.pal(5,"Set1")

# Define the plot of all the station counts with all the regions and a colour per serie and a legend
d <- ggplot(station_count, aes(x=year)) +
 geom_line(aes(y=artic.count, colour="Artic")) +
 geom_line(aes(y=north.count, colour="North")) + 
 geom_line(aes(y=tropic.count, colour="Tropic")) +
 geom_line(aes(y=south.count, colour="South")) +
 geom_line(aes(y=antarctic.count, colour="Antarctic")) +
 scale_colour_manual(name="Regions",
	values=c("Artic"=catColor[5], 
			 "North"=catColor[4],
			 "Tropic"=catColor[3],
			 "South"=catColor[2],
			 "Antarctic"=catColor[1])) 

# Add title and label the axis
d <- d + opts(title = "Number of stations per year") + xlab("Years") + ylab("Number of stations")

# Plot the chart and store it to a file as a png
png(file="number_stations_per_year.png", width=500, bg="grey")
print(d)
dev.off()

# Convert values of -99.00 to NA (R for not available data)
# Filter the data to remove NA values and process only those. Repeat for each of the bands
artic.data <- measurementsPerBand(artic)
artic.data[artic.data$Temperature == -99.00, "Temperature"] <- NA
artic.data.filter <- na.omit(artic.data)

north.data <- measurementsPerBand(north)
north.data[north.data$Temperature == -99.00, "Temperature"] <- NA
north.data.filter <- na.omit(north.data)

south.data <- measurementsPerBand(south)
south.data[south.data$Temperature == -99.00, "Temperature"] <- NA
south.data.filter <- na.omit(south.data)

antarctic.data <- measurementsPerBand(antarctic)
antarctic.data[antarctic.data$Temperature == -99.00, "Temperature"] <- NA
antarctic.data.filter <- na.omit(antarctic.data)

tropic.data <- measurementsPerBand(tropic)
tropic.data[tropic.data$Temperature == -99.00, "Temperature"] <- NA
tropic.data.filter <- na.omit(tropic.data)


# Calculate the mean Temperature, Latitude, and Altitude on a monthly basis for all the years of the dataset. Repeat for all the regions
tropic.data.mean <- aggregate(tropic.data.filter[,c("Temperature", "Latitude", "Altitude")], list(tropic.data.filter$Year, tropic.data.filter$Month), mean)
names(tropic.data.mean) <- c("Year", "Month", "Temperature", "Latitude", "Altitude")
tropic.data.mean<-tropic.data.mean[order(tropic.data.mean$Year, tropic.data.mean$Month),]

artic.data.mean <- aggregate(artic.data.filter[,c("Temperature", "Latitude", "Altitude")], list(artic.data.filter$Year, artic.data.filter$Month), mean)
names(artic.data.mean) <- c("Year", "Month", "Temperature", "Latitude", "Altitude")
artic.data.mean<-artic.data.mean[order(artic.data.mean$Year, artic.data.mean$Month),]

south.data.mean <- aggregate(south.data.filter[,c("Temperature", "Latitude", "Altitude")], list(south.data.filter$Year, south.data.filter$Month), mean)
names(south.data.mean) <- c("Year", "Month", "Temperature", "Latitude", "Altitude")
south.data.mean<-south.data.mean[order(south.data.mean$Year, south.data.mean$Month),]

north.data.mean <- aggregate(north.data.filter[,c("Temperature", "Latitude", "Altitude")], list(north.data.filter$Year, north.data.filter$Month), mean)
names(north.data.mean) <- c("Year", "Month", "Temperature", "Latitude", "Altitude")
north.data.mean<-north.data.mean[order(north.data.mean$Year, north.data.mean$Month),]

antarctic.data.mean <- aggregate(antarctic.data.filter[,c("Temperature", "Latitude", "Altitude")], list(antarctic.data.filter$Year, antarctic.data.filter$Month), mean)
names(antarctic.data.mean) <- c("Year", "Month", "Temperature", "Latitude", "Altitude")
antarctic.data.mean<-antarctic.data.mean[order(antarctic.data.mean$Year, antarctic.data.mean$Month),]

# Add a column called "type" to the dataset that contains the name of the region, and merge all the regions into a single dataset to ease the ggplot syntax
temp <- artic.data.mean
temp$type <- rep("Artic", nrow(artic.data.mean))
temp2 <- north.data.mean
temp2$type <- rep("North", nrow(north.data.mean))
temp<-rbind(temp, temp2)
temp2 <- tropic.data.mean
temp2$type <- rep("Tropic", nrow(tropic.data.mean))
temp<-rbind(temp, temp2)
temp2 <- south.data.mean
temp2$type <- rep("South", nrow(south.data.mean))
temp<-rbind(temp, temp2)
temp2 <- antarctic.data.mean
temp2$type <- rep("Antarctic", nrow(antarctic.data.mean))
temp<-rbind(temp, temp2)

data.mean <- temp

# Plot in a facetted chart the monthly temperatures for each region and the whole dataset. Color coded are the years.
d<-ggplot(data.mean, aes(Month, Temperature, colour=Year)) + geom_point() + facet_grid(. ~ type) + opts(title = "Temperature evolution per region") 
png(file="temperature_evolution_per_region.png", width=500, bg="grey")
print(d)
dev.off()


# Dummy plots to convert to polar coordinates each of the monthly average temperature for each region. Angle represents months, radius represents temperature
plot(south.data.mean$Temperature * cos(south.data.mean$Month/6*pi),south.data.mean$Temperature * sin(south.data.mean$Month/6*pi) )
plot(north.data.mean$Temperature * cos(north.data.mean$Month/6*pi),north.data.mean$Temperature * sin(north.data.mean$Month/6*pi) )
plot(tropic.data.mean$Temperature * cos(tropic.data.mean$Month/6*pi), tropic.data.mean$Temperature * sin(tropic.data.mean$Month/6*pi) )
plot(artic.data.mean$Temperature * cos(artic.data.mean$Month/6*pi), artic.data.mean$Temperature * sin(artic.data.mean$Month/6*pi) )
plot(antarctic.data.mean$Temperature * cos(antarctic.data.mean$Month/6*pi), antarctic.data.mean$Temperature * sin(antarctic.data.mean$Month/6*pi) )

# Plot the monthly temperature evolution for the north region in polar coordinates, color coded are the years. Same as above but more compact and using ggplot2
d<-ggplot(north.data.mean) + geom_point(aes(x=Month, y=Temperature , colour=Year)) + coord_polar()+ opts(title = "Monthly temperature evolution (North region)") 
png(file="temperature_evolution_per_north.png", width=500, bg="grey")
print(d)
dev.off()


# Persist to tab separated files (tsv) the mean dataset for each region. Those files are used in the Processing video
write.table(north.data.mean, "north.tsv", row.names=FALSE, col.names=FALSE, sep="\t")
write.table(south.data.mean, "south.tsv", row.names=FALSE, col.names=FALSE, sep="\t")
write.table(artic.data.mean, "artic.tsv", row.names=FALSE, col.names=FALSE, sep="\t")
write.table(antarctic.data.mean, "antarctic.tsv", row.names=FALSE, col.names=FALSE, sep="\t")
write.table(tropic.data.mean, "tropic.tsv", row.names=FALSE, col.names=FALSE, sep="\t")


# Plot mean monthly temperatures smoothed
d<- ggplot() + 
 geom_smooth(data=north.data.mean, aes(x=Year+(Month-1)/12.0, y=Temperature, colour="North")) +
 geom_smooth(data=south.data.mean, aes(x=Year+(Month-1)/12.0, y=Temperature, colour="South")) +
 geom_smooth(data=tropic.data.mean, aes(x=Year+(Month-1)/12.0, y=Temperature, colour="Tropic")) +
 geom_smooth(data=artic.data.mean, aes(x=Year+(Month-1)/12.0, y=Temperature, colour="Artic")) +
 geom_smooth(data=antarctic.data.mean, aes(x=Year+(Month-1)/12.0, y=Temperature, colour="Antarctic")) +
 scale_colour_manual(name="Regions",
	values=c("Artic"=catColor[5], 
			 "North"=catColor[4],
			 "Tropic"=catColor[3],
			 "South"=catColor[2],
			 "Antarctic"=catColor[1])) 

# Add title and axis labeling. Store to a png file
d<-d + opts(title = "Temperature evolution") + xlab("Years") + ylab("Temperature")
png(file="temperature_evolution_smoothed.png", width=500, bg="grey")
print(d)
dev.off()

# Obtain the yearly Altitude, Latitude, Temperature averages and calculate the correlation coefficient between the Temperature and the Mean Latitude	
north.data.yearly <-aggregate(north.data.mean[,c("Altitude", "Latitude", "Temperature")], list(north.data.mean$Year), mean)
names(north.data.yearly )<-c("Year","Altitude" ,"Mean_Latitude", "Temperature")
cor(north.data.yearly$Temperature, north.data.yearly$Mean_Latitude)

south.data.yearly <-aggregate(south.data.mean[,c("Altitude", "Latitude", "Temperature")], list(south.data.mean$Year), mean)
names(south.data.yearly )<-c("Year","Altitude" ,"Mean_Latitude", "Temperature")
cor(south.data.yearly$Temperature, south.data.yearly$Mean_Latitude)

tropic.data.yearly <-aggregate(tropic.data.mean[,c("Altitude", "Latitude", "Temperature")], list(tropic.data.mean$Year), mean)
names(tropic.data.yearly )<-c("Year","Altitude" ,"Mean_Latitude", "Temperature")
cor(tropic.data.yearly$Temperature, tropic.data.yearly$Mean_Latitude)

artic.data.yearly <-aggregate(artic.data.mean[,c("Altitude", "Latitude", "Temperature")], list(artic.data.mean$Year), mean)
names(artic.data.yearly )<-c("Year","Altitude" ,"Mean_Latitude", "Temperature")
cor(artic.data.yearly$Temperature, artic.data.yearly$Mean_Latitude)

antarctic.data.yearly <-aggregate(antarctic.data.mean[,c("Altitude", "Latitude", "Temperature")], list(antarctic.data.mean$Year), mean)
names(antarctic.data.yearly )<-c("Year","Altitude" ,"Mean_Latitude", "Temperature")
cor(antarctic.data.yearly$Temperature, antarctic.data.yearly$Mean_Latitude)


# Plot the yearly temperatures for the north region. All points + smoothed average
d<-ggplot(north.data.yearly, aes(x=Year))+geom_point(aes(y=Temperature))
d<- d+ opts(title="Temperature yearly average (North region)") + ylab("Average temperature")+ geom_smooth(aes(y=Temperature))
png(file="temperature_yearly_north_misleading.png", width=500, bg="grey")
print(d)
dev.off()

# Plot the yearly temperatures for the north region. All points + smoothed average. Color coded are the mean latitude value corresponding to that year
d<-ggplot(north.data.yearly, aes(x=Year))+geom_point(aes(y=Temperature, colour=Mean_Latitude))
d<- d+ opts(title="Temperature yearly average (North region)") + ylab("Average temperature")+ geom_smooth(aes(y=Temperature))
png(file="temperature_yearly_north_latitude.png", width=500, bg="grey")
print(d)
dev.off()


d<-ggplot(north.data.yearly, aes(x=Year))+geom_point(aes(y=Temperature, colour="Temperature")) + geom_point(aes(y=Mean_Latitude, colour="Mean Latitude"))
d<- d+ opts(title="Temperature yearly average (North region)") + ylab("Average temperature / Mean Latitude")+ geom_smooth(aes(y=Temperature))
png(file="temperature_yearly_north_latitude2.png", width=500, bg="grey")
print(d)
dev.off()




