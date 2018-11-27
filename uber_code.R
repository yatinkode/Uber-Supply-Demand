###########################################################################################################################################################################
# R script for Handling Uber Requests from Airport to City and vice versa
###########################################################################################################################################################################


####################loading required libraries#############################################################################################################################

load.libraries <- c('tidyr','dplyr','lubridate','ggplot2','reshape2')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)


############################################################################################################################################################################


########################Loading csv data in data frame#####################################################################################################################

uber<-read.csv("Uber Request Data.csv")

###########################################################################################################################################################################


############################################ Problem Understanding ########################################################################################################

#Visually identify the most pressing problems for Uber. 

#Hint: Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; identify the most problematic types of requests 
#(city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) using plots

#Find out the gap between supply and demand and show the same using plots.

#Find the time slots when the highest gap exists

#Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

#What do you think is the reason for this issue for the supply-demand gap? Write the answer in less than 100 words. You may accompany the write-up with plot(s).

#Recommend some ways to resolve the supply-demand gap.

###########################################################################################################################################################################

####################################################### Data Understanding ################################################################################################

str(uber)

#there are 6 columns in the date and 6745 rows

summary(uber)

#useful insight :	Driver id and drop timestamp columns contains NA values
#				  	Pickup Points		: 2 (Airport and City)
#				  	Driver.id 			: 2650 NAs
#					Status				: 3(Cancelled, No Cars Avaliable, Trip Completed)
#					Request.Timestamp 	: 6711 rows are in non standard format [Other :6711]
#					Drop.timestamp:		: 2812 rows in non standatd format [Other :2812], 3914 NA values

#getting first 10 rows
head(uber,10)

#getting last 10 rows
tail(uber,10)



###########################################################################################################################################################################


####################################################### Data Cleaning #####################################################################################################

#As observed in the summary Request.timestamp and Drop.timestamp is not in correct format 
#Converting request timestamp in a single standard format

uber$Request.timestamp<-parse_date_time(uber$Request.timestamp, orders = c('dmy_HMS', "dmy_HM"))

#Converting Drop timestamp in a single standard format
uber$Drop.timestamp<-parse_date_time(uber$Drop.timestamp, orders = c('dmy_HMS', "dmy_HM"))

############################################################################################################################################################################


############################################### Univariate Analysis #######################################################################################################

#Get proportion of factor columns
#------------------------------------------------------------------------------------------------

prop.table(table(uber$Pickup.point))*100

#Airport     City 
#48.00593 	 51.99407 

prop.table(table(uber$Status))*100

#Cancelled 		No Cars Available    Trip Completed 
# 18.73981          	39.28836          41.97183 

# Checking continuous variables
#-------------------------------------------------------------------------------------------------

#Checking if Request Ids are unique

length(unique(uber$Request.id))
#6745
#count of unique Request ids are same as length of data frame hence they are unique

length(!unique(uber$Driver.id))
#301
#There are 301 unique drivers in total

#As the average of Drivers/Request.Ids Each drivers is expected to take on 22 trips as the optimal solution

length(unique(uber$Request.id))/length(!unique(uber$Driver.id))
#[1] 22.40864

#But it is observed that each driver takes around 13 requests

median(table(uber$Driver.id))
#[1] 13.5

#############################################################################################################################################################################


############################################## Segmented Analysis ###########################################################################################################

#segmenting Pickup point

Airport_pp<-filter(uber,uber$Pickup.point=="Airport")

table(Airport_pp$Status)

#Cancelled 		No Cars Available    	Trip Completed 
#      198              	 1713              	  1327 

#Higher number of No Cars Avaliable while booking cabs from Airport to City

City_pp<-filter(uber,uber$Pickup.point=="City")

table(City_pp$Status)

#Cancelled 		No Cars Available    	Trip Completed 
#    1066               	  937              	  1504

#Higher number of Cars cancelled from City to Airport



##############################################################################################################################################################################


##############################################  Bivariate Analysis  ###########################################################################################################

table(uber$Status)

#Cancelled No Cars Available    Trip Completed 
#    1264              2650              2831 

no_cars<-filter(uber,uber$Status=="No Cars Available")

sum(!is.na(no_cars$Driver.id))
#0

# data frame no_cars is created with "No Cars Available" filtered from complete dataset.
# Then we observe that there are no drivers where no cars are available which is obvious


################################################################################################################################################################################


###################################################### Derived Columns Analysis ################################################################################################


uber$weekday<-weekdays(uber$Request.timestamp)												#Getting weekdays out of dates


uber$req_hour <- format(strptime(hour(uber$Request.timestamp), "%H"), format = "%H")		#getting hour out of timestamp from request timestamp


uber$drop_hour <- format(strptime(hour(uber$Drop.timestamp), "%H"), format = "%H")			#getting hour out of timestamp from drop timestamp



#Filtering on basis of Status for getting hourly trend
#-----------------------------------------------------------------------------------------------------------------------------

no_cars<-filter(uber,uber$Status=="No Cars Available")					#Filtering uber by Status as "No Cars Available"
sort(table(no_cars$req_hour),decreasing=T)[1:5]

# 	18  20  19  21  17 
#	322 290 283 265 232 

#No cars available are mostly in the evening


cancelled<-filter(uber,uber$Status=="Cancelled")						#Filtering uber by Status as "Cancelled"
sort(table(cancelled$req_hour),decreasing=T)[1:5]
#	08  05  09  07  06 
#	178 176 175 169 145

#More trips are likely to be cancelled in the morning					


complete_t<-filter(uber,uber$Status=="Trip Completed")					#Filtering uber by Status as "Trip Completed"
sort(table(complete_t$req_hour),decreasing=T)[1:5]

# 	05  07  09  06  19 
#	185 174 173 167 166 

# No trend analyzed since 19 is included with 5,7,9 and 6


#Getting Weekly trend
#------------------------------------------------------------------------------------------------------------------------------------------------------

sort(table(no_cars$weekday),decreasing=T)				#getting count of cars per weekday for "No cars avaliable" and sorting it in descending order

# Friday  Thursday   Tuesday    Monday Wednesday 
#   580       571       505       504       490	  

sort(table(cancelled$weekday),decreasing=T)				#getting count of cars per weekday for "Cancelled" and sorting it in descending order

#Wednesday    Monday  Thursday    Friday   Tuesday 	   
#  270       262       252       240       240 

sort(table(complete_t$weekday),decreasing=T)			#getting count of cars per weekday for "Trips Completed" and sorting it in descending order

#Monday Wednesday   Tuesday    Friday  Thursday 
#   601       577       562       561       530 	


#No specific trend is observed weekly for all 3 status of cabs



uber_morning<-uber[as.numeric(uber$req_hour)>=5 & as.numeric(uber$req_hour)<12,]										#assigning timeframe for a set of hours morning 5 to 12			

uber_morning$req_timeframe<-"morning"															#subsetting a dataframe for assigning value morning to it in timeframe column


uber_afternoon<-uber[as.numeric(uber$req_hour)>=12 & as.numeric(uber$req_hour)<16,]  			#assigning timeframe for a set of hours afternoon 12 to 16

uber_afternoon$req_timeframe<-"afternoon"														#subsetting a dataframe for assigning  value afternoon to it in timeframe column



uber_evening<-uber[as.numeric(uber$req_hour)>=16 & as.numeric(uber$req_hour)<21,] 				#assigning timeframe for a set of hours evening 16 to 21

uber_evening$req_timeframe<-"evening"															#subsetting a dataframe for assigning  value evening to it in timeframe column


#here the whole night starts from 21 and ends upto 5 in morning 
#since we are not able to assign >21 and <5 in a single condition we divide it in 2 parts i.e. >=21 and <5

uber_night<-uber[as.numeric(uber$req_hour)>=21,] 												#assigning timeframe for a set of hours night 21 to 24. 

uber_night$req_timeframe<-"night"																#subsetting a dataframe for assigning  value night to it in timeframe column


uber_night2<-uber[as.numeric(uber$req_hour)<5,]													#assigning timeframe for a set of hours night 0 to 5. 

uber_night2$req_timeframe<-"night"																#subsetting a dataframe for assigning  value night to it in timeframe column


uber<-rbind(uber_morning,uber_afternoon,uber_evening,uber_night,uber_night2)					#joining all the subbsetted dataframes together to make the whole dataframe again


#####################################################################################################################################################################################################################


#######################################################################################Plotting Graphs in R and Finding Solution ####################################################################################


#Status wise count of Requests
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Status according to Timeframe of the day for City Trips
ggplot(subset(uber,Pickup.point=="City")) + geom_bar(aes(x = req_timeframe, fill = Status))

#Highest number of Cancelled requests are seen in the morning in the City

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Status according to Timeframe of the day for Airport Trips
ggplot(subset(uber,Pickup.point=="Airport")) + geom_bar(aes(x = req_timeframe, fill = Status))

#Highest number of No Cars Available are in the evening at Airport

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Weekly trend of Requests at the Airport
ggplot(subset(uber,Pickup.point=="Airport")) + geom_bar(aes(x = weekday, fill = Status))

# We can See that the same daily trend shown from the daily graph is Repeated for Weekdays for Airport Pickup Point

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Weekly trend of Requests in the City

ggplot(subset(uber,Pickup.point=="City")) + geom_bar(aes(x = weekday, fill = Status))

#We can See that the same daily trend shown from the daily graph is Repeated for Weekdays for Airport Pickup Point

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Plotting for Status according to Weekday and timeframe in a single graph so that one single trend is seen

ggplot(uber, aes(x = req_timeframe)) + geom_bar(aes(fill = Status), position = "dodge") +facet_grid(weekday~Pickup.point)


##################################################################################################################################################################################################################


########################################################################################## Assumptions ############################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Needed Average of Drivers
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

unique_drivers<- length(unique(uber$Driver.id)) 												#Unique drivers/Cars currently available

#Requests completed currently in evening peak time per driver from Airport
nrow(subset(uber,Status=='Trip Completed' & req_timeframe=='evening' & Pickup.point=='Airport'))/unique_drivers
#1.162791

#Requests required to be completed in evening peak time per driver from Airport
nrow(subset(uber,req_timeframe=='evening' & Pickup.point=='Airport'))/unique_drivers
#5.043189


#Requests completed currently in morning peak time per driver from City
nrow(subset(uber,Status=='Trip Completed' & req_timeframe=='morning' & Pickup.point=='City'))/unique_drivers
#1.996678

#Requests required to be completed in morning peak time per driver from City
nrow(subset(uber,req_timeframe=='morning' & Pickup.point=='City'))/unique_drivers
#6.48505

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Status wise Issues and Solutions
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#As we can see, the highest number of “No cars available” are in the Evening from Airport

#Also highest number of “Cancelled” requests are in the Morning from the City

#These two issues are highly  impacting the business of Uber. Lets look into them in detail

#Causes of the Problem in Evening
#1. No Cars are available in the Evening from Airport to City, The following may be the reasons
#2. The flights departing from Airport are is high in the morning, due to which there are more passengers who want to go to the Airport in the morning.
#3. Due to higher number of flights traffic is caused for the route from City to Airport in the morning
#4. The drivers are cancelling the rides due to high traffic

#Solution to No Cars Available in Evening
#1. Extra bonus amount should be provided to the drivers taking trips from Airport to City in the evening peak hours
#2. Daily evening surge pricing to be implemented at the Airport in the Evening on Prime Cabs.
#3. More Drivers/ Cars should be introduced in the Evening at the Airport#
#4. Coupons for Airport to City share rides should be introduced helping to ease City traffic and reducing the load of the drivers.


#Causes of the Problem in Morning
#1. Higher requests are cancelled by the drivers in the morning from City to Airport, The following may be the reasons:
#2. The flights landing at the Airport are is high, due to which there are more passengers then drivers/cars
#3. The route from Airport to City may be crowded at evening due to which more cabs are not able to reach the Airport.
#4. Drivers need to unnecessarily wait at the Airport due to which their is time wastage where they could have completed some other rides instead

#Solution to Cancelled Requests in Morning
#1. Fine should be imposed on drivers for cancelling more than a certain amount of rides per day
#2. Surge pricing should be imposed for Airport rides in the morning during heavy requests.
#3. Shared pool must be more available in the morning from City to Airport
#4. Since the demand is high the Drivers/Cars should be substantially increased in the morning from City to Airport Rides


####################################################################################################################################################################################################################


#####################################################################################################################################################################################################################

#Plotting graphs in Tableau and getting the solution more clearly

write.csv(uber,file="ubercleaned.csv")

######################################################################################################################################################################################################################



                                           