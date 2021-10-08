###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(stringr)
library(ggplot2)

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","jobs.xlsx")

#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.
rawdata <- readxl::read_excel(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

# renaming all verbose column names
rawdata = rename(rawdata,Age = `How old are you?`)
rawdata = rename(rawdata,Job = `Job title`)
rawdata = rename(rawdata,Salary = `Annual salary`)
rawdata = rename(rawdata,YearsProExp = `Overall years of professional experience`)
rawdata = rename(rawdata,YearsExp = `Years of experience in field`)
rawdata = rename(rawdata,Education = `Highest level of education completed`)

# select data column of interest
selected_data = select(rawdata,c(Age,Industry,Job,Salary,Currency,Country,State,YearsProExp,YearsExp,Education,Gender,Race))

# We only interested in US data, so filter by country and currency
processeddata <- selected_data %>% dplyr::filter( Country == "United States" ) %>% dplyr::filter( Currency == "USD" )
                              

dplyr::glimpse(processeddata)

#check for #of groups and if there is any wierd entry
ggplot(processeddata, aes(x=Age)) + geom_bar(fill = "black")
# Age entries are divided into 7 groups 

ggplot(processeddata, aes(x=Industry)) + geom_bar(fill = "black")
# Industry entries are messy, need some string manipulation to downsize the number of groups

summary(processeddata$Salary)
# income range from 0 to 1650000, since salary = 0 is not reasonable, I should set a minimum value 10000
processeddata = processeddata %>% dplyr::filter( Salary > 10000 )
# make sure the filter worked
summary(processeddata$Salary)

# ensure the country and currency have only one entry
ggplot(processeddata, aes(x=Country)) + geom_bar(fill = "black")
ggplot(processeddata, aes(x=Currency)) + geom_bar(fill = "black")

ggplot(processeddata, aes(x=YearsProExp)) + geom_bar(fill = "black")
# checked the entries for YearsProExp, it is well divided into 8 non-overlapping categories

ggplot(processeddata, aes(x=YearsExp)) + geom_bar(fill = "black")
# checked the entries for YearsExp, it is well divided into 8 non-overlapping categories

ggplot(processeddata, aes(x=Education)) + geom_bar(fill = "black")
# checked the entry for education, found 6 categories + an NA category

ggplot(processeddata, aes(x=Race)) + geom_bar(fill = "black")
# checked the entry for race, found data messy, need string manipulation

ggplot(processeddata, aes(x=Gender)) + geom_bar(fill = "black")
# checked entry for gender, find 4 categories + an NA category

# save data as RDS
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


