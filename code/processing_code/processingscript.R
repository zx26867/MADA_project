###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(tidyverse) #for data processing
library(here) #to set paths
library(stringr)
library(ggplot2)
library(stringr)

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
processeddata <- selected_data %>% dplyr::filter(tolower(Country) == "united states" | tolower(Country) == "usa" | tolower(Country) == "us" ) %>% dplyr::filter( tolower(Currency) == "usd" )

# ensure the country and currency have only one entry
p5 = ggplot(processeddata, aes(x=Country)) + geom_bar(fill = "black") + coord_flip()
p5
#save figure
figure_file5 = here("results","resultfigure5.png")
ggsave(filename = figure_file5, plot=p5)


p6 = ggplot(processeddata, aes(x=Currency)) + geom_bar(fill = "black")
p6
#save figure
figure_file6 = here("results","resultfigure6.png")
ggsave(filename = figure_file6, plot=p6)

# remove rows with NA 
processeddata = processeddata %>% drop_na()

# now since currency and country only contains 1 outcome, delete them
processeddata = processeddata %>% select(-c(Currency,Country))                              

dplyr::glimpse(processeddata)

#check for #of groups and if there is any wierd entry
p7 = ggplot(processeddata, aes(x=Age)) + geom_bar(fill = "black")
p7
figure_file7 = here("results","resultfigure7.png")
ggsave(filename = figure_file7, plot=p7)
# Age entries are divided into 7 groups, remove 2 groups with almost no datapoints
processeddata <- processeddata %>% dplyr::filter( Age != "65 or over" ) %>% dplyr::filter( Age != "under 18" )
p8 = ggplot(processeddata, aes(x=Age)) + geom_bar(fill = "black")
p8
figure_file8 = here("results","resultfigure8.png")
ggsave(filename = figure_file8, plot=p8)

p9 = ggplot(processeddata, aes(x=Job)) + geom_bar(fill = "black") + coord_flip()
p9
figure_file9 = here("results","resultfigure9.png")
ggsave(filename = figure_file9, plot=p9)
# Job entries are too messy to manipulate, it is impossible to organize/group them as there are too many variations
# Therefore, Job title is dropped from the variable set
processeddata = processeddata %>% select(-Job)

p1 = ggplot(processeddata, aes(x=Industry)) + geom_bar(fill = "black") + coord_flip()
# Industry entries are messy, need some string manipulation to downsize the number of groups
p1
#save figure
figure_file1 = here("results","resultfigure1.png")
ggsave(filename = figure_file1, plot=p1)

indus = sort(table(processeddata$Industry),decreasing=TRUE)[1:20]
# These are the top 20 Industry entries and I will only include rows with these entries. 
# Other entries may be messy and may not contain enough data for modeling
indus = as.data.frame(indus)
s = ""
for (x in indus$Var1){s = paste(s, x,sep = "|")}
# print out the regex for copy-and-paste
s  
processeddata = processeddata %>% filter(grepl('^(Computing or Tech|Nonprofits|Education \\(Higher Education\\)|Health care|Accounting, Banking & Finance|Government and Public Administration|Engineering or Manufacturing|Marketing, Advertising & PR|Law|Business or Consulting|Media & Digital|Education \\(Primary\\/Secondary\\)|Insurance|Recruitment or HR|Retail|Art & Design|Property or Construction|Utilities & Telecommunications|Social Work|Transport or Logistics)$', Industry))

# plot Industry variable again to see if it works
p2 = ggplot(processeddata, aes(x=Industry)) + geom_bar(fill = "black") + coord_flip()
# it has top 20 industry, which is good
p2

#save figure
figure_file2 = here("results","resultfigure2.png")
ggsave(filename = figure_file2, plot=p2)

p3 = ggplot(processeddata, aes(x=State)) + geom_bar(fill = "black") + coord_flip()
p3
# State column contains messy entry too, we will select data from 50 states
figure_file3 = here("results","resultfigure3.png")
ggsave(filename = figure_file3, plot=p3)

processeddata = processeddata %>% filter(grepl('^(AL|Alabama|AK|Alaska|AZ|Arizona|AR|Arkansas|CA|California|CO|Colorado|CT|Connecticut|DE|Delaware|FL|Florida|GA|Georgia|HI|Hawaii|ID|Idaho|IL|Illinois|IN|Indiana|IA|Iowa|KS|Kansas|KY|Kentucky|LA|Louisiana|ME|Maine|MD|Maryland|MA|Massachusetts|MI|Michigan|MN|Minnesota|MS|Mississippi|MO|Missouri|MT|Montana|NE|Nebraska|NV|Nevada|NH|New Hampshire|NJ|New Jersey|NM|New Mexico|NY|New York|NC|North Carolina|ND|North Dakota|OH|Ohio|OK|Oklahoma|OR|Oregon|PA|Pennsylvania|RI|Rhode Island|SC|South Carolina|SD|South Dakota|TN|Tennessee|TX|Texas|UT|Utah|VT|Vermont|VA|Virginia|WA|Washington|WV|West Virginia|WI|Wisconsin|WY|Wyoming)$', State))
p4 = ggplot(processeddata, aes(x=State)) + geom_bar(fill = "black") + coord_flip()
p4
figure_file4 = here("results","resultfigure4.png")
ggsave(filename = figure_file4, plot=p4)

summary(processeddata$Salary)
# income range from 0 to 1650000, since salary = 0 is not reasonable, I should set a minimum value 10000
processeddata = processeddata %>% dplyr::filter( Salary > 10000 )
# make sure the filter worked
summary(processeddata$Salary)

p10 = ggplot(processeddata, aes(x=YearsProExp)) + geom_bar(fill = "black")
p10
figure_file10 = here("results","resultfigure10.png")
ggsave(filename = figure_file10, plot=p10)
# checked the entries for YearsProExp, it is well divided into 8 non-overlapping categories, remove a group with little data
processeddata <- processeddata %>% dplyr::filter( YearsProExp != "41 years or more" ) 
p11 = ggplot(processeddata, aes(x=YearsProExp)) + geom_bar(fill = "black")
p11
figure_file11 = here("results","resultfigure11.png")
ggsave(filename = figure_file11, plot=p11)

p12=ggplot(processeddata, aes(x=YearsExp)) + geom_bar(fill = "black")
p12
figure_file12 = here("results","resultfigure12.png")
ggsave(filename = figure_file12, plot=p12)
# checked the entries for YearsExp, it is well divided into 8 non-overlapping categories, remove a group with little data
processeddata <- processeddata %>% dplyr::filter( YearsExp != "41 years or more" ) 
p13=ggplot(processeddata, aes(x=YearsExp)) + geom_bar(fill = "black")
p13
figure_file13 = here("results","resultfigure13.png")
ggsave(filename = figure_file13, plot=p13)

p14=ggplot(processeddata, aes(x=Education)) + geom_bar(fill = "black")
p14
# checked the entry for education, found 6 categories
figure_file14 = here("results","resultfigure14.png")
ggsave(filename = figure_file14, plot=p14)

p15=ggplot(processeddata, aes(x=Race)) + geom_bar(fill = "black") + coord_flip()
p15
figure_file15 = here("results","resultfigure15.png")
ggsave(filename = figure_file15, plot=p15)
# checked the entry for race, found data messy, need string manipulation
# The plot shows over 95% of entry is white. Therefore this variable does not contain much variation and should be dropped.
processeddata = processeddata %>% select(-Race)

p16=ggplot(processeddata, aes(x=Gender)) + geom_bar(fill = "black")
p16
figure_file16 = here("results","resultfigure16.png")
ggsave(filename = figure_file16, plot=p16)
# checked entry for gender, find 4 categories
processeddata <- processeddata %>% dplyr::filter( Gender != "Other or prefer not to answer" ) 
p17=ggplot(processeddata, aes(x=Gender)) + geom_bar(fill = "black")
p17
figure_file17 = here("results","resultfigure17.png")
ggsave(filename = figure_file17, plot=p17)

# save data as RDS
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


