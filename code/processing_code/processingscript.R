###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths

###
#Data is about Provisional Covid-19 Deaths by Sex and Age

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","exampledata.xlsx")

#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.
#rawdata <- readxl::read_excel(data/raw_data/Provisional_COVID-19_Deaths_by_Sex_and_Age.csv)
library(readr)
rawdata <- read_csv("data/raw_data/Provisional_COVID-19_Deaths_by_Sex_and_Age.csv")

#take a look at the data
dplyr::glimpse(rawdata)
summary(rawdata)
#dataset is so small, we can print it to the screen.
#that is often not possible.
print(rawdata)

# Clean Data for Georgia to see Year, Sex, Age Group, and deaths for (Covid 19, Pneumonia, and Influenza)
library(lubridate)
processeddata <- rawdata %>%
  dplyr::rename_all(funs(make.names(.))) %>%
  dplyr::select( Year, State, Sex, Age.Group, COVID.19.Deaths, Pneumonia.Deaths, Influenza.Deaths)%>%
  filter(State=="Georgia", Year!="NA", Sex!="All Sexes", State!="United States")

summary(processeddata)
str(processeddata)
print(processeddata)


# save data as RDS
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)

######################
# Compare Male and Female Total Deaths caused Covid 19 for ALL Ages in the state of Georgia. 



# Make comparisons between causes of Deaths (Covid 19, Pneumonia, and Flu) in the Years 2020 and 2021 for All Ages. 


#




#####################
