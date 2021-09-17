###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#Install additional packages
install.packages(c("ggpubr", "AICcmodavg"))

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse)
library(ggpubr)
library(AICcmodavg)


#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
NYC_Virus_Deaths <- readRDS(data_location)

#re-label missing data (NA) as 0. It is fair to assume no reported deaths as 0, as it is assumed death data is coming from hospitals, who must report all deaths.
NYC_Virus_Deaths %>%
  replace_na(list(COVID.19.Deaths = 0, Influenza.Deaths = 0, Pneumonia.Deaths = 0))
 


######################################
#Data exploration/description
######################################
#I'm using basic R commands here.
#Lots of good packages exist to do more.
#For instance check out the tableone or skimr packages

#summarize data 
data_summary = summary(NYC_Virus_Deaths)

#look at summary
print(data_summary)

#do the same, but with a bit of trickery to get things into the 
#shape of a data frame (for easier saving/showing in manuscript)
summary_df = data.frame(do.call(cbind, lapply(NYC_Virus_Deaths, summary)))

#save data frame table to file for later use in manuscript
summarytable_file = here("results", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)

#Once the data is loaded, we will want to alter the month variable a bit to make it "month during pandemic". This will create a sequential variable data set, irrespective of year. This will help up later on.
NYC_Virus_Deaths$Month[NYC_Virus_Deaths$Year == "2021"& NYC_Virus_Deaths$Month == 1] <- 13
NYC_Virus_Deaths$Month[NYC_Virus_Deaths$Year == "2021"& NYC_Virus_Deaths$Month == 2] <- 14
NYC_Virus_Deaths$Month[NYC_Virus_Deaths$Year == "2021"& NYC_Virus_Deaths$Month == 3] <- 15
NYC_Virus_Deaths$Month[NYC_Virus_Deaths$Year == "2021"& NYC_Virus_Deaths$Month == 4] <- 16
NYC_Virus_Deaths$Month[NYC_Virus_Deaths$Year == "2021"& NYC_Virus_Deaths$Month == 5] <- 17
NYC_Virus_Deaths$Month[NYC_Virus_Deaths$Year == "2021"& NYC_Virus_Deaths$Month == 6] <- 18
NYC_Virus_Deaths$Month[NYC_Virus_Deaths$Year == "2021"& NYC_Virus_Deaths$Month == 7] <- 19
NYC_Virus_Deaths$Month[NYC_Virus_Deaths$Year == "2021"& NYC_Virus_Deaths$Month == 8] <- 20
NYC_Virus_Deaths$Month[NYC_Virus_Deaths$Year == "2021"& NYC_Virus_Deaths$Month == 9] <- 21

#make a scatterplot of data (Covid-19 Deaths, Months)
#Make a separate data set for each of the sex-specific variables. 
NVD_All <- NYC_Virus_Deaths %>% dplyr::filter(Sex == "All Sexes")
NVD_Female <- NYC_Virus_Deaths %>% dplyr::filter(Sex == "Female")
NVD_Male <- NYC_Virus_Deaths %>% dplyr::filter(Sex == "Male")

######################################
#Plotting Virus Deaths as a function of 
#Time, separated by gender and by total population (ALL)
######################################

#Plot newly formed data sets for covid deaths with line of best fit for each gender and combination.
Covid_Month_All <- NVD_All %>% ggplot(aes(x=Month, y=COVID.19.Deaths)) + geom_point() + geom_smooth(method='lm') + ggtitle("Covid Deaths per Month Since March 2020: all genders")
Covid_Month_Female <- NVD_Female %>% ggplot(aes(x=Month, y=COVID.19.Deaths)) + geom_point() + geom_smooth(method='lm')+ ggtitle("Covid Deaths per Month Since March 2020: Females")
Covid_Month_Male <- NVD_Male %>% ggplot(aes(x=Month, y=COVID.19.Deaths)) + geom_point() + geom_smooth(method='lm')+ ggtitle("Covid Deaths per Month Since March 2020: Males")

#look at each figure
plot(Covid_Month_All)
plot(Covid_Month_Female)
plot(Covid_Month_Male)

##save figures

#Total population
figure_file = here("results","COVID_ALL_figure.png")
ggsave(filename = figure_file, plot=Covid_Month_All)

#Females
figure_file = here("results","COVID_FEMALE_figure.png")
ggsave(filename = figure_file, plot=Covid_Month_Female)

#Males
figure_file = here("results","COVID_MALE_figure.png")
ggsave(filename = figure_file, plot=Covid_Month_Male)

##Repeat Previous steps for Influenza

#Plot newly formed data sets for Influenza deaths with line of best fit for each gender and combination.
Influenza_Month_All <- NVD_All %>% ggplot(aes(x=Month, y=Influenza.Deaths)) + geom_point() + geom_smooth(method='lm') + ggtitle("Flu Deaths per Month Since March 2020: all genders")
Influenza_Month_Female <- NVD_Female %>% ggplot(aes(x=Month, y=Influenza.Deaths)) + geom_point() + geom_smooth(method='lm')+ ggtitle("Flu Deaths per Month Since March 2020: Females")
Influenza_Month_Male <- NVD_Male %>% ggplot(aes(x=Month, y=Influenza.Deaths)) + geom_point() + geom_smooth(method='lm')+ ggtitle("Flu Deaths per Month Since March 2020: Males")

#look at each figure
plot(Influenza_Month_All)
plot(Influenza_Month_Female)
plot(Influenza_Month_Male)

##save figures

#Total population
figure_file = here("results","Flu_ALL_figure.png")
ggsave(filename = figure_file, plot=Influenza_Month_All)

#Females
figure_file = here("results","Flu_FEMALE_figure.png")
ggsave(filename = figure_file, plot=Influenza_Month_Female)

#Males
figure_file = here("results","Flu_MALE_figure.png")
ggsave(filename = figure_file, plot=Influenza_Month_Male)

##Repeat Previous steps for Pneumonia

#Plot newly formed data sets for Influenza deaths with line of best fit for each gender and combination.
Pneumonia_Month_All <- NVD_All %>% ggplot(aes(x=Month, y=Pneumonia.Deaths)) + geom_point() + geom_smooth(method='lm') + ggtitle("Pneumonia Deaths per Month Since March 2020: all genders")
Pneumonia_Month_Female <- NVD_Female %>% ggplot(aes(x=Month, y=Pneumonia.Deaths)) + geom_point() + geom_smooth(method='lm')+ ggtitle("Pneumonia Deaths per Month Since March 2020: Females")
Pneumonia_Month_Male <- NVD_Male %>% ggplot(aes(x=Month, y=Influenza.Deaths)) + geom_point() + geom_smooth(method='lm')+ ggtitle("Pneumonia Deaths per Month Since March 2020: Males")

#look at each figure
plot(Pneumonia_Month_All)
plot(Pneumonia_Month_Female)
plot(Pneumonia_Month_Male)

##save figures

#Total population
figure_file = here("results","Pneumonia_ALL_figure.png")
ggsave(filename = figure_file, plot=Pneumonia_Month_All)

#Females
figure_file = here("results","Pneumonia_FEMALE_figure.png")
ggsave(filename = figure_file, plot=Pneumonia_Month_Female)

#Males
figure_file = here("results","Pneumonia_MALE_figure.png")
ggsave(filename = figure_file, plot=Pneumonia_Month_Male)

##Finally, we will want to visualize how the three different virus deaths compare over time.
NVD_ALL_2 <- pivot_longer(NVD_All, cols=6:8, names_to = "Virus_Type", values_to = "Deaths")
  

Virus_Death_Plot <- NVD_ALL_2 %>% 
  ggplot(aes(x=Month, y=Deaths, color = Virus_Type)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  ggtitle("Comparison of Virus Deaths Over Time")

#View combined virus death plot
plot(Virus_Death_Plot)

#Save the figue
figure_file = here("results","Virus_Deaths_figure.png")
ggsave(filename = figure_file, plot=Virus_Death_Plot)

######################################
#Data fitting/statistical analysis
######################################

# Run an ANOVA test for the three different virus deaths over the entire provided time period.
anova_one_way <- aov(Deaths~Virus_Type, data = NVD_ALL_2)
summary(anova_one_way)


# place results from fit into a data frame with the tidy function
aov_table <- broom::tidy(anova_one_way)

#look at fit results
print(aov_table)

# save fit results table  
table_file_Covid = here("results", "Covid_resulttable.rds")
saveRDS(lmtable_Covid, file = table_file_Covid)

#Based on the reported p-value of 0.382, there is no significant difference between deaths resulting from COVID, Influenza, or Pneumonia. 
#Based on previous knowledge about the high virulence of COVID-19 compared to Influenza and Pneumonia and the results of the produced figures, one possible explanation for this finding is reporting bias.
#COVID was the main focus of reported deaths during the pandemic. If you compare the created figures, n for Influenza and Pneumonia deaths is quite low.
#Low n suggests that the ANOVA performed was biased due to low sample size.
#Additionally, Pneumonia is a known secondary infection of both Influenza and Covid-19 and usually the cause of death when an individual dies from Covid. It is possible that there is a conflation of cause of death.

  