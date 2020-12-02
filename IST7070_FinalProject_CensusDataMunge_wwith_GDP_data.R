library(readr)
library(tidyverse)
# load in data
mortality_life_expectancy <- read_csv("~/Documents/Classes/Fall 2020/IST 707/Final Project/archive/mortality_life_expectancy.csv")
midyear_population_age_sex <- read_csv("~/Documents/Classes/Fall 2020/IST 707/Final Project/archive/midyear_population_age_sex.csv")
midyear_population_age_country_code <- read_csv("~/Documents/Classes/Fall 2020/IST 707/Final Project/archive/midyear_population_age_country_code.csv")
age_specific_fertility_rates <- read_csv("~/Documents/Classes/Fall 2020/IST 707/Final Project/archive/age_specific_fertility_rates.csv")
birth_death_growth_rates <- read_csv("~/Documents/Classes/Fall 2020/IST 707/Final Project/archive/birth_death_growth_rates.csv")
country_names_area <- read_csv("~/Documents/Classes/Fall 2020/IST 707/Final Project/archive/country_names_area.csv")
midyear_population_5yr_age_sex <- read_csv("~/Documents/Classes/Fall 2020/IST 707/Final Project/archive/midyear_population_5yr_age_sex.csv")
midyear_population <- read_csv("~/Documents/Classes/Fall 2020/IST 707/Final Project/archive/midyear_population.csv")

# Filter out years from before year 2000
mortality_life_expectancy<-subset(mortality_life_expectancy, year > 1999)
midyear_population_age_sex<-subset(midyear_population_age_sex, year > 1999)
midyear_population_age_country_code<-subset(midyear_population_age_country_code, year > 1999)
age_specific_fertility_rates<-subset(age_specific_fertility_rates, year > 1999)
birth_death_growth_rates<-subset(birth_death_growth_rates, year > 1999)
midyear_population_5yr_age_sex<-subset(midyear_population_5yr_age_sex, year > 1999)
midyear_population<-subset(midyear_population, year > 1999)

# year min and Maxs
min(mortality_life_expectancy$year)
min(midyear_population_age_sex$year)
min(midyear_population_age_country_code$year)
min(age_specific_fertility_rates$year)
min(birth_death_growth_rates$year)
#min(country_names_area$year)
min(midyear_population_5yr_age_sex$year)
min(midyear_population$year)

max(mortality_life_expectancy$year)
max(midyear_population_age_sex$year)
max(midyear_population_age_country_code$year)
max(age_specific_fertility_rates$year)
max(birth_death_growth_rates$year)
#max(country_names_area$year)
max(midyear_population_5yr_age_sex$year)
max(midyear_population$year)



# initial filtering's dimentsions
# requirements need a dataset to be minimum 20 features with 5,000 examples
# after out filter of pre2000 data every dataset has plenty of examples 
# I will only coment the number of columns



dim(mortality_life_expectancy) # 15 
dim(midyear_population_age_sex) # 106
dim(midyear_population_age_country_code) #109
dim(age_specific_fertility_rates) # 13
dim(birth_death_growth_rates) # 8
dim(country_names_area) # this is a small dataset that can easily be appended to any other
dim(midyear_population_5yr_age_sex) # 10 
dim(midyear_population) # 4

# only country_names_area is a small dataset as it has only the land size of countries
# this is the one we can easily append any econmic data if we are interested in doing this for only the current year
# however we should probably do it by year if we're interested in pursing this!


# looking at data
View(mortality_life_expectancy) # mortality rates, general but we can seperate out sex if we ignore the both sex columns like life_expectancy 
# ID c('country_code','country_name','year') 
# can be split into general, and by sex if to append to other dataframes
View(midyear_population_age_sex)# spread by country and year just summing population by age 0 - 100
# ID c('country_code','country_name','year','sex')
View(midyear_population_age_country_code)#spread by country and year just summing population by age 0 - 100
# sane as midyear_population_age_sex except it has extra columns c('age','permutation_role','population') 
# ID c('country_code','country_name','year') 
#permutation_role is only returned as child so it's useless, i also don't reallyt know what's up w the other extra columns
View(age_specific_fertility_rates) # female specific of of course. since it's binary we can just consider it for nongendered dataframes?
# ID c('country_code','country_name','year') 
View(birth_death_growth_rates) # birth/death rates.. 
# ID c('country_code','country_name','year') 
View(country_names_area)
# ID c('country_code','country_name') 
View(midyear_population_5yr_age_sex)
# ID c('country_code','country_name','year') 
View(midyear_population) # smaller version of midyear_population_5yr_age_sex
# ID c('country_code','country_name','year') 


# combining dataframe with dplyr
# two dataframes  one merged by c('country_code','country_name','year')  & one merged by c('country_code','country_name','year','sex')
# Combing for all data except the giant dataframe of midyear_population_age_country_code
# inner_join will enure matching b/w the DFs
# could probably do as a fn but I will just repeat the steps for each DF

CensusData <- inner_join(mortality_life_expectancy,age_specific_fertility_rates,by=c('country_code','country_name','year'))
CensusData <- inner_join(CensusData,birth_death_growth_rates,by=c('country_code','country_name','year'))
CensusData <- inner_join(CensusData,midyear_population_5yr_age_sex,by=c('country_code','country_name','year'))
dim(CensusData) # now it's 37 columns which meets the dataset requirements
# can still merge country data.. or so on if we like
sum(is.na(CensusData)) # is zero. 
View(CensusData) # THis is the complete munged Census Data




# THis is to scale the numeric data
ScaledCensusData<-CensusData 
ScaledCensusData[,-c(1,2,31,33)]<-scale(ScaledCensusData[,-c(1,2,31,33)]) # Scales teh numeric Data


hist.data.frame(CensusData) # makes a long hist of all teh data

colnames(CensusData)
length(unique(CensusData$country_name)) # just looking

dim(CensusData) # rows 254k col 37  
# mandatory be above 5k columns and 20 features





GDPData<- read_csv("~/Documents/Classes/Fall 2020/IST 707/Final Project/archive/annualGDPdata.csv")
#View(GDPData)
GDPData$year<-GDPData$X1
colnames(GDPData) # I cant tidy them up 
# There are non country columns to remove
GDPData <- subset(GDPData, select = -c(X1,`Developing Asia`,`Developing Countries`,`East Asia & Pacific developing`,`Europe & Central Asia developing`,`High Income Countries`,
                                       `High income: OECD`,`Latin America & Caribbean developing`,`Low-Income Countries (LIC)`,`Middle-Income Countries (MIC)`,
                                       `Middle East & N. Africa developing`,`High Income: Non-OECD`,`South Asia developing`,`Sub-Saharan Africa developing`,`World (WBG members)`))

colnames(GDPData) # checking the new cols

# we have to remane some of the countrie columns to match the census data naming convention
# change `Hong Kong SAR, China` to Hong Kong
GDPData$`Hong Kong`<-GDPData$`Hong Kong SAR, China`
# change iran to Iran
GDPData$`Iran`<-GDPData$`Iran, Islamic Rep.`
# change to Russia
GDPData$`Russia`<-GDPData$`Russian Federation`
# change to Taiwain
GDPData$`Taiwan`<-GDPData$`Taiwan, China`

# removing old country Cols
GDPData <- subset(GDPData, select = -c(`Hong Kong SAR, China`,`Iran, Islamic Rep.`,`Russian Federation`,`Taiwan, China`))
colnames(GDPData) # looks done. Now to convert to a long dataframe



# using gather from tidyr to reshape the data columns into rows
GDPData<- GDPData %>% gather(country_name, GDP, -year) # Specifying that a new Column for country_name, and GDP be gathered, exluding year which will remain as is to be gathered around
# now a long DF with 3 columns: year, country_code, and GDP.   ready to munge/combine with Census Data

sum(is.na(GDPData)) # 836 NAs  not.terrible

# Looking at ten years of Data
# 2009 to 2018
GDPData<-subset(GDPData, year > 2009 & year < 2018) # subsetting to a ten year span of available data


# now to join the two dataframes
CensusGDPdata<-subset(CensusData, year > 2009 & year < 2018) # trimming Census data to match GDP Data
CensusGDPdata <- inner_join(CensusGDPdata,GDPData,by=c('country_name','year')) # joining economic data to the census data
sum(is.na(CensusGDPdata)) # is zero
View(CensusGDPdata)


# dataFrame requires Categorical Data

colnames(CensusGDPdata)
# removing age data
CensusGDPdata <- subset(CensusGDPdata, select = -c(mortality_rate_under5, mortality_rate_under5_male, mortality_rate_under5_female,mortality_rate_1to4,
                                       mortality_rate_1to4_male,mortality_rate_1to4_female, fertility_rate_15_19, fertility_rate_20_24, 
                                       fertility_rate_25_29, fertility_rate_30_34, fertility_rate_35_39, fertility_rate_40_44, fertility_rate_45_49))
colnames(CensusGDPdata)
dim(CensusGDPdata) # 25 columns

CensusGDPdata <- subset(CensusGDPdata, select = -c(age_group_indicator, total_flag,gross_reproduction_rate))
dim(CensusGDPdata) # 22 columns



