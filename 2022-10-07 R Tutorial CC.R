## ----setup, results=F---------------------------------------------
knitr::opts_chunk$set(echo = TRUE) # this line tells RMD to display the code
library(dplyr) # standardlibrary for manipulating data in R
library(ggplot2) # standard library for plotting data in R 
library(tidyr) # standard library we'll use to "spread" the data (see below)
library(rmarkdown) # a library for making pretty tables for the document
# set the working directory (the folder on my computer where i'm keeping the data and stuff)
setwd("/Users/tristanmisko/Documents/Berkeley/ECON/SSPI\ Research/Tutorials/2022-10-07\ R\ Data\ Manipulation\ Tutorial")


## -----------------------------------------------------------------
#"<-" is R's funny notation for "=".  I think "=" also works but I'm an R purist
SSPI_countries <- read.csv("SSPI_countries.csv")
# look at the first few rows of SSPI_countries dataframe object 
head(SSPI_countries)


## -----------------------------------------------------------------
# use the select function from the dplyr library to eliminate the whitespace
SSPI_countries <- select(SSPI_countries, c(2,3))
#see how we removed the whitespace
head(SSPI_countries)


## -----------------------------------------------------------------
# import the data
carbon <- read.csv("FAOSTAT_data_en_9-28-2022.csv")
# check the dimensions
c(nrow(carbon), ncol(carbon))
# check column names to select the ones we're going to need
colnames(carbon)
#select the right columns
carbon <- select(carbon, c("Area", "Year", "Value"))
head(carbon)


## -----------------------------------------------------------------
# import the forest data
forest <- read.csv("fra2020-extentOfForest.csv")
#look at the column names
colnames(forest)
#rename them looking at the online database so we can tell what we're talking aobut
colnames(forest) <- c("Area", "FL_1990", "FL_2000", "FL_2010", "FL_2015", "FL_2016", "FL_2017", "FL_2018", "FL_2019", "FL_2020")
# select only the potentially relevant years
forest <- forest %>% select(!c("FL_2010", "FL_2016", "FL_2017", "FL_2019"))
head(forest)


## -----------------------------------------------------------------
# make a vector containing the SSPI_country Country names
countries <- SSPI_countries$Country
# list out the unique country names in the filtered datset
unique(filter(carbon, Area %in% countries)$Area)


## ---- results=F---------------------------------------------------
# check what the right names for the SSPI countries are
unique(carbon$Area)
# add the name used in the data to the list
countries <- c(countries, "Republic of Korea", 
               "United Kingdom of Great Britain and Northern Ireland",
               "United States of America", "Slovakia", "Türkiye", "Czechia")
# filter again and check to see that we have 48/49 countries
unique(filter(carbon, Area %in% countries)$Area)


## -----------------------------------------------------------------
# run the filter for countries
carbon <- filter(carbon, Area %in% countries)
# run the filter for years
carbon <- filter(carbon, Year <= 1999 | Year >= 2015)
# check out how many observations we have left
nrow(carbon)


## -----------------------------------------------------------------
# define the modYear function which takes in a vector of years and returns the vector
# of years with all 1990s values replaced by "1990s"
modYear <- Vectorize(function(year){
    if (year <= 1999 & year >= 1990){
        return("1990s")
    } else {
        return(as.character(year))
    }
}, "year")

# apply the modYear function to the Year column of carbon, and store the results
# in a new column of carbon called modYear
carbon$modYear <- modYear(carbon$Year)
#look at the the table of carbon to check that we got the expected result
head(carbon)
# use the group_by and summarize operators to make a new dataframe which does 
# the averaging and removes the irrelevant years
carbon <- carbon %>% group_by(Area, modYear) %>% summarize(Value = mean(Value, rm.na = T))
head(carbon)


## -----------------------------------------------------------------
# run the spread function and store the value as carbon
carbon <- spread(carbon, modYear, Value)
head(carbon)


## -----------------------------------------------------------------
# import the data into a new dataframe
fix_bnl <- read.csv("FAOSTAT_data_en_9-28-2022.csv")
# define a logical vector that is true if the Area name contains Belgium or Luxembourg and
# stor it as a column in fix_bnl dataframe
fix_bnl$contains_bnl <- grepl("Belgium", fix_bnl$Area) | grepl("Luxembourg", fix_bnl$Area)
# select the relevant columns and then filter for the columns which contain_bnl
fix_bnl <- fix_bnl %>% select(c("Area", "Year", "Value", "contains_bnl")) %>%
    filter(contains_bnl)
# look at the data
head(fix_bnl)


## -----------------------------------------------------------------
ratios <- fix_bnl %>% group_by(Year) %>% summarize(ratio = first(Value)/last(Value))
head(ratios)


## -----------------------------------------------------------------
# filter the fix_bnl dataframe to be only year 2000
bl2000 <- fix_bnl %>% filter(Year == 2000)
# check to make sure we filtered correctly
head(bl2000)
# compute the weights: noitice when we divide and multiply vectors by constants,
# all the elements of the vector get divided/multiplied by that constant.
w <- bl2000$Value/sum(bl2000$Value)
# look at the weights: Belgium is much bigger, so it gets higher weights (checks out)
w


## -----------------------------------------------------------------
# filter data down to combined Belgium-Luxembourg values ranging 1990 to 1999 only
bnl_1990s <- fix_bnl %>% filter(Area == "Belgium-Luxembourg")
# compute the mean value across the 1990s for combined Belgium-Luxembourg
bnl_1990s_mean <- mean(bnl_1990s$Value)
# print out the Belgium-Luxembourg 1990s mean
bnl_1990s_mean


## -----------------------------------------------------------------
# multiply the weight vector w by the 1990s mean
imputations <- bnl_1990s_mean*w
# print out the result
imputations


## -----------------------------------------------------------------
#our logical vector: backticks are used to specify numeric column names
is.na(carbon$`1990s`)
# using our logical vector to select the appropriate elements of our vector
carbon$`1990s`[is.na(carbon$`1990s`)]
# using our logical vector to overwrite the appropriate elements of our vector
carbon$`1990s`[is.na(carbon$`1990s`)] <- imputations
# check to see that our fix worked (huzzah!)
head(carbon)


## -----------------------------------------------------------------
# create a logical vector to see which countries are missing
check_inclusion <- unique(countries) %in% unique(filter(forest, Area %in% countries)$Area)
# present the missing countries in a human readable format
head(arrange(data.frame(Area = unique(countries), check_inclusion), Area))


## -----------------------------------------------------------------
#make an additional dataframe, defining the column names and the vectors they contain
additional_data <- data.frame(Area = c("Kuwait", "Saudi Arabia", "United Arab Emirates"), 
                              FL_1990 = c(0,0,0),
                              FL_2000 = c(0,0,0),
                              FL_2015 = c(0,0,0),
                              FL_2018 = c(0,0,0),
                              FL_2020 = c(0,0,0))
# check out our new extra dataframe
head(additional_data)
# rowbind the additional zeros into the original dataframe
forest <- rbind(forest, additional_data)
# see that we've done what we expected
head(forest)
# run the filter and overwrite forest
forest <- forest %>% filter(Area %in% countries)
# check that the number of rows is right
nrow(forest)


## -----------------------------------------------------------------
# create the merged dataframe
carbon_capture <- merge(carbon, forest)
# check that it looks right
head(carbon_capture)


## -----------------------------------------------------------------
# drop unnecessary columns
carbon_capture <- carbon_capture %>% select(!c("2016","2017","2019"))
# rename the carbon columns so we know what they are
colnames(carbon_capture)[2:5] <- c("C1990s", "C2015", "C2018","C2020")
# view the resulting dataframe to make sure everything's looking good
head(carbon_capture)


## -----------------------------------------------------------------
# compute an average of forest land for 1990s
carbon_capture$FL_1990s <- (carbon_capture$FL_1990 + carbon_capture$FL_2000)/2
# compute the ratio of carbon stock per forest land for the 1990s
carbon_capture$r1990s <- carbon_capture$C1990s/carbon_capture$FL_1990s
# compute the ratio of carbon stock per forest land for 2018
carbon_capture$r2018 <- carbon_capture$C2018/carbon_capture$FL_2018
# compute the percentage change in ratios
carbon_capture$pchg_1990s_to_2018 <- (carbon_capture$r2018 - carbon_capture$r1990s)/carbon_capture$r1990s * 100


## -----------------------------------------------------------------
# write the dataframe to a new csv file which we can upload
write.csv(carbon_capture, "carbon_capture.csv") 

