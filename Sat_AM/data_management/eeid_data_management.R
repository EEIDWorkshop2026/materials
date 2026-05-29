
# Data location: 
# https://vectorbyte.crc.nd.edu/vectraits-dataset/557

# read in two datasets
# you will need to tell R where to find them on your computer
# I am using an .Rproj file, which sets my working directory within
# the working directory

# The function read.csv() reads in your .csv file containing all your data
# The green text within the " " marks indicates where your file is (its directory),
# which you will need to specify. 
dev = read.csv("data/VByte_557_development time_X[Interactor 1 Temp].csv")
longevity = read.csv("data/VByte_558_longevity_X[Interactor 1 Temp].csv")


head(dev) 
# This returns the first 6 lines of data from your new dataframe, "dev"

str(dev)
# This tells you the type of data contained in your  dataframe. 

hist(dev$Interactor1Temp)
# Great way to visualize the shape of your data. 
# If you data has anomalous data or outliers that could be the result of error,
# this is a great way to find it early. 

####---------------------tidyverse----------------------####

# resources:
# https://tidyverse.tidyverse.org/
# https://dplyr.tidyverse.org/  
# https://github.com/rstudio/cheatsheets/blob/main/tidyr.pdf

# the tidyverse is a powerful set of separate packages
# with functions that make working with data in R much easier
# however tidyverse functions often rely heavily on piping
# this is a pipe : %>%
# the pipe says the word, 'then'

#install if you don't have it
library(tidyverse)

## Group by, Mutate, and Summarise
# my favorite tidyverse command - group_by!
# group_by is kind of like using excels version of filter
# to create small a small dataset that is grouped by the variables

# Summarise
dev %>% 
  group_by(Interactor1Temp) %>% 
    summarise(Mean.development.time=mean(OriginalTraitValue,na.rm=TRUE))
# this gives you a summary table, it doesn't change dev!

# if you want to call this table something you would need to assign it
# when using summarise, you always want to call your summary table something different
# this is like making a pivot_table in excel

temp.table = dev %>% 
  group_by(Interactor1Temp) %>% 
    summarise(Mean.development.time=mean(OriginalTraitValue,na.rm=TRUE))
temp.table

## Summarise versus Mutate

# Mutate adds a column
# when mutating, you can just call the object you are making
# the same thing as your original dataframe
# because you are adding a column based on your
# original dataframe

dev = dev %>% 
  #take dev, then group by something
  #we re-assign dev to dev because we want to add the column to that dataframe
    group_by(Interactor1Temp,SecondStressorValue) %>% 
  #you can group_by multiple things
    mutate(sample.size=n())
#this adds a column to the dataframe 
#using the function n(), which counts things (e.g n rows in the group)

head(dev %>%
       select(c("sample.size","IndividualID", "OriginalTraitValue","SecondStressorValue","Interactor1Temp",)))
#this is just showing a few columns for effect

## Joining
# joining datasets together is a useful skill
# especially if we have two datasets we need to match on a specific column
# or set of columns
# https://dplyr.tidyverse.org/reference/mutate-joins.html

# lets join add the bat count data with the infection data

# always call your new dataframe something new
# don't write over and old dataframe in case you make
# a mistake joining

#inner_join(): includes all rows in x and y.
#when inner joining, non-matching rows will be dropped!

#left_join(): includes all rows in x.
# when left joining, every row in x is kept, but only those matching
# x are kept in y

#right_join(): includes all rows in y.
# when right joining, every row in y is kept, but only those matching
# y are kept in x


#full_join(): includes all rows in x or y.
# every row is kept in both x and y

# rename and select columns we want to join on
longevity2 = longevity %>%
  select(c("IndividualID","SecondStressorValue","Interactor1Temp","OriginalTraitValue")) %>%
  rename(
    c("longevity"="OriginalTraitValue" , 
  "resource" = "SecondStressorValue", 
  "temp" = "Interactor1Temp")
  )
#this is just keeping the columns we need to join with dev
# and renaming the OriginalTraitValue column to longevity so we know what it is after the join

# rename for easier tracking across columns
dev2 = dev %>%
  rename("development"="OriginalTraitValue",
         "resource" = "SecondStressorValue", 
  "temp" = "Interactor1Temp")
#this is just renaming the OriginalTraitValue column to development time so we know what it is after the join

# Join!
aa_data = left_join(
  x = dev2,
  y = longevity2,
  by = c("IndividualID","resource","temp")
  #columns to join on
)
head(aa_data %>%
       select(c("IndividualID","resource","temp","development","longevity")))

## Pivoting
# https://tidyr.tidyverse.org/articles/pivot.html
# sometimes our datasets are not in the format we want for an analysis
# right now, traits are in long form

aa_long = aa_data %>% 
  pivot_longer(
    cols = c("development","longevity"), 
    #what are the existing columns I want to make into rows?
    names_to =   "trait",
    #put the names of the columns in a column called 'trait'
    values_to = "trait_value"
    #the values that were in each of the columns get moved to a column called 'trait_value'
  )

head(aa_long %>%
       ungroup() %>%
  select(c("IndividualID","trait","trait_value")) )

## in class work ##
batcount = read.csv("data/bat_count.csv")


