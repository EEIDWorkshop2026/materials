#### Read in your data

batdat = read.csv("data_management/bat_data.csv")
# The function read.csv() reads in your .csv file containing all your data
# The green text within the " " marks indicates where your file is (its directory),
# which you will need to specify. 

head(batdat) 
# This returns the first 6 lines of data from your new dataframe, "batdat"

str(batdat)
# This tells you the type of data contained in your batdat dataframe. 
# As you can see, our "date" column is currently being read by R 
# as a character, not as a date. We need to change that!


#### Formatting dates

batdat$date_long<-as.Date(batdat$date, format="%m/%d/%y")
# This line of code tells R that our "date" column contains date data! We need to
# tell R, however, what form the data currently takes so that it can correctly
# assign months, days, and years. 

str(batdat)
# As you can see, our column "date" has been renamed date_long is now read as date data

# Additional package to read and change date data is "lubridate":
# https://lubridate.tidyverse.org/


#### Check your data

head(batdat)
# Good first pass at checking your data

unique(batdat$species)
# Returns unique values in the species column. If we had naming inconsistencies,
# this would be a good way to find them. 

hist(batdat$gdL)
# Great way to visualize the shape of your data. 
# If you data has anomalous data or outliers that could be the result of error,
# this is a great way to find it early. 

summary(batdat$gdL)
# Returns summary statistics of our data. 
# Again, great way to find anomalous data. 

plot(batdat$temp, batdat$gdL)
# The plot() function works like this: plot(x-axis variable, y-axis variable)
# This is agreat way to just quickly plot some of your data against itself
# and find anomalous data that could be the result of an error. 



####
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

#install package if you don't have it
#remove the hashtag to install the packages
#install.packages('tidyverse')

# load the tidyverse package
library(tidyverse)

# read in two datasets
# you will need to tell R where to find them on your computer
# I am using an .Rproj file, which sets my working directory within
# the working directory

# first read in batdat again
#batdat = read.csv("data_management/bat_data.csv")

# read in bat count
batcount = read.csv("data_management/bat_count.csv")
# in this dataset, each row the count of hibernation site
# for a partcular species on a particular date
# even though sites were counted on the same date
# species are repeated across dates

# my favorite tidyverse command - group_by!
# group_by is kind of like using excels version of filter
# to create small a small dataset that is grouped by the variables
# for example, if I group_by species, date
# this is the equivalent in excel of selecting a species (e.g. MYLU)
# and a specific date (e.g. March 1 2016, 3/1/16)
# except group_by does this for every species and date combination in your dataset
# this is incredibly useful because in other programs, you might need
# to write a for loop to have this capability

## Group by, Mutate, and Summarise

#first add a column to the infection data that is the log10 of fungal loads
batdat$lgdL = log10(batdat$gdL)

# Summarise
batdat %>% 
  group_by(species) %>% 
  summarise(mean.fungal.loads=mean(lgdL,na.rm=TRUE))
# this gives you a summary table, it doesn't change batdat!

# if you want to call this table something you would need to assign it
# when using summarise, you always want to call your summary table something different
# this is like making a pivot_table in excel

fungal.load.table = batdat %>% 
  group_by(species) %>% 
  summarise(mean.fungal.loads=mean(lgdL,na.rm=TRUE))

fungal.load.table

## Summarise versus Mutate

# Mutate adds a column
# when mutating, you can just call the object you are making
# the same thing as your original dataframe
# because you are adding a column based on your
# original dataframe
batdat = batdat %>% 
  #replace batdat with a new dataframe that has something you want
  group_by(site,species,date) %>% 
  #you can group_by multiple things
  mutate(sample.size=n())
#this adds a column to the dataframe
batdat

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

batdat_count = left_join(
  x = batdat,
  y = batcount,
  by = c("site","species","date")
)

View(batdat_count)
# now, every row in batdat remains, and we have merged in counts

## Pivoting
# https://tidyr.tidyverse.org/articles/pivot.html
#sometimes our datasets are not in the format we want for an analysis

head(batdat_count)
# right now, species is in long format
# but we could imagine wanting to test the abundance of one species 
# and how that influences another
# for example, does the number of MYLU influence the number of MYSE?
# for that, we would need to make columns of each species count
# with a row for a site and a date

#pivot_wider is how we take long data, and make it wide

batcounts.wide<- batcount %>% 
  #this says - make a new df called batcounts.wide using bat counts
  pivot_wider(names_from = species, 
              values_from = count
              ) 
##make columns for each of the values in the species column and fill those columns with values from the count column

# when we perform this, it automatically fills missing with NAs
# but we know that missing actually mean 0

batcounts.wide = batcount %>% 
  pivot_wider(
  names_from = species, 
  values_from = count,
  values_fill = 0
)

# more often, we might be working in the opposite direction
# IMO, a more common issue is that programs output SO much
# information in columns that we would rather have in rows

batcounts.long = batcounts.wide %>% 
  pivot_longer(
    cols = c("EPFU","MYLU","MYSE","PESU"), 
    #what are the existing columns I want to make into rows?
    names_to =   "species",
    #put the names of the columns in a column called 'species'
    values_to = "count"
    #the values that were in each of the columns get moved to a column called 'count'
  )

View(batcounts.long)
# there are frequently more than one way to specify the same thing

#option 2
batcounts.long = batcounts.wide %>% 
  pivot_longer(
    cols = c(starts_with("MY"), "EPFU", "PESU"),
    #if all the columns start with the same thing (or many of them)
    #we can use 'starts_with
    names_to =   "species",
    values_to = "count"
  )
View(batcounts.long)

#option 3
batcounts.long = batcounts.wide %>% 
  pivot_longer(
    cols = -c(site, date),
    #this says use every column but site and date
    names_to =   "species",
    values_to = "count"
  )
View(batcounts.long)

#all do the same thing!
