# What: Software Carpentry Workshop
# Who: Arthur Endsley & Marian Schmidt
# Where: University of Michigan, Ann Arbor
# When: Oct.17-18, 2016

#############################################


# Packages necessary 
install.packages(c('RSQLite','dplyr','tidyr','ggplot2'))
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)

# RSQLite in R ----
conn <- dbConnect(SQLite(), dbname='/Users/CTRoeRaymond/Desktop/survey.sqlite') #connects database
tables <- dbListTables(conn) #allows us to view database tables in R
tables

class(tables)
 
surveys <- dbGetQuery(conn, 'SELECT * FROM surveys;') #second argument is SQL code
head(surveys)
summary(surveys)
names(surveys)

#joining species and plots tables into survey table, based on primary key for each
surveys.full <- dbGetQuery(conn, 
                      'SELECT * FROM surveys 
                      JOIN species ON surveys.species_id = species.species_id
                      JOIN plots ON surveys.plot_id = plots.plot_id;')
names(surveys.full)

dbDisconnect(conn) #make sure to Disconnect from database!
rm(conn)

# Joining Data in R ----

#Data-Frame Review
#collection of vectors of equal lengths
#have classes of vectors, and type of data within each vector
x1<-c(1,2,3) #create vector
class(x1) #numeric
typeof(x1) #double
x2<-c('a','b','c')
class(x2) #character
typeof(x2) #character
#can create data frames
df<-data.frame(
   x1 = c(TRUE, FALSE, TRUE),
   x2= c(1, 'red', 2))
df
class(df$x1) #logical
typeof(df$x1) #logical

# List Review
# ways of storing different data types together
# confusing, tend to avoid
list(99, TRUE, 'balloons')
# vectors can be stored in lists as well
list(1:10, c(TRUE, FALSE))

surveys <- read.csv('~/Dropbox/UMich_Workshops/2016-10 Software Carpentry Workshop/ecology.csv')
class(surveys)
str(surveys) #shows information within each data (like in Rstudio!)

# All different ways to call year column
class(surveys$year) #gives us a vector
head(surveys$year) #called "integer"

class(surveys['year']) #gives us a dataframe

class(surveys[,'year']) #gives us a vector

class(surveys[,4])

class(surveys[['year']]) 

# Factors -----
# have labels that are connected to integers
# can only contain a pre-defined set of values, that we call levels
levels(surveys$sex) #gives levels
nlevels(surveys$sex) #number of levels
# by default, factors are un-ordered

spice <- factor(c('low','medium','low','high'))
levels(spice)
nlevels(spice)
# to order this factor, need to provide argument that ordered=TRUE
spice <- factor(c('low','medium','low','high'), level=c('low','medium','high'), ordered=TRUE)
max(spice)
spice<-ordered(spice, levels=c('high','medium','low'))
max(spice)

# Tabulation ----
tabulation <- table(surveys$taxa) #counts number of records with each value
tabulation
barplot(tabulation)
# re-order according to number of counts
surveys$taxa <- ordered(surveys$taxa, levels=c('Rodent','Bird','Rabbit','Reptile'))
barplot(table(surveys$taxa))

# Cross-Tabulation
table(surveys$year, surveys$taxa)
with(surveys, table(year,taxa)) #shortcut way

# Ordering ----
order(surveys$weight) # order of the indicies, use to sort 
sort(surveys$weight) # values in order (but only does on one vector)
# To sort entire data frame by something
surveys[order(surveys$weight),] #order rows of surveys data frame according to order of weights vector

# Question: What was the median weight of each rodent species between 1980 and 1990?
surveys$taxa == 'Rodent' #returns logical vector, with each row where taxa='Rodent' as TRUE
length(surveys$taxa == 'Rodent')
dim(surveys)
surveys[surveys$taxa == 'Rodent',] #Grabs all rows that are TRUE for 'Rodent'
surveys[surveys$taxa == 'Rodent','taxa'] #added onto above, but only want to see 'taxa' column

# Re-write to subset data frame to those records between 1980 and 1990
rodentssurveys[surveys$taxa == 'Rodent',]
surveys[surveys$year >= 1980 | surveys$year <= 1990 & surveys$taxa == 'Rodent',] #& Combines conditional statements
surveys[surveys$year %in% seq.int(1980,1990) & surveys$taxa =='Rodent',]

# dplyr ----
# CMD + SHIFT + M = %>% 
# provides easy tools for most common data manipulation tasks
# works with data frames
# uses C language to do the work

# select certain data frames
output <- select(surveys, year, taxa, weight) #from surveys data frame, I want year, taxa, weight columns)
head(output)

# filter data to just the taxa you're looking for
filter(surveys, taxa =='Rodent')
# can be explicit about package
dplyr::filter(surveys, taxa =='Rodent')
filter(select(surveys, year, taxa, weight), taxa=='Rodent') # can also combine data
# cleaner way to do this: PIPES!
surveys %>%
      filter(taxa =='Rodent') %>%
      select(year, taxa, weight)
rodent_surveys <- surveys %>%
   filter(taxa =='Rodent') %>%
   select(year, taxa, weight)%>%
   filter(year <= 1990 & year >=1980) #or filter(year %in% seq.int(1980,1990))

# to create new columns with existing data 
surveys %>%
   mutate(weight_kg = weight/1000) %>% #new name for variable (weight_kg) 
   head()

# Split, Apply, Combine

surveys %>% 
   filter(!is.na(weight)) %>%
   filter(taxa == 'Rodent') %>% 
   filter(year %in% 1980:1990) %>% 
   group_by(species_id) %>% 
   summarize(med_weight = median(weight)) %>% 
   print(n = 25)

# Removed all NA's
surveys_complete <- surveys %>% 
   filter(!is.na(weight)) %>%
   filter(species_id != '') %>% 
   filter(!is.na(hindfoot_length)) %>% 
   filter(sex != '') %>% 
   filter(taxa == 'Rodent') 

# Species that appear 50 or more times
common_species <- surveys_complete %>% 
   group_by(species_id) %>% 
   tally() %>% 
   filter (n >= 50) %>% 
   select (species_id)

common_surveys <- surveys_complete %>% 
   filter(species_id %in% common_species$species_id)

write.csv(common_surveys, file = '~Desktop/surveys_complete.csv', row.names = FALSE)

# ggplot2 ----
# "grammar of graphics plots"
# works step by step to add new elements

library(ggplot2)

# start by binding data to ggplot instance
# name x and y axis, color of points, etc.
#aes = aesthetics
ggplot(data=common_surveys,
       aes(x = weight, y = hindfoot_length, 
           color = species_id)) + geom_point()

# LOOK AT REST OF SOFTWARE CARPENTRY SITE



