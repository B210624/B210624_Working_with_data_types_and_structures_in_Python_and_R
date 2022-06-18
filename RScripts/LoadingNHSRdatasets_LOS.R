## Load packages and data
#Let’s load the packages and data needed for this script.
library(NHSRdatasets)
library(tidyverse)
#The tidyverse is a collection of R packages designed for data science introduced
# by Hadley Wickham and his team 
#that "share an underlying design philosophy, grammar, and data structures" of tidy data.

library(here)
#The here package enables easy file referencing in project-oriented workflows. 
# In contrast to using `setwd()` function, 
#which is fragile and dependent on the way you organise your files, here uses 
# the top-level directory of a project to easily 
# build paths to files.

library(knitr)
#knitr is an R package that integrates code into text documents. 
#Files can then be processed into a diverse array of document formats including 
# pdfs, Word documents, etc.

library(scales)
#The scales packages provides the internal scaling infrastructure to ggplot2 and
# its functions allow programmers to customise 
#the transformations, breaks, guides and palettes used in visualisations. 
# ggplot2 is a powerful package to draw graphics. 
#It implements the grammar of graphics (and hence its name). ggplot2 is included 
# in, and loaded with the tidyverse package

library(lubridate)
#lubridate provides tools that make it easier to manipulate dates in R. 
# lubridate is part of Hadley's tidyverse ecosystem but is not loaded by the 
# tidyverse package, which includes only what he thought were the core components. 
# If you are going to be doing a lot of date manipulations, you need to load it separately. 

library(caret)
#The caret package (short for Classification And REgression Training) is 
# a set of functions that attempt to 
#streamline the process for creating predictive models. 

# NHSRdatasets
#Here is the code to load the five NHSRdatasets from the NHSRdatasets R package.

#Load the LOS_model data.
data(LOS_model)

## Let’s have a look at the los  data
data(LOS_model)
los<-LOS_model
class(los)

#The tbl_df class is a subclass of data.frame. 

los
  

## Let's view at the los data

glimpse(los) 

#Here is the output of the `glimpse()' function. It starts off with the number of rows and columns and each column in separate rows.

#The `head()` function let's you get a look at top n rows of a data frame. By default it shows the first 6 rows in a data frame.
head(los)

#We can specify the number of rows we want to see in a data frame with the argument “n”. 
head(los, n=3)
#In the example above, we used n=3 to look at the first three rows of the ae_attendances data frame.

#The function `tail()` is the counterpart to `head()`. 
#tail() let's you to take a look at the bottom n rows of a data frame. 
#We can adjust the number of rows with the argument “n” as with the `head()` function.
tail(los, n=4)
#In the example above, we used n=4 to look at the last four rows of the ae_attendances data frame.


## Missing data

## Calculate how many NAs there are in each variable
los %>% 
  map(is.na) %>%
map(sum)
#The data is complete. We do not need to worry about handling missing data. 

#Age vs los
glm(los$LOS ~ los$Age, data=los, family="gaussian")  
df<- data.frame(x=los$Age, y = los$LOS)
linear_model_los_age_LOS<- lm(y ~ x, data=df)
summary(linear_model_los_age_LOS)
plot(linear_model_los_age_LOS)

#Age vs death
df<- data.frame(x=los$Age, y = los$Death)
linear_model_los_age_death<- lm(y ~ x, data=df)
summary(linear_model_los_age_death)
plot(linear_model_los_age_death)
#los vs death
df<- data.frame(x=los$LOS, y = los$Death)
linear_model_los_LOS_death<- lm(y ~ x, data=df)
summary(linear_model_los_LOS_death)
plot(linear_model_los_LOS_death)

glimpse(los)
typeof(los$Organisation)
typeof(los$Age)
typeof(los$LOS)
typeof(los$Death)
class(los$Organisation)
class(los$Age)
class(los$LOS)
class(los$Death)


### Let's visualise monthly four hour waiting time target performance 
glimpse(los)
write_csv(los, here("RawData", "los_perfom.csv"))


## Separating provisional los data into training and testing sets

#the training data:
prop<-(1-(15/nrow(los)))
#The proportion of the raw that needs to be assigned to the training data to 
#ensure there is only 10 to 15 records in the test data is: 
print(prop)

#the raw data into the same test and training data.
set.seed(333)
#Partitioning the raw data into the test and training data.
trainIndex_los<- createDataPartition(los$ID, p = prop, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex_los)
# All records that are in the trainIndex are assigned to the training data.
losTrain <- los[ trainIndex_los,]
nrow(losTrain)

### Our next task, it to save los training data to your working data folder 'Data'
write_csv(losTrain, here("Data", "los_perfom_train.csv"))


### Let's extract the  test data
#All records that are not in the trainIndex (`-trainIndex`) are assigned to the test data.
losTest  <- los[-trainIndex_los,]
nrow(losTest)
#There are 11 records in your test data. 

losTestMarker  <- losTest[1,]

### Our next task, it to save our  marker test data 
#to our working data folder 'Data'
write_csv(losTestMarker, here("Data", "los_perfom_test_marker.csv"))

### We then need to set aside the remaining records for you to test 
#(or collect with your) your data-capture tool.
losTest  <- losTest[2:nrow(losTest),]

### Our final task, is to save our ae_attendances_ENG_4hr_perfom test data to our working data folder 'Data'
write_csv(losTest, here("Data", "ae_attendances_test.csv"))

# You are now ready to start exploring your data. Happy coding!

