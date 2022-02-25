#import libraries
#to do filtering and processing of our data
#install.packages("dplyr")
#to work with strings in a convenient and easier fashion
#install.packages("stringr")
#creates the structure for our tidy text data 
#install.packages("tidytext")
#to apply tokenization in our data
#install.packages("tidyr")
#to download the sentiment data
#install.packages("textdata")
#to plot the data into meaningful graphs
#install.packages("ggplot2")

library(tidyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
source("./r_docs/labeling_data.R")

#loading the dataset
dataset = read.csv("./data/artists_songs.csv")

#labeling the data
labeled_dataset <- label_dataset(dataset)

#training model using random forest classification



