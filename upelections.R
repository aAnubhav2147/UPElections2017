setwd("/Users/udayshankar/Desktop/UP Elections R files")
getwd()
library("twitteR")
library("e1071")
library("ROAuth")
library("ggplot2")
library("lubridate")
library("miniUI")
library("shiny")
library("plyr")
library("RCurl", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("RJSONIO", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("taskscheduleR", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("syuzhet", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("tm", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("httr", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
install.packages("Hmisc")
consumer_key <- "kEMBMyT56PGPom9ouH2to4bR7"
consumer_secret <- "iSQsvdGwpO3sMSQNmSe7IfVshJnAoF2J1fcH8c7wA3UVyOd4Nx"
access_token <- "1118258383-zti8cbk0tt93lItM98s5u7geLdKRmEspeB4RvIL"
access_secret <- "pH6ZtwXNNR59sSM6Q5PEcsQfYcKqjBxM41ZoHSBw3y8nz"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
up_elections2017 <- searchTwitter("upelections2017",n=2000,lang = "en")
up_elections2017_df <- twListToDF(up_elections2017)
date <- Sys.Date()
date <- as.character(date)
name <- paste(date,".RData")
save(up_elections2017_df,file = name)
myscript <- system.file("/Users/udayshankar/Desktop/UP Elections R files","upelections.R")
taskscheduler_create(taskname = "upelection1",rscript = myscript,schedule = "ONCE",starttime = "14:07",startdate = format(Sys.Date(),"%d/%m/%Y"))
a <- file.path("/Users/udayshankar/Desktop/UPElectionsRfiles/upelections.R")
taskscheduler_create(taskname = "upelection1",rscript = a,schedule = "ONCE",starttime = "14:07",startdate = format(Sys.Date(),"%d/%m/%Y"))
Sys.getenv("PATH")
Sys.setenv( PATH=paste(Sys.getenv("/Users/udayshankar/Desktop/UPElectionsRfiles"),"/usr/texbin",sep=":") )
