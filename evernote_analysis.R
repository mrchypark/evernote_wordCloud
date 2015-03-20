# Load packages
install.packages(c("ggplot2","lubridate","stringr","scales","RCurl","rjson","dplyr","gridExtra","lazyeval","KoNLP","RColorBrewer","wordcloud"))
library(ggplot2)
library(lubridate)
library(stringr)
library(scales)
library(RCurl)
library(rjson)
library(dplyr)
library(gridExtra)
library(lazyeval)
require(KoNLP)
require(RColorBrewer)
require(wordcloud)

# go to 'https://developers.facebook.com/tools/explorer' to get your access token
access_token<-"CAACEdEose0cBAE8qADJxeyhjov807KO3YnkimiVKx6ZBlzW9ZCKEVgqVD6QGDYlMeEOXSLMimSrPnL558Twn7SKxWEpyog3DegNXWmjfz0JaA84XxOKnnPG7lV5kC2GP7UqLUmBBhZAdHOZB4X77fvM7Mf8lLSMNZATZAMeboPII3i9rQQyD0dTZBuxb6jlY0CsTmVXnjWZAkt3fGMjErqthSCdWxg4qeIUZD"
    
### MY FACEBOOK QUERY 
#facebook <- function(query,token){
#    myresult <- list()
#    i <- 0 
#    next.path<-sprintf( "https://graph.facebook.com/v2.2/%s&access_token=%s",query, access_token)
#    
#    # download all my posts
#    while(length(next.path)!=0) {
#        i<-i+1
#        myresult[[i]]<-fromJSON(getURL(next.path, ssl.verifypeer = FALSE, useragent = "R" ),unexpected.escape = "keep")
##        next.path<-myresult[[i]]$paging$'next'
#    }   
#    return (myresult)  
#}


#myposts<-facebook("me?fields=posts.include_hidden(true).limit(5){message,created_time,from,story},statuses.limit(0){message,updated_time,from}",access_token)

#######################################################################################  

# Set the working directory
getwd()
setwd("C:/Users/parkets/Documents/data/evernote/files/")
 
#########   FUNCTIONS   ############
 
# Receives a file
# Returns an array with all the dates extracted
get_dates_counts <- function(file) {
  
#######################################################################################    
    
    
  # Create an empty array for storing the dates
  times <- vector()
  
  # Find all the lines that have 'created' in them which therefore includes dates
  
  # Notes go from <note> to </note>
  note_begins <- grep('.*<note>.*',file)
  note_ends <- grep('.*</note>.*',file)

  # All the lines for when notes were created
  time_lines <- grep('.*<created>([0-9]+T[0-9]+Z)?</created>.*',file)
  
  # Select just those lines
  select_time_lines <- file[time_lines]
  
  # Extract the created date for each of them
  times <- sapply(select_time_lines, function(l) gsub('.*<created>([0-9]+T[0-9]+Z)?</created>.*','\\1', l), USE.NAMES=FALSE)
  
  
  #######################################################################################  

    # Create an empty array for storing the title
    title <- vector()

    # Find all the lines that have 'created' in them which therefore includes dates

    # All the lines for when notes were created
    title_lines <- grep('.*<title>.*</title>.*',file)

    # Select just those lines
    select_title_line <- file[title_lines]

    # Extract the created date for each of them
    title <- sapply(select_title_line, function(k) gsub('<(.*?)>','', k), USE.NAMES=FALSE)


####################################################################################### 
  
  ## Get the word count for each note
  
  # Create an empty vector for storing the word counts for each note
  counts <- vector()
  
  # Iterate over the lines of each note
  for (z in 1:length(lines)) {
    
    # Set beginning and end of lines for the given note
    note_begin <- note_begins[z]
    note_end <- note_ends[z]
    
    note_lines <- file[note_begin:note_end]
    
    # Have to get rid of any data files
    # Find the beginning of data sections
    data_start <- grep('.*<data .*',note_lines)
    data_end <- grep('.*</data>.*',note_lines)
    
    text_note_lines <- vector()
    
    # Delete lines between them
    if (length(data_start)>=1) {
      for (y in 1:(length(data_start)+1)) {
        if (y==1) {
          begin <- 1
          end <- data_start[y]-1
          text_note_lines <- c(text_note_lines,note_lines[begin:end])
        }
        else if (y==(length(data_start)+1)) {
          begin <- data_end[y-1]+1
          end <- length(note_lines)
          text_note_lines <- c(text_note_lines,note_lines[begin:end])
        }
        else {
          begin <- data_end[y-1]+1
          end <- data_start[y]-1
          if (begin < end) {
            text_note_lines <- c(text_note_lines,note_lines[begin:end])   
          }
          else if (begin == end) {
            text_note_lines <- c(text_note_lines,note_lines[begin])   
          }
        }
      }
    } else text_note_lines <- note_lines 
    
    # Have to get rid of html chars                                
    text_note_lines <- sapply(text_note_lines, function(t) gsub("<(.*?)>",'', t), USE.NAMES=FALSE)
    
    # Count the number of words per line
    nums <- sapply(text_note_lines, function(t) length(str_match_all(t,"\\S+")[[1]]), USE.NAMES=FALSE)
    # Sum over all the lines
    counts <- append(counts,sum(nums))
  }

  #######################################################################################  
  

  ## Get the word count for each note

  # Create an empty vector for storing the word counts for each note
  contents <- vector()

  # Iterate over the lines of each note
  for (z in 1:length(lines)) {
    
    # Set beginning and end of lines for the given note
    note_begin <- note_begins[z]
    note_end <- note_ends[z]
    
    contents_lines <- file[note_begin:note_end]
    
    # Have to get rid of any data files
    # Find the beginning of data sections
    data_start <- grep('.*<data .*',contents_lines)
    data_end <- grep('.*</data>.*',contents_lines)
    
    text_contents_lines <- vector()
    # Delete lines between them
    if (length(data_start)>=1) {
        for (y in 1:(length(data_start)+1)) {
            if (y==1) {
                begin <- 1
                end <- data_start[y]-1
                text_contents_lines <- c(text_contents_lines,contents_lines[begin:end])
            }
            else if (y==(length(data_start)+1)) {
                begin <- data_end[y-1]+1
                end <- length(contents_lines)
                text_contents_lines <- c(text_contents_lines,contents_lines[begin:end])
            }
            else {
                begin <- data_end[y-1]+1
                end <- data_start[y]-1
                if (begin < end) {
                    text_contents_lines <- c(text_contents_lines,contents_lines[begin:end])   
                }
                else if (begin == end) {
                    text_contents_lines <- c(text_contents_lines,contents_lines[begin])   
                }
            }
        }
    } else text_contents_lines <- contents_lines 
    
    # Extract contents lines
    text_contents_lines <- sapply(text_contents_lines, function(l) gsub('.*<content>.*</content>.*','\\1', l), USE.NAMES=FALSE)
    
    # Have to change unkown letters to space
    text_contents_lines <- sapply(text_contents_lines, function(t) gsub('&nbsp;',' ',t), USE.NAMES=FALSE)
    text_contents_lines <- sapply(text_contents_lines, function(t) gsub('&apos;',' ',t), USE.NAMES=FALSE)
    text_contents_lines <- sapply(text_contents_lines, function(t) gsub('&quot;',' ',t), USE.NAMES=FALSE)
    text_contents_lines <- sapply(text_contents_lines, function(t) gsub('&gt;',' ',t), USE.NAMES=FALSE)
    
    
    # Extract the created date for each of them
    text_contents_lines <- sapply(text_contents_lines, function(l) gsub('.*<.*?>([0-9]+T[0-9]+Z)?</.*?>.*','', l), USE.NAMES=FALSE)
    
    # Have to get rid of html chars                                
    text_contents_lines <- sapply(text_contents_lines, function(t) gsub("<(.*?)>",'', t), USE.NAMES=FALSE)
    
    # Make one line
    text_contents_lines<-paste(text_contents_lines,collapse=" ")
    
    # Change variable name to contents
    contents <- text_contents_lines
}

#######################################################################################  


  # Create a data frame from times and counts
  df_new <- data.frame(times,counts,title,contents)
  
  # Return the array of dates
  return (df_new)
}
 
#########   SCRIPT   ############
 
# Get all the files from the archive
files <- list.files()
 
# Get the total number of files
num <- length(files)
 
# Create an empty data frame to store all the dates and word counts
df <- data.frame(time=vector(),counts=vector(),title=vector(),contents=vector())
 
# Iterate over every file to compile a single array with all the dates
for (i in 1:num) {
  file <- scan(files[i],what="", sep="\n",encoding="UTF-8")
  df <- rbind(df, get_dates_counts(file))
}
 
# Rename the data frame columns
names(df) <- c("time", "counts","title","contents")

#######################################################################################


# Create a formatted time stamp
df$time <- as.POSIXct(sapply(df$time, function(t) as.POSIXct(t,format="%Y%m%dT%H%M%SZ", tz="GMT"), USE.NAMES=FALSE), origin="1970-01-01")
 
# Create an additional column to store the cumulative number of notes created over time
df$note_count <- sapply(df$time, function(t) sum(df$time <= t),USE.NAMES=FALSE)
 
# Create an additional column to store the cumulative sum of words written over time
df$word_count <- sapply(df$time, function(t) sum(df$count[which(df$time <= t)]) )
 
# To customer scale the X-axis time stamp labels needs to be in Date vs Posix format
df_2 <- df
df_2$time <- as.Date(df_2$time)
 
# Reset the working director for saving images
setwd("C:/Users/parkets/Documents/data/evernote/")
 
# Create a graphic of the number of notes I have created by week
pdf(file="New_Notes_By_Week.pdf",width=11,height=8.5)
ggplot(df, aes(x=time)) + geom_histogram(binwidth = 60*60*24*7,fill="blue") + 
  xlab("Date") + ylab("Number Of New Notes") + ggtitle("ChanYub' New Evernotes By Week")
dev.off()
 
# To calculate the linear regression have to zero the x-axis time stamp
df_3 <- df_2
df_3$time <- as.double(df_3$time)
df_3$time <- df_3$time - min(df_3$time)
 
# Create a linear model for number of notes by time
fit <- lm(note_count ~ 0 + time, data=df_3)
summary(fit)
z <- coef(fit)
 
# Graph the total number of notes created over time
pdf(file="Total_Notes_Over_Time.pdf",width=22,height=17)
ggplot(df_2, aes(time, note_count)) + geom_point() + 
  ylab("Total Number of Notes") + xlab("Date") + ggtitle("Total Number Of Notes Created Over Time") + 
  scale_x_date(labels = date_format("%b-%Y"), breaks = date_breaks("2 months")) + 
  geom_abline(intercept = (-z*as.double(min(df_2$time))), slope=z, colour="red") +
  geom_vline(xintercept = as.numeric(as.Date("2013-10-21")), colour="blue", linetype = "longdash")
dev.off()
 
# Create a linear regression for words vs. time
fit <- lm(word_count ~ 0 + time, data=df_3)
summary(fit)
z <- coef(fit)
 
# Visualize the total number of words vs. time
pdf(file="Total_Words_Over_Time.pdf",width=22,height=17)
ggplot(df_2, aes(time, word_count)) + geom_point() + 
  ylab("Total Number of Words") + xlab("Date") + ggtitle("Total Number Of Words Written Over Time") + 
  scale_x_date(labels = date_format("%b-%Y"), breaks = date_breaks("2 months")) + 
  geom_abline(intercept = (-z*as.double(min(df_2$time))), slope=z, colour="red") +
  geom_vline(xintercept = as.numeric(as.Date("2013-10-21")), colour="blue", linetype = "longdash")
dev.off()
 
# Create a linear regression for word count vs. note count
fit <- lm(word_count ~ 0 + note_count, data=df_2)
summary(fit)
z <- coef(fit)
 
# How many notebooks did I have when I became an enterprise client
# The 18th is the closest date I have for having created a notebook
num_notes <- df_2$note_count[which(df_2$time == as.Date("2013-10-18"))]
 
# Visualize the number of words vs. number of notes by time
pdf(file="Total_Words_Over_Notebooks.pdf",width=22,height=17)
ggplot(df_2, aes(note_count, word_count)) + geom_point() + 
  ylab("Total Number of Words") + xlab("Notes") + ggtitle("Total Number Of Words Written vs. Number Of Notes") + 
  geom_abline(slope=z, colour="red") +
  geom_vline(xintercept = num_notes, colour="blue", linetype = "longdash")
dev.off()

##############################################################################################################

# Seperate df by year
df12<-df_2[format(df_2$time,'%Y')==2012,]
df13<-df_2[format(df_2$time,'%Y')==2013,]
df14<-df_2[format(df_2$time,'%Y')==2014,]

# make one line
df12_sum<-paste(df12[,4],collapse=" ")
df13_sum<-paste(df13[,4],collapse=" ")
df14_sum<-paste(df14[,4],collapse=" ")

# Extract Noun
nouns12<-sapply(df12_sum,extractNoun,USE.NAMES=F)
nouns13<-sapply(df13_sum,extractNoun,USE.NAMES=F)
nouns14<-sapply(df14_sum,extractNoun,USE.NAMES=F)

# Count word file
df12_wordcount<-table(unlist(nouns12))
df13_wordcount<-table(unlist(nouns13))
df14_wordcount<-table(unlist(nouns14))

# Order words
df12_wordcount<-(df12_wordcount[order(df12_wordcount,decreasing = T)])
df13_wordcount<-(df13_wordcount[order(df13_wordcount,decreasing = T)])
df14_wordcount<-(df14_wordcount[order(df14_wordcount,decreasing = T)])

# Make Word Cloud
pal<-brewer.pal(12,"Paired")
wordcloud(names(df12_wordcount),freq=df12_wordcount,scale=c(5,0.5),min.freq=15,random.order=F,rot.per=.1,colors=pal)
wordcloud(names(df13_wordcount),freq=df13_wordcount,scale=c(5,0.5),min.freq=15,random.order=F,rot.per=.1,colors=pal)
wordcloud(names(df14_wordcount),freq=df14_wordcount,scale=c(5,0.5),min.freq=15,random.order=F,rot.per=.1,colors=pal)

