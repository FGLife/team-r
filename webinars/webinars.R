
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)
webinars <- read.csv(file = "webinars.csv", header = TRUE, sep = ",")
all_emails <-read.csv(file = "all_emails.csv", header = TRUE, sep = ",")

#clean data
webinars$Last.Name<- stri_trans_totitle(webinars$Last.Name)
webinars$First.Name<- stri_trans_totitle(webinars$First.Name)
webinars <- rename(webinars, Email = Email.Address)
webinars$Time.in.Session <- str_replace(webinars$Time.in.Session, " minutes", "")
webinars$Time.in.Session <- as.numeric(webinars$Time.in.Session)

fglife <- webinars[grep("fglife.com", webinars$Email, perl=TRUE), ]
webinars <- webinars[!grepl("fglife", webinars$Email, perl=TRUE),]

#overview
webinar_summary <- group_by(webinars,Webinar.Name)
webinar_summary <- summarise(webinar_summary,
                             Date = first(Webinar.Date),
                             Registered = n(),
                             Attended.Call = sum(Attended == 'Yes', na.rm=TRUE),
                             Avg.Listen = round(mean(Time.in.Session, na.rm=TRUE ),digits =1))
webinar_summary <- webinar_summary[order(webinar_summary$Date),]
webinar_summary

#combined registration and attendance list
webinars_repeater <- group_by(webinars, Email)
webinars_repeater <- summarise(webinars_repeater,
                          Last.Name = first(Last.Name),
                          First.Name = first(First.Name),
                          IMO = first(Marketing.Organization),
                          Registered = n(),
                          Attended.Call = sum(Attended == 'Yes', na.rm=TRUE),
                          Avg.Engagement = round(sum(Interest.Rating) /Attended.Call,digits =1)
                          )
webinars_repeater <- webinars_repeater[order(-webinars_repeater$Attended, webinars_repeater$Last.Name),]

#passionate audience?
webinar_lovers <- subset(webinars_repeater,Attended.Call > 2)
webinar_lovers <- select(webinar_lovers,
                         Last.Name,
                         First.Name,
                         IMO,
                         Email,
                         Webinars.Attended = Attended.Call,
                         Avg.Engagement)
webinar_lovers <- webinar_lovers[order(webinar_lovers$Last.Name),]

#histogram of agents/marketers by number of webinars attended
repeats <- ggplot(webinars_repeater, aes(Attended.Call))
repeats <- repeats + geom_histogram(binwidth = 1)
repeats <- repeats + xlab("Number of Webinars Attended")
repeats <- repeats + ylab("Attendees")
repeats

#emails not in hubspot
webinars_repeater$Email <- as.character(webinars_repeater$Email)
all_emails$Email <- as.character(all_emails$Email)
new_emails <- anti_join(webinars_repeater, all_emails, by = "Email")
write.csv(new_emails, file = "new_emails.csv")



#list.Webinars <- c("Safe Income Plus - Dare to Compare", "Retirement Pro - Get the Facts")  

for (i in webinar_summary$Webinar.Name) {
  webinar_single <- subset(webinars,Webinar.Name == i & Attended == "Yes")

  time_on_call <-ggplot(webinar_single, aes(Time.in.Session))
  time_on_call <- time_on_call + geom_histogram(binwidth = 5)
  time_on_call <- time_on_call + xlab("Minutes Listened")
  time_on_call <- time_on_call + ylab("Attendees") + ggtitle(i)
  time_on_call
  print(time_on_call)
  
  webinar_single_lovers <- select(webinar_single,
                                  Last.Name,
                                  First.Name,
                                  IMO = Marketing.Organization,
                                  Email,
                                  Time.in.Session
  )
  webinar_single_lovers <- webinar_single_lovers[order(-webinar_single_lovers$Time.in.Session),]
  #print(xtable(webinar_single_lovers[1:10,]))
  }
  

                                    
                