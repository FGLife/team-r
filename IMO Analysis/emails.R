

##############################################
#
#### IMO Marketer Summary ####
#
##############################################

#set firm
Focus_IMO <- "Collabrix Llc"

## Importing Hubspot data into R for Quarterly Report
library(dplyr)
library(stringi)
library(xtable)

## Preprocess files
#read in marketers data file
marketers <- read.csv(file="HS.csv", header=TRUE, sep=",")
marketers.prior <- read.csv(file="HS-old.csv", header=TRUE, sep=",")
# new sf
activity <- read.csv(file="AccountActivities.csv", header=TRUE, sep=",")


##clean prior period files
#pull only fields we need for prior numbers
marketers.prior <- select(marketers.prior,
                          Email,
                          Emails.Delivered,
                          Emails.Opened,
                          Emails.Clicked,
                          Number.of.Pageviews,
                          Number.of.Visits
                          )

#renames fields to prep for merge
marketers.prior <- rename(marketers.prior, Emails.Delivered.Prior = Emails.Delivered)
marketers.prior <- rename(marketers.prior, Emails.Opened.Prior = Emails.Opened)
marketers.prior <- rename(marketers.prior, Emails.Clicked.Prior = Emails.Clicked)
marketers.prior <- rename(marketers.prior, Number.of.Pageviews.Prior = Number.of.Pageviews)
marketers.prior <- rename(marketers.prior, Number.of.Visits.Prior = Number.of.Visits)


#merges prior fields with most recent data
marketers <- merge(marketers,marketers.prior)

##cleans current file

#date conversion from csv in date only format
marketers$Last.email.open.date<-as.Date(marketers$Last.email.open.date, "%m/%d/%y")
marketers$Last.email.click.date<-as.Date(marketers$Last.email.click.date, "%m/%d/%y")
marketers$Time.of.Last.Session<-as.Date(marketers$Time.of.Last.Session, "%m/%d/%y")

#add open rate calculation
marketers$Open.Rate <-marketers$Emails.Opened / marketers$Emails.Delivered
marketers$Open.Rate <- ifelse(is.na(marketers$Open.Rate),
                                           0, marketers$Open.Rate )

#adds true or false based on recency of activity
marketers$Opened.In.Last.Week <- ifelse(marketers$Last.email.open.date > Sys.Date() - 30, "TRUE", "FALSE")
marketers$Visited.In.Last.Week <- ifelse(marketers$Time.of.Last.Session > Sys.Date() - 30, "TRUE", "FALSE")

#clears nulls from key fields

#cleans column names
marketers <- rename(marketers, SF.Top.Level.Account = Top.Level.Account)
marketers <- rename(marketers, Top.Level.Account = Top.Level.Account..Text.)
marketers <- rename(marketers, SF.Account.Name = Name)

#title case conversion

marketers$Top.Level.Account <- stri_trans_totitle(marketers$Top.Level.Account)
marketers$First.Name <- stri_trans_totitle(marketers$First.Name)
marketers$Last.Name <- stri_trans_totitle(marketers$Last.Name)
marketers$Company.Name <- stri_trans_totitle(marketers$Company.Name)
marketers$MGA.Name <- stri_trans_totitle(marketers$MGA.Name)
marketers$SF.Account.Name <- stri_trans_totitle(marketers$SF.Account.Name)


#creates summary table by firm
#distinct(select(marketers, Top.Level.Account))
IMO_summary <- group_by(marketers, Top.Level.Account)
IMO_summary <- summarise(IMO_summary, marketers = n(), 
                         Opt_outs = sum(Opted.out.of.all.email == 'true', na.rm=TRUE),
                         mean_open = mean(Open.Rate),
                         median_open = median(Open.Rate),
                         max_open = max(Open.Rate),
                         opened_mail = sum(Opened.In.Last.Week == 'TRUE', na.rm=TRUE),
                         visited_web = sum(Visited.In.Last.Week == 'TRUE', na.rm=TRUE))

#add open rate/visitation pct calculation
IMO_summary$opened_mail_pct <- round(IMO_summary$opened_mail/IMO_summary$marketers, 2)
IMO_summary$visited_web_pct <- round(IMO_summary$visited_web/IMO_summary$marketers, 2)

IMO_summary$mean_open <- round(IMO_summary$mean_open,2)
IMO_summary$median_open <- round(IMO_summary$median_open, 2)
IMO_summary$max_open <- round(IMO_summary$max_open, 2)

##Activity in Last Two Weeks

IMO_drilldown <- subset(IMO_summary, IMO_summary$Top.Level.Account==Focus_IMO)

IMO_drilldown <- select(IMO_drilldown,
                        Opt_outs,
                        mean_open,
                        opened_mail_pct,
                        visited_web_pct
                        )


IMO_drilldown[,c(3,4)]

## Historical Activity

IMO_drilldown[,c(1,2)]

##Email Opens
IMO_drilldown_detail <- subset(marketers, marketers$Top.Level.Account==Focus_IMO)
IMO_drilldown_detail$First.Name <-strtrim(IMO_drilldown_detail$First.Name,12)
IMO_drilldown_detail$Last.Name <-strtrim(IMO_drilldown_detail$Last.Name,12)
IMO_drilldown_detail.opens <- select(IMO_drilldown_detail,
                                     First.Name, Last.Name, SF.Account.Name, Open.Rate)
IMO_drilldown_detail.opens <- IMO_drilldown_detail.opens[order(-IMO_drilldown_detail.opens$Open.Rate, IMO_drilldown_detail.opens$Last.Name),]
IMO_drilldown_detail.opens$Open.Rate <- round(IMO_drilldown_detail.opens$Open.Rate, digits = 2)

IMO_drilldown_detail.email <- subset(IMO_drilldown_detail, 
                                     IMO_drilldown_detail$Opened.In.Last.Week=="TRUE")
colnames(IMO_drilldown_detail.email)[colnames(IMO_drilldown_detail.email)=="Last.email.open.date"] <- "Last.email"
IMO_drilldown_detail.email <- IMO_drilldown_detail.email[order(IMO_drilldown_detail.email$Last.Name),]

## Website visits
IMO_drilldown_detail.web <- subset(IMO_drilldown_detail, IMO_drilldown_detail$Visited.In.Last.Week=="TRUE")
colnames(IMO_drilldown_detail.web)[colnames(IMO_drilldown_detail.web)=="Last.email.open.date"] <- "Last.email"
colnames(IMO_drilldown_detail.web)[colnames(IMO_drilldown_detail.web)=="Time.of.Last.Session"] <- "Last.session"
IMO_drilldown_detail.web <- IMO_drilldown_detail.web[order(IMO_drilldown_detail.web$Last.Name),]
IMO_drilldown_detail.web$Last.Page.Seen <- substr(IMO_drilldown_detail.web$Last.Page.Seen, 9, 55)
IMO_detail.web <- select(IMO_drilldown_detail.web,
                         First.Name, Last.Name, Average.Pageviews, Last.Page.Seen)

#optouts

IMO_drilldown_detail.optouts <- subset(IMO_drilldown_detail, 
                                       IMO_drilldown_detail$Opted.out.of.all.email=="true")
print(xtable::xtable(IMO_drilldown_detail.optouts[,c(2,3,4)]),type="html",html.table.attributes="border=1")

#sf account summary


#Merging in calls at Sf.Account.Name level

calls <- group_by(activity, SF.Account.Name)
calls <- summarise (calls,
                Calls = n()
                )





IMO_drilldown_detail_sf <-group_by(IMO_drilldown_detail, SF.Account.Name)
IMO_drilldown_detail_sf <- summarise(IMO_drilldown_detail_sf, 
                                     marketers = n(), 
                                     mean_open = round(mean(Open.Rate, na.rm = TRUE),2),
                                     annuity_sales = mean(YTD.Annuity, na.rm=TRUE),
                                     life_sales = mean(YTD.Life, na.rm=TRUE)
)
IMO_drilldown_detail_sf <- IMO_drilldown_detail_sf[order(-IMO_drilldown_detail_sf$annuity_sales),]
#IMO_drilldown_sf[,c(1,2,3,4)]

#

IMO_drilldown_detail.optouts <- subset(IMO_drilldown_detail, 
                                   IMO_drilldown_detail$Opted.out.of.all.email=="true")

IMO_drilldown_detail.recent <- subset(IMO_drilldown_detail, 
                                  IMO_drilldown_detail$Opened.In.Last.Week=="TRUE" | IMO_drilldown_detail$Visited.In.Last.Week=="TRUE")
colnames(IMO_drilldown_detail.recent)[colnames(IMO_drilldown_detail.recent)=="Last.email.open.date"] <- "Last.email"
colnames(IMO_drilldown_detail.recent)[colnames(IMO_drilldown_detail.recent)=="Time.of.Last.Session"] <- "Last.session"

IMO_drilldown_detail.recent$Last.Page.Seen <- substr(IMO_drilldown_detail.recent$Last.Page.Seen, 8, 43)

#test
IMO_drilldown_detail.opens <- IMO_drilldown_detail.opens[order(IMO_drilldown_detail.opens$Open.Rate),]
######
#creates summary table by firm

marketers$Top.Level.Account <- ifelse(marketers$Top.Level.Account == "", "Not available", marketers$Top.Level.Account)

IMO_summary2 <- group_by(marketers, Top.Level.Account)
IMO_summary2 <- summarise(IMO_summary2, marketers = n(), 
                         Opt_outs = sum(Opted.out.of.all.email),
                         mean_open = mean(Open.Rate),
                         median_open = median(Open.Rate),
                         max_open = max(Open.Rate),
                         opened_mail = sum(Opened.In.Last.Week == 'TRUE', na.rm=TRUE),
                         visited_web = sum(Visited.In.Last.Week == 'TRUE', na.rm=TRUE))

IMO_summary2 <- summarise(IMO_summary2, marketers = n(), 
                         mean_open = mean(Open.Rate, na.rm = TRUE), 
                         median_open = median(Open.Rate, na.rm = TRUE),
                         max_open = max(Open.Rate, na.rm = TRUE),
                         opened_mail = sum(Opened.In.Last.Week == 'TRUE', na.rm=TRUE),
                         visited_web = sum(Visited.In.Last.Week == 'TRUE', na.rm=TRUE)
)

IMO_drilldown_detail_sf <-group_by(IMO_drilldown_detail, SF.Account.Name)
IMO_drilldown_detail_sf <- summarise(IMO_drilldown_detail_sf, 
                         marketers = n(), 
                         mean_open = mean(Open.Rate, na.rm = TRUE),
                         annuity_sales = mean(YTD.Annuity, na.rm=TRUE),
                         life_sales = mean(YTD.Life, na.rm=TRUE)
)

#### Merging sf activity history
# new sf
activity <- read.csv(file="AccountActivities.csv", header=TRUE, sep=",")
#date conversion from csv in date only format
activity$Date <-as.Date(activity$Date, "%m/%d/%y")
activity <- subset(activity, Date >= "2015-1-1")
activity <- rename(activity, SF.Account.Name = Top.Level.Account)
activity$SF.Account.Name <- stri_trans_totitle(activity$SF.Account.Name)

activity_summary <- group_by(activity, SF.Account.Name)
activity_summary <- summarise (activity_summary, 
                               Calls = n()
)
IMO_drilldown_detail_sf <- left_join(IMO_drilldown_detail_sf, activity_summary, by = "SF.Account.Name")




#calls in salesforce crosstab
activity$xlt <- as.POSIXlt(activity$Date)
mytable <- table(activity$SF.Account.Name, 1900 + activity$xlt$year) 
ftable(mytable)
mytable2 <- as.data.frame.matrix(mytable)

write.csv(mytable, file = "gary2.csv")

##End SF


# web
sf_acct_web <- read.csv(file="SF_Account_Web.csv", header=TRUE, sep=",")
sf_acct_web$SF.Account.Name <- as.character(sf_acct_web$SF.Account.Name)
sf_acct_web$Twitter <- as.character(sf_acct_web$Twitter)
sf_acct_web$Facebook <- as.character(sf_acct_web$Facebook)
sf_acct_web$LinkedIn <- as.character(sf_acct_web$LinkedIn)
sf_acct_web$Website <- as.character(sf_acct_web$Website)
sf_acct_web <- select (sf_acct_web,
                       SF.Account.Name,
                       Twitter,
                       Facebook,
                       LinkedIn,
                       Website)

IMO_drilldown_detail_web <- left_join(IMO_drilldown_detail_sf,sf_acct_web, by = "SF.Account.Name")


plot( IMO_drilldown_sf$annuity_sales, IMO_drilldown_sf$mean_open)
abline(lm(IMO_drilldown_sf$mean_open~IMO_drilldown_sf$annuity_sales))


textrecent <- select(marketers, 
                     Email, 
                     Emails.Opened, 
                     Emails.Opened.Prior,
                     Emails.Delivered,
                     Emails.Delivered.Prior,
                     Open.Rate.Recent = (Emails.Opened - Emails.Opened.Prior)/(Emails.Delivered - Emails.Delivered.Prior))
