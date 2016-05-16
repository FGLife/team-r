###############################################################
#
#### Analysis of MYGA sales ####
#
###############################################################

#data wrangling
library(dplyr)
library(ggplot2)
library(tidyr)

## Preprocess files
#read in suitability data file
allapp <- read.csv(file="FGLDistSuitab.csv", header=TRUE, sep=",")
dell <- read.csv(file = "dell.csv", header=TRUE, sep=",")
fgl <- read.csv(file = "MYGA-append.csv", header=TRUE, sep=",")

#clip and reshape key values into a table
appdata <- subset(allapp, grepl("^SUIT*", allapp$XmlKey))
appdata <- select(appdata, TransIdentifier, XmlKey, XmlValue)
appdata <-spread(appdata, XmlKey, XmlValue)

#Income & expense analysis - source
appdata <- rename(appdata, Monthly_Income = SUIT42)
#appdata$Monthly_Income <- as.numeric(as.character(appdata$Monthly_Income)
appdata$Monthly_Income <- as.numeric(as.character(appdata$Monthly_Income))

appdata <- rename(appdata, Monthly_Expense = SUIT43)
#appdata$Monthly_Expense <- as.numeric(as.character(appdata$Monthly_Expense))
appdata$Monthly_Expense <- as.numeric(as.character(appdata$Monthly_Expense))
appdata$Monthly_Discretionary <- appdata$Monthly_Income - appdata$Monthly_Expense

#size of policy and funds - source
appdata <- rename(appdata, Liquid_Src = SUIT88)
appdata$Liquid_Src <- as.numeric(as.character(appdata$Liquid_Src))
appdata <- rename(appdata, Non_Liquid_Src = SUIT98)
appdata$Non_Liquid_Src <- as.numeric(as.character(appdata$Non_Liquid_Src))
appdata$Purchase <- appdata$Non_Liquid_Src + appdata$Liquid_Src
appdata$Liquid_Src_Pct <- round(appdata$Liquid_Src/appdata$Purchase, 2)
appdata$Non_Liquid_Src_Pct <- round(appdata$Non_Liquid_Src/appdata$Purchase, 2)

#networth
appdata <- rename(appdata, Cash = SUIT45)
appdata$Cash <- as.numeric(as.character(appdata$Cash))
appdata <- rename(appdata, Checking = SUIT46)
appdata$Checking <- as.numeric(as.character(appdata$Checking))
appdata <- rename(appdata, CD = SUIT47)
appdata$CD <- as.numeric(as.character(appdata$CD))
appdata <- rename(appdata, IRA = SUIT48)
appdata$IRA <- as.numeric(as.character(appdata$IRA))
appdata <- rename(appdata, Stocks_Bonds = SUIT49)
appdata$Stocks_Bonds <- as.numeric(as.character(appdata$Stocks_Bonds))
appdata <- rename(appdata, MF = SUIT50)
appdata$MF <- as.numeric(as.character(appdata$MF))
appdata <- rename(appdata, Annuities = SUIT51)
appdata$Annuities <- as.numeric(as.character(appdata$Annuities))
appdata <- rename(appdata, Life_Ins = SUIT52)
appdata$Life_Ins <- as.numeric(as.character(appdata$Life_Ins))
appdata <- rename(appdata, Other_Liq = SUIT53)
appdata$Other_Liq <- as.numeric(as.character(appdata$Other_Liq))

appdata$Tot_Liq_Assets <- appdata$Cash + 
  appdata$Checking + appdata$CD + 
  appdata$IRA + appdata$Stocks_Bonds + 
  appdata$MF + appdata$Annuities + 
  appdata$Life_Ins + appdata$Other_Liq

appdata <- rename(appdata, Val_of_Home = SUIT56)
appdata$Val_of_Home <- as.numeric(as.character(appdata$Val_of_Home))
appdata <- rename(appdata, Other_RE = SUIT57)
appdata$Other_RE <- as.numeric(as.character(appdata$Other_RE))
appdata <- rename(appdata, IRA_under = SUIT58)
appdata$IRA_under <- as.numeric(as.character(appdata$IRA_under))
appdata <- rename(appdata, Class_B_MF = SUIT59)
appdata$Class_B_MF <- as.numeric(as.character(appdata$Class_B_MF))
appdata <- rename(appdata, Gold = SUIT60)
appdata$Gold <- as.numeric(as.character(appdata$Gold))
appdata <- rename(appdata, Ann_with_Pen = SUIT61)
appdata$Ann_with_Pen <- as.numeric(as.character(appdata$Ann_with_Pen))
appdata <- rename(appdata, Life_with_Pen = SUIT62)
appdata$Life_with_Pen <- as.numeric(as.character(appdata$Life_with_Pen))
appdata <- rename(appdata, Other_Non_Liq = SUIT63)
appdata$Other_Non_Liq <- as.numeric(as.character(appdata$Other_Non_Liq))
appdata$Other_Non_Liq[is.na(appdata$Other_Non_Liq)] <- 0

appdata$Tot_Non_Liq_Assets <- appdata$Val_of_Home + appdata$Other_RE + 
  appdata$IRA_under + appdata$Class_B_MF + appdata$Gold + 
  appdata$Ann_with_Pen + appdata$Life_with_Pen + appdata$Other_Non_Liq

appdata$Tot_Assets <- appdata$Tot_Liq_Assets + appdata$Tot_Non_Liq_Assets

#First wave of sales
wave2 <- merge(appdata, dell)
wave2 <- merge(wave2,fgl)
wave2 <- subset(wave2, Issued == "Yes")
wave2$StatusDate <-as.Date(as.character(wave2$StatusDate), format = "%m/%d/%y")
wave2$DateCreated <-as.Date(as.character(wave2$StatusDate), format = "%m/%d/%y")
wave2$Amount <- as.numeric(as.character(wave2$Amount))
wave2$AgentID <- as.character(wave2$AgentID)


#wave3 <- select(wave2, TransIdentifier, Liquid_Src, Non_Liquid_Src,Amount, Purchase) 
#wave3$difference <- round(wave3$Amount,0) - round(wave3$Purchase, 0)
#wave4 <- subset(wave3, Amount != Purchase)
#wave4 <- select(wave4, TransIdentifier, Liquid_Src, Non_Liquid_Src,Amount, Purchase)

myga1035pre <- subset(wave2, StatusDate < "2016-01-15")
myga1035pre <- select(myga1035pre, 
                      TransIdentifier, 
                      StatusDate, 
                      Purchase, 
                      Tot_Assets )

myga1035pre <- summarise (myga1035pre,
                          Time = "Pre-1035s",
                          Cases = n(),
                          Avg_Premium = round(mean(myga1035pre$Purchase, na.rm=TRUE),0),
                          Med_Premium = round(median(myga1035pre$Purchase, na.rm=TRUE),0),
                          Avg_Assets = round(mean(myga1035pre$Tot_Assets, na.rm=TRUE),0)
)

myga1035post <- subset(wave2, StatusDate > "2016-01-15")
myga1035post <- select(myga1035post, 
                       TransIdentifier, 
                       StatusDate,
                       Purchase,
                       Tot_Assets
)

myga1035post <- summarise(myga1035post,
                          Time = "Post-1035s",
                          Cases = n(),
                          Avg_Premium = round(mean(myga1035post$Purchase),0),
                          Med_Premium = round(median(myga1035post$Purchase),0),
                          Avg_Assets = round(mean(myga1035post$Tot_Assets, na.rm=TRUE),0)
)

myga1035analysis <- union(myga1035pre, myga1035post)


wave2 <- group_by(wave2, StatusDate)
time_seq <- summarise(wave2, 
                      Sales = sum(Purchase)
)
ggplot(time_seq,aes(StatusDate,Sales)) + geom_area() + ylab("Daily Sales") + xlab('Date of Entry')
#size of policy - source of funds - analysis
annuities <- select(appdata, 
                    TransIdentifier, 
                    Liquid_Src, 
                    Non_Liquid_Src, 
                    Liquid_Src_Pct, 
                    Non_Liquid_Src_Pct, 
                    Purchase)

summarise(annuities,
          Cases = n(),
          Total_Premium = sum(annuities$Purchase,na.rm=TRUE),
          Avg_Premium = round(mean(annuities$Purchase),0),
          Med_Premium = round(median(annuities$Purchase),0),
          Avg_Liq_Src = round(mean(annuities$Liquid_Src_Pct, na.rm=TRUE),2),
          Avg_Non_Liq_Src = round(mean(annuities$Non_Liquid_Src_Pct, na.rm=TRUE),2))

ggplot(data=annuities,
       aes(Purchase)) + geom_histogram(binwidth = 10000) + xlim(-15000, 200000)  

#whats driving size of annuity purchase
e <- ggplot(appdata, aes(x = Purchase , y = Checking))
e + geom_point() + xlim(0, 1000000) + ylim(0, 100000)


#Net worth analysis
networth <- select(appdata, TransIdentifier, Tot_Liq_Assets, Tot_Non_Liq_Assets, Tot_Assets)
networth_summary <- summarise (networth,
                               mean_assets = round(mean(networth$Tot_Assets, na.rm=TRUE),0),
                               mean_liq_assets = round(mean(networth$Tot_Liq_Assets,na.rm=TRUE),0),
                               mean_non_liq_assets = round(mean(networth$Tot_Non_Liq_Assets,na.rm=TRUE),0),
                               median_assets = round(median(networth$Tot_Assets, na.rm=TRUE),0),
                               median_liq_assets = round(median(networth$Tot_Liq_Assets,na.rm=TRUE),0),
                               median_non_liq_assets = round(median(networth$Tot_Non_Liq_Assets,na.rm=TRUE),0)
)
networth_summary <- t(networth_summary)
networth_summary
ggplot(data=networth, 
       aes(Tot_Assets)) + geom_histogram(binwidth = 100000) + xlim(0,3000000)


#Source of networth

#liquid assets
networth_source_liq <- summarise (appdata, 
                                  Value = "Median",
                                  Cash = round(median(appdata$Cash, na.rm=TRUE),0),
                                  Checking = round(median(appdata$Checking, na.rm=TRUE),0),
                                  CDs = round(median(appdata$CD, na.rm=TRUE),0),
                                  IRAs = round(median(appdata$IRA, na.rm=TRUE),0),
                                  Stocks_Bonds = round(median(appdata$Stocks_Bonds, na.rm=TRUE),0),
                                  Mutual_Funds = round(median(appdata$MF, na.rm=TRUE),0),
                                  Annuities = round(median(appdata$Annuities, na.rm=TRUE),0),
                                  Life_Ins = round(median(appdata$Life_Ins, na.rm=TRUE),0),
                                  Other = round(median(appdata$Other_Liq, na.rm=TRUE),0))


networth_source_liq2 <- summarise (appdata, 
                                   Value = "Average",
                                  Cash = round(mean(appdata$Cash, na.rm=TRUE),0),
                                  Checking = round(mean(appdata$Checking, na.rm=TRUE),0),
                                  CDs = round(mean(appdata$CD, na.rm=TRUE),0),
                                  IRAs = round(mean(appdata$IRA, na.rm=TRUE),0),
                                  Stocks_Bonds = round(mean(appdata$Stocks_Bonds, na.rm=TRUE),0),
                                  Mutual_Funds = round(mean(appdata$MF, na.rm=TRUE),0),
                                  Annuities = round(mean(appdata$Annuities, na.rm=TRUE),0),
                                  Life_Ins = round(mean(appdata$Life_Ins, na.rm=TRUE),0),
                                  Other = round(mean(appdata$Other_Liq, na.rm=TRUE),0))

networth_source_liq <- union (networth_source_liq, networth_source_liq2)

networth_source_liq <-t(networth_source_liq)  

#non-liquid assets
networth_source_nonliq <- summarise (appdata, 
                                  Value = "Median",
                                  Val_of_Home = round(median(appdata$Val_of_Home, na.rm=TRUE),0),
                                  Other_RE = round(median(appdata$Other_RE, na.rm=TRUE),0),
                                  IRA_under = round(median(appdata$IRA_under, na.rm=TRUE),0),
                                  Class_B_MF = round(median(appdata$Class_B_MF, na.rm=TRUE),0),
                                  Gold = round(median(appdata$Gold, na.rm=TRUE),0),
                                  Ann_with_Pen = round(median(appdata$Ann_with_Pen, na.rm=TRUE),0),
                                  Life_with_Pen = round(median(appdata$Life_with_Pen, na.rm=TRUE),0),
                                  Other_Non_Liq = round(median(appdata$Other_Non_Liq, na.rm=TRUE),0))


networth_source_nonliq2 <- summarise (appdata, 
                                   Value = "Average",
                                   Val_of_Home = round(mean(appdata$Val_of_Home, na.rm=TRUE),0),
                                   Other_RE = round(mean(appdata$Other_RE, na.rm=TRUE),0),
                                   IRA_under = round(mean(appdata$IRA_under, na.rm=TRUE),0),
                                   Class_B_MF = round(mean(appdata$Class_B_MF, na.rm=TRUE),0),
                                   Gold = round(mean(appdata$Gold, na.rm=TRUE),0),
                                   Ann_with_Pen = round(mean(appdata$Ann_with_Pen, na.rm=TRUE),0),
                                   Life_with_Pen = round(mean(appdata$Life_with_Pen, na.rm=TRUE),0),
                                   Other_Non_Liq = round(mean(appdata$Other_Non_Liq, na.rm=TRUE),0))

networth_source_nonliq <- union (networth_source_nonliq, networth_source_nonliq2)

networth_source_nonliq <-t(networth_source_nonliq)  
                                  

#Income & expense analysis - analysis
avg_monthly_cashflow <- select (appdata, TransIdentifier, Monthly_Income, Monthly_Expense, Monthly_Discretionary)

summarise(avg_monthly_cashflow,
          minimum_inc = round(min(avg_monthly_cashflow$Monthly_Income),0),
          mean_inc = round(mean(avg_monthly_cashflow$Monthly_Income),0),
          median_inc = round(median(avg_monthly_cashflow$Monthly_Income),0),
          maximum_inc = round(max(avg_monthly_cashflow$Monthly_Income),0))
ggplot(data=avg_monthly_cashflow, 
       aes(Monthly_Income)) + geom_histogram() + xlim(0,30000)

summarise(avg_monthly_cashflow,
          minimum_exp = round(min(avg_monthly_cashflow$Monthly_Expense),0),
          mean_exp = round(mean(avg_monthly_cashflow$Monthly_Expense),0),
          median_exp = round(median(avg_monthly_cashflow$Monthly_Expense),0),
          maximum_exp = round(max(avg_monthly_cashflow$Monthly_Expense),0))
ggplot(data=avg_monthly_cashflow, 
       aes(Monthly_Expense)) + geom_histogram() + xlim(0,15000)

summarise(avg_monthly_cashflow,
          minimum_discr = round(min(avg_monthly_cashflow$Monthly_Discretionary),0),
          mean_discr = round(mean(avg_monthly_cashflow$Monthly_Discretionary),0),
          median_discr = round(median(avg_monthly_cashflow$Monthly_Discretionary),0),
          maximum_discr = round(max(avg_monthly_cashflow$Monthly_Discretionary),0))
ggplot(data=avg_monthly_cashflow, 
       aes(Monthly_Discretionary)) + 
  geom_histogram() + xlim(-500,20000) + xlab("Monthy Discretionary Income")

#value of home - analysis
homedata <- select(appdata, TransIdentifier, Val_of_Home)

summarise(homedata,
          minimum = round(min(homedata$Val_of_Home),0),
          mean = round(mean(homedata$Val_of_Home),0),
          median = round(median(homedata$Val_of_Home),0),
          maximum = round(max(homedata$Val_of_Home),0))
ggplot(data=homedata, 
       aes(Val_of_Home)) + geom_histogram(binwidth = 50000) + xlim(-100, 1000000) + xlab("Value of Home")

#need answer key
work_status <- subset(allapp, allapp$XmlKey == "SUIT02")


surr_chg_amount <- subset(allapp, allapp$XmlKey == "SUIT106")

mult_pol_per_HH <- subset(allapp, allapp$XmlKey == "SUIT109" 
                          & allapp$XmlValue == "true")

ct = c(1, 2, 3, 4, 5)
status = c("Retired", "Employed", "Unemployed", "Other", "NA")
work_key = data.frame(ct, status)
owner_status <- count (appdata, ct = SUIT01)
owner_status <- mutate(owner_status, pct =round(n / sum(n), 2))
owner_status_final <- merge(work_key, owner_status)

ct = c(1, 2, 3, 4)
status = c("Retired", "Employed", "Unemployed", "Other")
work_key = data.frame(ct, status)
jt_owner_status <- count (appdata, ct = SUIT03)
jt_owner_status <- merge(work_key,jt_owner_status)
jt_owner_status <- mutate(jt_owner_status, pct =round(n / sum(n), 2))
#jt_owner_status_final <- merge(work_key, jt_owner_status)
print(xtable::xtable(jt_owner_status_final),type="html",html.table.attributes="border=1")

jt_owner_status <- count (appdata, ct = SUIT03)

#agent concentration
agents <- group_by(wave2, AgentID)
agents <- summarise(agents,
                    Cases = n(),
                    Large_cases = length(which(Purchase > 150000)),
                    Avg_case_size = mean(Purchase),
                    Med_case_size = median(Purchase),
                    St_dev = sd(Purchase),
                    Total_sales = sum(Purchase)
                    )
agents_summary <- summarize(agents,
                            Avg_cases = mean(Cases),
                            Avg_case_size = mean(Avg_case_size)
)

p <- ggplot(data = agents, aes(Avg_case_size,Cases)) 
p <- p + geom_point() + xlab("Avg Premium Per Case")
p <- p + ylab("Number of Cases Sold")
p


ag_regression <- lm(agents$St_dev~agents$Avg_case_size)
lm(agents$St_dev~agents$Avg_case_size)
summary(ag_regression)

p <- ggplot(data = agents, aes(Avg_case_size,St_dev)) 
p <- p + geom_point() + xlab("Avg Premium Per Case")
p <- p + ylab("Standard Deviation of Case Size") 
p <- p + ggtitle("Mix of Cases Submitted By Agent")
p <- p + geom_smooth(method='lm')
p

ag_regression <- lm(agents$St_dev~agents$Cases)
lm(agents$St_dev~agents$Cases)
summary(ag_regression)


#agents by decile
agents$Decile_Sales <- ntile(agents$Total_sales,10)
agents$Decile_Cases <- ntile(agents$Cases,10)

agents_decile_sales <- group_by(agents, Decile_Sales)
agents_decile_sales <- summarise(agents_decile_sales,
                                 Avg_case_size = round(mean(Avg_case_size),0),
                                 Med_case_size = round(median(Med_case_size),0),
                                 Total_sales = sum(Total_sales),
                                 Avg_cases = round(mean(Cases),1),
                                 Tot_Cases = sum(Cases),
                                 St_dev = round(mean(St_dev, na.rm = TRUE),0)
                           )

p <- ggplot(data=agents_decile_sales, aes(Decile_Sales, Total_sales)) 
p <- p + geom_bar(stat = "identity") + xlab("Decile") + ylab("Total Sales")
p <- p + ggtitle("Total Sales by Decile")
p

agents_decile_cases <- group_by(agents, Decile_Cases)
agents_decile_cases <- summarise(agents_decile_cases,
                                 Avg_cases = round(mean(Cases),1),
                                 Avg_case_size = round(mean(Avg_case_size),0),
                                 Med_case_size = round(median(Med_case_size),0),
                                 Total_cases = sum(Cases),
                                 Total_sales = sum(Total_sales),
                                 Avg_case_size = round(mean(Avg_case_size),0),
                                 St_dev = round(mean(St_dev, na.rm = TRUE),0)
                                 
)


#customer age histogram

ggplot(data=wave2, 
       aes(Insured.Age)) + 
  geom_histogram(binwidth = 5) + xlim(40,100) + xlab("Age of Clients")

#average age
average_age <- summarise(wave2,
                         Avg_age = round(mean(Insured.Age),1),
                         Med_age = round(median(Insured.Age),1),
                         Max_age = max(Insured.Age)
                         )

#customers by age decile
wave2$Decile_Age <- ntile(wave2$Insured.Age,10)
customers_decile_age <- group_by(wave2, Decile_Age)
customers_decile_age <- summarise(customers_decile_age,
                                 Min_age = min(Insured.Age),
                                 Max_age = max(Insured.Age),
                                 Avg_case = round(mean(Purchase),0),
                                 Total_sales = sum(Purchase),
                                 Tot_Cases = n()
)

p <- ggplot(data=customers_decile_age, aes(Decile_Age, Avg_case)) 
p <- p + geom_bar(stat = "identity") + xlab("Decile") + ylab("Average Premium")
p <- p + ggtitle("Total Sales by Decile")
p

#Sex analysis

customers_sex <- group_by(wave2, Insured.Gender)
customers_sex <- summarise(customers_sex,
                           Avg_age = round(mean(Insured.Age),0),
                           Avg_case = round(mean(Purchase),0),
                           Total_sales = sum(Purchase),
                           Pct_sales = Total_sales / sum(wave2$Purchase),
                           Tot_Cases = n(),
                           Pct_Cases = Tot_Cases / length(wave2$TransIdentifier),
                           Mean_Home = round(mean(Val_of_Home),0),
                           Mean_Assets = round(mean(Tot_Assets, na.rm=TRUE),0)
)

#1035 analysis

cases_1035 <- group_by(wave2, Is.1035)
cases_1035 <- summarise(cases_1035,
          Cases = n(),
          Total_Premium = sum(Purchase,na.rm=TRUE),
          Avg_Premium = round(mean(Purchase),0),
          Med_Premium = round(median(Purchase),0),
          Avg_Liq_Src = round(mean(Liquid_Src_Pct, na.rm=TRUE),2),
          Avg_Non_Liq_Src = round(mean(Non_Liquid_Src_Pct, na.rm=TRUE),2))



p <- ggplot(data=agents_decile_sales, aes(Decile_Sales, Total_sales)) 
p <- p + geom_bar(stat = "identity") + xlab("Decile") + ylab("Total Sales")
p <- p + ggtitle("Total Sales by Decile")
p

ggplot(data=wave2, 
       aes(Insured.Age)) + 
  geom_histogram(binwidth = 5) + xlim(40,100) + xlab("Age of Clients")

model_purchase <- select(wave2,
                         Purchase,
                         Is.1035,
                         Insured.Gender,
                         Tot_Liq_Assets,
                         Val_of_Home,
                         Checking,
                         CD,
                         Insured.Age,
                         Tot_Assets)



model_purchase$Sex <-  ifelse(model_purchase$Insured.Gender == "M", 1, 0)

test <- glm(Purchase~Sex, family=binomial(link='logit'), data = model_purchase)
summary(test)

#ruled out age here
summary(lm(Purchase ~ Insured.Gender + Is.1035 + Checking + Insured.Age, data = model_purchase))

summary(lm(Purchase ~ factor(Sex), data = model_purchase))

summary(lm(Purchase ~ Sex, data = model_purchase))

summary(lm(Purchase ~ Insured.Gender, data = model_purchase))

summary(lm(Purchase ~ Insured.Gender + Is.1035 + Tot_Assets + Tot_Liq_Assets, data = model_purchase))

summary(lm(Purchase ~ Insured.Gender + Is.1035 + Tot_Liq_Assets, data = model_purchase))

#strongest fit
summary(lm(Purchase ~ Insured.Gender + Is.1035 + Checking, data = model_purchase))

#just gender
summary(lm(Purchase ~ Insured.Gender, data = model_purchase))

#just 1035
summary(lm(Purchase ~ Is.1035, data = model_purchase))

#just checking
summary(lm(Purchase ~ Checking, data = model_purchase))


