---
Author: "PratimaKshetry"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
setwd("E:/dataset/ExploratoryData-Nyt")

data<-read.csv("E:/dataset/ExploratoryData-Nyt/nyt1.csv",stringsAsFactors=FALSE, strip.white=TRUE,na.strings=c("NA",""))

breaks <- c(0,18,25,35,45,55,65,106) ##Create breaking point vector  

labels <- c('<18','18-24','25-34','35-44', '45-54','55-64','65+')  ##Create labels

data$Age_Group<-cut(data$Age,
                     breaks=breaks, 
                     labels=labels,
                     right=F,
                     ordered_result = T)

##Now Plot the impressions and click through rate
data$ctr<-data$Clicks/data$Impressions
data$ctr[is.na(data$ctr)] <- 0   # replace the NA's with '0'
mean_ctr <- ddply(data, "Age_Group", 
                  summarise, 
                  ctr.mean=mean(ctr))

ggplot(data=mean_ctr,
       aes(x=Age_Group, 
           y=ctr.mean, 
           fill=Age_Group)) +
  geom_bar(stat="identity") +
  labs(title="Average CTR per Age Grou - Day 5",
       x="Age Groups",
       y= "Mean of CTR")

#Define a new variable to segment or categorize users based on their click behavior 

####1.People who clicked and those who did not clicked
data$have_clicked <- ifelse(data$Clicks>=1, 1, 0)
prop.table(table(as.factor(data$have_clicked)))

###Relative frequency of Clicks by Age Group
Clicked <- prop.table(table(data$Age_Group, data$have_clicked==1))  
Clicked <- data.frame(Clicked)
Clicked <- subset(Clicked, Var2==TRUE) 
Clicked$Var2 <- NULL
Clicked<- rename(Clicked,c("Var1" = "AgeGroup"))
Clicked
s <- sum(Clicked$Freq)
head(s)
Clicked$RelFrq <- Clicked$Freq/s    #calculate relative frequency
labl <- data.table(Clicked)[, per := sprintf("%.1f%%",
                             RelFrq*100), by = AgeGroup]

head(labl)  ###Tested

ggplot(Clicked, aes(x=AgeGroup, y=RelFrq, fill=AgeGroup)) +
  geom_bar(stat="identity") +
  labs(title="At least 1 Clicks by Age Group - Day 1",
       x= "Age Groups", 
       y= "Relative Frequency of Clicks")+
  geom_text(data = labl,
            aes(x = AgeGroup, y = RelFrq, 
                label = per),vjust=1.0)

##People who logged in and those who did not
data$have_Logged <- ifelse(data$Signed_In>=1, 1, 0)
prop.table(table(as.factor(data$have_Logged)))

###Freq of LoggedIn by Age Grup
data$have_Logged <- ifelse(data$Signed_In>=1, 1, 0)
prop.table(table(data$have_Logged))
log<-prop.table(table(data$Age_Group,data$have_Logged))
log<-data.frame(log)
log1<-subset(log,log$Var2==1)
log1
log<-rename(log,c("Var1"="AgeGroup"))

sumlog<-sum(log1$Freq)
relLog<-log1$Freq/sumlog
relLog
lablog<-data.table(log1)[,per12 :=sprintf("%.1f%%",
                                          relLog*100, 
                                          by=AgeGroup)]
ggplot(log1,
       aes(x=AgeGroup,
           y=relLog,
           fill=AgeGroup))+
  geom_bar(stat="identity")+
  labs(title="Relative Frequency of LoggedIn Behavior per Age Group-Day 1",
       x="Age Group",
       y= "Relative Frequency")+
  geom_text(data=lablog,
            aes(
              label=per12),vjust=1.0)

####Frequency of Logged in By Gender
log<-prop.table(table(data$Gender,data$have_Logged))
log<-data.frame(log)
log
log1<-subset(log,log$Var2==1)
log1
log1<-rename(log1,c("Var1"="Gender"))


# we want to attach value labels 1=Male, 0=Female
log1$Gender <- factor(log1$Gender,
                      levels = c(0,1),
                      labels = c("Female", "Male"))

sumlog<-sum(log1$Freq)
relLog<-log1$Freq/sumlog
relLog

lablog<-data.table(log1)[,per12 :=sprintf("%.1f%%",
                                          relLog*100, 
                                          by=Gender)]

lablog

ggplot(log1,
       aes(x=Gender,
           y=relLog,
           fill=Gender))+
  geom_bar(stat="identity")+
  labs(title="Relative Frequency of LoggedIn Behavior per Gender-Day 5",
       x="Gender",
       y= "Relative Frequency")+
  geom_text(data=lablog,
            aes(
              label=per12),vjust=1.0)

#Frequency table of Clicks by Age group
attach(data)
mytable<-table(Age_Group,Clicks)
mytable
newtable<-data.matrix(mytable)
newtable
means1<-colMeans(newtable)
newtab<-table(Impressions, Age_Group)
par(mfrow=c(1,1))
barplot(newtab,legend=rownames(newtab),
        col=c("darkblue","red","orange","green","black","white","cyan"),
        xlab="Age Group",
        ylab="Count of People",
        main="Impressions Per Age Group - Day 5",
        ylim=c(0,30000),
        beside=TRUE      
)
###########################Metrics#################################

###Calculate average Impressions of page across Age Group
data$mean_Imp<-data$Impressions
mean_Impression_AgeGroup <- ddply(data, "Age_Group", 
                  summarise, 
                  mean_Imp.mean=mean(mean_Imp))
mean_Impression_AgeGroup

#####Average Click Through Rate across Age Groups
data$ctr<-data$Clicks/data$Impressions
data$ctr[is.na(data$ctr)] <- 0   # replace the NA's with '0'
mean_ctr <- ddply(data, "Age_Group", 
                  summarise, 
                  ctr.mean=mean(ctr))
mean_ctr

#######Average Impressions across Gender
data$AvgeImp<-data$Impressions
mean_Imp_Gender <- ddply(data, "Gender", 
                        summarise, 
                        ImpressionByGender.mean=mean(AvgeImp))
mean_Imp_Gender

corrplot(cor(data), method="number", tl.cex=0.3)

#####Now extending analysis across days#######

###Read csv file for Displaying histograms.
data1<-read.csv("E:/Rdataset/HW1-ExploratoryData-Nyt/nyt1.csv",stringsAsFactors=FALSE, strip.white=TRUE,na.strings=c("NA",""))
data2<-read.csv("E:/Rdataset/HW1-ExploratoryData-Nyt/nyt2.csv",stringsAsFactors=FALSE, strip.white=TRUE,na.strings=c("NA",""))
data3<-read.csv("E:/Rdataset/HW1-ExploratoryData-Nyt/nyt3.csv",stringsAsFactors=FALSE, strip.white=TRUE,na.strings=c("NA",""))
data4<-read.csv("E:/Rdataset/HW1-ExploratoryData-Nyt/nyt4.csv",stringsAsFactors=FALSE, strip.white=TRUE,na.strings=c("NA",""))
data5<-read.csv("E:/Rdataset/HW1-ExploratoryData-Nyt/nyt5.csv",stringsAsFactors=FALSE, strip.white=TRUE,na.strings=c("NA",""))
data6<-read.csv("E:/Rdataset/HW1-ExploratoryData-Nyt/nyt6.csv",stringsAsFactors=FALSE, strip.white=TRUE,na.strings=c("NA",""))
data7<-read.csv("E:/Rdataset/HW1-ExploratoryData-Nyt/nyt7.csv",stringsAsFactors=FALSE, strip.white=TRUE,na.strings=c("NA",""))

####Histogram of Impressions

day1imp<-data1$Impressions
day2imp<-data2$Impressions
day3imp<-data3$Impressions
day4imp<-data4$Impressions
day5imp<-data5$Impressions
day6imp<-data6$Impressions
day7imp<-data7$Impressions

par(mfrow=c(2,4))

hist(day1imp,
     prob=TRUE,
     xlab="Day 1 Impressions")
hist(day2imp,
     prob=TRUE,
     xlab="Day 2 Impressions")
hist(day3imp,
     prob=TRUE,
     xlab="Day 3 Impressions")
hist(day4imp,
     prob=TRUE,
     xlab="Day 4 Impressions")
hist(day5imp,
     prob=TRUE,
     xlab="Day 5 Impressions")
hist(day6imp,
     prob=TRUE,
     xlab="Day 6 Impressions")
hist(day7imp,
     prob=TRUE,
     xlab="Day 7 Impressions")


#Interaction plot of CTR in behavior of age groups across gender
par(mfrow=c(1,1))
means_ <- with(data, aggregate(x=list(Y=ctr),
                               by=list(A=Age_Group, B=Gender),mean))

with(means_, interaction.plot(x.factor=A, trace.factor=B, response=Y, type='b',
                              main = "CTR by gender and age group, day 1",
                              xlab = "Age groups", ylab= "Click Through Rate"))


###########d. Describe and interpret any patterns you find.
####Ans: Following are the few observations made on the given dataset:
  ###a. In the above data, we see that people from the age group less than 18 years are most likely (and most frequently) to click an advertisement than any other age group.
  ###b. It appears that females are most likely to click an advertisement than a male, however males are more likely view the page with a LoggedIn status. 
  ###c. People from the age group of 35-44 show more Signed In behavior. We can also note that the data for the LoggedIn Behavior per age group is normally distributed. We see no anomalies in the given data.
  ###d. From the interaction plots, the difference between CTR is higher between males and females for the age group >18 and 65 + while the CTR behavior is almost the same for male and female across all age groups. Also, from the interaction plots, we see that the advert impression across gender varies as per age groups. However, people from the age group of less than 18 years have more Page views followed by people from the age group of 35-44.
        ## the median impression (page views) of the New York Times web page is 4-5. 
 
```

You can also embed plots, for example:

```{r, echo=FALSE}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
