##Checking out drug use data from 2012

##getting the necessary packages
library(ggplot2)
library(scales)
library(dplyr)

##importing the data
load("data")
Drugs <- da34933.0001
rm(da34933.0001)
View(Drugs)


##Which drugs are the most popular
drug.names <- as.vector(names(Drugs))
drug.popularity <- data.frame()
for (drug in drug.names) {
  if (grepl('EVER', drug) || drug=='METHDES' || grepl('EVR', drug)) {
    tab <- table(Drugs[drug])
    drug.popularity <- rbind(drug.popularity, data.frame(substr(drug,1,nchar(drug)-4), (tab[1]/(tab[1]+tab[2]))))
  }
}

drugs.used <- rbind(head(drug.popularity,10), drug.popularity[14,])
names(drugs.used) <- c('Drug','Percent.Have.Tried')

##plotting the percentage of types of drugs tried
drugpop <- ggplot(drugs.used, aes(reorder(Drug,Percent.Have.Tried),Percent.Have.Tried)) + 
                geom_bar(stat='identity', fill='springgreen') +
                scale_y_continuous(label=percent) + 
                ggtitle('Percent of Responders Trying Drugs') +
                xlab('Drug') +
                ylab('Percent Have Tried') +
                coord_flip()


##after seeing that Marijuana, Meth, and Crack are the 3 most popular illicit drugs
##let's see which of those are being used most frequently
mj <- data.frame('Marijuana',Drugs['TOTMJ'])
mt <- data.frame('Meth',Drugs['MTHYRTOT'])
ck <- data.frame('Crack',Drugs['TOTCRACK'])
names(mj) <- c('Drug','Days.Per.Year')
names(mt) <- c('Drug','Days.Per.Year')
names(ck) <- c('Drug','Days.Per.Year')
big3use <- rbind(mj,mt,ck)

##looking at the avg number of days per year that someone uses each
big3plot <- ggplot(big3use, aes(Drug,Days.Per.Year)) +
                geom_bar(stat='summary',fill='springgreen') +
                ggtitle('Average Use of Top Three Illegal Drugs') +
                xlab('Drug') +
                ylab('Average Days Used')
 
##looking at how these responders were distributed
big3density <- ggplot(big3use, aes(Days.Per.Year,color=Drug)) + 
                geom_density() +
                scale_color_brewer(palette='Dark2') +
                ggtitle('Distribution of Use of Top Three Illegal Drugs') +
                xlab('Days Used') +
                ylab('Density')


##How many drugs have most people tried
number.drugs <- Drugs[(grepl('EVER',drug.names)|grepl('EVR',drug.names)|grepl('METHDES',drug.names))][,c(1,2,3,4,5,6,7,8,9,10,14)]
indx <- sapply(number.drugs, is.factor)
number.drugs[indx] <- lapply(number.drugs[indx], function(x) as.integer(x))
number.drugs[indx] <- lapply(number.drugs[indx], function(x) abs(x-2))

number.drugs$total.tried <- apply(number.drugs, 1, sum, na.rm=T)

num.tried.plot <- ggplot(number.drugs, aes(total.tried)) +
                    geom_bar(fill='springgreen') +
                    ggtitle('Number of Drugs Tried') +
                    xlab('Listed Drugs Tried') +
                    ylab('Number of Survey Responders')

nums <- table(number.drugs$total.tried)
pct.none <- nums[1]/(sum(nums[c(-1)])+nums[1])
pct.used <- 1 - pct.none

##How many drugs do most drug users use
using <- c()
for (drug in drug.names) {
  if (grepl('REC', drug)) {
    using <- rbind(using, drug)
  }
}
users.using <- data.frame(Drugs[,using])[,c(1,2,3,5,6,7,8,9,10,20)]
users.using$Current <- 0
for (r in 1:nrow(users.using)) {
  Cnt <- 0
  for (c in 1:ncol(users.using)) {
    if (is.na(users.using[r,c])) {
      Cnt <- Cnt + 0
    }
    else if (as.integer(users.using[r,c])==1||as.integer(users.using[r,c])==2) {
      Cnt <- Cnt+1
    }
  }
  users.using$Current[r] <- Cnt
}

using.plot <- ggplot(users.using, aes(Current)) +
                geom_bar(fill='springgreen') +
                ggtitle('Number of Drugs Used in the Past Year') +
                xlab('Drugs Used') +
                ylab('Number of Survey Responders') +
                scale_x_continuous(breaks=c(0:10))


##printing all of the plots
drugpop
big3plot
big3density
num.tried.plot
using.plot
  
