

setwd("/courses/computing_data_analysis/assignments/week3")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

#SECTION 1

outcome[, 11] <- as.numeric(outcome[, 11])

hist(outcome[, 11], xlab = "30-Day death rate", main = "Heart Attack 30-day Death Rate")


#SECTION 2


# 30day-heart.attack -> 11
# 30day-heart.failure -> 17
# 30day-pneumonia -> 28

outcome[, 11] <- as.numeric(outcome[, 11])
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])

par(mfrow = c(3, 1))
xlabel <- "30-day Death Rate"

ranges <- range(outcome[,c(11,17,23)], na.rm = TRUE )

hist(outcome[, 11], xlab = xlabel, xlim=ranges, prob=TRUE
     , main =  substitute( "Heart Attack (" * bar(x) == k * ")", list(k=mean(outcome[, 11], na.rm=TRUE))) )
lines( density(outcome[, 11], na.rm = TRUE), col="green" )
abline( v= median(outcome[, 11], na.rm=TRUE), col="red", lwd=2 )

hist(outcome[, 17], xlab = xlabel, xlim=ranges, prob=TRUE
     , main =  substitute( "Heart Failure (" * bar(x) == k * ")", list(k=mean(outcome[, 17], na.rm=TRUE))) )
lines( density(outcome[, 17], na.rm = TRUE), col="green" )
abline( v= median(outcome[, 17], na.rm=TRUE), col="red", lwd=2 )



hist(outcome[, 23], xlab = xlabel, xlim=ranges, prob=TRUE, 
     , main = substitute( "Heart Failure (" * bar(x) == k * ")", list(k=mean(outcome[, 23], na.rm=TRUE))) )

lines( density(outcome[, 23], na.rm = TRUE), col="green" )
abline( v= median(outcome[, 23], na.rm=TRUE), col="red", lwd=2 )


# Section 3
count_per_state <- table(outcome$State) 

# Keep only hospitals with more than 20 hospitals
outcome2 <- outcome[ count_per_state[ outcome$State ] > 20,]

table(outcome2$State)

death <- outcome2[,11]
state <- outcome2$State

par(mfrow = c(1, 1))

boxplot (death ~ state, ylab="30-day Death Rate"
         , main="Heart Attack 30-day Death Rate by State", las=2)

# Section 4

hospital <- read.csv("hospital-data.csv", colClasses = "character")


outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")

death <- as.numeric(outcome.hospital[, 11]) ## Heart attack outcome
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)

library(lattice)

xyplot( death ~ npatient | owner
        , xlab="Number of patients seen"
        , ylab="30-day Death Rate"
        , main="Heart Attack 30-day Death Rate by State"
        , panel = function(x, y, ...){
               panel.xyplot(x, y,...)
               panel.lmline(x,y, col="lightblue", lwd=2)
        })


