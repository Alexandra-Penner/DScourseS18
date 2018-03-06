# Problem Set 6
library(mice)
library(VIM)

# read data
results <- read.csv("Results.csv") # NFL stats by player
combine <- read.csv("Combine.csv") # NFL combine results


# Clean
# merge by players name
results <- results[results$Year >= 1999,] # Combine data is only available back to 1999, so we can truncate results
colnames(combine)[2]<- "Player"
NFL <- merge(x = combine, y = results, by = "Player")

# remove redundant columns
# firstname, last name, feet, inches, 
# round, pick, pickround, extra indexing, playerid
NFL <- NFL[,-c(2,3,4,6,7, 20, 22, 23, 27, 28, 29, 31, 32, 35, 59, 60)]
# remove sub stats to facilitate comparison across positions
NFL <- NFL[,-c(25:44)]
NFL <- NFL[,-c(19,23,24)]
# remove player identifiers and pick
NFL <- NFL[,-c(1,2,15,16,19)] 

# missing data is present as zero so impute missing first
df <- NFL[,c(1:15)]
df[df==0] <- NA
NFL[,c(1:15)] <- df

aggr_plot <- aggr(NFL, col=c('navyblue','red'), numbers=TRUE, 
                  sortVars=TRUE, labels=names(data), cex.axis=.7, 
                  gap=3, ylab=c("Histogram of missing data","Pattern"))


# almost all twenty yard, wonderlic, ten yard, and nflgrade are missing, implausible to usefully impute
NFL <- NFL[,-c(6,7,13,14)]



# impute with predictive mean matching
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)


