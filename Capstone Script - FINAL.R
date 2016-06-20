### CAPSTONE PROJECT ###

### Answer the question: Is the configuration of Executive Branch
### and Legislative Branch (which party in power in White House, Senate,
### and House of Representatives) helpful in predicting changes in 
### staffing industry utilization drivers of Median Family Income, 
### Employment Rate, and Labor Force Participation Rate?


## Deliverables ##
# 1. Code for your project, well-documented on github.

# 2. A final paper explaining the problem, your approach and your findings in complete technical detail. Include ideas for further research, as well as up to 3 concrete recommendations for your client on how to use your findings.

# 3. A slide deck or a blog post which presents your analysis to your clients (e.g. non-technical and business teams) in an easy to understand, but compelling way. As a data scientist in a company, you'll be frequently called upon to produce these kinds of materials.

# 4. Extra credit: Actually present or send your report/slide deck to your designated client and let us know what kind of response you received from them.

####################################################

# Read in and format Family Income Adjusted for Inflation 1950 to 2014
# http://www2.census.gov/programs-surveys/cps/tables/time-series/historical-income-families/f08ar.xls
# Downloaded file as csv to local directory
MedFamInc <- read.csv("Median_Family_Income.csv", header = FALSE, stringsAsFactors = FALSE)
MedFamInc <- MedFamInc[c(7, 9:72), c(1, 4)]
names(MedFamInc) <- c("Year", "Med14") 
MedFamInc$Year <- substr(x = MedFamInc$Year, start = 1, 4)
MedFamInc$Year <- as.numeric(as.character(sub(",", "", MedFamInc$Year)))
MedFamInc$Med14 <- as.numeric(as.character(sub(",", "", MedFamInc$Med14)))
MedFamInc <- MedFamInc[order(MedFamInc$Year), ]
MedFamInc$Year <- as.integer(MedFamInc$Year)

# Use MedFamInc$Med14 as DV in masterData (adjust everything else to 2014 dollars)

#################################################

# Read in and format BLS U-3 Unemployment Rate 1950 to 2014
# U-3 is... 
# Total unemployed, as a percent of all civilian labor force 
# Data Series Number: LNS14000000
# Downloaded file as csv to local directory

df <- read.csv("U3_1950_2014.csv", header = FALSE, skip = 11, stringsAsFactors = FALSE)
names(df) <- df[c(1), ]
df <- df[-c(1), ]
df <- as.matrix(as.data.frame(lapply(df, as.numeric)))
df2 <- as.data.frame.numeric(rowMeans(subset(df, select = c(Jan:Dec)), na.rm = TRUE))
df2$AnnMean <- df2$`rowMeans(subset(df, select = c(Jan:Dec)), na.rm = TRUE)`
df2$`rowMeans(subset(df, select = c(Jan:Dec)), na.rm = TRUE)` <- NULL
df <- cbind(df, df2)
df <- df[,-c(2:13)]
df$AnnMean <- df$AnnMean / 100
df$AnnU3 <- df$AnnMean
df$AnnMean <- NULL
U3 <- df
U3$Year <- as.integer(U3$Year)

# Use U3$AnnU3 as DV in masterData

#################################################

# Read in and format Labor Force Participation Rate 1950 to 2014
# Data Series Number: LNS11300000
# Downloaded file as csv to local directory

df <- read.csv("LFPR_1950_2014.csv", header = FALSE, skip = 11, stringsAsFactors = FALSE)
names(df) <- df[c(1), ]
df <- df[-c(1), ]
df <- as.matrix(as.data.frame(lapply(df, as.numeric)))
df2 <- as.data.frame.numeric(rowMeans(subset(df, select = c(Jan:Dec)), na.rm = TRUE))
df2$AnnMean <- df2$`rowMeans(subset(df, select = c(Jan:Dec)), na.rm = TRUE)`
df2$`rowMeans(subset(df, select = c(Jan:Dec)), na.rm = TRUE)` <- NULL
df <- cbind(df, df2)
df <- df[,-c(2:13)]
df$AnnMean <- df$AnnMean / 100
df$AnnLFPR <- df$AnnMean
df$AnnMean <- NULL
LFPR <- df
LFPR$Year <- as.integer(LFPR$Year)

# Use LFPR$AnnLFPR as DV in masterData

###############################################

# Create and read in Party-In-Power table 1950 to 2014
# https://en.wikipedia.org/wiki/List_of_Presidents_of_the_United_States
# http://www.infoplease.com/ipa/A0774721.html

PIP <- read.csv("Party_In_Power.csv", header = TRUE, stringsAsFactors = FALSE)
str(PIP)
PIP <- PIP[ , c(2:3, 7, 11:12, 18:19, 26:27)]
str(PIP)

##############################################

# Create masterData file to serve as basis for analysis

masterData <- data.frame(1950:2014)
names(masterData) <- c("Year")
masterData$Year <- as.integer(masterData$Year)

###############################################

# Merge all data files into masterData
# masterData, MedFamInc, U3, LFPR, Party-in-Power
masterData <- merge(masterData, PIP)
masterData <- merge(masterData, MedFamInc)
masterData <- merge(masterData, U3)
masterData <- merge(masterData, LFPR)
str(masterData)

############################################

MD <- masterData

# Create EmpRate. EmpRate = 1 - U3 unemployment rate.
MD$EmpRate <- (1 - MD$AnnU3)

# Remove the U3 variable (now using the Employment Rate).
MD <- MD[,-c(11)]

# Analyze the correlations between IV (Party in Power, Recession or Not)
# and DV (Median Family Income, Employment Rate, and Labor Force
# Participation Rate

cor(MD)

### Need to determine how to best depict correlation findings #########


### Need to recreate the graphs I created in Excel in R ###


### Plot time series of MFI, ER, and LFPR using ggplot2 ###

AD <- MD[,c(10:12)]

AD <- as.data.frame(scale(AD))
AD$Year <- MD$Year

library(ggplot2)

p <- ggplot() + 
  geom_line(data = AD, aes(x = Year, y = Med14, color = "red")) +
  geom_line(data = AD, aes(x = Year, y = EmpRate, color = "green"))  +
  geom_line(data = AD, aes(x = Year, y = AnnLFPR, color = "blue")) +
  xlab('Year') +
  ylab('Change')

p
