NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

################
## Question 1 ##
################

# ~~uncomment me before submission~~

# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

totalEmissionsPerYear <- aggregate(NEI$Emissions, by=list(NEI$year), FUN=sum)


png("plot1.png")

plot(totalEmissionsPerYear,xlab = "year", ylab="Amount of PM2.5 emitted, in tons")
regressionLine = lm(totalEmissionsPerYear$x ~ totalEmissionsPerYear$Group.1)
abline(regressionLine)

dev.off()


################
## Question 2 ##
################

# ~~uncomment me before submission~~
# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

png("plot2.png")

baltimoreOnly = NEI[NEI$fips == "24510",]

totalEmissionsPerYearInBaltimore <- aggregate(baltimoreOnly$Emissions, by=list(baltimoreOnly$year), FUN=sum)

plot(totalEmissionsPerYearInBaltimore,xlab = "year", ylab="Amount of PM2.5 emitted, in tons",  main="Emissions / Time in Baltimore")
regressionLineBaltimore = lm(totalEmissionsPerYearInBaltimore$x ~ totalEmissionsPerYearInBaltimore$Group.1)
abline(regressionLineBaltimore)

dev.off()

################
## Question 3 ##
################

# ~~uncomment me before submission~~
# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

png("plot3.png")

# install.packages('data.table')

library(ggplot2)

require(data.table)

baltimoreOnly = NEI[NEI$fips == "24510",]

baltimoreOnlyTable <- data.table(baltimoreOnly)

aggregatedBaltimoreOnlyTable <- baltimoreOnlyTable[, list(totalEmissions = sum(Emissions)), by = list(year,type )]

ggplot(aggregatedBaltimoreOnlyTable, aes(year, y = totalEmissions, color = type)) + geom_point() + geom_line()

dev.off()

################
## Question 4 ##
################

# ~~uncomment me before submission~~
# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

png("plot4.png")

# install.packages('data.table')

library(ggplot2)

require(data.table)

coalRelatedCodes <- SCC[grep("[Cc]oal", SCC$Short.Name),]$SCC

coalCombustionOnly <- NEI[NEI$SCC %in% coalRelatedCodes,]

coalCombustionOnlyTable <- data.table(coalCombustionOnly)

aggregatedCoalCombustionOnlyTable <- coalCombustionOnlyTable[, list(totalEmissions = sum(Emissions)), by = list(year,type )]

ggplot(aggregatedCoalCombustionOnlyTable, aes(year, y = totalEmissions, color = type)) + geom_point() + geom_line()

dev.off()


