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