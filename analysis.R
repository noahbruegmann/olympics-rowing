library(data.table)
library(lattice)
data <- read.csv('~/Dropbox/OlympicRowingResults2016.csv')
data <- data.table(data)
head(data)
data$Athletes <- data$Class
data[Class == 8]$Athletes <- 9

# How many mens v womens boats and competitors?
data[,.(Boats = .N, Athletes = sum(Athletes)), .(Gender)]

# How many sculling v sweeping boats and competitors?
data[,.(Boats = .N, Athletes = sum(Athletes)), .(Scull.sweep)]

# How many light v heavy competitors and boats?
data[,.(Boats = .N, Athletes = sum(Athletes)), .(Weight)]

data[,.(Boats = .N, Athletes = sum(Athletes)), .(Gender, Weight)]

# What teams send the highest number of boats? Competitors?
size.country <- data[,.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]
summary(size.country)
size.country[, .(Country, Boats, Athletes)][order(-Boats)]
size.country[,Size.Category := 'small']
size.country[Boats > 2, Size.Category := 'medium']
size.country[Boats > 5, Size.Category := 'large']
# Add general size buckets to main dataset
data <- merge(data, size.country[,Athletes := NULL], by='Country')
data[, Boats := NULL]

# How about for light/heavy?
data[Weight == 'L',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]
data[Weight == 'H',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]

# Which events have the most athletes?
data[,.(Athletes = sum(Athletes)), .(Gender, Weight, Scull.sweep, Class)][order(Athletes)]
data[,.(Boats = .N), .(Gender, Weight, Scull.sweep, Class)][order(Boats)]

# Who got the most first places?
data[Place == 1,.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]

# Most medals?
medals.country <- data[Place <= 3,.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]

# Most A finals?
data[Place <= 6,.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]

# % of boats or athletes medaling
medal.percent <- merge(size.country, medals.country, by = 'Country', all.x = TRUE)
medal.percent[, percentathletes := Athletes.y/Athletes.x]
medal.percent[, percentboats := Boats.y/Boats.x]
medal.percent[is.na(medal.percent)] <- 0
medal.percent[order(percentboats)]
medal.percent[Size.Category == 'large'][order(percentboats)]

# Breaking down performance by scull/sweep
data[Place == 1 & Scull.sweep == 'scull',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]
data[Place == 1 & Scull.sweep == 'sweep',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]

scull.medals <- data[Place <= 3 & Scull.sweep == 'scull',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]
sweep.medals <- data[Place <= 3 & Scull.sweep == 'sweep',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]
merge(scull.medals, sweep.medals, by = 'Country', all = T)[order(Boats.x+Boats.y)]

data[Place <= 6 & Scull.sweep == 'scull',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]
data[Place <= 6 & Scull.sweep == 'sweep',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]

# Light/heavy
data[Place <= 3 & Weight == 'L',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]
data[Place <= 3 & Weight == 'H',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]

# M/W
data[Place <= 3 & Gender == 'W',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]
data[Place <= 3 & Gender == 'M',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]

# Who did the best from the small countries list?
data[Place <= 3 & Size.Category == 'small',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]
data[Place <= 3 & Size.Category == 'medium',.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]

# Size of team v medal haul by boat
xyplot(Boats.y ~ Boats.x, data = medal.percent, main = 'Overall', xlab = 'Boats brought', ylab = 'Boats medaling', type = c("p","r"), 
       panel=function(x, y, ...) {
         panel.xyplot(x, y, ...);
         ltext(x = medal.percent$Boats.x, y = medal.percent$Boats.y, labels=medal.percent$Country, pos=1, offset=1, cex=0.8)
       })

# How does size of country's team correlate with medal haul?
plotperformance <- function(data, maintitle) {
  size.country <- data[,.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]
  medals.country <- data[Place <= 3,.(Boats = .N, Athletes = sum(Athletes)), .(Country)][order(Boats)]
  medal.percent <- merge(size.country, medals.country, by = 'Country', all.x = TRUE)
  medal.percent[, percentathletes := Athletes.y/Athletes.x]
  medal.percent[, percentboats := Boats.y/Boats.x]
  medal.percent[is.na(medal.percent)] <- 0
  xyplot(Athletes.y ~ Athletes.x, data = medal.percent, main = maintitle, xlab = 'Athletes brought', ylab = 'Athletes medaling', type = c("p","r"), 
         panel=function(x, y, ...) {
           panel.xyplot(x, y, ...);
           ltext(x = medal.percent$Athletes.x, y = medal.percent$Athletes.y, labels=medal.percent$Country, pos=1, offset=1, cex=0.8)
         })
}

plotperformance(data, 'Overall')
# Same graph as above, for women and men separately
plotperformance(data[Gender == 'M'], 'Mens teams only')
plotperformance(data[Gender == 'W'], 'Womens teams only')
# Light/heavy
plotperformance(data[Weight == 'L'])
plotperformance(data[Weight == 'H'])
#Scull/sweep
plotperformance(data[Scull.sweep == 'scull'])
plotperformance(data[Scull.sweep == 'sweep'])
