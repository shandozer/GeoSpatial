# PLOT OF US-Dots, Zip-Binned Colors

library(maps)

# BASE-MAP of US with pretty colors

map('state', bg="steelblue", col="#A8DDB5", fill=T)

# READ AND PREP BHR CSV data 
# (required: each row is a zip with a corresponding 'n'. Optional: county names and lat/lon per zip. Can also get elsewhere.)
bhr <- read.csv("./Desktop/RMaps_7-30-14/BHR_ByZipCode-2014.07.29.csv", header=T)
bhr <- na.omit(bhr)
bhr <- bhr[order(bhr$n, decreasing=F),]

# MAKE COLOR BINS

fillstat <- c()

for (i in 1:length(bhr$n)) {
    
    if (bhr$n[i] >= 100) {
        
        fillstat <- c(fillstat, "red")
        
    } else {
    
    if (bhr$n[i] >= 80 & bhr$n[i] < 100) {
        
        fillstat <- c(fillstat, "orange")
        
    } else {
        
    if (bhr$n[i] >= 60 & bhr$n[i] < 80) {
        
        fillstat <- c(fillstat, "green")
        
    } else {
    
    if (bhr$n[i] >= 40 & bhr$n[i] < 60) {
        
        fillstat <- c(fillstat, "blue")
        
    } else {
    
        fillstat <- c(fillstat, "gray")
    }
}
}
}
}

# NOW ADD DOTS

symbols(x=bhr$long, y=bhr$lat, circles=bhr$r, fg=fillstat, add=T, inches=0.04, bg="white")
# ADD MAJOR CITY NAMES ?
# map.cities(x=us.cities,minpop=500000, label=T, pch=3, col="blue")
# add annotations? 
    # text(locator(1), 'sometext') ... point and click to add 'sometext' to graphic to that location.

