# PLOT OF US-Dots, Zip-Binned Colors

library(maps)
data(us.cities)

# BASE-MAP of US with pretty colors

map('state', bg="steelblue", col="#A8DDB5", fill=T)

# OPTIONAL TITLE
# title(main = 'BHR Recruitment Efforts', family="Times")

# READ AND PREP BHR CSV data 
# (required: each row is a zip with a corresponding 'n'. Optional: county names and lat/lon per zip. Can also get elsewhere.)

bhr <- read.csv("/Users/vhasfcbuckls/Desktop/GeoSpatial/BHR_ByZipCode-2014.07.29.csv", header=T)
bhr <- na.omit(bhr)
bhr <- bhr[order(bhr$n, decreasing=F),]

# ADD THE RADII: one varies by n (zip counts); the other is fixed
bhr$r <- sqrt(bhr$n / pi)
bhr$fixrad <- .6

# MAKE COLOR BINS

fillred <- c()

for (i in 1:length(bhr$n)) {
    
    if (bhr$n[i] >= 100) {
        
        fillred <- c(fillred, "red")
        
    } else {
        
        if (bhr$n[i] >= 50 & bhr$n[i] < 100) {
            
            fillred <- c(fillred, "red")
            
    } else {
        
        if (bhr$n[i] >= 25 & bhr$n[i] < 50) {
            
            fillred <- c(fillred, "red")
        
    } else {
        
        fillred <- c(fillred, "#F7C53A")
        }
    }
}
}

fillstat <- c()

for (i in 1:length(bhr$n)) {
    
    if (bhr$n[i] >= 100) {
        
        fillstat <- c(fillstat, "red")
        
    } else {
    
    if (bhr$n[i] >= 50 & bhr$n[i] < 100) {
        
        fillstat <- c(fillstat, "green")
        
    } else {
        
    if (bhr$n[i] >= 25 & bhr$n[i] < 50) {
        
        fillstat <- c(fillstat, "blue")
        
    } else {
    
        fillstat <- c(fillstat, "#F7C53A")
    }
}
}
}


# NOW ADD DOTS
# VARIABLE SIZING AND COLORS

# symbols(x=bhr$long, y=bhr$lat, circles=bhr$r, fg=fillstat, add=T, inches=0.04, bg=fillstat)
# 

# FIXED SIZING AND VARIABLE COLORS

# symbols(x=bhr$long, y=bhr$lat, circles=bhr$fixrad, fg=fillstat, add=T, inches=0.04, bg=fillstat)
# legend(x=c(-125), y=c(32), legend=c('100', '50', '25', '< 25'), fill=c('red', 'green', 'blue', '#F7C53A'), title = 'N per Zip')

# FIXED RADII, 2 COLORS: Red and Yellow
symbols(x=bhr$long, y=bhr$lat, circles=bhr$fixrad, fg=fillred, add=T, inches=0.04, bg=fillred)
legend(x=c(-125), y=c(32), legend=c('> 25', '< 25'), fill=c('red', '#F7C53A'), title = 'N per Zip')

# ADD MAJOR CITY NAMES ?
# map.cities(x=us.cities,minpop=500000, label=T, pch=3, col="blue")
# add annotations? 
# text(locator(1), 'sometext') ... point and click to add 'sometext' to graphic to that location.

