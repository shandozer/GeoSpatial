# PLOT OF US-Dots, Zip-Binned Colors
library(maps)
data(us.cities)

# U.S. BASE-MAP with pretty colors

map('state', bg="steelblue", col="#A8DDB5", fill=T)

# OPTIONAL TITLE
# title(main = 'BHR Recruitment Efforts', family="Times")

# READ AND PREP BHR CSV data 
# (required: each row is a zip with a corresponding 'n'. Optional: county names and lat/lon per zip. Can also get elsewhere.)

#bhr <- read.csv("/Users/vhasfcbuckls/Desktop/GeoSpatial/data/BHR_ByZipCode-2014.07.29.csv", header=T)
bhr <- read.csv("/Users/vhasfcbuckls/Desktop/GeoSpatial/data/BHR_ByZipCode-2014-09-16.csv", header=T)
bhr <- na.omit(bhr)
bhr <- bhr[order(bhr$n, decreasing=F),]
bhr$county <- tolower(bhr$county)

# ADD THE RADII: one varies by n (zip counts); the other is fixed
bhr$r <- sqrt(bhr$n / pi)
bhr$fixrad <- .2

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

symbols(x=bhr$long, y=bhr$lat, circles=bhr$fixrad, fg=fillstat, add=T, inches=0.04, bg=fillstat)
legend(locator(1), legend=c('100+', '50+', '25+', '< 25'), fill=c('red', 'green', 'blue', '#F7C53A'), title = 'N per Zip')

# ZOOMED BAYAREA MAP: REGIONS
# 1- Define your 'Bay Area' as a char vector...
state <- c("california")
counties <- c("san francisco", "marin", "contra costa", "alameda")#, 'santa clara', "san mateo")

regions <- c()
for (i in 1:length(counties)) {
    regions <- c(regions, paste(state,counties[i], sep=','))
}

# 2- CONSTRUCT NEW DATAFRAME: Bay Area Limits
bay <- subset(bhr, bhr$county %in% counties)
#bay.city <- ddply(bay, "City", summarise, cityN = sum(n))

# 3- MAP 
# Pleasant Earthy-green background
# col="#A8DDB5"
# 'REGIONS' MUST TAKE THIS FORM, FOR EXAMPLE: #regions <- c("california,san francisco")
map('county', bg="steelblue", col="#cccccc", fill=T, regions=regions, fg='darkred')

#################
# ALTERNATE ZOOM: 
map('county', 'california', bg='steelblue', ylim=c(37.6, 37.95), xlim=c(-122.62, -122), fill=T, col='darkgray')
symbols(x=(bhr$long + .015), y=bhr$lat, circles=bhr$fixrad, fg=fillstat, add=T, inches=0.04, bg=fillstat)

# 4- Create a Dot-coloring scheme: FILL THE BAY!
fillbay <- c()

for (i in 1:length(bay$n)) {
    
    if (bay$n[i] >= 100) {
        
        fillbay <- c(fillbay, "red")
        
    } else {
        
        if (bay$n[i] >= 50 & bay$n[i] < 100) {
            
            fillbay <- c(fillbay, "green")
            
        } else {
            
            if (bay$n[i] >= 25 & bay$n[i] < 50) {
                
                fillbay <- c(fillbay, "blue")
                
            } else {
                
                fillbay <- c(fillbay, "#F7C53A")
            }
        }
    }
}

# 5- Apply fill-scheme and add dots
symbols(x=bay$long, y=bay$lat, circles=bay$fixrad, fg=fillbay, add=T, inches=0.04, bg=fillbay)
# 6- Optional Legend: top-left corner of legend added to click-location
legend(locator(1), legend=c('100+', '50+', '25+', '< 25'), fill=c('red', 'green', 'blue', '#F7C53A'), title = 'N per Zip')

# 7- Optional Labels: mid-point of text added (slightly smaller than usual) to click-location
text(locator(1), 'Alameda', cex=0.8)
text(locator(1), 'Contra Costa', cex=0.8)
text(locator(1), 'Marin', cex=0.8)
text(locator(1), 'San Francisco', cex=0.8)

# FIXED RADII, 2 COLORS: Red and Yellow
# symbols(x=bhr$long, y=bhr$lat, circles=bhr$fixrad, fg=fillred, add=T, inches=0.04, bg=fillred)
# legend(x=c(-125), y=c(32), legend=c('> 25', '< 25'), fill=c('red', '#F7C53A'), title = 'N per Zip')

# ADD MAJOR CITY NAMES ?
# map.cities(x=us.cities,minpop=75000, label=T, pch=1, col="black", cex=.75)
# add annotations? 
# text(locator(1), 'sometext') ... point and click to add 'sometext' to graphic to that location.

################################
# SAN FRANCISCO-ONLY SECTION
map('county', bg="steelblue", col="#cccccc", fill=T, regions='california,san francisco')
bhr.sf <- subset(bhr, county == 'san francisco')

# longitude needs small correction factor to avoid oceanic dots!
correction <- .015
symbols(x=(bhr$long+correction), y=bhr$lat, circles=bhr$fixrad, fg=fillstat, add=T, inches=0.04, bg=fillstat)
legend(locator(1), legend=c('100+', '50+', '25+', '< 25'), fill=c('red', 'green', 'blue', '#F7C53A'), title = 'N per Zip')

# OPTION: plot the zips (text) next to dots
#text(bhr$long, bhr$lat, bhr$zip, col='darkred', pos = 4, cex=.6)

#tail(bhr.sf, 20)

# CALIFORNIA STATE MAPPING
map('county', 'california', bg='steelblue', col='darkgray', fill=T)
#symbols(x=bhr$long, y=bhr$lat, circles=bhr$r, fg=fillstat, add=T, inches=0.04, bg=fillstat)
bhr.ca <- subset(bhr, bhr$state == 'CA')
fillca <- c()

for (i in 1:length(bhr.ca$n)) {
    
    if (bhr.ca$n[i] >= 100) {
        
        fillca <- c(fillca, "red")
        
    } else {
        
        if (bhr.ca$n[i] >= 50 & bhr.ca$n[i] < 100) {
            
            fillca <- c(fillca, "green")
            
        } else {
            
            if (bhr.ca$n[i] >= 25 & bhr.ca$n[i] < 50) {
                
                fillca <- c(fillca, "blue")
                
            } else {
                
                fillca <- c(fillca, "#F7C53A")
            }
        }
    }
}
symbols(x=bhr.ca$long, y=bhr.ca$lat, circles=bhr.ca$fixrad, fg=fillca, add=T, inches=0.01, bg=fillca)

legend(locator(1), legend=c('100+', '50+', '25+', '< 25'), fill=c('red', 'green', 'blue', '#F7C53A'), title = 'N per Zip')
map.cities(x=us.cities,minpop=400000, label=T, pch=3, col="blue")
