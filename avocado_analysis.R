library(readr)
library(ggplot2)
library(lubridate)
library(ggseas)
library(plyr)
library(dplyr)
library(ggmap)
library(tidyr)

# read in data
avocado <- read_csv("avocado.csv")

# reformat data
colnames(avocado)[5] <- "Small/Medium - Total Volume"
colnames(avocado)[6] <- "Large - Total Volume"
colnames(avocado)[7] <- "XL - Total Volume"
avocado$Date <- as.Date(avocado$Date)

# get counts of regions of data
regions <- unique(avocado$region)
counts <- rep(0, length(regions))

for (i in 1 : length(regions)) {
  counts[i] <- nrow(avocado[which(avocado$region == regions[i]), ])
}

region_counts <- cbind(regions, counts) # each region has 338 data points


# get TotalUS data for average prices
us_conven <- avocado[which(avocado$region == 'TotalUS' & 
                                      avocado$type == 'conventional'), ]
us_organic <- avocado[which(avocado$region == 'TotalUS' &  
                                      avocado$type == 'organic'), ]
ggplot(data=us_conven, 
       aes(x = Date, y = AveragePrice)) + 
  labs(x = "Date", y = "Average Price",
       title = "Average Prices of Avocados in US from 2015-2018") +
  geom_line(color="blue") +
  geom_line(data=us_organic, aes(x = Date, y = AveragePrice),
            color = "darkgreen")

### analysis of total US average prices of conventional and organic avocados ###
# order by date
us_organic <- us_organic[order(us_organic$Date), ]
us_organic_price <- us_organic[, c(2, 3)]
us_conven <- us_conven[order(us_conven$Date), ]
us_conven_price <- us_conven[, c(2, 3)]

# create time series from data
us_conven_ts <- ts(us_conven_price, frequency=51, 
                   start = decimal_date(as.Date("2015-01-04")))
us_organic_ts <-ts(us_organic_price, frequency=51, 
                   start = decimal_date(as.Date("2015-01-04")))

# decompose
cbind(us_conven_ts, us_organic_ts) %>%
  ggseas::tsdf() %>% 
  gather("series", "price", c(us_conven_ts.AveragePrice, us_organic_ts.AveragePrice)) %>% 
  ggseas::ggsdc(aes(x, price, colour = series), method = "decompose") +
  geom_line() +
  labs(x = "Time", y ="Average Price", title = "Decomposition of Avocado Prices", colour ="Avocado Type") +
  scale_color_manual(labels = c("Conventional", "Organic"), values = c("royalblue3", "springgreen3"))

# clean up cities data
regions[3] <- "Baltimore"
regions[6] <- "Buffalo"
regions[10] <- "Cincinnati"
regions[12] <- "Dallas"
regions[15] <- "`Grand Rapids`"
regions[16] <- "`Great Lakes`"
regions[17] <- "Harrisburg"
regions[18] <- "Hartford"
regions[22] <- "`Las Vegas`"
regions[23] <- "`Los Angeles`"
regions[25] <- "Miami"
regions[28] <- "`New Orleans`"
regions[29] <- "`New York`"
regions[34] <- "Phoenix"
regions[38] <- "Raleigh"
regions[39] <- "Richmond"
regions[42] <- "`San Diego`"
regions[43] <- "`San Francisco`"
regions[45] <- "`South Carolina`"
regions[49] <- "`St Louis`"
regions <- regions[c(-26, -30, -31, -46, -47, -52, -53, -54)]
regions <- regions[-43]


# plot cities included in analysis in US map
mp <- NULL
mapUS <- borders("state", colour="white", fill="gray59")
mp <- ggplot() + mapUS 

#Now Layer the cities on top
for (i in 1:length(regions)) {
  coord <- geocode(regions[i], source = "dsk")
  mp <- mp + geom_point(aes_string(x = coord$lon, y = coord$lat), color = "blue3", size = 2)
}
mp <- mp + theme(axis.title.x = element_blank(), 
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
mp <- mp + labs(title = "Map of Data Collection Locations for Avocados")