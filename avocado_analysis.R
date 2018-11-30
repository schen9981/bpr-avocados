library(readr)
library(ggplot2)
library(lubridate)
library(ggseas)
library(plyr)
library(dplyr)
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
  labs(x = "time", y ="price", title = "Decomposition of Avocado Prices")

# model the linear relationship


