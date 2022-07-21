library(tidyverse)

list.files()

# CHECKING FILES INSIDE "activity.zip"  FOLDER-
unzip("activity.zip", list = TRUE)


# READING DATA - 
dta <- read_csv( unz("activity.zip",  "activity.csv") )

dta$date <- as.Date(dta$date, "%Y-%m-%d")


# TOTAL NUMBER OF STEPS TAKEN PER DAY-

tot_steps <-
  dta %>% group_by(date) %>%
  summarise(tot_step_perday = sum(steps))

hist(x = tot_steps$tot_step_perday, 
     xlab = "Bin of Total steps",
     ylab = "Number of days",
     main = "Total Number of Steps per Day")


# MEAN AND MEDIAN - 
mean(tot_steps$tot_step_perday, na.rm = TRUE)

median(tot_steps$tot_step_perday, na.rm = TRUE)


# Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

sum(is.na(dta$steps))
sum(is.na(dta$date))
sum(is.na(dta$interval))

# only steps variable has NA values.
# Total number of rows with NA / missing is 2304


# 5 min interval and avg steps across all days

min5_avgsteps <- dta %>% group_by(interval) %>% 
  summarise(avg_stps = mean(steps, na.rm = TRUE) )


# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

min5_avgsteps$interval[ min5_avgsteps$avg_stps == max(min5_avgsteps$avg_stps)  ]


plot( x = min5_avgsteps$interval, 
      y = min5_avgsteps$avg_stps, type = "l",
      xlab = "Time interval during Days",
      ylab = "Avg Steps",
      main = "Avg Steps Vs Time interval ")



#imputing missing values-

# FINDING THE DAY OF WEEK-

dta$day <- weekdays(dta$date)



















