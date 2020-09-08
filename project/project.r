# CMPT 318 (Fall 2018)
# Final Project
#
# Authors:
#   Aarish Kapila
#   Che Jung (Kent) Lee
#   Karan Sharma
#   Razvan Andrei Cretu
#   Yernur Nursultanov

library("depmixS4")
library("data.table")

# FUNCTIONS =======================================================================================

split_time <- function(df)
{
  morning <- df[df$Hour <= 11,]
  afternoon <- df[df$Hour >= 12 & df$Hour <= 17,]
  night <- df[df$Hour >= 18 & df$Hour <= 23,]
  return(list(morning, afternoon, night))
}

# https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
# get season of dates
get_season <- function(dates) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(dates, format = "2012-%m-%d"))

  ifelse (d >= WS | d < SE, "Winter", 
    ifelse (d >= SE & d < SS, "Spring", 
      ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

load_data <- function(filename)
{
  # df: 1556444 row 9 col
  df <- read.table(paste0("./data/", filename), header = TRUE, sep = ",")
  # remove NA
  df <- df[complete.cases(df),]
  # there are 3 built-in date/time classes in R: Date, POSIXct, and POSIXlt
  # we use POSIXct because
  #   Date only contains date, but we have time
  #   POSIXlt is too large (40 bytes / record) and cannot be used in data.table
  # if tz (timezone) is not specified, there will be 180 NA Timestamp
  df$Timestamp <- as.POSIXct(paste(df$Date, df$Time), tz = "UTC", format = "%d/%m/%Y %T")
  # this will assign Time with current date, which is technically incorrect
  # however, this is useful for grouping and doing analysis on minute based
  df$Time <- as.POSIXct(df$Time, format = "%T")
  df$Hour <- as.numeric(format(df$Timestamp, "%H"))
  df$Day <- weekdays(df$Timestamp, abbreviate = TRUE)
  # levels: pre-defined set values; used to sort in defined order (alphabetic in default)
  # as.factor does not accept levels parameter
  df$Month <- factor(months(df$Timestamp, abbreviate = TRUE), levels = month.abb)
  df$Season <- factor(get_season(df$Timestamp), levels = c("Spring","Summer","Fall","Winter"))

  wed <- df[df$Day == "Wed",]
  sat <- df[df$Day == "Sat",]

  wed <- split_time(wed)
  wed.m <- wed[[1]]
  wed.a <- wed[[2]]
  wed.n <- wed[[3]]

  sat <- split_time(sat)
  sat.m <- sat[[1]]
  sat.a <- sat[[2]]
  sat.n <- sat[[3]]

  return(list(wed.m, wed.a, wed.n, sat.m, sat.a, sat.n))
}

aggregate_data <- function(df)
{
  dt <- data.table(df)
  minute <- dt[, list(min = min(Global_active_power), max = max(Global_active_power), mean = mean(Global_active_power), sd = sd(Global_active_power)), by = Time]
  month <- dt[, list(min = min(Global_active_power), max = max(Global_active_power), mean = mean(Global_active_power), sd = sd(Global_active_power)), by = Month]
  season <- dt[, list(min = min(Global_active_power), max = max(Global_active_power), mean = mean(Global_active_power), sd = sd(Global_active_power)), by = Season]

  # sort dataframes in proper order
  minute <- minute[order(minute$Time),]
  month <- month[order(month$Month),]
  season <- season[order(season$Season),]

  return(list(minute, month, season))
}

plot_minute <- function(title, grouped_train, grouped_tests)
{
  # save images to disk
  png(paste0("./figs/" ,paste0(title, ".png")), width = 1920, height = 1080)
  # type: line, yaxt: custom y axis range (because each dataframe has different y range), lwd: line thickness, ylim: y axis range
  plot(grouped_train[[1]]$Time, grouped_train[[1]]$mean, type = "l", main = title, yaxt = "n", lwd = 2, panel.first = grid(), xlab = "", ylab = "Average Global Active Power", ylim = c(0.5,8), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  # lines(): add lines on above plot
  colour <- c(rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5), rgb(0, 0, 1, 0.5), rgb(0, 1, 1, 0.5), rgb(1, 0, 1, 0.5))
  for (i in 1:length(grouped_tests)) {
    lines(grouped_tests[[i]][[1]]$Time, grouped_tests[[i]][[1]]$mean, col = colour[i], lwd = 2)
  }
  # regression line based on train data
  abline(lm(grouped_train[[1]]$mean ~ grouped_train[[1]]$Time), col = "brown", lty = 3, lwd = 2)
  axis(2, at = seq(0.5, 8, by = 0.5))
  legend("topright", legend = c("Train","Test 1","Test 2","Test 3","Test 4","Test 5","Regression"), lty = c(1,1,1,1,1,1,3), col = c("black","red","green","blue","cyan","magenta","brown"), bty = "n")
  dev.off()
}

plot_month <- function(title, grouped_data)
{
  png(paste0("./figs/" ,paste0(title, ".png")), width = 1920, height = 1080)
  # empty plot used as a base for the below lines (type = "n": no plot)
  # xaxt: custom x axis range (because Month is factor)
  plot(0, type = "n", xaxt = "n", yaxt = "n", panel.first = grid(), main = title, xlab = "", ylab = "Average Global Active Power", xlim = c(1,12), ylim = c(0.5,8), , cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  colour <- c(rgb(0, 0, 0, 0.5), rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5), rgb(0, 0, 1, 0.5), rgb(0, 1, 1, 0.5), rgb(1, 0, 1, 0.5))
  for (i in 1:length(grouped_data)) {
    lines(1:12, grouped_data[[i]][[2]]$mean, col = colour[i], lwd = 2, type = "o", pch = 16)
  }
  abline(lm(grouped_data[[1]][[2]]$mean ~ as.numeric(grouped_data[[1]][[2]]$Month)), col = "brown", lty = 3, lwd = 2)
  axis(1, at = 1:12, labels = grouped_data[[1]][[2]]$Month)
  axis(2, at = seq(0.5, 8, by = 0.5))
  legend("topright", legend = c("Train","Test 1","Test 2","Test 3","Test 4","Test 5","Regression"), lty = c(1,1,1,1,1,1,3), col = c("black","red","green","blue","cyan","magenta","brown"), bty = "n")
  dev.off()
}

plot_season <- function(title, grouped_data)
{
  png(paste0("./figs/" ,paste0(title, ".png")), width = 1920, height = 1080)
  plot(0, type = "n", xaxt = "n", yaxt = "n", panel.first = grid(), main = title, xlab = "", ylab = "Average Global Active Power", xlim = c(1,4), ylim = c(0.5,8), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  colour <- c(rgb(0, 0, 0, 0.5), rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5), rgb(0, 0, 1, 0.5), rgb(0, 1, 1, 0.5), rgb(1, 0, 1, 0.5))
  for (i in 1:length(grouped_data)) {
    lines(1:4, grouped_data[[i]][[3]]$mean, col = colour[i], lwd = 2, type = "o", pch = 16)
  }
  abline(lm(grouped_data[[1]][[3]]$mean ~ as.numeric(grouped_data[[1]][[3]]$Season)), col = "brown", lty = 3, lwd = 2)
  axis(1, at = 1:4, labels = grouped_data[[1]][[3]]$Season)
  axis(2, at = seq(0.5, 8, by = 0.5))
  legend("topright", legend = c("Train","Test 1","Test 2","Test 3","Test 4","Test 5","Regression"), lty = c(1,1,1,1,1,1,3), col = c("black","red","green","blue","cyan","magenta","brown"), bty = "n")
  dev.off()
}

# there are too many points in a time period of a test data; plotting all tests in a graph is unreadable
# therefore, need to plot a single time period at a time
plot_point_anomaly <- function(title, grouped_train, test)
{
  png(paste0("./figs/" ,paste0(title, ".png")), width = 1920, height = 1080)
  plot(test$Time, test$Global_active_power, panel.first = grid(), main = title, xlab = "", ylab = "Global Active Power", pch = 16, col = rgb(0, 0, 0, 0.5), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  lines(grouped_train[[1]]$Time, grouped_train[[1]]$max, col = "red", lwd = 2)
  lines(grouped_train[[1]]$Time, grouped_train[[1]]$min, col = "green", lwd = 2)
  legend("topright", legend = c(paste(substr(title, 1, 6), "Data Points"),"Train Max","Train Min"), lty = c(NA,1,1), pch = c(16,NA,NA), col = c("black","red","green"), bty = "n")
  dev.off()
}

record_point_anomaly <- function(filename, grouped_train, tests)
{
  for (i in 1:length(tests)) {
    tests[[i]]$Train_max <- grouped_train[[1]]$max[match(tests[[i]]$Time, grouped_train[[1]]$Time)]
    tests[[i]]$Train_min <- grouped_train[[1]]$min[match(tests[[i]]$Time, grouped_train[[1]]$Time)]
    output <- tests[[i]][tests[[i]]$Global_active_power >= tests[[i]]$Train_max | tests[[i]]$Global_active_power <= tests[[i]]$Train_min,]
    write.csv(output, file = paste0("./output/", paste0(paste(paste("Test", i), filename), ".csv")))
  }
}

# https://stackoverflow.com/questions/743812/calculating-moving-average
moving_average <- function(feature, window_size)
{
  filter(feature, rep(1/window_size, window_size), sides = 1)
}

plot_collective_anomaly <- function(title, test, window_size, threshold)
{
  png(paste0("./figs/" ,paste0(title, ".png")), width = 1920, height = 1080)
  test$MA <- moving_average(test$Global_active_power, window_size)
  test$Threshold_max <- test$MA + threshold
  test$Threshold_min <- test$MA - threshold
  plot(1:nrow(test), test$Global_active_power, panel.first = grid(), main = title, xlab = "Observations", ylab = "Global Active Power", pch = 16, col = rgb(0, 0, 0, 0.5), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  lines(1:nrow(test), test$Threshold_max, col = "red", lwd = 2)
  lines(1:nrow(test), test$Threshold_min, col = "green", lwd = 2)
  legend("topright", legend = c(paste(substr(title, 1, 6), "Data Points"),"Avg + Threshold","Avg - Threshold"), lty = c(NA,1,1), pch = c(16,NA,NA), col = c("black","red","green"), bty = "n")
  dev.off()
}

record_collective_anomaly <- function(filename, tests, window_size, threshold)
{
  for (i in 1:length(tests)) {
    tests[[i]]$MA <- moving_average(tests[[i]]$Global_active_power, window_size)
    tests[[i]]$Threshold_max <- tests[[i]]$MA + threshold
    tests[[i]]$Threshold_min <- tests[[i]]$MA - threshold
    output <- tests[[i]][tests[[i]]$Global_active_power >= tests[[i]]$Threshold_max | tests[[i]]$Global_active_power <= tests[[i]]$Threshold_min,]
    write.csv(output, file = paste0("./output/", paste0(paste(paste("Test", i), filename), ".csv")))
  }
}

# get a vector of nrow(distinct observation)
get_ntimes <- function(train)
{
  ntimes <- c()
  idx <- 1
  for (day in unique(train$Date)) {
    ntimes[idx] <- nrow(train[train$Date == day,])
    idx <- idx + 1
  }
  return(ntimes)
}

# test optimal nstates
get_nstates <- function(trains, k)
{
  best_nstates <- c()
  for (i in 1:length(trains)) {
    t <- get_ntimes(trains[[i]])
    bic_values <- c()
    idx <- 1
    for (j in 1:k) {
      # use try (or tryCatch) to skip errors / warnings generated by unsolvable parameters
      # more details: https://stackoverflow.com/questions/25363871/na-nan-inf-error-when-fitting-hmm-using-depmixs4-in-r
      try({
        mod <- depmix(response = trains[[i]]$Global_active_power ~ 1, data = trains[[i]], nstates = j, family = gaussian(), ntimes = t)
        fm <- fit(mod)
        bic_values[idx] <- BIC(fm)
      })
      # one trick in R: when declare empty vector v <- c() and assign v[1] <- 1, v[5] <- 5, print(v): 1 NA NA NA 5
      # so if bic_values is not assigned a value due to error, the next valid bic value will fill the gap with NA
      idx <- idx + 1
    }
    # index of minimum positive value
    best_nstates[i] <- which.min(bic_values[bic_values > 0])
  }
  return(bic_values)
}

# apply HMM to train data and calculate log likelihood of test data using the fitted model
HMM <- function(train, tests, k)
{
  train.mod <- depmix(response = train$Global_active_power ~ 1, data = train, nstates = k, family = gaussian(), ntimes = get_ntimes(train))
  train.fm <- fit(train.mod)

  log_like <- c()
  log_like[1] <- logLik(train.fm) / length(unique(train$Date))

  for (i in 1:length(tests)) {
    # initialize depmix object for test data
    test.mod <- depmix(response = tests[[i]]$Global_active_power ~ 1, data = tests[[i]], nstates = k, family = gaussian(), ntimes = get_ntimes(tests[[i]]))
    # update it with parameters in the train fitted model
    test.fm <- setpars(test.mod, getpars(train.fm))
    # calculate log likelihood
    fb <- forwardbackward(test.fm)
    log_like[i+1] <- fb$logLike / length(unique(tests[[i]]$Date))
  }

  return(log_like)
}

plot_logLike <- function(day, morningLogLike, afternoonLogLike, nightLogLike)
{
  png(paste0("./figs/" ,paste0(day, " Log Likelihood.png")), width = 1920, height = 1080)
  plot(morningLogLike, col = "blue", type = "o", xaxt = "n", panel.first = grid(), main = paste0("Log Likelihood per dataset on ", day), xlab = "Dataset", ylab = "Log Likelihood", xlim = c(1,6), ylim = c(-6000, 0), pch = 16, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  lines(afternoonLogLike, col = "red", lwd = 2)
  lines(nightLogLike, col = "purple", lwd = 2)
  legend("topright", legend = c("Morning log likelihood","Afternoon log likelihood","Night log likelihood"), lty = c(1,1,1), col = c("blue","red","purple"), bty = "n")
  axis(1, at = 1:6, labels = c("Train","Test 1","Test 2","Test 3","Test 4","Test 5"))
  dev.off()
}


# SCRIPT ==========================================================================================

# PHASE 1

# each contains a list of 6 dataframes:
#   morning, afternoon, night of Wed and Sat
train <- load_data("Train Data.txt")
test1 <- load_data("test1.txt")
test2 <- load_data("test2.txt")
test3 <- load_data("test3.txt")
test4 <- load_data("test4.txt")
test5 <- load_data("test5.txt")

# each contains a list of 3 dataframes:
#   minute, month, season
train.wed.m <- aggregate_data(train[[1]])
train.wed.a <- aggregate_data(train[[2]])
train.wed.n <- aggregate_data(train[[3]])
train.sat.m <- aggregate_data(train[[4]])
train.sat.a <- aggregate_data(train[[5]])
train.sat.n <- aggregate_data(train[[6]])

test1.wed.m <- aggregate_data(test1[[1]])
test1.wed.a <- aggregate_data(test1[[2]])
test1.wed.n <- aggregate_data(test1[[3]])
test1.sat.m <- aggregate_data(test1[[4]])
test1.sat.a <- aggregate_data(test1[[5]])
test1.sat.n <- aggregate_data(test1[[6]])

test2.wed.m <- aggregate_data(test2[[1]])
test2.wed.a <- aggregate_data(test2[[2]])
test2.wed.n <- aggregate_data(test2[[3]])
test2.sat.m <- aggregate_data(test2[[4]])
test2.sat.a <- aggregate_data(test2[[5]])
test2.sat.n <- aggregate_data(test2[[6]])

test3.wed.m <- aggregate_data(test3[[1]])
test3.wed.a <- aggregate_data(test3[[2]])
test3.wed.n <- aggregate_data(test3[[3]])
test3.sat.m <- aggregate_data(test3[[4]])
test3.sat.a <- aggregate_data(test3[[5]])
test3.sat.n <- aggregate_data(test3[[6]])

test4.wed.m <- aggregate_data(test4[[1]])
test4.wed.a <- aggregate_data(test4[[2]])
test4.wed.n <- aggregate_data(test4[[3]])
test4.sat.m <- aggregate_data(test4[[4]])
test4.sat.a <- aggregate_data(test4[[5]])
test4.sat.n <- aggregate_data(test4[[6]])

test5.wed.m <- aggregate_data(test5[[1]])
test5.wed.a <- aggregate_data(test5[[2]])
test5.wed.n <- aggregate_data(test5[[3]])
test5.sat.m <- aggregate_data(test5[[4]])
test5.sat.a <- aggregate_data(test5[[5]])
test5.sat.n <- aggregate_data(test5[[6]])

plot_minute("Minutely Wednesday Morning", train.wed.m, list(test1.wed.m, test2.wed.m, test3.wed.m, test4.wed.m, test5.wed.m))
plot_minute("Minutely Wednesday Afternoon", train.wed.a, list(test1.wed.a, test2.wed.a, test3.wed.a, test4.wed.a, test5.wed.a))
plot_minute("Minutely Wednesday Night", train.wed.n, list(test1.wed.n, test2.wed.n, test3.wed.n, test4.wed.n, test5.wed.n))
plot_minute("Minutely Saturday Morning", train.sat.m, list(test1.sat.m, test2.sat.m, test3.sat.m, test4.sat.m, test5.sat.m))
plot_minute("Minutely Saturday Afternoon", train.sat.a, list(test1.sat.a, test2.sat.a, test3.sat.a, test4.sat.a, test5.sat.a))
plot_minute("Minutely Saturday Night", train.sat.n, list(test1.sat.n, test2.sat.n, test3.sat.n, test4.sat.n, test5.sat.n))

plot_month("Monthly Wednesday Morning", list(train.wed.m, test1.wed.m, test2.wed.m, test3.wed.m, test4.wed.m, test5.wed.m))
plot_month("Monthly Wednesday Afternoon", list(train.wed.a, test1.wed.a, test2.wed.a, test3.wed.a, test4.wed.a, test5.wed.a))
plot_month("Monthly Wednesday Night", list(train.wed.n, test1.wed.n, test2.wed.n, test3.wed.n, test4.wed.n, test5.wed.n))
plot_month("Monthly Saturday Morning", list(train.sat.m, test1.sat.m, test2.sat.m, test3.sat.m, test4.sat.m, test5.sat.m))
plot_month("Monthly Saturday Afternoon", list(train.sat.a, test1.sat.a, test2.sat.a, test3.sat.a, test4.sat.a, test5.sat.a))
plot_month("Monthly Saturday Night", list(train.sat.n, test1.sat.n, test2.sat.n, test3.sat.n, test4.sat.n, test5.sat.n))

plot_season("Seasonally Wednesday Morning", list(train.wed.m, test1.wed.m, test2.wed.m, test3.wed.m, test4.wed.m, test5.wed.m))
plot_season("Seasonally Wednesday Afternoon", list(train.wed.a, test1.wed.a, test2.wed.a, test3.wed.a, test4.wed.a, test5.wed.a))
plot_season("Seasonally Wednesday Night", list(train.wed.n, test1.wed.n, test2.wed.n, test3.wed.n, test4.wed.n, test5.wed.n))
plot_season("Seasonally Saturday Morning", list(train.sat.m, test1.sat.m, test2.sat.m, test3.sat.m, test4.sat.m, test5.sat.m))
plot_season("Seasonally Saturday Afternoon", list(train.sat.a, test1.sat.a, test2.sat.a, test3.sat.a, test4.sat.a, test5.sat.a))
plot_season("Seasonally Saturday Night", list(train.sat.n, test1.sat.n, test2.sat.n, test3.sat.n, test4.sat.n, test5.sat.n))


# PHASE 2 APPROACH 1.1

tests <- list(test1, test2, test3, test4, test5)
for (i in 1:length(tests)) {
  test_title <- paste("Test", i)
  plot_point_anomaly(paste(test_title, "Wednesday Morning Point Anomaly") , train.wed.m, tests[[i]][[1]])
  plot_point_anomaly(paste(test_title, "Wednesday Afternoon Point Anomaly"), train.wed.a, tests[[i]][[2]])
  plot_point_anomaly(paste(test_title, "Wednesday Night Point Anomaly"), train.wed.n, tests[[i]][[3]])
  plot_point_anomaly(paste(test_title, "Saturday Morning Point Anomaly"), train.sat.m, tests[[i]][[4]])
  plot_point_anomaly(paste(test_title, "Saturday Afternoon Point Anomaly"), train.sat.a, tests[[i]][[5]])
  plot_point_anomaly(paste(test_title, "Saturday Night Point Anomaly"), train.sat.n, tests[[i]][[6]])
}

record_point_anomaly("Wednesday Morning Point Anomaly", train.wed.m, list(test1[[1]], test2[[1]], test3[[1]], test4[[1]], test5[[1]]))
record_point_anomaly("Wednesday Afternoon Point Anomaly", train.wed.a, list(test1[[2]], test2[[2]], test3[[2]], test4[[2]], test5[[2]]))
record_point_anomaly("Wednesday Night Point Anomaly", train.wed.n, list(test1[[3]], test2[[3]], test3[[3]], test4[[3]], test5[[3]]))
record_point_anomaly("Saturday Morning Point Anomaly", train.sat.m, list(test1[[4]], test2[[4]], test3[[4]], test4[[4]], test5[[4]]))
record_point_anomaly("Saturday Afternoon Point Anomaly", train.sat.a, list(test1[[5]], test2[[5]], test3[[5]], test4[[5]], test5[[5]]))
record_point_anomaly("Saturday Night Point Anomaly", train.sat.n, list(test1[[6]], test2[[6]], test3[[6]], test4[[6]], test5[[6]]))


# PHASE 2 APPROACH 1.2

window_size <- 60
threshold <- 1.5
for (i in 1:length(tests)) {
  test_title <- paste("Test", i)
  plot_collective_anomaly(paste(test_title, "Wednesday Morning Collective Anomaly"), tests[[i]][[1]], window_size, threshold)
  plot_collective_anomaly(paste(test_title, "Wednesday Afternoon Collective Anomaly"), tests[[i]][[2]], window_size, threshold)
  plot_collective_anomaly(paste(test_title, "Wednesday Night Collective Anomaly"), tests[[i]][[3]], window_size, threshold)
  plot_collective_anomaly(paste(test_title, "Saturday Morning Collective Anomaly"), tests[[i]][[4]], window_size, threshold)
  plot_collective_anomaly(paste(test_title, "Saturday Afternoon Collective Anomaly"), tests[[i]][[5]], window_size, threshold)
  plot_collective_anomaly(paste(test_title, "Saturday Night Collective Anomaly"), tests[[i]][[6]], window_size, threshold)
}

record_collective_anomaly("Wednesday Morning Collective Anomaly", list(test1[[1]], test2[[1]], test3[[1]], test4[[1]], test5[[1]]), window_size, threshold)
record_collective_anomaly("Wednesday Afternoon Collective Anomaly", list(test1[[2]], test2[[2]], test3[[2]], test4[[2]], test5[[2]]), window_size, threshold)
record_collective_anomaly("Wednesday Night Collective Anomaly", list(test1[[3]], test2[[3]], test3[[3]], test4[[3]], test5[[3]]), window_size, threshold)
record_collective_anomaly("Saturday Morning Collective Anomaly", list(test1[[4]], test2[[4]], test3[[4]], test4[[4]], test5[[4]]), window_size, threshold)
record_collective_anomaly("Saturday Afternoon Collective Anomaly", list(test1[[5]], test2[[5]], test3[[5]], test4[[5]], test5[[5]]), window_size, threshold)
record_collective_anomaly("Saturday Night Collective Anomaly", list(test1[[6]], test2[[6]], test3[[6]], test4[[6]], test5[[6]]), window_size, threshold)


# PHASE 2 APPROACH 2

# get consistent HMM results
set.seed(1)

# code below is commented out because it takes about 1 hour to run. The result is a vector of best nstates for each train time period
# best_nstates <- get_nstates(list(train[[1]], train[[2]], train[[3]], train[[4]], train[[5]], train[[6]]), 15)

# the nstates below are from the get_nstates() above
# each contains a vector of 6 log likelihood values:
#   train, test 1 ~ test 5
wed.m.hmm <- HMM(train[[1]], list(test1[[1]], test2[[1]], test3[[1]], test4[[1]], test5[[1]]), 5)
wed.a.hmm <- HMM(train[[2]], list(test1[[2]], test2[[2]], test3[[2]], test4[[2]], test5[[2]]), 11)
wed.n.hmm <- HMM(train[[3]], list(test1[[3]], test2[[3]], test3[[3]], test4[[3]], test5[[3]]), 16)
sat.m.hmm <- HMM(train[[4]], list(test1[[4]], test2[[4]], test3[[4]], test4[[4]], test5[[4]]), 6)
sat.a.hmm <- HMM(train[[5]], list(test1[[5]], test2[[5]], test3[[5]], test4[[5]], test5[[5]]), 14)
sat.n.hmm <- HMM(train[[6]], list(test1[[6]], test2[[6]], test3[[6]], test4[[6]], test5[[6]]), 14)

plot_logLike("Wednesday", wed.m.hmm, wed.a.hmm, wed.n.hmm)
plot_logLike("Saturday", sat.m.hmm, sat.a.hmm, sat.n.hmm)

cat("\nPlots saved to './figs/'")