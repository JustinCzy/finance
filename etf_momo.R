---
title: "etf_momo_two"
author: "jjc"
date: "12/5/2019"
output: html_document
---

####################### LIBRARY #########################
library(PerformanceAnalytics)
library(quantmod)


####################### GET DATA ########################
#Set ETFs, starting date, and momentum lookback period
momo_tickers <- c("VUG", "VCLT", "EDV", "VIOV", "IAU", "VTV", "VEA", "VNQ", "VWO", "VIOG", "BNDX", "VGIT", "PDBC")
#c("SPY", "EFA", "EEM", "TLT", "IEF", "GLD", "DBC", "VNQ")
#FABER IVY PORTFOLIO: #c("SPY", "EFA", "IEF", "DBC", "VNQ")
#PAPA BEAR PORTFOLIO: #c("VUG", "VCLT", "EDV", "VIOV", "IAU", "VTV", "VEA", "VNQ", "VWO", "VIOG", "BNDX", "VGIT", "PDBC")
#INTERNATIONAL PORTFOLIO: #c("SPY", "IWM", "TLT", "SHY", "GLD", "EWJ", "EWU", "EWG", "EEM")
#momo_start <- as.Date("2002-01-01")

#Get adjusted closing prices in single xts
momo_env <- new.env()
getSymbols(momo_tickers, from = momo_start, env = momo_env)
momo_list <- eapply(momo_env, Ad)
momo_close <- do.call(merge, momo_list)

colnames(momo_close) <- sub(pattern = ".Adjusted", replacement = "", x = colnames(momo_close))

momo_close <- (momo_close[, momo_tickers])


####################### GET KEN FRENCH DATA ############

#Choose MONTHLY or DAILY data!

#Monthly 12 industries
french_indu <- read.csv("12_Industry_Portfolios.csv", skip = 11, nrow = 1132 - 12)
industries <-  xts(french_indu[,-1], order.by = as.Date(paste0(french_indu[,1], "01"), format = "%Y%m%d"))
industries <- industries / 100

#Daily 30 industries
french_indu_daily <- read.csv("30_Industry_Portfolios_Daily.csv", skip = 9, nrow = 24612)
industries <-  xts(french_indu_daily[1:24600,-1], order.by = as.Date(french_indu_daily[1:24600,1], format = "%Y%m%d"))
industries <- sapply(industries, as.numeric)
industries <-  xts(industries, order.by = as.Date(french_indu_daily[1:24600,1], format = "%Y%m%d"))
industries <- industries / 100


####################### SELECT DESIRED DATA ###########
momo_close <- industries

#Calculate PERIODIC returns and ROC
momo_roll <- 12
period <- "months"

momo_periodic <- to.period(momo_close, period = period, OHLC = FALSE)

momo_returns <- momo_close
#momo_returns <- xts(apply(momo_periodic, MAR = 2, Return.calculate), order.by = index(momo_periodic))

momo_roc <- rollapply(1 + momo_returns, width = momo_roll, align = "right", FUN = prod, na.rm = TRUE) - 1


######################## Equal Weight Portfolios #######

#Calculate EW portfolio of ETFs
all_port <- Return.portfolio(na.exclude(momo_returns), rebalance_on = "quarters")
colnames(all_port) <- "Equal_Weight"


####################### PLOT ###########################
#Set colors, dates, and dollar labels
plot_cols <- c(rainbow(n = ncol(momo_returns)), 1)

date_range <- seq(as.Date("1900-01-01"), as.Date("2020-01-01"), by = "10 years")
date_labels <- format(date_range, "%Y")

#dollar_range <- vector()
#for (i in 1:20) {
#  dollar_range <- [i + 1] ^ 2 
#}

dollar_range <- c(.3, 1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000, 1000000)
dollar_label <- c("$ 0.30", paste0("$", format(dollar_range[-1], big.mark = ",", scientific = FALSE, digits = 0)))

#dollar_range <- c(seq(0, 2, .25), seq(2.50, 4.5, .5), seq(5, 10, 1))
#dollar_label <- paste0("$", format(dollar_range, nsmall = 2, digits = 2))

percent_range <- seq(-.7, 2, .1)
percent_label <- paste0(format(percent_range * 100, digits = 0), "%")


#Plot all returns with EW portfolio
par(mfrow = c(1, 1), mar = c(4, 4, 4, 4), oma = c(0, 0, 0, 0))

plot.zoo(cumprod(1+merge(na.fill(momo_returns, 0), all_port, fill = 0)), plot.type = "single", col = plot_cols, lwd = c(rep(1, ncol(momo_returns)), 3), xlab = "", ylab = "", xaxt = "n", yaxt = "n", log = "y")
axis(side = 2, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 4, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = colnames(merge(momo_returns, all_port)), lwd = c(rep(2, ncol(momo_returns)), 3), col = plot_cols, cex = .75, ncol = 1)
title(main = "Growth of $1 in ETFs")


#Rolling 10-year Returns
plot.zoo(na.exclude(rollapply(1 + na.exclude(merge(momo_returns, all_port)), width = 12 * 10, align = "right", FUN = prod))^(1/10) - 1, plot.type = "single", col = plot_cols, lwd = c(rep(1, ncol(momo_returns)), 3), xlab = "", ylab = "", yaxt = "n", xaxt = "n")
#, xlim = c(as.Date("2008-01-01"), as.Date("2020-01-01")))
axis(side = 2, at = seq(-.5, 1.8, .1), labels = paste0(format(seq(-.5, 1.8, .1) * 100, digits = 0), "%"), las = 2)
axis(side = 4, at = seq(-.5, 1.8, .1), labels = paste0(format(seq(-.5, 1.8, .1) * 100, digits = 0), "%"), las = 2)
axis(side = 1, at = date_range, labels = date_labels)

abline(h = 0, lty = 3, lwd = 2, col = 2)

legend("topright", legend = c(colnames(momo_returns), "Eq Wt"), lwd = c(rep(2, ncol(momo_returns)), 3), col = plot_cols, ncol = 3, cex = .8)
title(main = "Rolling 3-year Return (annualized)")

#Plot histogram of ROC
plot.zoo(na.exclude(momo_roc), plot.type = "single", col = plot_cols, type = "h", lwd = seq(10, 4, length.out = ncol(momo_roc)), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = percent_range, labels = percent_label, las = 2)
axis(side = 4, at = percent_range, labels = percent_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = colnames(momo_roc), fill = plot_cols, cex = .75, ncol = 1)
title(main = "Rolling 1-year Return")

#Plot points of ROC
plot.zoo(na.exclude(momo_roc), plot.type = "single", type = "p", col = plot_cols, pch = 19, cex = 1, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
abline(h = 0, col = 2, lty = 3)
axis(side = 2, at = percent_range, labels = percent_label, las = 2)
axis(side = 4, at = percent_range, labels = percent_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = colnames(momo_roc), fill = plot_cols, cex = .8, ncol = 1)
title(main = "Rolling 1-year Return")

#Barplot of Calendar Yearly Return
momo_year <- to.period(momo_close, "years", OHLC = FALSE)[-1,]
yearly_return <- na.exclude(Return.calculate(momo_year))

par(mar = c(5, 4, 4, 4))

barplot(yearly_return, beside = TRUE, col = plot_cols[-length(plot_cols)], names.arg = format.Date(index(yearly_return), "%Y"), xlab = "", ylab = "", yaxt = "n", cex.names = .9)
abline(h = 0, col = 1, lty = 1)
abline(v = seq(1 + ncol(yearly_return) + .5, (1 + ncol(yearly_return)) * (nrow(yearly_return) - 1) + .5, 1 + ncol(yearly_return)), col = 1, lty = 1)

axis(side = 2, at = percent_range, labels = percent_label, las = 2)
axis(side = 4, at = percent_range, labels = percent_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

title(main = "Calendar Year Returns")

par(xpd = TRUE)
legend("bottom", legend = colnames(momo_roc), fill = plot_cols, cex = .8, ncol = ncol(momo_roc)/2, inset = -.15, bty = "n")
par(xpd = FALSE)

par(mar = c(4, 4, 4, 4))



####################### TABLE OF RETURNS ################
write.csv(rbind(table.AnnualizedReturns(merge(na.fill(momo_returns, 0), all_port, fill = 0)), maxDrawdown(merge(na.fill(momo_returns, 0), all_port, fill = 0))), "etf_perform.csv")

#DBC contribution
ew_port_det <- Return.portfolio(momo_returns, rebalance_on = "quarters", verbose = TRUE)
plot.zoo(ew_port_det$contribution$DBC, lwd = 2, col = 1, ylab = "", xlab = "")
abline(h = mean(ew_port_det$contribution$DBC), col = 1, lty = 2, lwd = 2)

title(main = "Contribution of DBC to an Equal-Weight Portfolio")

#Correlation of ETFs
write.csv(t(cor(na.exclude(momo_returns), na.exclude(all_port))), "etf_cor.csv")


####################### ORDER BY ROC ####################

#Date Index with NAs removed
date_index <- index(na.exclude(momo_roc))

#Order columns by best 1-year ROC to prior day
momo_split <- split.xts(momo_roc[paste0(first(date_index), "/")], f = "days")
momo_split <- lapply(momo_split, order, decreasing = TRUE)
momo_winner <- do.call(rbind, momo_split)

#Remove last row from winners to match rows in returns
momo_winner <- momo_winner[-nrow(momo_winner),]

#Set returns to same dates as ROC then remove 1st row to lag winners relative to returns
momo_return_test <- momo_returns[date_index]
momo_return_test <- momo_return_test[-1,]

#Collect ordered returns
momo_top <- data.frame()

for (j in 1:ncol(momo_return_test)) {
  for (i in 1:nrow(momo_return_test)) {
     momo_top[i, j] <- cbind(as.numeric(momo_return_test[i, momo_winner[i,j]]))
  }
}

#Name columns and convert to xts
colnames(momo_top) <- seq(1, ncol(momo_top), 1)
momo_zoo <- xts(momo_top, order.by = index(momo_return_test))

#Create a initial entry of 0 returns (starting date)
momo_first <- xts(t(data.frame(rep(0, ncol(momo_zoo)))), order.by = index(first(momo_zoo)) - 1,1) 
colnames(momo_first) <- colnames(momo_zoo)
momo_zoo <- rbind(momo_first, momo_zoo)

#Collect ordered names of ETFs
momo_etfs <- data.frame()

for (j in 1:ncol(momo_return_test)) {
  for (i in 1:nrow(momo_return_test)) {
     momo_etfs[i, j] <- colnames(momo_return_test[i, momo_winner[i,j]])
  }
}

#Create zoo of ETF names
momo_etfs_zoo <- xts(momo_etfs, order.by = index(momo_return_test))

#Calculate time in each ordered spot for ETFs
momo_time <- data.frame(row.names = 1:ncol(momo_return_test))
for (j in 1:ncol(momo_return_test)) {
  for (i in 1:ncol(momo_etfs)) {
    momo_time[j, i] <- length(which(momo_etfs[, j] == colnames(industries)[i])) / nrow(momo_etfs)
  }  
}  
colnames(momo_time) <- colnames(industries)#momo_tickers

momo_time_total <- colSums(momo_time[1:3,]) / 3

#length(which(momo_etfs[,1] == momo_tickers[1])) / nrow(momo_etfs)

#momo_return_test[100, as.character(momo_etfs[100, 1:3])]

#Current ordered list of past year's return
write.csv(momo_roc["2019-11-29", as.numeric(momo_winner[nrow(momo_winner)-1, 1:8])], "momo_roc.csv")

####################### PLOT ###########################

#Plot time of each ETF in top 3
barplot(momo_time_total, ylim = c(0, .12), yaxt = "n", col = 4, cex.names = .7)
axis(side = 2, at = seq(0, 1, .05), labels = paste0(seq(0, 1, .05) * 100, "%"), las = 2)

time_top_text <- data.frame(seq(.65, 14.1, length.out = ncol(momo_time)), momo_time_total)
for (i in 1:nrow(time_top_text)) {
  text(x = time_top_text[i, 1], y = time_top_text[i, 2], labels = paste0(round(time_top_text[i, 2] * 100, digits = 2), "%"), pos = 3)
}

title(main = "Percentage Exposure in the Top 3 Rank")

#Plot ETF time in each spot of ranking
barplot(t(momo_time), beside = FALSE, col = plot_cols, yaxt = "n", names.arg = seq(1, ncol(momo_time), 1))
axis(side = 2, at = seq(0, 1, .1), labels = paste0(seq(0, 1, .1) * 100, "%"), las = 2)


title(main = "Percentage of Months at each Rank")

par(xpd = TRUE)
legend("right", legend = rev(momo_tickers), fill = rev(plot_cols[-length(plot_cols)]), inset = -.12, cex = .9)
par(xpd = FALSE)

#Plot performance of ordered ETFs
par(mfrow = c(1, 1), mar = c(4, 6, 4, 6))

ordered_cols <- rainbow(n = ncol(momo_zoo))

plot.zoo(cumprod(1 + momo_zoo), plot.type = "single", col = ordered_cols, lwd = 2, xlab = "", ylab = "", xaxt = "n", yaxt = "n", log = "y")

axis(side = 2, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 4, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = seq(1, ncol(momo_zoo), 1), lwd = 3, col = ordered_cols)
title(main = "Growth of $1 in Portfolios Divided by Past Year's Return")

#XY plot of annualized returns and std dev for ordered strategies
ordered_perform <- table.AnnualizedReturns(momo_zoo)
xy_limits <- c(.18, .26, -.03, .13)

par(mar = c(4, 4, 4, 4), oma = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 2), byrow = TRUE))
 
plot(x = as.numeric(ordered_perform[2,]), y = as.numeric(ordered_perform[1,]), col = ordered_cols, pch = 19, cex = 4, xlab = "Annualized Standard Deviation", ylab = "Annualized Return", xaxt = "n", yaxt = "n")#, log = "xy")
#, xlim = xy_limits[1:2], ylim = [xy_limits[3:4]])
text(x = as.numeric(ordered_perform[2,]), y = as.numeric(ordered_perform[1,]), labels = seq(1, ncol(momo_zoo), 1), font = 2, col = 1, cex = 1.5)

axis(side = 1, at = seq(-.06, .26, .01), labels = paste0(seq(-.06, .26, .01) * 100, "%"))
axis(side = 2, at = seq(-.06, .26, .02), labels = paste0(seq(-.06, .26, .02) * 100, "%"))

title(main = "Performance of Industry Portfolios Ordered by Past Year's Return")

barplot(as.numeric(ordered_perform[3,]), col = 3, names.arg = seq(1, ncol(momo_top), 1), ylab = "Sharpe Ratio")

par(mfrow = c(1, 1))
####################### TOP 3 ###########################

#Calculate portfolios of top 1/3 and bottom 1/3 ETFs
momo_top_port <- Return.portfolio(na.exclude(momo_zoo[,1:3]), rebalance_on = "months")
#1:(ncol(momo_zoo)/3)]))
colnames(momo_top_port) <- "Best_Performers"

momo_bottom_port <- Return.portfolio(na.exclude(momo_zoo[,10:12]), rebalance_on = "months")
#(ncol(momo_zoo) * (2/3)):ncol(momo_zoo)]))
colnames(momo_bottom_port) <- "Worst_Performers"

#Calculate portfolio with weights set to top port times
weighted_port <- Return.portfolio(na.exclude(momo_zoo), weights = colSums(momo_time[1:3,]) / 3, rebalance_on = "months")
colnames(weighted_port) <- "Weighted"

#Plot performance of top three ETFs
par(mfrow = c(1, 1), mar = c(4, 6, 4, 6), oma = c(0, 0, 0, 0))

#Market Returns: momo_return_test$SPY, 

plot.zoo(cumprod(1 + merge(all_port, momo_top_port, momo_bottom_port, weighted_port, fill = 0)[index(na.exclude(momo_top_port))]), plot.type = "single", col = 1:4, lwd = 2, xlab = "", ylab = "", log = "y", xaxt = "n", yaxt = "n")
axis(side = 2, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 4, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = colnames(merge(all_port, momo_top_port, momo_bottom_port, weighted_port)), lwd = 3, col = 1:4)
title(main = "Growth of $1 in Different Momentum Strategies")


######################### TRADES ###########################
top_3_sorted <- t(apply(momo_etfs_zoo[,1:3], MARGIN = 1, FUN = sort))

#List all months with no change in first rank
top_3_sorted[which(top_3_sorted[,1] == lag(top_3_sorted[,1])),]

top_3_sorted[which(top_3_sorted[,1] == lag(top_3_sorted[,1]) | top_3_sorted[,1] == lag(top_3_sorted[,2], 1) | top_3_sorted[,1] == lag(top_3_sorted[,3], 1)),]



######################### TABLE TOP 3 ######################
trades_year <- 

write.csv(rbind(table.AnnualizedReturns(merge(all_port, momo_top_port, momo_bottom_port, weighted_port, fill = 0)[index(na.exclude(momo_top_port))]), maxDrawdown(merge(all_port, momo_top_port, momo_bottom_port, weighted_port, fill = 0)[index(na.exclude(momo_top_port))])), "best_worst_perform.csv")


######################### BEST or WORST MINUS EW ############
best_excess <- momo_top_port - all_port
worst_excess <- momo_bottom_port - all_port

plot.zoo(cumprod(1 + merge(best_excess, worst_excess)[index(na.exclude(momo_top_port))]), plot.type = "single", col = 2:3, lwd = 2, xlab = "", ylab = "", log = "y", xaxt = "n", yaxt = "n")
axis(side = 2, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 4, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = c("Best Minus Equal-Weight", "Worst Minus Equal-Weight"), lwd = 3, col = 2:3)
title(main = "Difference between Strategy and Equal-Weight Portfolio")


######################### MUSCULAR STRATEGY (PAPA BEAR) #####
#Order columns by average of 3, 6, 12 month ROC to prior day
momo_3m <- rollapply(1 + momo_returns, width = 3, align = "right", FUN = prod, na.rm = TRUE) - 1
momo_6m <- rollapply(1 + momo_returns, width = 6, align = "right", FUN = prod, na.rm = TRUE) - 1
momo_12m <- rollapply(1 + momo_returns, width = 12, align = "right", FUN = prod, na.rm = TRUE) - 1


momo_avg_roc <- data.frame()
for (j in 1:length(momo_tickers)) {
  for (i in 1:nrow(momo_3m)) {
    momo_avg_roc[i, momo_tickers[j]] <- mean(c(momo_3m[i, momo_tickers[j]], momo_6m[i, j], momo_roc[i, momo_tickers[j]]), na.rm = TRUE)
  }  
}
momo_avg_roc <- xts(momo_avg_roc, order.by = index(momo_3m))
momo_avg_roc <- momo_avg_roc[index(momo_return_test)]

momo_split_avg <- split.xts(momo_avg_roc, f = period)
momo_split_avg <- lapply(momo_split_avg, order, decreasing = TRUE)
momo_winner_avg <- do.call(rbind, momo_split_avg)



#Collect ordered returns
momo_winner_avg_lag <- momo_winner_avg[-nrow(momo_winner_avg),]

momo_return_test_lag <- momo_return_test[-1,]
momo_top_avg <- data.frame()

for (j in 1:ncol(momo_return_test_lag)) {
  for (i in 1:nrow(momo_return_test_lag)) {
     momo_top_avg[i, j] <- cbind(as.numeric(momo_return_test_lag[i, momo_winner_avg_lag[i ,j]]))
  }
}

#Name columns and convert to xts
colnames(momo_top_avg) <- seq(1, ncol(momo_top_avg), 1)
momo_zoo_avg <- xts(momo_top_avg, order.by = index(na.exclude(momo_return_test_lag)))

#Create a initial entry of 0 returns (starting date)
momo_zoo_avg <- rbind(momo_first, momo_zoo_avg)

#Collect ordered names of ETFs
momo_etfs_avg <- data.frame()

for (j in 1:ncol(momo_return_test_lag)) {
  for (i in 1:nrow(momo_return_test_lag)) {
     momo_etfs_avg[i, j] <- colnames(momo_return_test_lag[i, momo_winner_avg[i,j]])
  }
}

#Create zoo of ETF names
momo_etfs_zoo_avg <- xts(momo_etfs_avg, order.by = index(momo_return_test_lag))

#Calculate time in each ordered spot for ETFs
momo_time_avg <- data.frame(row.names = 1:ncol(momo_return_test))
for (j in 1:ncol(momo_return_test)) {
  for (i in 1:ncol(momo_etfs_avg)) {
    momo_time_avg[j, i] <- length(which(momo_etfs_avg[, j] == momo_tickers[i])) / nrow(momo_etfs_avg)
  }  
}  
colnames(momo_time_avg) <- momo_tickers

momo_time_total_avg <- colSums(momo_time_avg[1:3,]) / 3

########################### PLOT ##############################

#Plot time of each ETF in top 3
par(mfrow = c(1, 1))

barplot(momo_time_total_avg, ylim = c(0, .21), yaxt = "n", col = 4)
axis(side = 2, at = seq(0, 1, .05), labels = paste0(seq(0, 1, .05) * 100, "%"), las = 2)

time_top_text_avg <- data.frame(seq(.65, 9.1, length.out = ncol(momo_time_avg)), momo_time_total_avg)
for (i in 1:nrow(time_top_text_avg)) {
  text(x = time_top_text_avg[i, 1], y = time_top_text_avg[i, 2], labels = paste0(round(time_top_text_avg[i, 2] * 100, digits = 2), "%"), pos = 3)
}

title(main = "Percentage Exposure in the Top 3 Rank")

#Plot ETF time in each spot of ranking
barplot(t(momo_time_avg), beside = FALSE, col = plot_cols, yaxt = "n", names.arg = seq(1, ncol(momo_time_avg), 1))
axis(side = 2, at = seq(0, 1, .1), labels = paste0(seq(0, 1, .1) * 100, "%"), las = 2)


title(main = "Percentage of Months at each Rank")

par(xpd = TRUE)
legend("right", legend = rev(momo_tickers), fill = rev(plot_cols[-length(plot_cols)]), inset = -.12)
par(xpd = FALSE)

#Plot performance of ordered ETFs
ordered_cols <- rainbow(n = ncol(momo_zoo_avg))

plot.zoo(cumprod(1 + momo_zoo_avg), plot.type = "single", col = ordered_cols, lwd = 2, xlab = "", ylab = "", xaxt = "n", yaxt = "n", log = "y")

axis(side = 2, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 4, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = seq(1, ncol(momo_zoo_avg), 1), lwd = 3, col = ordered_cols)
title(main = "Growth of $1 in Portfolios Formed from Past 1-, 6-, 12-month Return")

#XY plot of annualized returns and std dev for ordered strategies
ordered_perform_avg <- table.AnnualizedReturns(momo_zoo_avg)
xy_limits <- c(.18, .26, -.03, .13)

par(mar = c(4, 4, 4, 4), oma = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 2), byrow = TRUE))
 
plot(x = as.numeric(ordered_perform_avg[2,]), y = as.numeric(ordered_perform_avg[1,]), col = ordered_cols, pch = 19, cex = 4, xlab = "Annualized Standard Deviation", ylab = "Annualized Return", xaxt = "n", yaxt = "n")#, log = "xy")
#, xlim = xy_limits[1:2], ylim = [xy_limits[3:4]])
text(x = as.numeric(ordered_perform_avg[2,]), y = as.numeric(ordered_perform_avg[1,]), labels = seq(1, ncol(momo_zoo_avg), 1), font = 2, col = 1, cex = 1.5)

abline(h = 0, col = 2, lty = 3)

axis(side = 1, at = seq(-.16, .32, .01), labels = paste0(seq(-.16, .32, .01) * 100, "%"))
axis(side = 2, at = seq(-.32, .64, .04), labels = paste0(seq(-.32, .64, .04) * 100, "%"))

title(main = "Performance of ETF Portfolios Ordered by Past Year's Return")

barplot(as.numeric(ordered_perform_avg[3,]), col = 3, names.arg = seq(1, ncol(momo_top), 1), ylab = "Sharpe Ratio")

par(mfrow = c(1, 1))

####################### TOP 3 ###########################

#Calculate portfolios of top 1/3 and bottom 1/3 ETFs
momo_top_port_avg <- Return.portfolio(na.exclude(momo_zoo_avg[,1:3]), rebalance_on = "months")
#1:(ncol(momo_zoo)/3)]))
colnames(momo_top_port_avg) <- "Best_Performers"

momo_bottom_port_avg <- Return.portfolio(na.exclude(momo_zoo_avg[,10:12]), rebalance_on = "months")
#(ncol(momo_zoo) * (2/3)):ncol(momo_zoo)]))
colnames(momo_bottom_port_avg) <- "Worst_Performers"

#Calculate portfolio with weights set to top port times
weighted_port_avg <- Return.portfolio(na.exclude(momo_zoo_avg), weights = colSums(momo_time_avg[1:3,]) / 3, rebalance_on = "months")
colnames(weighted_port_avg) <- "Weighted"

#Plot performance of top three ETFs
par(mfrow = c(1, 1), mar = c(4, 6, 4, 6), oma = c(0, 0, 0, 0))

plot.zoo(cumprod(1 + merge(all_port, momo_top_port_avg, momo_bottom_port_avg, weighted_port_avg, fill = 0)[index(na.exclude(momo_top_port_avg))]), plot.type = "single", col = 1:4, lwd = 2, xlab = "", ylab = "", log = "y", xaxt = "n", yaxt = "n")
axis(side = 2, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 4, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = colnames(merge(all_port, momo_top_port_avg, momo_bottom_port_avg, weighted_port_avg)), lwd = 3, col = 1:4)
title(main = "Growth of $1 in 1-, 3-, 12-month Momentum Strategies")


########################## 12-m vs. 1,3,12-m ###################
multi_vs_yr <- momo_top_port_avg - momo_top_port

plot.zoo(cumprod(1 + merge(multi_vs_yr)[index(na.exclude(momo_top_port_avg))]), plot.type = "single", col = 1:4, lwd = 2, xlab = "", ylab = "", log = "y") #, xaxt = "n", yaxt = "n")
#axis(side = 2, at = dollar_range, labels = dollar_label, las = 2)
#axis(side = 4, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

title(main = "Difference between Return of 1-, 3-, 12-month Strategy and 12-month Strategy")


######################## 10-yr Portfolio Returns ###################
#Create 4 ranked portfolios (from top to bottom)
mid_level <- merge(Return.portfolio(na.exclude(momo_zoo[,4:6]), rebalance_on = "months"), Return.portfolio(na.exclude(momo_zoo[,7:9]), rebalance_on = "months"))
quarters <- merge(all_port, momo_top_port_avg, mid_level, momo_bottom_port_avg)
colnames(quarters) <- c("Equal-Weight", "1-3", "4-6", "7-9", "10-12")

#Calculate rolling 10-year returns (annualized)
roll_return <- rollapply(1 + quarters, width = 10 * 12, align = "right", FUN = prod)^(1/10) - 1

#Calculate excess by rank


#Plot all four levels or only top and bottom
roll_percent_range <- seq(-.2, .4, .05)
roll_percent_label <- paste0(roll_percent_range * 100, "%")

par(mar = c(4, 4, 4, 4))

#plot.zoo(na.exclude(roll_return[,c(1, 2, 5)]), plot.type = "single", col = c(1, 2, 3), lwd = 2, xlab = "", ylab = "", xaxt = "n", yaxt = "n")

plot.zoo(na.exclude(roll_return), plot.type = "single", col = c(1, 2, 5, 6, 3), lwd = 2, xlab = "", ylab = "", xaxt = "n", yaxt = "n")

abline(h = 0, col = 2, lty = 3)

axis(side = 2, at = roll_percent_range, labels = roll_percent_label, las = 2)
axis(side = 4, at = roll_percent_range, labels = roll_percent_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = colnames(roll_return), lwd = 3, col = c(1, 2, 5, 6, 3), cex = .8)
title(main = "10-year Percentage Return by Past 12-month Return Rank (annualized)")

