####################### LIBRARY #########################
library(PerformanceAnalytics)
library(quantmod)


####################### GET DATA ########################
#Set ETFs, starting date, and momentum lookback period
momo_tickers <- c("SPY", "EFA", "EEM", "TLT", "IEF", "GLD", "DBC", "VNQ")
#FABER IVY PORTFOLIO: #c("SPY", "EFA", "IEF", "DBC", "VNQ")
#PAPA BEAR PORTFOLIO: #c("VUG", "VCLT", "EDV", "VIOV", "IAU", "VTV", "VEA", "VNQ", "VWO", "VIOG", "BNDX", "VGIT", "PDBC")
#INTERNATIONAL HEAVY PORTFOLIO: #c("SPY", "IWM", "TLT", "SHY", "GLD", "EWJ", "EWU", "EWG", "EEM")
momo_start <- as.Date("2002-01-01")
momo_roll <- 21 * 12

#Get closing prices in single xts
momo_env <- new.env()
getSymbols(momo_tickers, from = momo_start, env = momo_env)
momo_list <- eapply(momo_env, Ad)
momo_close <- do.call(merge, momo_list)

#Calculate returns and ROC (and SD)
momo_returns <- xts(apply(momo_close, MAR = 2, Return.calculate), order.by = index(momo_close))
momo_returns <- momo_returns[,c(7, 8, 5, 4, 6, 3, 2, 1)]
#FABER IVY: #[,c(4, 5, 3, 2, 1)]
#PAPA BEAR: #[,c(9, 3, 10, 12, 5, 8, 7, 1, 13, 4, 2, 11, 6)]
#Original TEST: #[, c(9, 3, 4, 5, 2, 7, 1, 6, 8)]
colnames(momo_returns) <- momo_tickers
momo_roc <- rollapply(1 + momo_returns, width = momo_roll, align = "right", FUN = prod, na.rm = TRUE) - 1
#momo_sd <- ((1 + rollapply(momo_returns, width = 21, align = "right", FUN = sd, na.rm = TRUE))^12 - 1) * 100

#Calculate EW portfolio of ETFs
all_port <- Return.portfolio(na.exclude(momo_returns), rebalance_on = "quarters")
colnames(all_port) <- "Equal_Weight"


####################### PLOT ###########################

#Plot all returns with EW portfolio
plot_cols <- c(rainbow(n = ncol(momo_returns)), 1)

date_range <- seq(as.Date("1990-01-01"), as.Date("2020-01-01"), by = "1 years")
date_labels <- format(date_range, "%Y")

par(mfrow = c(1, 1), mar = c(4, 4, 4, 4), oma = c(0, 0, 0, 0))

plot.zoo(cumprod(1+merge(na.exclude(momo_returns), all_port)), plot.type = "single", col = plot_cols, lwd = c(rep(1, ncol(momo_returns)), 3), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
#, xlim = c(as.Date("2005-01-01"), as.Date("2021-01-01")))
axis(side = 2, at = seq(-.5, 5, .25), labels = paste0("$", format(seq(-.5, 5, .25), nsmall = 2, digits = 2)), las = 2)
axis(side = 4, at = seq(-.5, 5, .25), labels = paste0("$", format(seq(-.5, 5, .25), nsmall = 2, digits = 2)), las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = colnames(merge(momo_returns, all_port)), lwd = c(rep(2, ncol(momo_returns)), 3), col = plot_cols)
title(main = "Growth of $1 in ETFs")

#text(x = index(last(momo_returns)), y = last(cumprod(1 + na.exclude(momo_returns))), labels = colnames(momo_returns), pos = 4, col = plot_cols)

#Rolling 3-year Returns
plot.zoo(na.exclude(rollapply(1 + na.exclude(merge(momo_returns, all_port)), width = 252 * 3, align = "right", FUN = prod))^(1/3) - 1, plot.type = "single", col = plot_cols, lwd = c(rep(1, ncol(momo_returns)), 3), xlab = "", ylab = "", yaxt = "n", xaxt = "n")
#, xlim = c(as.Date("2008-01-01"), as.Date("2020-01-01")))
axis(side = 2, at = seq(-.5, 1.8, .1), labels = paste0(format(seq(-.5, 1.8, .1) * 100, digits = 0), "%"), las = 2)
axis(side = 4, at = seq(-.5, 1.8, .1), labels = paste0(format(seq(-.5, 1.8, .1) * 100, digits = 0), "%"), las = 2)
axis(side = 1, at = date_range, labels = date_labels)

abline(h = 0, lty = 3, lwd = 2, col = 2)

legend("top", legend = c(colnames(momo_returns), "Eq Wt"), lwd = c(rep(2, ncol(momo_returns)), 3), col = plot_cols, ncol = 5, cex = .8)
title(main = "Rolling 3-year Return (annualized)")


####################### ORDER BY ROC ####################

#Date Index with NAs removed
date_index <- index(na.exclude(momo_return_test))

#Order columns by best 1-year ROC to prior day
momo_split <- split.xts(momo_roc[paste0(first(date_index), "/")], f = "days")
momo_split <- lapply(momo_split, order)
momo_winner <- do.call(rbind, momo_split)

momo_return_test <- momo_returns[paste0(first(date_index) + 1, "/")]

#Collect ordered returns
momo_top <- data.frame()

for (j in 1:ncol(momo_return_test)) {
  for (i in 1:nrow(momo_return_test)) {
     momo_top[i, j] <- cbind(as.numeric(momo_return_test[i, momo_winner[i,j]]))
  }
}

colnames(momo_top) <- seq(1, ncol(momo_top), 1)
momo_zoo <- xts(momo_top, order.by = index(momo_return_test))


####################### PLOT ###########################

#Plot performance of ordered ETFs
ordered_cols <- rainbow(n = ncol(momo_zoo))
dollar_range <- seq(-.5, 8, .25)
dollar_label <- paste0("$", format(dollar_range, nsmall = 2, digits = 2))

plot.zoo(cumprod(1+momo_zoo), plot.type = "single", col = ordered_cols, lwd = 2, xlab = "", ylab = "", xaxt = "n", yaxt = "n", log = "y")

axis(side = 2, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 4, at = dollar_range, labels = dollar_label, las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = seq(1, ncol(momo_zoo), 1), lwd = 3, col = ordered_cols)
title(main = "Growth of $1 in Divided by Past Year's Return")

#XY plot of annualized returns and std dev for ordered strategies
ordered_perform <- table.AnnualizedReturns(momo_zoo)
xy_limits <- c(.18, .26, -.03, .13)

par(mar = c(4, 4, 4, 4), oma = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 2), byrow = TRUE))
 
plot(x = as.numeric(ordered_perform[2,]), y = as.numeric(ordered_perform[1,]), col = ordered_cols, pch = 19, cex = 4, xlab = "Annualized Standard Deviation", ylab = "Annualized Return", xaxt = "n", yaxt = "n")
#, xlim = xy_limits[1:2], ylim = [xy_limits[3:4]])
text(x = as.numeric(ordered_perform[2,]), y = as.numeric(ordered_perform[1,]), labels = seq(1, ncol(momo_zoo), 1), font = 2, col = 1, cex = 1.5)

axis(side = 1, at = seq(-.06, .26, .02), labels = paste0(seq(-.06, .26, .02) * 100, "%"))
axis(side = 2, at = seq(-.06, .26, .02), labels = paste0(seq(-.06, .26, .02) * 100, "%"))

title(main = "Performance of ETF Portfolios Ordered by Past Year's Return")

barplot(as.numeric(ordered_perform[3,]), col = 3, names.arg = seq(1, ncol(momo_top), 1), ylab = "Sharpe Ratio")


####################### TOP 3 ###########################

#Calculate portfolios of top 1/3 and bottom 1/3 ETFs
momo_top_port <- Return.portfolio(na.exclude(momo_zoo[, 1:(ncol(momo_zoo)/3)]))
colnames(momo_top_port) <- "Best_Performers"

momo_bottom_port <- Return.portfolio(na.exclude(momo_zoo[,(ncol(momo_zoo) * (2/3)):ncol(momo_zoo)])) #, rebalance_on = "quarters")
colnames(momo_bottom_port) <- "Worst_Performers"

#Plot performance of top three ETFs
par(mfrow = c(1, 1), mar = c(4, 4, 4, 4), oma = c(0, 0, 0, 0))

#Market Returns: momo_return_test$SPY, 

plot.zoo(cumprod(1 + merge(momo_return_test$SPY,all_port, momo_top_port, momo_bottom_port)[index(na.exclude(momo_top_port))]), plot.type = "single", col = 1:4, lwd = 2, xlab = "", ylab = "", xaxt = "n", yaxt = "n", log = "y")
axis(side = 2, at = seq(-.5, 5, .25), labels = paste0("$", format(seq(-.5, 5, .25), nsmall = 2, digits = 2)), las = 2)
axis(side = 4, at = seq(-.5, 5, .25), labels = paste0("$", format(seq(-.5, 5, .25), nsmall = 2, digits = 2)), las = 2)
axis(side = 1, at = date_range, labels = date_labels)

legend("topleft", legend = colnames(merge(momo_return_test$SPY, all_port, momo_top_port, momo_bottom_port)), lwd = 3, col = 1:4)
title(main = "Growth of $1 in Different Momentum Strategies")
