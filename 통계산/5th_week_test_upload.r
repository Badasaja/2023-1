CON <- c(16, 18, 19, 21, 24, 20)
COF <- c(10, 13, 10, 8, 12, 13)
VON <- c(21, 23, 19, 26, 22, 24)
VOF <- c(12, 16, 13, 14, 16, 13)

raw_dat <-c(CON, COF, VON, VOF)
raw_dat

a <- rep(6,4)
a
group <- rep(c('CON','COF','VON','VOF'), a)
group
group_df <- data.frame(raw_dat, group)
group_df
group_df <- transform(group_df, group = factor(group)) 
sapply(group_df, class)  
aov(raw_dat~group,data = group_df)
