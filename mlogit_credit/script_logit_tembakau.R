library(mlogit)

H <- dfidx(Heating, choice = "depvar", varying = c(3:12))

t <- mlogit(SOURCES~LOAN+OP_COST|0, data=df)
summary(t)