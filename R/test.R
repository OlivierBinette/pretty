
dat = ChickWeight
weight <- dat$weight
Time <- dat$Time

hist(weight, axes=T)
axis(2)

pos = par("usr")
b = boxplot(weight)
