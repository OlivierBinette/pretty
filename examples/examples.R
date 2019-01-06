
source("../prettyplot.R")

###############
# Scatterplots
###############

# PrettyPlot version
png("sp-pp.png", width=1000, height=800, res=150)
plot(cars)
dev.off()

# Base version
png("sp-base.png", width=1000, height=800, res=150)
graphics::plot(cars)
dev.off()

###############
# Histograms
###############

# PrettyPlot version
png("hist-pp.png", width=1000, height=800, res=150)
hist(rnorm(1000))
dev.off()

# Base version
png("hist-base.png", width=1000, height=800, res=150)
graphics::hist(rnorm(1000))
dev.off()


###########
# Boxplots
###########

# PrettyPlot version
png("bp-pp.png", width=1000, height=800, res=150)
with(ChickWeight,
     boxplot(weight ~ Time)
)
dev.off()

# Base version
png("bp-base.png", width=1000, height=800, res=150)
with(ChickWeight,
     graphics::boxplot(weight ~ Time)
)
dev.off()

################
# Labeling axes
################

png("labels.png", width=1000, height=800, res=150)
load("ozone.RData")
df = ozone[, c("O3", "T12", "Ne12", "Vx")]
m = apply(df, 2, mean)
with(df, 
     plot(O3 ~ T12, xmark=m["T12"], ymark=m["O3"])
)
axelines(m["T12"], m["O3"], col=2)
dev.off()

#######################
# Correlation matrices
#######################

png("cor.png", width=800, height=800, res=150)
load("ozone.RData")
df = ozone[, c("O3", "T12", "Ne12", "Vx")]
cor.im(df)
dev.off()


