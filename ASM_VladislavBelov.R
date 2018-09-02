library(ggplot2)
library(data.table)
library(MASS)
library(fitdistrplus)

# getwd()

## Reading the data
data.raw <- as.data.table(read.table("Data06/tuk.dat"))
data <- data.raw[, !c("V1"), with = FALSE]

names(data) <- c("per.brozek", "per.siri", "density", "age", "weight.total", "height", "adiposity", "weight.fatfree", 
                    "c.neck", "c.chest", "c.abdomen", "c.hip", "c.thigh", "c.knee", "c.ankle", "c.biceps", 
                    "c.forearm", "c.wrist")


## Numerical descriptive statistics
summary(data)

data[, age.factor := cut(data$age, breaks=c(22, 35, 50, 65, Inf), right = FALSE, 
                         labels = c("22-34", "35-49", "50-64", "65-81"))]

data[, weight.factor := cut(data$weight.total, breaks=c(100, 175, Inf), right = FALSE, 
                         labels = c("100-174", "175+"))]


## Graphical descriptive statistics
ggplot(data = data) +
  geom_histogram(mapping = aes(x = weight.total, fill = age.factor), alpha=0.75)

ggplot(data = data) +
  geom_point(mapping = aes(x = age, y = weight.total), alpha=0.75, size = 3.0) +
  xlab("Age, years") + 
  ylab("Total Weight, lbs")


## Data analysis
# Independence of total weight and age
age_weight.table <- table(data$age.factor, data$weight.factor)
chisq.test(age_weight.table)

normalized.age <- (data$age-min(data$age))/(max(data$age)-min(data$age))

# Distribution of the variable 'density'
descdist(data$density, discrete = FALSE)
density.fit.norm <- fitdist(data$density, distr = "norm", method = "mle")
density_fit_plot <- plot(density.fit.norm)


age.fit.norm <- fitdist(normalized.age, distr = "gamma", method = "mme")
age.fit.weibull <- fitdist(data$age, distr = "weibull", method = "mle", lower = c(0, 0))

