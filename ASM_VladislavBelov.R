library(ggplot2)
library(data.table)
library(MASS)
library(fitdistrplus)
library(nortest)
library(goft)
library(GGally)
library(broom)
library(ggfortify)
library(car)


# getwd()


# Reading the data --------------------------------------------------------

data.raw <- as.data.table(read.table("Data06/tuk.dat"))
data <- data.raw[, !c("V1"), with = FALSE]

names(data) <- c("per.brozek", "per.siri", "density", "age", "weight.total", "height", "adiposity", "weight.fatfree", 
                    "c.neck", "c.chest", "c.abdomen", "c.hip", "c.thigh", "c.knee", "c.ankle", "c.biceps", 
                    "c.forearm", "c.wrist")

data[, weight.total := weight.total * 0.453592]
data[, weight.fatfree:= weight.fatfree * 0.453592]
data[, height := height * 2.54]

## Numerical descriptive statistics
summary(data)

data[, fat := weight.total - weight.fatfree]
data[, fat.factor := cut(data$fat, breaks=c(0, 10, 16, 21, Inf), right = FALSE, 
                         labels = c("0-9", "10-15", "16-20", "21+"))]
data[, weight.factor := cut(weight.total, breaks=c(50, 81, Inf), right = FALSE, 
                            labels = c("50-80", "81+"))]

data[, age.factor := cut(age, breaks=c(22, 35, 50, 65, Inf), right = FALSE, 
                         labels = c("22-34", "35-49", "50-64", "65-81"))]

data[, height.factor := cut(height, breaks=c(74, 178, Inf), right = FALSE, 
                            labels = c("74-177", "178+"))]

# Define a set of data dependent only on circumferences
data.c <- data[, c("c.neck", "c.chest", "c.abdomen", "c.hip", "c.thigh", "c.knee", "c.ankle", "c.biceps", 
                   "c.forearm", "c.wrist"), with = FALSE]


# Graphical descriptive statistics ----------------------------------------

# Body density histogram
ggplot(data = data) +
  geom_histogram(mapping = aes(x = density, fill = weight.factor), 
                 color = "black", size = 1.0, alpha=0.75, binwidth = 0.0065) + 
  labs(title = "Body Density Histogram", 
       x = "Density, [g/cm^3]", y = "Frequency",
       fill = "Total Weight, [kg]")

# Body weight vs. Siri
ggplot(alpha = 0.75) + 
  geom_point(data = data, mapping = aes(x = weight.total, y = per.siri, fill = fat.factor), 
             size = 3, pch = 21, alpha = 0.75) +
  labs(title = "Body Weight vs. Fat Percentage", 
       x = "Total Body Weight, [kg]", y = "Body Fat Percentage by Siri", 
       fill = "Amount of Fat, [kg]")

# Age vs. Fat Percentage
ggplot(data = data, aes(x = age.factor, y = per.siri)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4, notch = TRUE, fill = "coral2") + 
  labs(title = "Age vs. Fat Percentage", 
       x = "Age, [years]", y = "Body Fat Percentage by Siri", 
       fill = "Weight, [kg]")

# Body weight vs. density
ggplot(data = data) + 
  geom_point(mapping = aes(x = weight.total, y = density, fill = fat.factor), 
             size = 3, pch = 21, alpha = 0.75) + 
  labs(title = "Body Weight vs. Density", 
       x = "Total Body Weight, [kg]", y = "Density, [g/cm^3]", 
       fill = "Amount of Fat, [kg]")

# Fat-free body mass histogram
ggplot(data = data) +
  geom_histogram(mapping = aes(x = weight.fatfree), 
                 color = "black", size = 1.0, alpha=0.75) + 
  labs(title = "Fat-Free Weight Histogram", 
       x = "Fat-Free Weight, [kg]", y = "Frequency")

# Pairwise comparison of circumference data and its influence on Siri's doby fat percentage
ggpairs(cbind(data.c, data[, c("per.siri"), with = FALSE]), 
        columnLabels = c("Neck", "Chest", "Abdomen", "Hip", "Thigh", "Knee", "Ankle", "Biceps", 
                         "Forearm", "Wrist", "Siri"), title = "Pairwise Comparison of Circumference Data")


# Data analysis -----------------------------------------------------------

# Independence of total weight and age
age_weight.table <- table(data$age.factor, data$fat.factor)
chisq.test(age_weight.table)

normalized.age <- (data$age-min(data$age))/(max(data$age)-min(data$age))

# Distribution of the variable 'density'
descdist(data$density, discrete = FALSE)
density.fit.norm <- fitdist(data$density, distr = "norm", method = "mle")
density_fit_plot <- plot(density.fit.norm)
lillie.test(data$density)
shapiro.test(data$density)

# Distribution of the variable 'c.chest'
descdist(data$c.chest, discrete = FALSE)
chest.fit.lnorm <- fitdist(data$c.chest, distr = "lnorm", method = "mle")
chest_fit_plot <- plot(chest.fit.lnorm)
lnorm_test(data$c.chest)

# Distribution of the variable 'per.siri'
descdist(data$per.siri, discrete = FALSE)
siri.fit.norm <- fitdist(data$per.siri, distr = "norm", method = "mle")
siri_fit_plot <- plot(siri.fit.norm)
lillie.test(data$per.siri)
shapiro.test(data$per.siri)

# Distribution of the variable 'per.brozek'
descdist(data$per.brozek, discrete = FALSE)
brozek.fit.norm <- fitdist(data$per.brozek, distr = "norm", method = "mle")
brozek_fit_plot <- plot(brozek.fit.norm)
lillie.test(data$per.brozek)
shapiro.test(data$per.brozek)

# Distribution of the variable 'chest'
descdist(data$c.chest, discrete = FALSE)
fat.fit.lnorm <- fitdist(data$c.chest, distr = "lnorm", method = "mle")
fat_fit_plot <- plot(fat.fit.lnorm)
lnorm_test(data$c.chest)

# Comparative CDF plot for 'per.brozek' and 'per.siri'
ggplot(data = data) + 
  stat_ecdf(mapping = aes(per.brozek, color = "chocolate3"), geom = "step", size = 1.25) + 
  stat_ecdf(mapping = aes(per.siri, color="royalblue4"), geom = "step", size = 1.25) + 
  scale_colour_manual(name = 'Legend', 
                      values =c('chocolate3'='chocolate3','royalblue4'='royalblue4'), labels = c('Brozek CDF','Siri CDF'))
wilcox.test(data$per.brozek, data$per.siri, paired = FALSE, alternative = "two.sided")
ks.test(data$per.brozek, data$per.siri, paired = FALSE, alternative = "two.sided")


# Multivariate Linear Regression ------------------------------------------

data.regression <- cbind(data[, c("per.siri", "age", "weight.total", "height", "adiposity", "weight.fatfree"), with = FALSE],
                         data.c)

# Simple 2-variable linear regression
lm.simple <- lm (per.siri ~ c.abdomen + weight.total + 1, data = data.regression)
summary(lm.simple)
autoplot(lm.simple)
lillie.test(residuals(lm.simple))

new_data <- data.frame(cbind(sort(data.regression$c.abdomen), sort(data.regression$weight.total)))
names(new_data) <- list("c.abdomen", "weight.total")
lm.simple.conf <- predict(lm.simple, newdata = new_data, interval = "confidence")

ggplot(data.regression, aes(x=c.abdomen, y=per.siri)) +
  geom_point(size=1, alpha=0.7) +
  geom_line(aes(x=new_data$c.abdomen, y=lm.simple.conf[,1], colour = "Fit")) + 
  geom_line(aes(x=new_data$c.abdomen, y=lm.simple.conf[,2], colour = "Confidence Interval")) +
  geom_line(aes(x=new_data$c.abdomen, y=lm.simple.conf[,3], colour = "Confidence Interval"))
+
  labs(color="Lines:") + 
  geom_line(aes(x=new_data$c.abdomen, y=lm.simple.conf[,1], colour = "Fit")) + 
  geom_line(aes(x=new_data$c.abdomen, y=lm.simple.conf[,2], colour = "Confidence Interval")) + 
  geom_line(aes(x=new_data$c.abdomen, y=lm.simple.conf[,3], colour = "Confidence Interval")) + 
  theme_bw() +
  xlab("Nitrogen Oxides Concentration (parts per 10 million)") +
  ylab("Log - Mean Value of Owner-Occupied Homes") +
  ggtitle("Linear Model with Log-Transformation of the Dependant Variable (Tolerance - 5%)") +
  coord_cartesian(xlim=c(0.385, 0.8710), ylim=c(1.5, 4))








lm.all <- lm(per.siri ~ -1 + ., data = data.regression)
summary(lm.all)
stepAIC(lm.all)

lm.postaic <- lm(formula = per.siri ~ weight.total + adiposity + weight.fatfree + 
                   c.chest + c.abdomen + c.thigh + c.ankle + c.biceps + c.forearm - 
                   1, data = data.regression)
summary(lm.postaic)

plot(lm.all)
