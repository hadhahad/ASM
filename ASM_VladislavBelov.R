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
library(cowplot)
library(scatterplot3d)
library(zoo)


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
data$height[[42]] <- 176.349  # calculated using the adiposity index and the total weight

## Numerical descriptive statistics
summary(data)


# Auxiliary columns definition
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
       fill = "Total Weight, [kg]") + 
  theme_grey()


# Body weight vs. Siri
ggplot(alpha = 0.75) + 
  geom_point(data = data, mapping = aes(x = weight.total, y = per.siri, fill = fat.factor), 
             size = 3, pch = 21, alpha = 0.75) +
  labs(title = "Body Weight vs. Fat Percentage", 
       x = "Total Body Weight, [kg]", y = "Body Fat Percentage by Siri", 
       fill = "Amount of Fat, [kg]") + 
  theme_grey()


# Age vs. Fat Percentage
ggplot(data = data, aes(x = age.factor, y = per.siri)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4, notch = TRUE, fill = "coral2") + 
  labs(title = "Age vs. Fat Percentage", 
       x = "Age, [years]", y = "Body Fat Percentage by Siri", 
       fill = "Weight, [kg]") + 
  theme_grey()


# Body weight vs. density
ggplot(data = data) + 
  geom_point(mapping = aes(x = weight.total, y = density, fill = fat.factor), 
             size = 3, pch = 21, alpha = 0.75) + 
  labs(title = "Body Weight vs. Density", 
       x = "Total Body Weight, [kg]", y = "Density, [g/cm^3]", 
       fill = "Amount of Fat, [kg]") + 
  theme_grey()


# Fat-free body mass histogram
ggplot(data = data) +
  geom_histogram(mapping = aes(x = weight.fatfree), 
                 color = "black", size = 1.0, alpha=0.75) + 
  labs(title = "Fat-Free Weight Histogram", 
       x = "Fat-Free Weight, [kg]", y = "Frequency") + 
  theme_grey()


# Pairwise comparison of circumference data and its influence on Siri's doby fat percentage
ggpairs(cbind(data.c, data[, c("per.siri"), with = FALSE]), 
        columnLabels = c("Neck", "Chest", "Abdomen", "Hip", "Thigh", "Knee", "Ankle", "Biceps", 
                         "Forearm", "Wrist", "Siri"), 
        title = "Pairwise Comparison of Circumference Data") + 
  theme_grey()


# Boxplot for Siri's and Brozek's body fat percentage
temp_DT <- melt(data[, c("per.siri", "per.brozek"), with = FALSE], measure.vars = c("per.siri", "per.brozek"))
temp_DT[, variable := ifelse(variable == "per.siri", "Siri", "Brozek")]
ggplot(data = temp_DT, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill = variable), outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=TRUE) + 
  theme_grey() + 
  labs(title = "Siri's and Brozek's Fat Percentage", 
       x = "", y = "Fat Percentage", fill = "Equation Type")


# Comparative CDF plot for 'per.brozek' and 'per.siri'
ggplot(data = data) + 
  stat_ecdf(mapping = aes(per.brozek, color = "chocolate3"), geom = "step", size = 1.25) + 
  stat_ecdf(mapping = aes(per.siri, color="royalblue4"), geom = "step", size = 1.25) + 
  scale_colour_manual(name = 'Legend', 
                      values =c('chocolate3'='chocolate3','royalblue4'='royalblue4'), labels = c('Brozek CDF','Siri CDF')) + 
  theme_grey() + 
  labs(title = "Comparative CDF Plot", 
       x = "Values", y = "Probability")


# Data analysis -----------------------------------------------------------

# Distribution of the total body weight ('weight.total')
descdist(data$weight.total, discrete = FALSE)
weight.fit.gamma <- fitdist(data$weight.total, distr = "gamma", method = "mle")
weight.fit.norm <- fitdist(data$weight.total, distr = "norm", method = "mle")
params.gamma <- as.list(weight.fit.gamma$estimate)
params.norm <- as.list(weight.fit.norm$estimate)
plot(weight.fit.norm)
plot(weight.fit.gamma)
# the chi-squared goodness of fit test
h <- hist(data[weight.total < 150]$weight.total, breaks = 20)
p.gamma <- rollapply(pgamma(h$breaks, shape = 39.7315165, rate = 0.4895573), 2, function(x) x[2]-x[1])
p.norm <- rollapply(pnorm(h$breaks, mean = 81.15868, sd = 13.30421), 2, function(x) x[2]-x[1])
chisq.test(h$counts, p=p.gamma, rescale.p=TRUE, simulate.p.value=TRUE)
chisq.test(h$counts, p=p.norm, rescale.p=TRUE, simulate.p.value=TRUE)
confint(weight.fit.gamma)
# Closer look at the Q-Q plot
qq.weight.gamma <-ggplot(data, aes(sample = weight.total)) + 
  stat_qq(distribution = stats::qgamma, dparams = params.gamma, size = 1.5, alpha = 0.95) +
  stat_qq_line(distribution = stats::qgamma, dparams = params.gamma, size = 1.5, alpha = 0.85, color = "orange") + 
  theme_grey() + 
  labs(title = "Q-Q Plot for the Total Weight, Gamma", 
       x = "Theoretical", y = "Sample")
qq.weight.norm <-ggplot(data, aes(sample = weight.total)) + 
  stat_qq(distribution = stats::qnorm, dparams = params.norm, size = 1.5, alpha = 0.95) +
  stat_qq_line(distribution = stats::qnorm, dparams = params.norm, size = 1.5, alpha = 0.85, color = "royalblue3") + 
  theme_grey() + 
  labs(title = "Q-Q Plot for the Total Weight, Normal", 
       x = "Theoretical", y = "Sample")
plot_grid(qq.weight.gamma, qq.weight.norm, labels = "AUTO")


# Distribution of the variable 'density'
descdist(data$density, discrete = FALSE)
density.fit.norm <- fitdist(data$density, distr = "norm", method = "mle")
plot(density.fit.norm)
lillie.test(data$density)
shapiro.test(data$density)
confint(density.fit.norm)
ks.test(jitter(data$density), "pnorm", mean = 1.05557381, sd = 0.01899364)


# Distribution of the variable 'per.siri'
descdist(data$per.siri, discrete = FALSE)
siri.fit.norm <- fitdist(data$per.siri, distr = "norm", method = "mle")
plot(siri.fit.norm)
lillie.test(data$per.siri)
shapiro.test(data$per.siri)
confint(siri.fit.norm)
ks.test(jitter(data$per.siri), "pnorm", mean = 19.150794, sd = 7.622948)


# Distribution of the variable 'per.brozek'
descdist(data$per.brozek, discrete = FALSE)
brozek.fit.norm <- fitdist(data$per.brozek, distr = "norm", method = "mle")
plot(brozek.fit.norm)
lillie.test(data$per.brozek)
shapiro.test(data$per.brozek)
confint(brozek.fit.norm)
ks.test(jitter(data$per.brozek), "pnorm", mean = 18.938492, sd = 7.735462)


# Distribution of the variable 'chest'
descdist(data$c.chest, discrete = FALSE)
fat.fit.lnorm <- fitdist(data$c.chest, distr = "lnorm", method = "mle")
fat_fit_plot <- plot(fat.fit.lnorm)
lnorm_test(data$c.chest)  # rejected, but the fit is quite good


# Distribution equality for 'per.siri' and 'p.brozek'
wilcox.test(data$per.brozek, data$per.siri, paired = FALSE, alternative = "two.sided")
ks.test(jitter(data$per.siri), jitter(data$per.brozek), paired = FALSE, alternative = "two.sided")
t.test(data$per.siri, data$per.brozek, var.equal = TRUE)


# Multivariate Linear Regression ------------------------------------------

data.regression <- cbind(data[, c("per.siri", "age", "weight.total", "height", "adiposity", "weight.fatfree"), with = FALSE],
                         data.c)[weight.total <150]


# Simple 2-variable linear regression
lm.simple <- lm (per.siri ~ c.abdomen + weight.total + 1, data = data.regression)
summary(lm.simple)
summary(lm.simple)$coefficients # to obtain precise p-values
autoplot(lm.simple) + theme_grey()
confint(lm.simple, level = 0.95)
lillie.test(residuals(lm.simple))
s3d <-scatterplot3d(data.regression$c.abdomen, data.regression$weight.total, data.regression$per.siri, pch=16, highlight.3d=TRUE,
                    type="h", box=FALSE, main="2-Variable Linear Regression", 
                    xlab = "Abdomen CC, [cm]",
                    ylab = "Total Weight, [kg]", 
                    zlab = "Siri")
s3d$plane3d(lm.simple)


# All variables included (except for Brozek's percentage and density)
lm.all <- lm(per.siri ~ -1 + ., data = data.regression)
summary(lm.all)
summary(lm.all)$coefficients
confint(lm.all, level = 0.95)
autoplot(lm.all) + theme_grey()
lillie.test(residuals(lm.all))


# Propose a better model
cor(data.regression$weight.total, data.regression$weight.fatfree) # High correlation
stepAIC(update(lm.all, . ~ . - weight.total))
lm.postaic <- lm(formula = per.siri ~ age + height + weight.fatfree + c.abdomen + 
                   c.knee + c.biceps + c.wrist - 1, data = data.regression)

summary(lm.postaic)
summary(lm.postaic)$coefficients
confint(lm.postaic, level = 0.95)
autoplot(lm.postaic) + theme_grey()
lillie.test(residuals(lm.postaic))
