#' ---
#' Title: "Environmental Predictors of *Triturus cristatus* Data Analysis"
#' Author: "Lynsey Rebecca Harper"
#' Date: "7th December 2016"
#' ---
#' 
#' 
#' ## Analysis of Environmental Factors
#' 
#' In 2015, Gartcosh Nature Reserve (GNR), North Lanarkshire, Scotland, was
#' surveyed for great crested newt (GCN) by torchlight to obtain adult
#' counts as an estimate of population size and determine the status
#' of the population following translocation to the reserve 10 years 
#' earlier.
#' 
#' Environmental factors were recorded in addition to GCN adult counts 
#' in GNR. These were: air temperature, water temperature, pH, 
#' conductivity, total dissolved solids, water clarity,
#' rainfall, wind, bright moonlight and HSI score.
#' 
#' 

## Clear R's memory
rm(list=ls())

## Check working directory
getwd()

## Set working directory
setwd("/Users/Lynsey/Documents/PredictorsPaper")

## Check working directory set properly
getwd()

## Load required packages
p <- c("ggplot2","grid","gridExtra","lme4","glmmADMB","MASS","car","scales",
       "AICcmodavg","gtools","xlsx","reshape2","dplyr","plyr","arm",
       "RVAideMemoire","ResourceSelection","bbmle","RColorBrewer", 
       "MuMIn","rpart","mgcv","ncf","glmmML","LMERConvenienceFunctions",
       "ggmap","ggsn","coin")
new.packages <- p[!(p %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://star-www.st-andrews.ac.uk/cran/")
lapply(p, require, character.only = TRUE)

## Load custom functions
f <- c("CheckResidsFunction.R","OverdispersalFunction.R",
       "CheckConvergenceFunction.R", "HighstatLibV6.R",
       "MyLibrary.R", "glmmML_pres_function.R",
       "ggplotLegendFunction.R", "grid_arrange_shared_legend_Function.R")
lapply(f, source)


#' ---
#' 
#' ## Map of study site
#' 

## Load data containing coordinates and peak GCN counts for ponds 
HSI.dat <- read.csv("HSI.csv", header = TRUE)
attach(HSI.dat)

## Check the points plot ok with a ggplot
gtest <- ggplot(HSI.dat, aes(x=Longitude,y=Latitude)) + geom_point(aes(size=Peak_GCN_2015))
gtest

## Now we want to apply them to a map. First get the map by downloading
## the Google road map for Gartcosh. The code below will fetch the
## map from Google, centred on a specific latitude and longitude.
## A road map is just one option - google if you want a different 
## option.
devtools::install_github("dkahle/ggmap")
library(ggmap)
Gartcosh <- get_map(location = c(lon = -4.0713087, lat = 55.889608), 
                    zoom = 15, maptype = 'roadmap')
ggmap(Gartcosh)

## Now plot the map.
## Here, there are points at each site, sized by peak counts (using 
## scale_shape for hollow/filled circles)
g1 <- ggmap(Gartcosh, extent = "device", crop = T)
g1 <- g1 + geom_point(data = HSI.dat, colour="black", fill="black", 
                      aes(x=Longitude, y=Latitude), size = 1)
g1 <- g1 + theme(panel.border = element_rect(colour = "black", fill=NA, size=2))
g1 <- g1 + scalebar(x.min = -4.085, x.max = -4.060,
                    y.min = 55.8825, y.max = 55.894,
                    dd2km = TRUE, model = 'WGS84', 
                    location = "bottomright", dist = 0.25, height = 0.02,
                    st.dist = 0.02, st.bottom = FALSE, st.size = 5)
g1 <- g1 + annotate("text", x = -4.0717649, y = 55.891342, label = "BB1", cex=5)
g1 <- g1 + annotate("text", x = -4.0709496, y = 55.890981, label = "BB2", cex=5)
g1 <- g1 + annotate("text", x = -4.0700912, y = 55.891282, label = "BB3", cex=5)
g1 <- g1 + annotate("text", x = -4.0693831, y = 55.891715, label = "BB4", cex=5)
g1 <- g1 + annotate("text", x = -4.0686536, y = 55.891282, label = "BB5", cex=5)
g1 <- g1 + annotate("text", x = -4.0676236, y = 55.891883, label = "BB6", cex=5)
g1 <- g1 + annotate("text", x = -4.0689754, y = 55.892292, label = "BB7", cex=5)
g1 <- g1 + annotate("text", x = -4.0675163, y = 55.892292, label = "BB8", cex=5)
g1 <- g1 + annotate("text", x = -4.0662074, y = 55.891606, label = "SS1", cex=5)
g1 <- g1 + annotate("text", x = -4.0645981, y = 55.891727, label = "SS2", cex=5)
g1 <- g1 + annotate("text", x = -4.0656710, y = 55.891269, label = "SS3", cex=5)
g1 <- g1 + annotate("text", x = -4.0620017, y = 55.889621, label = "GQ1", cex=5)
g1 <- g1 + annotate("text", x = -4.0620232, y = 55.889116, label = "GQ2", cex=5)
g1 <- g1 + annotate("text", x = -4.0598130, y = 55.890126, label = "GQ3", cex=5)
g1 <- g1 + annotate("text", x = -4.0610147, y = 55.890018, label = "GQ4", cex=5)
g1 <- g1 + annotate("text", x = -4.0609288, y = 55.889332, label = "GQ5", cex=5)
g1 <- g1 + annotate("text", x = -4.0598774, y = 55.889308, label = "GQ6", cex=5)
g1 <- g1 + annotate("text", x = -4.0592122, y = 55.889645, label = "GQ7", cex=5)
g1 <- g1 + annotate("text", x = -4.0623879, y = 55.885241, label = "RJ1", cex=5)
g1 <- g1 + annotate("text", x = -4.0624952, y = 55.884904, label = "RJ2", cex=5)
g1 <- g1 + annotate("text", x = -4.0628171, y = 55.884651, label = "RJ3", cex=5)
g1 <- g1 + annotate("text", x = -4.0638041, y = 55.885072, label = "RJ4", cex=5)
g1 <- g1 + annotate("text", x = -4.0642655, y = 55.884843, label = "RJ5", cex=5)
g1 <- g1 + annotate("text", x = -4.0643942, y = 55.884549, label = "RJ6", cex=5)
g1


#' ---
#' 
#' ## Comparison of HSI scores
#' 
#'
#' Now, compare HSI scores from 2006 to HSI scores from 2015 to see
#' whether GNR ponds have improved or worsened in terms of habitat
#' suitability for GCN. Following translocation in 2006, amphibian 
#' fencing prevented dispersal of GCN between zones of GNR. This was
#' removed in May 2011 and so opportunity for dispersal and habitat
#' connectivity may have improved in the period that followed.
#' 
#' We are comparing the same set of ponds, which have been measured
#' twice, before and after amphibian fencing was removed. Therefore,
#' a paired-sample t-test is the most appropriate statistic.
#'

## Find range and median of HSI scores: 2006, 2015 and best-case
range(HSI_2006)
median(HSI_2006, na.rm = FALSE)

range(HSI_2015)
median(HSI_2015, na.rm = FALSE)

range(Best_Case)
median(Best_Case, na.rm = FALSE)

## Derive the difference in HSI score for each pond between 2006 and
## 2015
HSI.dat$Difference <- HSI.dat$HSI_2015-HSI.dat$HSI_2006

## Inspect data
HSI.dat

## Now check means and difference of means
mean(HSI_2006)
mean(HSI_2015)
mean(HSI_2015)-mean(HSI_2006)

## The mean HSI in 2015 is higher which suggests the null hypothesis 
## of no difference in HSI score following removal of fencing may be 
## rejected.

## The sample size (N=24) is small, therefore the distribution of the 
## differences should be approximately normal. Check using a boxplot 
## and QQ plot. 
boxplot(HSI.dat$Difference)
qqnorm(HSI.dat$Difference)
qqline(HSI.dat$Difference)

## There is some skew in the data indicating a non-normal distribution.
## Test data for normality using Shapiro-Wilk test.
shapiro.test(HSI.dat$Difference)

## The p-value is highly significant thus the alternative hypothesis,
## the data are not normally distributed is accepted.
## As the data are not normally distributed, the assumptions of the
## t-test are violated. The non-parametric equivalent of the paired
## sample t-test, the Wilcoxon Matched Pairs Signed Ranks Test, must
## be used instead. This approach has ~95% power of the t-test.

## Perform Wilcoxon Matched Pairs Signed Ranks Test to determine
## whether the mean of the HSI scores from 2006 is less than the
## mean of HSI scores from 2015.
wilcox.test(HSI_2006, HSI_2015, paired=TRUE)

## From this result, the statistic to be used for a Wilcoxon test is 53.5. 
## It is usually denoted as “W”, but in R, it is presented as “V”. 
## However, the exact p value cannot be calculated because of ties. 
## To address this, we need to use the coin package and do another
## Wilcoxon test.
wilcoxsign_test(HSI_2006 ~ HSI_2015, distribution="exact")

## The p-value is significant therefore there has been substantial 
## improvement in HSI scores since translocation in 2006 and fencing 
## removed.

## Calculate the effect size
-2.3174/sqrt(48)

## Repeat analyses for comparison of HSI score in 2015 with best case
## scenario predicted by McNeill (2010). Derive the difference in HSI 
## score for each pond in 2015 and best case scenario.
HSI.dat$DifferenceBC <- HSI.dat$Best_Case-HSI.dat$HSI_2015

## Inspect data
HSI.dat

## Now check means and difference of means
mean(HSI_2015)
mean(Best_Case)
mean(Best_Case)-mean(HSI_2015)

## Calculate standard deviation for each HSI score
sd(Best_Case)
sd(HSI_2015)

## The best case scenario average is only marginally higher therefore
## the null hypothesis may not be rejected.

## The sample size (N=24) is small, therefore the distribution of the 
## differences should be approximately normal. Check using a boxplot 
## and QQ plot. 
boxplot(HSI.dat$DifferenceBC)
qqnorm(HSI.dat$DifferenceBC)
qqline(HSI.dat$DifferenceBC)

## There is some skew in the data which may indicate a non-normal
## distribution. Test data for normality using Shapiro-Wilk test:
shapiro.test(HSI.dat$DifferenceBC)

## The p-value is not significiant thus the null hypothesis,
## the data are normally distributed, is accepted.
## As the data are normally distributed, the assumptions of the
## t-test are met and the paired t-test can be applied to the data.

## Perform paired t-test to determine whether the mean of the HSI 
## scores from 2015 is less than the mean of the best case scenario 
## scores predicted by McNeill (2010).
t.test(HSI_2015, Best_Case, paired = TRUE, alt = "less")

## p-value significant therefore HSI scores in 2015 significantly lower
## than best-case HSI scores following removal of fencing predicted by
## McNeill (2010)


#'
#' Repeat analysis without RJ5 and RJ6 ponds which were dry in 2006
#' and thus should potentially be excluded.
#' 

## Create new data frame without RJ5 and RJ6
new.df <- HSI.dat[-c(20:21),-c(11:12)]

## Find range and median of HSI scores: 2006, 2015 and best-case
range(new.df$HSI_2006)
median(new.df$HSI_2006, na.rm = FALSE)

range(new.df$HSI_2015)
median(new.df$HSI_2015, na.rm = FALSE)

range(new.df$Best_Case)
median(new.df$Best_Case, na.rm = FALSE)

## Derive the difference in HSI score for each pond between 2006 and
## 2015
new.df$Difference <- new.df$HSI_2015-new.df$HSI_2006

## Inspect data
new.df

## Now check means and difference of means
mean(new.df$HSI_2006)
mean(new.df$HSI_2015)
mean(new.df$HSI_2015)-mean(new.df$HSI_2006)

## The average in 2015 is higher which suggests the null hypothesis 
## of no difference in HSI score following removal of fencing may be 
## rejected

## The sample size (N=24) is small, therefore the distribution of the 
## differences should be approximately normal. Check using a boxplot 
## and QQ plot. 
boxplot(new.df$Difference)
qqnorm(new.df$Difference)
qqline(new.df$Difference)

## There is some skew in the data indicated a non-normal distribution. 
## Test data for normality using Shapiro-Wilk test:
shapiro.test(new.df$Difference)

## The p-value is highly significant thus the alternative hypothesis,
## the data are not normally distributed is accepted.
## As the data are not normally distributed, the assumptions of the
## t-test are violated. The non-parametric equivalent of the paired
## sample t-test, the Wilcoxon Matched Pairs Signed Ranks Test, must
## be used instead. This approach has ~95% power of the t-test.

## Perform Wilcoxon Matched Pairs Signed Ranks Test to determine
## whether the mean of the HSI scores from 2006 is less than the
## mean of HSI scores from 2015.
wilcox.test(new.df$HSI_2006, new.df$HSI_2015, paired=TRUE)

## From this result, the statistic to be used for a Wilcoxon test is 53.5. 
## It is usually denoted as “W”, but in R, it is presented as “V”. 
## However, the exact p value cannot be calculated because of ties. 
## To address this, we need to use the coin package and do another
## Wilcoxon test.
wilcoxsign_test(new.df$HSI_2006 ~ new.df$HSI_2015, distribution="exact")

## The p-value is not significant therefore there has been no substantial 
## improvement in HSI scores since translocation in 2006 and fencing 
## removed.

## Calculate the effect size
-1.87/sqrt(48)

## Repeat analyses for comparison of HSI score in 2015 with best case
## scenario predicted by McNeill (2010).
## Derive the difference in HSI score for each pond in 2015 and
## best case scenario
new.df$DifferenceBC <- new.df$Best_Case-new.df$HSI_2015

## Inspect data
new.df

## Now check means and difference of means
mean(new.df$HSI_2015)
mean(new.df$Best_Case)
mean(new.df$Best_Case)-mean(new.df$HSI_2015)

## The best case scenario average is only marginally higher therefore
## the null hypothesis may not be rejected.

## Calculate standard deviation for each HSI score
sd(Best_Case)
sd(HSI_2015)

## The sample size (N=24) is small, therefore the distribution of the 
## differences should be approximately normal. Check using a boxplot 
## and QQ plot. 
boxplot(new.df$DifferenceBC)
qqnorm(new.df$DifferenceBC)
qqline(new.df$DifferenceBC)

## There is some skew in the data indicating a non-normal distribution. 
## Test data for normality using Shapiro-Wilk test:
shapiro.test(new.df$DifferenceBC)

## The p-value is not significiant thus the null hypothesis,
## the data are normally distributed cannot be rejected.
## As the data are normally distributed, the assumptions of the
## t-test are met and the paired t-test can be applied to the data.

## Perform paired t-test to determine whether the mean of the HSI 
## scores from 2015 is less than the mean of the best case scenario 
## scores predicted by McNeill (2010).
t.test(new.df$HSI_2015, new.df$Best_Case, paired = TRUE, alt = "less")

## p-value significant therefore HSI scores in 2015 significantly lower
## than best-case HSI scores following removal of fencing predicted by
## McNeill (2010)



#' ---
#' 
#' ## HSI Score
#' 
#' HSI score is broadly cited in the literature as having a positive linear 
#' relationship with great crested newt counts. I will analyse this variable 
#' separately using a GLM. 
#' 
#' Typically, ARG-UK uses peak adult counts from torchlight survey to indicate 
#' population trends. However, these can be statistical outliers. For example, 
#' if a survey at a particular pond happens to be conducted on a 'good' night, 
#' then a spike in abundance may be seen. If other ponds are surveyed on poor
#' nights, then the peak count will be lower. Therefore, it may be better to 
#' use the average GCN adult count over the five survey visits.
#' 
#' The peak and average GCN counts obtained for each pond in 2015 were assessed 
#' for relationship to pond HSI score estimated in 2015.
#' 

## Run simple linear regression models to investigate HSI score and GCN counts.
## GLM is appropriate as we expect a linear relationship.
HSIpeak <- glm(Peak_GCN_2015 ~ HSI_2015,
               family = poisson,
               data = HSI.dat)

summary(HSIpeak)
anova(HSIpeak)
drop1(HSIpeak, test = "Chi")


HSIavg <- glm(Round_Avg_GCN_2015 ~ HSI_2015,
              family = poisson,
              data = HSI.dat)

summary(HSIavg)
anova(HSIavg)
drop1(HSIavg, test = "Chi")

## Overdispersion test (chi-square)
1-pchisq(232.16, df=22)   # peak counts model overdispersed
1-pchisq(178.37, df=22)   # average counts model overdispersed

## Try negative binomial distribution to resolve overdispersion
HSIpeak.nb <- glm.nb(Peak_GCN_2015 ~ HSI_2015,
                     data = HSI.dat)

summary(HSIpeak.nb)
anova(HSIpeak.nb)
drop1(HSIpeak.nb, test = "Chi")


HSIavg.nb <- glm.nb(Round_Avg_GCN_2015 ~ HSI_2015,
                    data = HSI.dat)

summary(HSIavg.nb)
anova(HSIavg.nb)
drop1(HSIavg.nb, test = "Chi")

## Overdispersion test (chi-square)
1-pchisq(25.829, df=22)   # model not overdispersed
1-pchisq(26.335, df=22)   # model not overdispersed

## Negative binomial distribution has resolved model overdispersion.
## Calculate pseudo-R squared of model i.e. the amount of variation explained
## by the model.
## Pseudo-R squared = 1-residual deviance/null deviance
1-(25.829/26.400)   # 2.16% variation in peak counts explained by HSI
1-(26.335/26.831)   # 1.84% variation in average counts explained by HSI

## Hosmer and Lemeshow Goodness of Fit Test
hoslem.test(HSI.dat$Peak_GCN_2015, fitted(HSIpeak.nb))
hoslem.test(HSI.dat$Round_Avg_GCN_2015, fitted(HSIavg.nb))

## Models fit well as p-value is not significant 
## i.e. no significant difference between the models and the observed data

## Check plots of model residuals using custom function in R script
## "CheckResidsFunction.R"
chkres(HSIpeak)
plot(HSI.dat$Peak_GCN_2015 ~ fitted(HSIpeak))

chkres(HSIavg)
plot(HSI.dat$Round_Avg_GCN_2015 ~ fitted(HSIavg))

## Plot relationships
p4a <- ggplot(data=HSI.dat, aes(x=HSI_2015, y=Peak_GCN_2015))
p4a <- p4a + ggtitle("(a)")
p4a <- p4a + geom_point(cex=3)
p4a <- p4a + geom_smooth(method="glm", linetype="dashed", colour="black")
p4a <- p4a + scale_x_continuous(limits=c(0.6,0.9))
p4a <- p4a + scale_y_continuous(limits=c(0,60))
p4a <- p4a + labs(x="HSI score", y="Peak counts of adult great crested newt")
p4a <- p4a + theme(panel.background = element_rect(fill = "white"),
                   plot.title = element_text(face="bold", hjust=0),
                   axis.line.x = element_line(colour = "black", size=0.5, linetype='solid'),
                   axis.line.y = element_line(colour = "black", size=0.5, linetype='solid'),
                   axis.text = element_text(colour = "black"),
                   axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                   axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), angle = 90),
                   text = element_text(size=24),
                   legend.position="none")
p4a

p4b <- ggplot(data=HSI.dat, aes(x=HSI_2015, y=Round_Avg_GCN_2015))
p4b <- p4b + ggtitle("(b)")
p4b <- p4b + geom_point(cex=3)
p4b <- p4b + geom_smooth(method="glm", linetype="dashed", colour="black")
p4b <- p4b + scale_x_continuous(limits=c(0.6,0.9))
p4b <- p4b + scale_y_continuous(limits=c(0,60))
p4b <- p4b + labs(x="HSI score", y="Average counts of adult great crested newt")
p4b <- p4b + theme(panel.background = element_rect(fill = "white"),
                   plot.title = element_text(face="bold", hjust=0),
                   axis.line.x = element_line(colour = "black", size=0.5, linetype='solid'),
                   axis.line.y = element_line(colour = "black", size=0.5, linetype='solid'),
                   axis.text = element_text(colour = "black"),
                   axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                   axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), angle = 90),
                   text = element_text(size=24),
                   legend.position="none")
p4b

grid.arrange(arrangeGrob(p4a,p4b, nrow=1, ncol=2))



#' ---
#' 
#' ## Abiotic Factors Analysis
#' 
#' Now, determine influence of abiotic factors on GCN abundance in
#' 2015. 
#' 

## Import data
count.dat <- read.csv("2015_data.csv", header = TRUE)
attach(count.dat)

## Check data
names(count.dat)
head(count.dat)
dim(count.dat)
str(count.dat)

## Convert factor variables classed as integers to factors
count.dat$Survey <- as.factor(count.dat$Survey)
count.dat$Night <- as.factor(count.dat$Night)
count.dat$Water_Clarity <- as.factor(count.dat$Water_Clarity)
count.dat$Rainfall <- as.factor(count.dat$Rainfall)

## Check data structure
str(count.dat)

## Find range and median of covariates
range(Air_Temp)
median(Air_Temp, na.rm = FALSE)

range(Water_Temp)
median(Water_Temp, na.rm = FALSE)

range(TDS)
median(TDS, na.rm = FALSE)

range(Conductivity)
median(Conductivity, na.rm = FALSE)

range(pH)
median(pH, na.rm = FALSE)

## Test data for normality
shapiro.test(GCN)

#'
#' Significant p value so data deviates from normality assumptions.
#' However, ANCOVAs are fairly robust to deviations from normality
#' and this is the test most suited to this type of data.
#' 

## Exploratory plots
hist(GCN)
plot(GCN ~ Air_Temp)
plot(GCN ~ Water_Temp)
plot(GCN ~ Water_Clarity)
plot(GCN ~ pH)
plot(GCN ~ Conductivity)
plot(GCN ~ TDS)
plot(GCN ~ Rainfall)      
plot(GCN ~ Wind)          
plot(GCN ~ Moon_visible)
plot(GCN ~ Moon_phase)


#
#' Test data for collinearity between continuous variables, or covariates,
#' to determine which variable to include for model selection. The 
#' corvif function and pairplot with the Pearson correlation coefficients 
#' used by Zuur *et al.* (2009) in Ch21 will be applied. These are 
#' contained in a custom script "HighstatLibV6.R" which was loaded 
#' earlier.
#' 

## Matrix of Spearman's rank correlations for dataset
## With Spearman's rank as no assumptions are made about linearity in a
## relationship between two variables
cor(count.dat[,5:9], method = "spearman")

#'
#' Booth *et al.* (1994) and Zuur *et al.* (2009) suggest that 
#' correlations between pairs of variables with magnitudes ± 0.3 
#' indicate high collinearity.
#' 

## View pairwise plots of variables
pairs(count.dat[,5:9])

## Using another script provided by Highland Statistics, "MyLibrary.R",
## pairwise plots where the strength of colinearity is indicated by the 
## size of the text can also be produced.
plot(count.dat[,5:9], lower.panel = panel.cor, 
     upper.panel = panel.smooth2)


#'
#' There is strong collinearity between air and water temperature, and
#' TDS and conductivity. Thus, one variable from each of these pairs
#' must be removed before model selection.
#' 
#' Air temperature will impact water temperature and accounts for the
#' same variance in GCN activity. Water temperature may be warmer than 
#' air temperature as water can retain some heat on hot days and takes
#' longer to cool down. GCN survey guideslines state that survey should
#' only take place when air temperature is greater than 5°C. For these
#' reasons, it is logical to include air temperature and remove water 
#' temperature to counter high collinearity between these explanatory 
#' variables.
#' 
#' TDS (Total Dissolved Solids) and Conductivity (electrolytes present 
#' in water) also appear to be collinear. As conductivity is more
#' commonly reported in other studies of GCN activity, this will be
#' retained as an explanatory variable. 
#' 
#' Now, check the variance inflation factors (VIFs) of variables to 
#' assess the extent of remaining collinearity. This is done using a 
#' Poisson GLM (count data) and logit link function containing all 
#' explanatory variables to the GCN presence/absence data, then 
#' calculating the VIFs for each variable from the resulting model.
#' Variables are removed in order of highest VIF/biological sense,
#' until all variables in model have VIF <3.
#' 

## Model with all variables
glmGCN <- glm(GCN ~ Air_Temp + Water_Temp + Conductivity + TDS 
              + pH + Water_Clarity + Rainfall + Wind + Moon_visible
              + Moon_phase,
              data = count.dat, 
              family = poisson)
vif(glmGCN)

## Remove TDS as it has highest VIF after moon phase
glmGCN <- glm(GCN ~ Air_Temp + Water_Temp + Conductivity + pH 
              + Water_Clarity + Rainfall + Wind + Moon_visible
              + Moon_phase,
              data = count.dat, 
              family = poisson)
vif(glmGCN)

## Remove water temperature as it has highest VIF after moon phase
glmGCN <- glm(GCN ~ Air_Temp + Conductivity + pH + Water_Clarity 
              + Rainfall + Wind + Moon_visible + Moon_phase,
              data = count.dat, 
              family = poisson)
vif(glmGCN)

## Remove rainfall as it has highest VIF after moon phase
glmGCN <- glm(GCN ~ Air_Temp + Conductivity + pH + Water_Clarity 
              + Wind + Moon_visible + Moon_phase,
              data = count.dat, 
              family = poisson)
vif(glmGCN)

## Now only moon visibility and moon phase are collinear, but it is
## necessary to include both of these variables in the model. As a
## result, this collinearity and individual/combined effects on 
## newt counts must be addressed in the Discussion.


#'
#' All VIF values (except moon visibility and phase) are below 3 thus 
#' this indicates the model is free from extensive collinearity between 
#' explanatory variables (Zuur *et al.* 2009).
#' 

## Check possible relationships between response variable and nominal
## variables (factors) in a design plot.
factors <- data.frame(GCN = GCN,
                      Water_Clarity = Water_Clarity,
                      Wind = Wind,
                      Moon_visible = Moon_visible,
                      Moon_phase = Moon_phase)

plot.design(GCN ~ Water_Clarity + Rainfall + Wind + Moon_visible 
            + Moon_phase, data = factors, axes = T, xtick = T)

#'
#' The highest mean values were observed with wind and moon visibility/
#' phase. Classification trees allow detailed investigation into the 
#' relative importance of explanatory variables.
#' 

f1 <- formula(GCN ~ Air_Temp + Conductivity + pH + Water_Clarity
              + Wind + Moon_visible + Moon_phase)

GCN_tree <- rpart(f1, data = count.dat, method = "class",
                  minsplit=5, cp = 0.001)

par(xpd = NA, mar = c(1.5, 1.5, 1.5, 1.5))
plot(GCN_tree,uniform=TRUE, margin=0.1)
text(GCN_tree, use.n = T, cex = 1.0)
par(xpd = F, mar = c(4.5, 4.5, 0.5, 0.5))

#'
#' Most important variables are:
#' - Air temperature
#' - pH
#' - Conductivity
#' - Water clarity
#' - Moon phase
#' 

## Perform cross-validation (pruning tree)
par(mar = c(4.5, 4.5, 4.5, 0.5))
plotcp(GCN_tree)
par(mar = c(4.5, 4.5, 0.5, 0.5))

#'
#' A tree of size of 1-19 is optimal (i.e. 1-19 explanatory variables) 
#' but the best tree will be subjective.
#' 
#' Currently, I have 6 explanatory variables that have passed collinearity
#' and VIF assessment:
#' 
#' - Air temperature
#' - pH
#' - Conductivity
#' - Water clarity
#' - Moon visibility
#' - Moon phase
#' 
#' All are commonly reported to influence GCN activity in the 
#' existing literature.
#' 
#' Many variables occur more than once in the tree which indicates 
#' weak non-linear relationships with the response variable thus a 
#' GAM may be more appropriate to model.
#' 
#' A GAM with poisson distribution and logistic link function will be
#' used to relate GCN adult counts with the explanatory variables.
#' 
#' Forward selection will be applied to find the optimal set of 
#' explanatory variables based on model AIC values.
#' 

GCN.gam1 <- gam(GCN ~ s(Air_Temp), data=count.dat, family=poisson)
GCN.gam2 <- gam(GCN ~ s(pH), data=count.dat, family=poisson)
GCN.gam3 <- gam(GCN ~ s(Conductivity), data=count.dat, family=poisson)
GCN.gam4 <- gam(GCN ~ Water_Clarity, data=count.dat, family=poisson)
GCN.gam5 <- gam(GCN ~ Moon_visible, data=count.dat, family=poisson)
GCN.gam6 <- gam(GCN ~ Moon_phase, data=count.dat, family=poisson)

AIC(GCN.gam1,GCN.gam2,GCN.gam3,GCN.gam4,GCN.gam5,GCN.gam6)

## Water clarity identified as best explanatory variable and most 
## optimal model.
summary(GCN.gam1)
summary(GCN.gam2)
summary(GCN.gam3)
summary(GCN.gam4)
summary(GCN.gam5)
summary(GCN.gam6)


#'
#' However, the other variables are biologically important for GCN. 
#' Therefore, GLM should be applied instead of GAM. GLM is also a 
#' parametric method. 
#' 
#' A GLMM can account for dependencies within sites and is appropriate
#' where nested model selection will be used and the data are structured 
#' hierarchically. Ponds are nested within GNR thus a mixed model
#' is necessary to account for spatial dependencies within this site. 
#' Each survey represents 3 different nights in 2015 thus survey must
#' also be treated as a random effect along with pond inorder to 
#' understand the overall variation in my response variable, GCN adult
#' counts. All other explanatory variables are fixed effects.
#' 

## Examine possible interactions:
## Air temperature likley to be colder on clear nights
## pH likely to be associated with water conductivity
## Water clarity (vegetation, turbidity) may influence pH
plot(Air_Temp ~ Moon_visible) # Interaction
plot(pH ~ Conductivity)       # No interaction
plot(pH ~ Water_Clarity)      # No interaction
plot(as.numeric(Moon_visible) ~ Moon_phase) # Interaction

## Create new dataframe containing explanatory variables to be
## modelled in which covariates will be standardised
GCN.dat <- data.frame(count.dat[,c(3:5,8:10,13:15)])
str(GCN.dat)

## Examine model output, fit and test for overdispersion
GCNmodel <- glmer(GCN ~ (1|Date) + (1|Pond) + Air_Temp + pH + Conductivity
                  + Water_Clarity + Moon_visible + Moon_phase
                  + Air_Temp*Moon_visible + Moon_visible*Moon_phase,
                  family = poisson,
                  data = GCN.dat)

summary(GCNmodel)
anova(GCNmodel)
drop1(GCNmodel, test = "Chi")   # for binomial/integer y models, 
                                # statistics for significance of each term

display(GCNmodel)
se.ranef(GCNmodel)             # levels of random factor centred around 0


#' 
#' Air temperature is not significant but the interaction term between
#' air temperature and bright moonlight is.
#'  

## Check for overdispersion of final model
## Obtain residual deviance and degrees of freedom
overdisp.glmer(GCNmodel)

## Overdispersion test (chi-square)
1-pchisq(236.078, df=104)   # model overdispersed

## summary() can give inflated value for model residual deviance so usual
## methods of calculating residual deviance can be unreliable for GLMMs.
## Also use customised function from Rob Thomas & the Guidebook team:
## 'Data Analysis with R Statistical Software'
overdisp_fun(GCNmodel)

## Model is overdispersed
## Ratio reported is equivalent to overdispersion parameter
## 1 = no overdispersion, >2 = excessive overdispersion

## Perform model validation checks
## Fit model using REML and check normalised residuals for normal distribution
sresid <- resid(GCNmodel, type = "pearson")
hist(sresid)

## Plot residuals against fitted values
fits <- fitted(GCNmodel)
plot(sresid ~ fits)

## Plot QQ plot and heteroscedasticity plot
mcp.fnc(GCNmodel)

## Plot the residuals against each x vairable
plot(sresid ~ GCN.dat$Air_Temp)
plot(sresid ~ GCN.dat$pH)
plot(sresid ~ GCN.dat$Conductivity)
plot(sresid ~ GCN.dat$Water_Clarity)
plot(sresid ~ GCN.dat$Moon_visible)
plot(sresid ~ GCN.dat$Moon_phase)

## Check model residuals
chkres(GCNmodel)

## Plot the fitted data against the observed data
plot(GCN.dat$GCN ~ fitted(GCNmodel))

## Get R-squared value
## marginal R-squared = proportion of variance in response variable explained
## by fixed variables only
## conditional R-squared = proportion of variance in response explained by
## fixed and random variables
r.squaredGLMM(GCNmodel)   # fixed = 31.37%; fixed + random = 73.30%

## Hosmer and Lemeshow Goodness of Fit Test
hoslem.test(GCN.dat$GCN, fitted(GCNmodel))

## Model fits well as p-value is not significant 
## i.e. no significant difference between the model and the observed data


#'
#' Overdispersion is the main problem with a Poisson distribution. In 
#' all other aspects, the model seems to fit well to the count data.
#' Try other distributions, such as quasi-poisson and negative binomial,
#' to resolve overdispersion.
#' 


## Penalised quasi-likelihood model
## Similar to poisson distributed model but designed to handle
## overdispersed data
GCN.qp <- glmmPQL(GCN ~ Air_Temp + pH + Conductivity
                  + Water_Clarity + Moon_visible + Moon_phase
                  + Air_Temp*Moon_visible + Moon_visible*Moon_phase, 
                  random = list(~1|Date, ~1|Pond),
                  family = quasipoisson(link="log"), 
                  data = GCN.dat) 

summary(GCN.qp)
Anova(GCN.qp, type = "III")

## Model could not be fit.
## Apply negative binomial distribution to model to see if this resolves
## overdispersion.
GCNmodel.nb <- glmer.nb(GCN ~ (1|Date) + (1|Pond) + Air_Temp + pH + Conductivity
                        + Water_Clarity + Moon_visible + Moon_phase
                        + Air_Temp*Moon_visible + Moon_visible*Moon_phase,
                        data = GCN.dat)

summary(GCNmodel.nb)
anova(GCNmodel.nb)
drop1(GCNmodel.nb, test = "Chi")  # for binomial/integer y models, 
                                  # statistics for significance of each term

display(GCNmodel.nb)
se.ranef(GCNmodel.nb)             # levels of random factor centred around 0

## Check for overdispersion
overdisp.glmer(GCNmodel.nb)

## Overdispersion test (chi-square)
1-pchisq(123.37, df=104)   # model not overdispersed

## summary() can give inflated value for model residual deviance so usual
## methods of calculating residual deviance can be unreliable for GLMMs.
## Also use customised function from Rob Thomas & the Guidebook team:
## 'Data Analysis with R Statistical Software'
overdisp_fun(GCNmodel.nb)

## Model is no longer overdispersed
## Ratio reported is equivalent to overdispersion parameter
## 1 = no overdispersion, >2 = excessive overdispersion

## Perform model validation checks
## Fit model using REML and check normalised residuals for normal distribution
sresid <- resid(GCNmodel.nb, type = "pearson")
hist(sresid)

## Plot residuals against fitted values
fits <- fitted(GCNmodel.nb)
plot(sresid ~ fits)

## Plot QQ plot and heteroscedasticity plot
mcp.fnc(GCNmodel.nb)

## Plot the residuals against each x vairable
plot(sresid ~ GCN.dat$Air_Temp)
plot(sresid ~ GCN.dat$pH)
plot(sresid ~ GCN.dat$Conductivity)
plot(sresid ~ GCN.dat$Water_Clarity)
plot(sresid ~ GCN.dat$Moon_visible)
plot(sresid ~ GCN.dat$Moon_phase)

## Check model residuals
chkres(GCNmodel.nb)

## Plot the fitted data against the observed data
plot(GCN.dat$GCN ~ fitted(GCNmodel.nb))

## Hosmer and Lemeshow Goodness of Fit Test
hoslem.test(GCN.dat$GCN, fitted(GCNmodel.nb))

## Model fits well as p-value is not significant 
## i.e. no significant difference between the model and the observed data


#' ---
#' 
#' ## Plot and Evaluate Model
#' 
#' I will invoke several functions in R to obtain predictions from the GLMM and
#' plot these against the observed GCN count data.
#' 
#' I will create new data frames in which two of the three continuous 
#' variables will be set to the mean value for this study and the 
#' factors specified to equal one level. This will enable me to plot 
#' the third continuous relationship smoothly.
#'  
#' The function predictSE() gives standard errors also, which allows me 
#' to get CIs for the predictions.
#' 
#' First I will rename the final model and specify the data frame to be used for
#' getting predictions. I must make survey night, pond and water clarity 
#' factors again as specifying the data frame will use the data as it is.
#' 

## Obtain predicted values for the full data set: GCNdist
## se.fit = TRUE will obtain the standard error for each of these predictions
fit <- data.frame(predictSE(GCNmodel.nb, newdata=GCN.dat, 
                            se.fit = TRUE, print.matrix=T))

## Create new data set with fitted values and original data for plots 1 and 2
box.dat <- cbind(GCN.dat, fit) 

## PLOT 2A: Water clarity and GCN counts
p2a <- ggplot(box.dat) + ggtitle('(a)')
p2a <- p2a + geom_jitter(aes(x=Water_Clarity, y=GCN, colour=Water_Clarity), cex=3)
p2a <- p2a + geom_boxplot(aes(x=Water_Clarity, y=fit, colour=Water_Clarity), outlier.size=3, outlier.colour = "black", alpha=0.7) 
p2a <- p2a + labs(y= "Number of adult great crested newt", x = "Water Clarity") 
p2a <- p2a + coord_cartesian(ylim=c(0,100))
p2a <- p2a + scale_x_discrete(breaks=c("1","2","3"),
                            labels=c("Clear", "Medium", "Turbid"))
p2a <- p2a + scale_colour_manual(values=c("cadetblue2","cadetblue3","cadetblue4"))
p2a <- p2a + theme(panel.background = element_rect(fill = "white"),
                 plot.title = element_text(face="bold", hjust=0),
                 axis.line.x = element_line(colour = "black", size=0.5, linetype='solid'),
                 axis.line.y = element_line(colour = "black", size=0.5, linetype='solid'),
                 axis.text = element_text(colour = "black"),
                 axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                 axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), angle = 90),
                 text = element_text(size=24),
                 legend.position="none")
p2a

## PLOT 2B: Moon visibility and GCN counts
#p2b <- ggplot(box.dat) + ggtitle('b')
#p2b <- p2b + geom_jitter(aes(x=Moon_visible, y=GCN, colour=Moon_visible), cex=1)
#p2b <- p2b + geom_boxplot(aes(x=Moon_visible, y=fit, colour=Moon_visible), outlier.colour="black", alpha=0.7)
#p2b <- p2b + labs(y="", x ="Moonlight") 
#p2b <- p2b + coord_cartesian(ylim=c(0,100))
#p2b <- p2b + scale_x_discrete(breaks=c("N","Y"),
#                            labels=c("Absent","Present"))
#p2b <- p2b + scale_colour_manual(values=c("black","cornsilk3"))
#p2b <- p2b + theme(panel.background = element_rect(fill = "white"),
#                 plot.title = element_text(face="bold", hjust=0),
#                 axis.line.x = element_line(colour = "black", size=0.5, linetype='solid'),
#                 axis.line.y = element_line(colour = "black", size=0.5, linetype='solid'),
#                 axis.text = element_text(colour = "black"),
#                 axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
#                 axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), angle = 90),
#                 text = element_text(size=24),
#                 legend.position="none")
#p2b

## PLOT 2C: Moon phase and GCN counts
fPhase <- ordered(box.dat$Moon_phase, 
                  levels = c("New moon","Waxing crescent","Waxing gibbous",
                             "Full moon","Waning gibbous","Last quarter"))
fPhase

p2c <- ggplot(box.dat) + ggtitle('(b)')
p2c <- p2c + geom_jitter(aes(x=fPhase, y=GCN, colour=fPhase, shape=Moon_visible), cex=3)
p2c <- p2c + geom_boxplot(aes(x=fPhase, y=fit, colour=fPhase), outlier.size=3, outlier.colour="black", alpha=0.7)
p2c <- p2c + labs(y="", x ="Moon phase") 
p2c <- p2c + coord_cartesian(ylim=c(0,100))
p2c <- p2c + scale_shape_manual(name="Moon visible",
                                values=c(16,17),
                                labels=c("No","Yes"))
p2c <- p2c + scale_colour_manual(guide=FALSE,
                                 values=c("black",
                                          "goldenrod4",
                                          "goldenrod3",
                                          "goldenrod1",
                                          "goldenrod3",
                                          "goldenrod4"))
p2c <- p2c + theme(panel.background = element_rect(fill = "white"),
                   plot.title = element_text(face="bold", hjust=0),
                   axis.line.x = element_line(colour = "black", size=0.5, linetype='solid'),
                   axis.line.y = element_line(colour = "black", size=0.5, linetype='solid'),
                   axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                   axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), angle = 90),
                   axis.text.x = element_text(colour = "black", size=12),
                   axis.text.y = element_text(colour = "black"),
                   text = element_text(size=24),
                   legend.key=element_blank(),
                   legend.position="bottom")
p2c

## PLOT 2D: Air temperature and GCN counts
## Create a range of air temperature values which increase by 0.094 degrees
## to predict adult counts as temperature increases
range <- seq(from=min(GCN.dat$Air_Temp), to=max(GCN.dat$Air_Temp), by=0.094)

## Create new data frame where only Air_Temp changes
## Water_Temp and pH are set as the mean values in this study
## All factors are set to one level
d1 <- data.frame(Date=rep("05/04/15", length(range)), 
                 Pond=rep("RJ1", length(range)),
                 Air_Temp=range,
                 pH=rep(7.651583, length(range)),
                 Conductivity=rep(219.1833, length(range)),
                 Water_Clarity=rep("2", length(range)),
                 Moon_visible=rep("Y", length(range)),
                 Moon_phase=rep("Full moon", length(range)))

## Get predictions for new dataset
fit <- data.frame(predictSE(GCNmodel.nb, newdata=d1, se.fit = TRUE, 
                            print.matrix=T)) 

## Calculate upper 95% CIs from the SEs of the predictions
ciu <- (fit$fit+1.96*fit$se.fit) 

## Calculate lower 95% CIs from the SEs of the predictions
cil <- (fit$fit-1.96*fit$se.fit)

## This is now the data frame
dat.AT <- cbind(d1, fit, ciu, cil) 

## Plot showing relationship between air temp and GCN counts with predicted 
## values from model
p2d <- ggplot(GCN.dat) + ggtitle('(c)')
p2d <- p2d + geom_point(aes(x=Air_Temp, y=GCN, colour=Air_Temp, shape=Moon_visible), cex=3)
p2d <- p2d + geom_line(aes(x=dat.AT$Air_Temp, y=dat.AT$fit, colour = dat.AT$Air_Temp), size = 1)
p2d <- p2d + geom_ribbon(aes(x=dat.AT$Air_Temp, ymin = dat.AT$cil, ymax = dat.AT$ciu), alpha = 0.25)
p2d <- p2d + scale_y_continuous(limits=c(0,100))
p2d <- p2d + scale_x_continuous(limits=c(0,12), breaks = c(0,2,4,6,8,10,12))
p2d <- p2d + scale_colour_gradient(limits=c(0, 12), low="slategray4", high="slategray2")
p2d <- p2d + labs(y = "Number of adult great crested newt", x = "Air Temperature ("*degree~"C)")
p2d <- p2d + theme(panel.background = element_rect(fill = "white"),
                 plot.title = element_text(face="bold", hjust=0),
                 axis.line.x = element_line(colour = "black", size=0.5, linetype='solid'),
                 axis.line.y = element_line(colour = "black", size=0.5, linetype='solid'),
                 axis.text = element_text(colour = "black"),
                 axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                 axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), angle = 90),
                 text = element_text(size=24),
                 legend.position="none")
p2d

## PLOT 2E: pH and GCN counts
## Create a range of pH values which increase by 0.0156 units
## to predict adult counts as pH increases
range <- seq(from=min(GCN.dat$pH), to=max(GCN.dat$pH), by=0.0156)

## Create new data frame where only pH changes
## Water_Temp and Air_Temp are set as the mean values in this study
## All factors are set to one level
d2 <- data.frame(Date=rep("05/04/15", length(range)), 
                 Pond=rep("RJ1", length(range)),
                 Air_Temp=rep(7.235833, length(range)),
                 pH=range,
                 Conductivity=rep(219.1833, length(range)),
                 Water_Clarity=rep("2", length(range)),
                 Moon_visible=rep("Y", length(range)),
                 Moon_phase=rep("Full moon", length(range)))

## Get predictions for new dataset
fit <- data.frame(predictSE(GCNmodel.nb, newdata=d2, se.fit = TRUE, 
                            print.matrix=T)) 

## Calculate upper 95% CIs from the SEs of the predictions
ciu <- (fit$fit+1.96*fit$se.fit) 

## Calculate lower 95% CIs from the SEs of the predictions
cil <- (fit$fit-1.96*fit$se.fit)

## This is now the data frame
dat.pH <- cbind(d2, fit, ciu, cil) 

## Plot showing relationship between pH and GCN counts with predicted 
## values from model
p2e <- ggplot(GCN.dat) + ggtitle('(d)')
p2e <- p2e + geom_point(aes(x=pH, y=GCN, colour=pH), cex=3)
p2e <- p2e + geom_ribbon(aes(x=dat.pH$pH, ymin = dat.pH$cil, ymax = dat.pH$ciu), alpha = 0.25)
p2e <- p2e + geom_line(aes(x=dat.pH$pH, y=dat.pH$fit, colour = dat.pH$pH), size = 1)
p2e <- p2e + scale_colour_gradient(limits=c(6,9), low="Green", high="#339999")
p2e <- p2e + scale_y_continuous(limits=c(0,100))
p2e <- p2e + scale_x_continuous(limits=c(6.5,9.0))
p2e <- p2e + labs(y = "", x = "pH")
p2e <- p2e + theme(panel.background = element_rect(fill = "white"),
                 plot.title = element_text(face="bold", hjust=0),
                 axis.line.x = element_line(colour = "black", size=0.5, linetype='solid'),
                 axis.line.y = element_line(colour = "black", size=0.5, linetype='solid'),
                 axis.text = element_text(colour = "black"),
                 axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
                 axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), angle = 90),
                 text = element_text(size=24),
                 legend.position="none")
p2e

## PLOT 2F: conductivity and GCN counts
## Create a range of conductivity values which increase by 4.65 units
## to predict adult counts as conductivity increases
range <- seq(from=min(GCN.dat$Conductivity), to=max(GCN.dat$Conductivity), by=4.65)

## Create new data frame where only conductivity changes
## Water_Temp and Air_Temp are set as the mean values in this study
## All factors are set to one level
d3 <- data.frame(Date=rep("05/04/15", length(range)), 
                 Pond=rep("RJ1", length(range)),
                 Air_Temp=rep(7.235833, length(range)),
                 pH=rep(7.651583, length(range)),
                 Conductivity=range,
                 Water_Clarity=rep("2", length(range)),
                 Moon_visible=rep("Y", length(range)),
                 Moon_phase=rep("Full moon", length(range)))

## Get predictions for new dataset
fit <- data.frame(predictSE(GCNmodel.nb, newdata=d3, se.fit = TRUE, 
                            print.matrix=T)) 

## Calculate upper 95% CIs from the SEs of the predictions
ciu <- (fit$fit+1.96*fit$se.fit) 

## Calculate lower 95% CIs from the SEs of the predictions
cil <- (fit$fit-1.96*fit$se.fit)

## This is now the data frame
dat.cond <- cbind(d3, fit, ciu, cil) 

## Plot showing relationship between pH and GCN counts with predicted 
## values from model
#p2f <- ggplot() + ggtitle('f')
#p2f <- p2f + geom_ribbon(aes(x=dat.cond$Conductivity, ymin = dat.cond$cil, ymax = dat.cond$ciu), alpha = 0.25)
#p2f <- p2f + geom_line(aes(x=dat.cond$Conductivity, y=dat.cond$fit, colour = dat.cond$Conductivity), size = 1)
#p2f <- p2f + geom_point(aes(x=Conductivity, y=GCN, colour=Conductivity), cex=0.95)
#p2f <- p2f + scale_colour_gradient(limits=c(74,628), low="darkorchid4", high="darkorchid")
#p2f <- p2f + scale_y_continuous(limits=c(0,100))
#p2f <- p2f + scale_x_continuous(limits=c(0,700), breaks=seq(0,700,100))
#p2f <- p2f + labs(y = "", x = expression(paste("Conductivity (",mu,"S/cm)")))
#p2f <- p2f + theme(panel.background = element_rect(fill = "white"),
#                   plot.title = element_text(face="bold", hjust=0),
#                   axis.line.x = element_line(colour = "black", size=0.5, linetype='solid'),
#                   axis.line.y = element_line(colour = "black", size=0.5, linetype='solid'),
#                   axis.text = element_text(colour = "black"),
#                   axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
#                   axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), angle = 90),
#                   text = element_text(size=24),
#                   legend.position="none")
#p2f


## SHOW ALL PLOTS
## grid.arrange() is a function within the gridEXTRA package that allows to
## arrange how ggplots are viewed, similar to par(mfrow=c())
## Within this function, I can use arrangeGrob() to specify how many plots I
## want on each row
## I want to display my factors on the top row and my continuous predictors
## on the bottom row

## Extract common legend
mylegend <- g_legend(p2c)

## Show all plots with common legend
p2a_d <- grid.arrange(arrangeGrob(p2a, p2c + theme(legend.position="none"),
                                  p2d, p2e, nrow=2, ncol=2),
                      mylegend, heights=c(10, 1))

