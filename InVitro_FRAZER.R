detach(InVitro_Final)
detach(ExVivo_Final)
attach(InVitro_Final)
library(ggpubr)
library(rstatix)
library(FSA)
library(ggstatsplot)
library(car)
library(stats)

#Calculate average load in the cartridges (difference between average pre-weight and average post-weight)
InVitro_Final$Avg_preweight = (C_PAv_1 + C_PAv_2 + C_PAv_3) / 3
InVitro_Final$Avg_postweight = (C_PAp_1 + C_PAp_2 + C_PAp_3) / 3
InVitro_Final$Charge = InVitro_Final$Avg_preweight - InVitro_Final$Avg_postweight

#Create subsets for each device, useful for the graphs
Blitz<-subset(InVitro_Final, Percuteur=="Blitz")
Matador<-subset(InVitro_Final, Percuteur=="Matador")
Jarvis<-subset(InVitro_Final, Percuteur=="Jarvis")

#Descriptive stats of cartridge load and penetration depth 
mean(Jarvis$Charge)
sd(Jarvis$Charge)
mean(Jarvis$P_pen)
sd(Jarvis$P_pen)

mean(Blitz$Charge)
sd(Blitz$Charge)
mean(Blitz$P_pen)
sd(Blitz$P_pen)

mean(Matador$Charge)
sd(Matador$Charge)
mean(Matador$P_pen)
sd(Matador$P_pen)

###### MODEL

#Construct basic model with all potential predictors, validate colinearity between variables using VIF
m1 <- lm(P_pen ~ Percuteur + Charge, data = InVitro_Final, na.action = na.fail)
vif(m1)
## --> GVIF^(1/(2*Df)) very high for Charge (41.7), removed from model

m2 <- lm(P_pen ~ Percuteur, data = InVitro_Final, na.action = na.fail)
## --> Only one term remains, no VIF collinearity possible, no model averaging required.
## --> Proceed directly to ANOVA

m3 <- aov(P_pen ~ Percuteur, data = InVitro_Final)


###### Validate assumptions of the ANOVA

#Normality of response variable for each factor
shapiro.test(residuals(m3))
## --> p-value below 0.05: reject null hypothesis of non-normality
## --: ANOVA is impossible because assumptions not validated. Non-parametric test must be done instead.
## --> Friedman or Kruskal-Wallis should be performed; since groups are independent, Kruskal-Wallis was selected


###### Kruskal-Wallis
m4 <- kruskal.test(P_pen ~ Percuteur, data = InVitro_Final)
m4
## --: model significant at p-value 0.0004024
## --> Proceed to post-hoc anaylsis; Dunn test tends to be more conservative than required, hence selection of Wilcoxon
## --> Proceed to Wilcoxon test with Benjamini Hochberg correction for group comparison

###### Wilcoxon test
m4.wlcx <- pairwise.wilcox.test(InVitro_Final$P_pen, InVitro_Final$Percuteur, "BH")
m4.wlcx
## --: All relationships significant: Blitz-Jarvis (p=0.00087), Blitz-Matador (p=0.00087), Jarvis-Matador (p=0.02622)
