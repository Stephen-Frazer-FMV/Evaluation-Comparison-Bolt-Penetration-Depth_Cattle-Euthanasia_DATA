detach(InVitro_Final)
detach(ExVivo_Final)
detach(JB.AR.vs.JB.NR)
attach(ExVivo_Final)
library(ggpubr)
library(rstatix)
library(FSA)
library(ggstatsplot)
library(multcomp)
library(ggforce)
library(ggbeeswarm)
library(dplyr)
library(ggsignif)
library(ggbreak)
library(MuMIn)
library(psych)
library(car)
library(ggpubr)
library(ggpmisc)



#Calculate average load in the cartridges (difference between average pre-weight and average post-weight)
ExVivo_Final$Avg_preweight = (C_PAv_1 + C_PAv_2 + C_PAv_3) / 3
ExVivo_Final$Avg_postweight = (C_PAp_1 + C_PAp_2 + C_PAp_3) / 3
ExVivo_Final$Charge = ExVivo_Final$Avg_preweight - ExVivo_Final$Avg_postweight
ExVivo_Final$Charge

#Create subsets for each device, useful for the graphs and descriptive stats
Blitz<-subset(ExVivo_Final, Percuteur=="Blitz")
Matador<-subset(ExVivo_Final, Percuteur=="Matador")
Jarvis<-subset(ExVivo_Final, Percuteur=="Jarvis") 

#Descriptive stats of cartridge load and penetration depth 
mean(P_pen_max)
sd(P_pen_max)
mean(P_pen_BW)
sd(P_pen_BW)

mean(Blitz$Charge)
sd(Blitz$Charge)
mean(Blitz$P_pen)
sd(Blitz$P_pen)
mean(Blitz$P_pen_max)
sd(Blitz$P_pen_max)
mean(Blitz$P_pen_BW)
sd(Blitz$P_pen_BW)

mean(Jarvis$Charge)
sd(Jarvis$Charge)
mean(Jarvis$P_pen)
sd(Jarvis$P_pen)
mean(Jarvis$P_pen_max)
sd(Jarvis$P_pen_max)
mean(Jarvis$P_pen_BW)
sd(Jarvis$P_pen_BW)

mean(Matador$Charge)
sd(Matador$Charge)
mean(Matador$P_pen)
sd(Matador$P_pen)
mean(Matador$P_pen_max)
sd(Matador$P_pen_max)
mean(Matador$P_pen_BW)
sd(Matador$P_pen_BW)

###### MODEL

#Construct basic model with all potential predictors, validate colinearity between variables using VIF
m1 <- lm(P_pen ~ Percuteur + Race  + Sexe + Age + Max_BT_density + M_Pol_Cant + M_Pol_Muse + M_Cant, data = ExVivo_Final, na.action = na.fail)
vif(m1)

mean(P_pen)
sd(P_pen)
#Do model averaging using m2
model_set <- dredge(m1)
avg_model <- model.avg(model_set)
summary(avg_model)


#Construct basic model with all potential predictors, validate colinearity between variables using VIF
m1 <- lm(P_pen ~ Percuteur + Charge + Age + Max_BT_density, data = ExVivo_Final, na.action = na.fail)
vif(m1)
## --> GVIF^(1/(2*Df)) very high for Charge (17.6), removed from model

m2 <- lm(P_pen ~ Percuteur+ Age + Max_BT_density, data = ExVivo_Final, na.action = na.fail)
vif(m2)
## --> GVIF^(1/(2*Df)) below 5 for all predictors: no significant colinearity detected; no factors removed from model

#Do model averaging using m2
model_set <- dredge(m2)
avg_model <- model.avg(model_set)
summary(avg_model)
## --> Device used is the only significant predictor; all the rest are removed and an aov() will be done with this predictor only

###### Validate assumptions of the ANOVA
m3 <- aov(P_pen ~ Percuteur, data = ExVivo_Final)

#Normality residuals of the ANOVA model
shapiro.test(residuals(m3))
## --> p-values above 0.05, cannot reject null hypothesis of non-normality

#Homogeneity of variance with Bartlett's test
bartlett.test(P_pen ~ Percuteur, data = ExVivo_Final)
## --> p-value above 0.05, cannot reject null hypothesis of non-homogenous variance


###### ANOVA test
summary(m3)
## --: Percuteur is significant (p = 4.05e-05)
## --> Post-hoc TukeyHSD may be conducted

tukey_hsd(m3)
## --: All relationships are significant, Blitz-Jarvis (p=0.0483), Blitz-Matador (p=0.0000244), Jarvis-Matador (p=0.0159)

plot(m3)


### Validate ICC for the two sets of observations
df <- data.frame(P_pen = ExVivo_Final$P_pen, P_pen_BW = ExVivo_Final$P_pen_BW)
icc <- ICC(df[c("P_pen", "P_pen_BW")])
icc
## --> Single_raters_absolute has an ICC of 0.98, indicating a very high level of agreement between the two measurement sets




########################################
### Descriptive Stats
########################################

mean(Matador$Max_BT_density)
sd(Matador$Max_BT_density)
mean(M_Cant)
sd(M_Cant)
mean(M_Pol_Cant)
sd(M_Pol_Cant)
mean(M_Pol_Muse)
sd(M_Pol_Muse)
mean(P_pen_max)
sd(P_pen_max)
mean(P_pen)
sd(P_pen)
mean(Max_BT_density)
sd(Max_BT_density)


mmm1 <- aov(M_Pol_Muse ~ Percuteur)
summary(mmm1)
mmm2 <- aov(M_Pol_Cant ~ Percuteur)
summary(mmm2)
mmm3 <- aov(M_Cant ~ Percuteur)
summary(mmm3)


table(Cerv_BW)
table(Jarvis$Cerv_BW)
table(Matador$Cerv_BW)
table(Blitz$Cerv_BW)

table(TC_BW)
table(Blitz$TC_BW)
table(Jarvis$TC_BW)
table(Matador$TC_BW)





########################################
### PLOTS
########################################

## Basic Penetration depth plot

ggplot(ExVivo_Final, aes(x=Percuteur, y = P_pen)) +
  geom_boxplot(width = 0.6,color="black", fill=c("red","blue","green")) +
  coord_flip() +ylab("Penetration depth (cm)") +
  xlab("PCB model") +
  scale_x_discrete(labels=c("Matador" = "MS", 
                            "Jarvis" = "JB-NR",
                            "Blitz" = "BK")) +
  scale_y_continuous(limits=c(6,10), breaks=seq(6,10,0.5), minor_breaks=seq(6,10,0.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, face = "bold"),
        panel.grid.major.y = element_blank()
  ) +
  geom_signif(comparisons = list(c("Jarvis", "Matador"), c("Jarvis", "Blitz"), c("Matador", "Blitz")),
              annotation = c("*", "*", "***"),
              y_position = c(9.6, 9.4, 9.8))


## Hist of age; first one not used, but conserved in case wanted

max_freq <- max(table(cut(ExVivo_Final$Age, breaks = seq(0, 120, by = 5))))

# Create histogram (NOT USED)
hist(ExVivo_Final$Age, breaks = seq(0, 120, by = 5), 
     main = "", 
     xlab = "Age (months)", 
     ylab = "Frequency",
     col = "lightblue",
     xlim = c(0, 120),  # Set x-axis limits
     ylim = c(0, max_freq))

####### Histogram with coloring by PCB group

# Define color mapping
max_freq <- max(table(cut(ExVivo_Final$Age, breaks = seq(0, 120, by = 1))))
color_map <- c("Blitz" = "red", "Jarvis" = "blue", "Matador" = "green")

# Create the plot
ggplot(ExVivo_Final, aes(x = Age, fill = Percuteur)) +
  geom_histogram(binwidth = 6, center = 9, position = "stack", color = "#555555") +
  scale_fill_manual(values = color_map, labels = c("BK", "JB-NR", "MS")) +
  scale_x_continuous(breaks = seq(6, 114, by = 6), limits = c(6, 114), oob = scales::oob_keep) +
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0,10)) +
  labs(x = "Age (months)", y = "Count", fill = "Percuteur") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()
  )+
  theme(legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(0.75, "cm")) +
  theme(legend.title.align = 0.0)+ guides(fill=guide_legend(title="PCB Model"))



##### Penetration depth and brain regions : WORKS!!!!!!!!!

ExVivo_Final$Regions <- factor(
  with(ExVivo_Final, ifelse(Cerv_BW == "Oui" & TC_BW == "Non", "Forebrain and\ncerebellum\n",
                            ifelse(Cerv_BW == "Non" & TC_BW == "Oui", "Forebrain and\nbrainstem\n",
                                   ifelse(Cerv_BW == "Oui" & TC_BW == "Oui", "Forebrain, brainstem\n and cerebellum\n", "Forebrain only\n")))))
# Set the order of levels
ExVivo_Final$Regions <- factor(ExVivo_Final$Regions, levels = c("Forebrain only\n", "Forebrain and\ncerebellum\n", "Forebrain and\nbrainstem\n", "Forebrain, brainstem\n and cerebellum\n"))

ExVivo_Final$Percuteur <- as.factor(ExVivo_Final$Percuteur)

# Create a mapping from 'Percuteur' to a numerical y-position
percuteur_map <- setNames(1:length(unique(ExVivo_Final$Percuteur)), levels(ExVivo_Final$Percuteur))

# Create a new column in the data frame with the y-position for each point
ExVivo_Final$y_pos <- as.numeric(percuteur_map[ExVivo_Final$Percuteur])

avoid_overlap <- function(df, y_pos_col, overlap_col) {
  
  overlap_threshold <- 0.15  # adjusted as per your comment
  
  df <- df %>%
    arrange(Percuteur, !!sym(overlap_col)) %>%
    group_by(Percuteur) %>%
    mutate(overlap = abs(dplyr::lag(P_pen, default = first(P_pen)) - P_pen) <= overlap_threshold,
           adjustment = 0)  # start with no adjustment
  
  df$overlap[which(df$Percuteur != dplyr::lag(df$Percuteur, default = first(df$Percuteur)))] <- FALSE
  
  # Apply the adjustment as per your specification
  for (i in 2:nrow(df)) {
    if(df$overlap[i]) {
      previous_adjustment <- df$adjustment[i - 1]
      if (previous_adjustment > 0) {
        df$adjustment[i] <- -abs(previous_adjustment) - 0.07
      } else if (previous_adjustment < 0) {
        df$adjustment[i] <- abs(previous_adjustment) + 0.08
      } else {
        df$adjustment[i] <- 0.14
      }
    }
  }
  
  df[[y_pos_col]] <- df[[y_pos_col]] + df$adjustment
  df$adjustment <- NULL
  df$overlap <- NULL
  
  df
}

# Apply the function to ExVivo_Final
ExVivo_Final <- avoid_overlap(ExVivo_Final, "y_pos", "P_pen")

names(percuteur_map)

ggplot(data = ExVivo_Final) +
  aes(x = P_pen, y = y_pos, shape = Regions, color = Percuteur) +
  geom_point(size = 3.5) +
  scale_x_continuous(limits = c(6,10), breaks = seq(6,10,1), minor_breaks = seq(6,10,0.5)) +
  scale_y_continuous(limits = c(0.75, 3.3), breaks = 1:length(percuteur_map), 
                     labels = c("Blitz" = "BK",
                                "Jarvis" = "JB-NR",
                                "Matador" = "MS")) +
  scale_shape_manual(values = c("Forebrain only\n" = 0, "Forebrain and\ncerebellum\n" = 1, "Forebrain and\nbrainstem\n" = 3, "Forebrain, brainstem\n and cerebellum\n" = 10)) +
  scale_color_manual(values = c("Matador" = "#00BA38", "Jarvis" = "blue", "Blitz" = "red")) +
  xlab("Penetration Depth (cm)") +
  ylab("PCB Model") +
  labs(shape = "Brain regions") + 
  theme_bw() +
  ggtitle("") +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust=0),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 13),
        panel.grid.minor.y = element_blank())





###########################################
##### Poids de charge

color_map <- c("Blitz" = "red", "Jarvis" = "blue", "Matador" = "green")

ExVivo_Final$Charge <- as.numeric(as.character(ExVivo_Final$Charge))
ExVivo_Final$P_pen <- as.numeric(as.character(ExVivo_Final$P_pen))

plot <- ggplot(ExVivo_Final, aes(x = P_pen, y = Charge, color = Percuteur)) +
  geom_point(cex=1.2) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linetype = "solid",
    aes(group = Percuteur),
    show.legend = FALSE
  ) +
  scale_color_manual(values = color_map) +
  labs(x = "P_pen", y = "Charge") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )
plot

plot_with_stats <- plot +
  stat_regline_equation(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right",
    label.y = 0.15,
    size = 4
  )

plot_with_stats


##### Cartridge fill

# Calculate average Charge for each device
avg_charge <- ExVivo_Final %>%
  group_by(Percuteur) %>%
  summarise(avg_charge = mean(Charge, na.rm = TRUE))

# Calculate the overall average charge
avg_charge_all <- mean(ExVivo_Final$Charge, na.rm = TRUE)

# Calculate a scaling factor for each device to make average charges equal
avg_charge <- avg_charge %>%
  mutate(scaling_factor_charge = avg_charge_all / avg_charge)

# Join the scaling factor back to the main dataset
ExVivo_Final <- ExVivo_Final %>%
  left_join(avg_charge, by = "Percuteur")

# Adjust the Charge based on the scaling factor
ExVivo_Final <- ExVivo_Final %>%
  mutate(Charge_scaled = Charge * scaling_factor_charge)

# Now, let's calculate the KE scaling factor based on the average KE
ke_jarvis <- 407
ke_blitz <- 252
ke_matador <- 406
avg_ke <- mean(c(ke_jarvis, ke_blitz, ke_matador))

ExVivo_Final <- ExVivo_Final %>%
  mutate(scaling_factor_ke = case_when(
    Percuteur == "Jarvis" ~ ke_jarvis / avg_ke,
    Percuteur == "Blitz" ~ ke_blitz / avg_ke,
    Percuteur == "Matador" ~ ke_matador / avg_ke
  ))

# Now create Charge_adj that considers both formulation and KE
ExVivo_Final <- ExVivo_Final %>%
  mutate(Charge_adj = Charge_scaled * scaling_factor_ke)



plot(Charge_adj~P_pen, data=ExVivo_Final)

ggplot(ExVivo_Final, aes(x = P_pen, y = Charge_adj, color = Percuteur)) +
  geom_point() +
  scale_color_manual(values = c("Jarvis" = "blue", "Blitz" = "red", "Matador" = "green")) +
  theme_minimal() +
  labs(
    title = "Charge_adj vs. P_pen for different devices",
    x = "P_pen",
    y = "Charge_adj",
    color = "Device"
  )


summary(lm(Charge_adj~P_pen, data=ExVivo_Final))


################################

my_formula <- y ~ x

# Create the plot
p <- ggplot(ExVivo_Final, aes(x = P_pen, y = Charge_adj, color = Percuteur)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = my_formula) +
  scale_color_manual(values = c("Jarvis" = "blue", "Blitz" = "red", "Matador" = "green")) +
  theme_minimal() +
  labs(
    title = "Charge_adj vs. P_pen for different devices",
    x = "P_pen",
    y = "Charge_adj",
    color = "Device"
  )

# Add the equation and R² for each group
p + stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "~~~")), 
                 formula = my_formula, parse = TRUE)
  