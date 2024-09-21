#########################""Generate a survival fit objective######################

# Loading relevant libraries 
library(tidyverse) # includes ggplot2, for data visualisation. dplyr, for data manipulation.
library(survival)
library(ggsurvfit)
library(survminer)
library(BiocManager)

BiocManager::install("survival")
BiocManager::install("ggsurvfit")
BiocManager::install("survminer")


# Setting up environment ===================================================
# Clean environment
rm(list = ls(all.names = TRUE)) # will clear all objects including hidden objects
gc() # free up memory and report the memory usage
options(max.print = .Machine$integer.max, scipen = 999, stringsAsFactors = F, dplyr.summarise.inform = F) # avoid truncated output in R console and scientific notation

# Set seed
set.seed(42)

# Theme ====================================================================
biostatsquid_theme <- theme(plot.title = element_text(size = rel(2)),
                            panel.grid.major.y = element_line(colour = 'gray'),
                            panel.grid.minor.y = element_line(colour = 'gray'),
                            panel.grid.major.x = element_blank(),
                            panel.grid.minor.x = element_blank(),
                            plot.background = element_rect(fill = NULL, colour = 'white'),
                            panel.background = element_rect(fill = 'white'),
                            # Axis stuff
                            axis.line = element_line(colour = 'black', linewidth = 1),
                            axis.text = element_text(colour = "black", face = 'bold'),
                            axis.text.x = element_text(size = rel(1)),
                            axis.text.y = element_text(size = rel(1)),
                            axis.title = element_text(size = rel(1.2)),
                            axis.ticks = element_line(colour = 'black', linewidth = 1.2),
                            # Legend stuff
                            legend.position = "bottom",
                            legend.margin = margin(6, 6, 6, 6),
                            legend.title = element_text(face = 'bold'),
                            legend.background = element_blank(),
                            legend.box.background = element_rect(colour = "black"))

# Load the data ============================================================
df <- survival::rotterdam
head(df)
colnames(df)

# Key columns for survival analysis
# 1. Censoring status: 1 = event happened, 0 = censored (or TRUE and FALSE)
unique(df$death)
df <- df %>% mutate(status = death)
head(df)
table(df$status)

# 2. Time-to-event (we can use either rtime or dtime)
df$dtime %>% head()
df <- df %>% mutate(dtime_yrs = dtime/365.25)
head(df)

# # If you need to calculate time-to-event from dates
df2 <- data.frame(surgery_date = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by = 'day'), 12),
                  day_of_last_followup = sample(seq(as.Date('2000/01/01'), as.Date('2020/01/01'), by = 'day'), 12))
head(df2)
class(df2$surgery_date)
df2 <- df2 %>% mutate(os_yrs = as.numeric(as.duration(day_of_last_followup - surgery_date), 'years'),
                      os_days = as.numeric(as.duration(day_of_last_followup - surgery_date), 'days'))
head(df2)

##################2. Create a survival object#################################

# Create a survival object
surv_obj <- Surv(time = df$dtime_yrs, event = df$status)
head(surv_obj)

################3. Kaplan Meier curves in R##################################

# Create survival curve
s1 <- survfit(surv_obj ~ 1, data = df)
summary(s1)

# Kaplan-Meier plots ======================================================
## Plot -------------------------
km_curve <- ggsurvfit(s1, linewidth = 1) +
  labs(x = 'Years', y = 'Overall survival') +
  add_confidence_interval() +
  add_risktable() +
  scale_ggsurvfit() + 
  biostatsquid_theme +
  coord_cartesian(xlim = c(0, 8))

##########################4. Estimating survival after x years##########################

## Estimating x-year survival ----------------------
# What is the probability of surviving beyond a certain number of years, x?
summary(s1, times = c(0, 0.5, 2, 5, 7))$surv

km_curve +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'red', size = 1) +
  geom_hline(yintercept = summary(s1, times = 5)$surv, linetype = 'dashed', colour = 'red', size = 1) + 
  coord_cartesian(xlim = c(0, 8))
# Estimating median survival dtime ======================================================
s1

########################5. Log rank test###############################################

# Log rank test ======================================================
table(df$chemo)

s2 <- survfit(surv_obj ~ chemo, data = df)
ggsurvplot(s2, data = df,
           size = 1,
           palette = c('#E7B800', '#2e9fdf'),
           censor.shape = '|', censor.size = 4,
           conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE,
           risk.table.col = 'strata',
           legend.labs = list('0' = 'No chemo', '1' = 'Chemo'),
           risk.table.height = 0.25,
           ggtheme = theme_bw())

# Log rank test ======================================================
# Now we can compute the test of the difference between the survival curves using survdiff
# Comparing survival dtimes between groups aka Log rank test
logrankres_chemo <- survdiff(Surv(dtime_yrs, status) ~ chemo, data = df)
logrankres_chemo

# What about hormonal treatment?
logrankres_hor <- survdiff(Surv(dtime_yrs, status) ~ hormon, data = df)
logrankres_hor

##################################6. Cox regression##################################

# Cox regression ======================================================
# Fit the model
cox_res <- coxph(Surv(dtime_yrs, status) ~ hormon + chemo + size + er + pgr + nodes + meno + grade + age, data = df)
cox_res

# If we have a binary categorical column
df$treatment <- rep(c('drugA', 'placebo'), nrow(df)/2)
table(df$treatment)
cox_res2 <- coxph(Surv(dtime_yrs, status) ~ hormon + treatment, data = df) 
cox_res2

df$treatment <- factor(df$treatment, levels = c('drugA', 'placebo'))
class(df$treatment)
df$treatment <- relevel(df$treatment, ref = 'placebo')
unique(df$treatment)
cox_res2 <- coxph(Surv(dtime_yrs, status) ~ hormon + treatment, data = df) 
cox_res2
df$treatment <- NULL

# Get a summary of the model - we get the CI of the estimated HR and the different test scores
summary(cox_res)

# Before interpreting the results, verify whether the proportional hazards assumptions are respected
# Cox model assumes that the HR between any two groups remains constant over time
test <- survival::cox.zph(cox_res)
test

# Plot the Schoenfeld residuals over time for each covariate
survminer::ggcoxzph(test, point.size = 0.1)

