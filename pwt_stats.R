# install.packages("nlme")
# install.packages("lmerTest")
# install.packages("tidyverse")
# install.packages("forcats")
# install.packages("broom")
# install.packages("irr")
# install.packages("effsize")
# install.packages("feather")
# install.packages("scales")
# install.packages("MuMIn")

library(nlme)
library(lmerTest)
library(tidyverse)
library(forcats)
library(broom)
library(irr)
library(effsize)
library(feather)
library(scales)
library(MuMIn)

month_level_pwt_vocab <- read_csv("data/month_level_pwt_vocab_ransubj.csv")
subj_level_pwt_obsrep_mo <- read_csv("data/subjlevel_pwt_obsrep_mo_ransubj.csv")
gathervocab_month_level_pwt <- read_csv("data/gathervocab_month_level_pwt_ransubj.csv")
missing_surveys <- read_csv("data/missing_surveys_ransubj.csv")
point_walk_experience <- read_csv("data/point_walk_experience_ransubj.csv")

## RESULTS SECTION  ---------------------------------------------
## Observed vs. Reported Data
# shapiro tests
shapiro.test(subj_level_pwt_obsrep_mo$obs_1stpoint_mo) #Non-normal
shapiro.test(subj_level_pwt_obsrep_mo$rep_1stpoint_mo) #Normal
shapiro.test(subj_level_pwt_obsrep_mo$obs_walk3_mo) #Non-normal
shapiro.test(subj_level_pwt_obsrep_mo$rep_walk3_mo) #Normal (marginal)
shapiro.test(subj_level_pwt_obsrep_mo$obs_1stprod_any_mo) #Non-normal
shapiro.test(subj_level_pwt_obsrep_mo$rep_1stprod_any_mo) #Non-normal

# are rep vs. obs correlated?
cor.test(subj_level_pwt_obsrep_mo$obs_1stpoint_mo, subj_level_pwt_obsrep_mo$rep_1stpoint_mo, method = "kendall")
obs_rep_point_cor <- cor.test(subj_level_pwt_obsrep_mo$obs_1stpoint_mo, subj_level_pwt_obsrep_mo$rep_1stpoint_mo, method = "kendall") %>% tidy()

cor.test(subj_level_pwt_obsrep_mo$obs_walk3_mo, subj_level_pwt_obsrep_mo$rep_walk3_mo, method = "kendall")
obs_rep_walk_cor <- cor.test(subj_level_pwt_obsrep_mo$obs_walk3_mo, subj_level_pwt_obsrep_mo$rep_walk3_mo, method = "kendall") %>% tidy()

cor.test(subj_level_pwt_obsrep_mo$obs_1stprod_any_mo, subj_level_pwt_obsrep_mo$rep_1stprod_any_mo, method = "kendall")
obs_rep_talk_cor <- cor.test(subj_level_pwt_obsrep_mo$obs_1stprod_any_mo, subj_level_pwt_obsrep_mo$rep_1stprod_any_mo, method = "kendall") %>% tidy()

# are rep vs. obs different?
wilcox.test(subj_level_pwt_obsrep_mo$obs_1stpoint_mo, subj_level_pwt_obsrep_mo$rep_1stpoint_mo, conf.int=T, paired = T)
obs_rep_point_diff <- wilcox.test(subj_level_pwt_obsrep_mo$obs_1stpoint_mo, subj_level_pwt_obsrep_mo$rep_1stpoint_mo, conf.int=T, paired = T) %>% tidy()
obs_rep_point_diff_effsize <- effsize::cohen.d(subj_level_pwt_obsrep_mo$obs_1stpoint_mo, subj_level_pwt_obsrep_mo$rep_1stpoint_mo, paired = T)

wilcox.test(subj_level_pwt_obsrep_mo$obs_walk3_mo, subj_level_pwt_obsrep_mo$rep_walk3_mo, conf.int=T, paired = T)
obs_rep_walk_diff <- wilcox.test(subj_level_pwt_obsrep_mo$obs_walk3_mo, subj_level_pwt_obsrep_mo$rep_walk3_mo, conf.int=T, paired = T) %>% tidy()
obs_rep_walk_diff_effsize <- effsize::cohen.d(subj_level_pwt_obsrep_mo$obs_walk3_mo, subj_level_pwt_obsrep_mo$rep_walk3_mo, paired = T)

wilcox.test(subj_level_pwt_obsrep_mo$obs_1stprod_any_mo, subj_level_pwt_obsrep_mo$rep_1stprod_any_mo, conf.int=T, paired = T)
obs_rep_talk_diff <- wilcox.test(subj_level_pwt_obsrep_mo$obs_1stprod_any_mo, subj_level_pwt_obsrep_mo$rep_1stprod_any_mo, conf.int=T, paired = T) %>% tidy()
obs_rep_talk_diff_effsize <- effsize::cohen.d(subj_level_pwt_obsrep_mo$obs_1stprod_any_mo, subj_level_pwt_obsrep_mo$rep_1stprod_any_mo, paired = T)

# vocab rep vs. obs corr by month
# CHI tokens by CDI by month
month_level_pwt_vocab%>%
  filter(!is.na(CHItypes))%>%
  group_by(month)%>%
  summarise(estimate = cor.test(CDInounprod, CHItypes, method = "kendall")$estimate,
            pval = round(cor.test(CDInounprod, CHItypes, method = "kendall")$p.value,3))


## Pointing, Walking, and Talking Onsets
# no zero order correlations except reported talk and point
# walk vs. talk
cor.test(subj_level_pwt_obsrep_mo$obs_1stprod_any_mo,subj_level_pwt_obsrep_mo$obs_walk3_mo, method = "kendall")
cor.test(subj_level_pwt_obsrep_mo$rep_1stprod_any_mo,subj_level_pwt_obsrep_mo$rep_walk3_mo, method = "kendall")
obs_talk_vs_walk_cor <- cor.test(subj_level_pwt_obsrep_mo$obs_1stprod_any_mo,subj_level_pwt_obsrep_mo$obs_walk3_mo, method = "kendall") %>% tidy()
rep_talk_vs_walk_cor <- cor.test(subj_level_pwt_obsrep_mo$rep_1stprod_any_mo,subj_level_pwt_obsrep_mo$rep_walk3_mo, method = "kendall") %>% tidy()

# walk vs. point
cor.test(subj_level_pwt_obsrep_mo$obs_1stpoint_mo, subj_level_pwt_obsrep_mo$obs_walk3_mo, method = "kendall")
cor.test(subj_level_pwt_obsrep_mo$rep_1stpoint_mo, subj_level_pwt_obsrep_mo$rep_walk3_mo, method = "kendall")
obs_point_vs_walk_cor <- cor.test(subj_level_pwt_obsrep_mo$obs_1stpoint_mo, subj_level_pwt_obsrep_mo$obs_walk3_mo, method = "kendall") %>% tidy()
rep_point_vs_walk_cor <- cor.test(subj_level_pwt_obsrep_mo$rep_1stpoint_mo, subj_level_pwt_obsrep_mo$rep_walk3_mo, method = "kendall") %>% tidy()

# talk vs. point
cor.test(subj_level_pwt_obsrep_mo$obs_1stpoint_mo, subj_level_pwt_obsrep_mo$obs_1stprod_any_mo, method = "kendall")
cor.test(subj_level_pwt_obsrep_mo$rep_1stpoint_mo, subj_level_pwt_obsrep_mo$rep_1stprod_any_mo, method = "kendall")
obs_point_vs_talk_cor <- cor.test(subj_level_pwt_obsrep_mo$obs_1stpoint_mo, subj_level_pwt_obsrep_mo$obs_1stprod_any_mo, method = "kendall") %>% tidy()
rep_point_vs_talk_cor <- cor.test(subj_level_pwt_obsrep_mo$rep_1stpoint_mo, subj_level_pwt_obsrep_mo$rep_1stprod_any_mo, method = "kendall") %>% tidy()


## Comparing Vocabulary Before and After Milestone Onset
# vocab of pointers vs non-pointers within month
vocab_point_vs_nonp_comp_rep <- wilcox.test(data = gathervocab_month_level_pwt %>% filter(month=="10",vocab_type=="CDIcomp"), wordcount~repp) %>% tidy()
vocab_point_vs_nonp_comprep_effsize <- effsize::cohen.d(formula = wordcount~repp, data = gathervocab_month_level_pwt %>% filter(month=="10",vocab_type=="CDIprod"))
vocab_point_vs_nonp_comp_meandiff = mean((gathervocab_month_level_pwt %>% filter(month=="10",vocab_type=="CDIcomp",repp==TRUE))$wordcount)-
  mean((gathervocab_month_level_pwt %>% filter(month=="10",vocab_type=="CDIcomp",repp==FALSE))$wordcount)

vocab_point_vs_nonp_rep <- wilcox.test(data = gathervocab_month_level_pwt %>% filter(month=="10",vocab_type=="CDIprod"), wordcount~repp) %>% tidy()
vocab_point_vs_nonp_prodrep_effsize <- effsize::cohen.d(formula = wordcount~repp, data = gathervocab_month_level_pwt %>% filter(month=="10",vocab_type=="CDIprod"))
vocab_point_vs_nonp_prod_meandiff = mean((gathervocab_month_level_pwt %>% filter(month=="10",vocab_type=="CDIprod",repp==TRUE))$wordcount)-
  mean((gathervocab_month_level_pwt %>% filter(month=="10",vocab_type=="CDIprod",repp==FALSE))$wordcount)

vocab_point_vs_nonp_obs <- wilcox.test(data = gathervocab_month_level_pwt %>% filter(month=="11",vocab_type=="CHItypes"), wordcount~obsp) %>% tidy()
vocab_point_vs_nonp_obs_effsize <- effsize::cohen.d(formula = wordcount~obsp, data = gathervocab_month_level_pwt %>% filter(month=="11",vocab_type=="CHItypes"))
vocab_point_vs_nonp_obs_meandiff = mean((gathervocab_month_level_pwt %>% filter(month=="11",vocab_type=="CHItypes",obsp==TRUE))$wordcount)-
  mean((gathervocab_month_level_pwt %>% filter(month=="11",vocab_type=="CHItypes",obsp==FALSE))$wordcount)

# vocab of walkers vs crawlers within month
vocab_walk_vs_crawl_comp_rep <- wilcox.test(data = gathervocab_month_level_pwt %>%filter(month=="11",vocab_type=="CDIcomp"), wordcount~rep3w) %>% tidy()
vocab_walk_vs_crawl_comprep_effsize <- effsize::cohen.d(formula = wordcount~rep3w, data = gathervocab_month_level_pwt %>% filter(month=="11",vocab_type=="CDIcomp"))
vocab_walk_vs_crawl_comp_meandiff = mean((gathervocab_month_level_pwt %>% filter(month=="11",vocab_type=="CDIcomp",rep3w==TRUE))$wordcount)-
  mean((gathervocab_month_level_pwt %>% filter(month=="11",vocab_type=="CDIcomp",rep3w==FALSE))$wordcount)

vocab_walk_vs_crawl_rep <- wilcox.test(data = gathervocab_month_level_pwt %>%filter(month=="11",vocab_type=="CDIprod"), wordcount~rep3w) %>% tidy()
vocab_walk_vs_crawl_prodrep_effsize <-  effsize::cohen.d(formula = wordcount~rep3w, data = gathervocab_month_level_pwt %>% filter(month=="11",vocab_type=="CDIprod"))
vocab_walk_vs_crawl_prod_meandiff = mean((gathervocab_month_level_pwt %>% filter(month=="11",vocab_type=="CDIprod",rep3w==TRUE))$wordcount)-
  mean((gathervocab_month_level_pwt %>% filter(month=="11",vocab_type=="CDIprod",rep3w==FALSE))$wordcount)

vocab_walk_vs_crawl_obs <- wilcox.test(data = gathervocab_month_level_pwt %>%filter(month=="12",vocab_type=="CHItypes"), wordcount~obs3w) %>% tidy()
vocab_walk_vs_crawl_obs_effsize <- effsize::cohen.d(formula = wordcount~obs3w, data = gathervocab_month_level_pwt %>% filter(month=="11",vocab_type=="CHItypes"))
vocab_walk_vs_crawl_obs_meandiff = mean((gathervocab_month_level_pwt %>% filter(month=="12",vocab_type=="CHItypes",obs3w==TRUE))$wordcount)-
  mean((gathervocab_month_level_pwt %>% filter(month=="12",vocab_type=="CHItypes",obs3w==FALSE))$wordcount)


## Longitudinal Models of Point/Walk Status
# Reported Comprehension (CDIcomp>100)
mod0_rep_comp <- lmer(data = month_level_pwt_vocab %>% filter(CDIcomp>100), # obs: 261,  subjs: 41
                      log(CDIcomp)~month+(1|subj), REML = FALSE)
modp_rep_comp <- lmer(data = month_level_pwt_vocab %>% filter(CDIcomp>100),
                      log(CDIcomp)~month+repp+(1|subj), REML = FALSE)
modw_rep_comp <- lmer(data = month_level_pwt_vocab %>% filter(CDIcomp>100),
                      log(CDIcomp)~month+rep3w+(1|subj), REML = FALSE)
modb_rep_comp <- lmer(data = month_level_pwt_vocab %>% filter(CDIcomp>100),
                      log(CDIcomp)~month+repp+rep3w+(1|subj), REML = FALSE)

aov_rep_comp_addpoint <- anova(mod0_rep_comp,modp_rep_comp) %>% tidy() # pointing improves model fit
aov_rep_comp_addwalk <- anova(mod0_rep_comp,modw_rep_comp) %>% tidy() # walking doesn't
aov_rep_comp_w_pw <- anova(modw_rep_comp,modb_rep_comp) %>% tidy() # adding pointing after walking improves fit
aov_rep_comp_p_pw <- anova(modp_rep_comp,modb_rep_comp) %>% tidy() # adding walking after pointing doesn't

# Best rep comp model: pointing
modp_rep_comp_summary <- anova(modp_rep_comp) %>% tidy()
modp_rep_comp_Rsq <- (r.squaredGLMM(modp_rep_comp) %>% tidy())$R2m

# Reported Production (CDIprod>1)
mod0_rep_prod <- lmer(data = month_level_pwt_vocab %>% filter(CDIprod>1), # includes 297 of 528 observations (43/44 subjs)
                      log(CDIprod)~month+(1|subj), REML = FALSE)
modp_rep_prod <- lmer(data = month_level_pwt_vocab %>% filter(CDIprod>1),
                      log(CDIprod)~month+repp+(1|subj), REML = FALSE)
modw_rep_prod <- lmer(data = month_level_pwt_vocab %>% filter(CDIprod>1),
                      log(CDIprod)~month+rep3w+(1|subj), REML = FALSE)
modb_rep_prod <- lmer(data = month_level_pwt_vocab %>% filter(CDIprod>1),
                      log(CDIprod)~month+repp+rep3w+(1|subj), REML = FALSE)

aov_rep_prod_addpoint <- anova(mod0_rep_prod,modp_rep_prod) %>% tidy() # pointing marginally improves fit
aov_rep_prod_addwalk <- anova(mod0_rep_prod,modw_rep_prod) %>% tidy() # walking doesn't improve fit
aov_rep_prod_w_pw <- anova(modw_rep_prod,modb_rep_prod) %>% tidy() # adding pointing after walking marginally improves fit
aov_rep_prod_p_pw <- anova(modp_rep_prod,modb_rep_prod) %>% tidy() # adding walking after pointing doesn't

# Best rep prod model: pointing
modp_rep_prod_summary <- anova(modp_rep_prod) %>% tidy()
modp_rep_prod_Rsq <- (r.squaredGLMM(modp_rep_prod) %>% tidy())$R2m

# Observed Production (CHItypes>1)
mod0_obs <- lmer(data = month_level_pwt_vocab %>% filter(CHItypes>1), # uses only 140 of 528 possible observations (39/44 subjects)
                 log(CHItypes)~month+(1|subj), REML = FALSE)
modp_obs <- lmer(data = month_level_pwt_vocab %>% filter(CHItypes>1),
                 log(CHItypes)~month+obsp+(1|subj), REML = FALSE)
modw_obs <- lmer(data = month_level_pwt_vocab %>% filter(CHItypes>1),
                 log(CHItypes)~month+obs3w+(1|subj), REML = FALSE)
modb_obs <- lmer(data = month_level_pwt_vocab %>% filter(CHItypes>1),
                 log(CHItypes)~month+obsp+obs3w+(1|subj), REML = FALSE)

aov_obs_addpoint <- anova(mod0_obs,modp_obs) %>% tidy()
aov_obs_addwalk <- anova(mod0_obs,modw_obs) %>% tidy()
aov_obs_p_pw <- anova(modp_obs,modb_obs) %>% tidy()
aov_obs_w_pw <- anova(modw_obs,modb_obs) %>% tidy()

# Best obs model: age only
mod0_obs_summary <- anova(mod0_obs) %>% tidy()
mod0_obs_Rsq <- (r.squaredGLMM(mod0_obs) %>% tidy())$R2m


## Models of Pointing and Walking Experience
# Reported Comprehension (17mos only)
mod_pointexp_rep_comp <- lm(data = point_walk_experience %>% filter(month == 17 & noCDI == FALSE & CDIcomp > 30),
                            log(CDIcomp) ~ point_experience)
mod_pointexp_rep_comp_summary <- summary(mod_pointexp_rep_comp) %>% tidy()
mod_walkexp_rep_comp <- lm(data = point_walk_experience %>% filter(month == 17 & noCDI == FALSE & CDIcomp > 30),
                           log(CDIcomp) ~ walk_experience)
mod_walkexp_rep_comp_summary <- summary(mod_walkexp_rep_comp) %>% tidy()
mod_bothexp_rep_comp <- lm(data = point_walk_experience %>% filter(month == 17 & noCDI == FALSE & CDIcomp > 30),
                           log(CDIcomp) ~ point_experience + walk_experience)
mod_bothexp_rep_comp_summary <- summary(mod_bothexp_rep_comp) %>% tidy()

anova(mod_pointexp_rep_comp)
anova(mod_walkexp_rep_comp)
aov_pointexp_both <- anova(mod_pointexp_rep_comp,mod_bothexp_rep_comp) %>% tidy()
anova(mod_walkexp_rep_comp,mod_bothexp_rep_comp)

# Best rep comp model: pointing experience
mod_pointexp_rep_comp_summary <- summary(mod_pointexp_rep_comp) %>% tidy()
# plot(mod_pointexp_rep_comp)
# One subject is a residual outlier (low CDIcomp), and Cook's D and DFBETA > 1, so removing this point and re-running
# dfbetasPlots(mod_pointexp_rep_comp)

# Reported Production (17mos only)
mod_pointexp_rep_prod <- lm(data = point_walk_experience %>% filter(month == 17 & noCDI == FALSE),
                            log(CDIprod+1) ~ point_experience)
mod_pointexp_rep_prod_summary <- summary(mod_pointexp_rep_prod) %>% tidy()
mod_walkexp_rep_prod <- lm(data = point_walk_experience %>% filter(month == 17 & noCDI == FALSE),
                           log(CDIprod+1) ~ walk_experience)
mod_walkexp_rep_prod_summary <- summary(mod_walkexp_rep_prod) %>% tidy()
mod_bothexp_rep_prod <- lm(data = point_walk_experience %>% filter(month == 17 & noCDI == FALSE),
                           log(CDIprod+1) ~ point_experience + walk_experience)
mod_bothexp_rep_prod_summary <- summary(mod_bothexp_rep_prod) %>% tidy()

anova(mod_pointexp_rep_prod)
anova(mod_walkexp_rep_prod)
anova(mod_pointexp_rep_prod,mod_bothexp_rep_prod)
anova(mod_walkexp_rep_prod,mod_bothexp_rep_prod)

# Best model: pointing experience (marginal)
mod_pointexp_rep_prod_summary <- summary(mod_pointexp_rep_prod) %>% tidy()
# plot(mod_pointexp_rep_prod)
# outlierTest(mod_pointexp_rep_prod)
# dfbetasPlots(mod_pointexp_rep_prod)

# Observed Production (17mos only)
mod_pointexp_obs <- lm(data = point_walk_experience %>% filter(month == 17),
                       log(CHItypes+1) ~ point_experience)
mod_pointexp_obs_summary <- summary(mod_pointexp_obs) %>% tidy()
mod_walkexp_obs <- lm(data = point_walk_experience %>% filter(month == 17),
                      log(CHItypes+1) ~ walk_experience)
mod_walkexp_obs_summary <- summary(mod_walkexp_obs) %>% tidy()
mod_bothexp_obs <- lm(data = point_walk_experience %>% filter(month == 17),
                      log(CHItypes+1) ~ point_experience + walk_experience)
mod_bothexp_obs_summary <- summary(mod_bothexp_obs) %>% tidy()

anova(mod_pointexp_obs)
anova(mod_walkexp_obs)
anova(mod_pointexp_obs,mod_bothexp_obs)
anova(mod_walkexp_obs,mod_bothexp_obs)
# Best model: lol none

# How many kids are in above models, but can't point or walk?
# reported 
point_walk_experience %>% 
  filter(CDIcomp > 100, repp == F) %>%
  distinct(subj)                            # 5 kids

point_walk_experience %>%
  filter(CDIprod > 1, repp == F) %>%
  distinct(subj)                            # 10 kids

point_walk_experience %>%
  filter(CDIcomp > 100, rep3w == F) %>%
  distinct(subj)                            # 13 kids

point_walk_experience %>%
  filter(CDIprod > 1, rep3w == F) %>%
  distinct(subj)                            # 16 kids

# observed
point_walk_experience %>%
  filter(CHItypes > 1, obsp == F) %>%
  distinct(subj)                            # 7 kids

point_walk_experience %>%
  filter(CHI > 1, obsp == F) %>%
  distinct(subj)                            # 8 kids

point_walk_experience %>%
  filter(CHItypes > 1, obs3w == F) %>%
  distinct(subj)                            # 11 kids

point_walk_experience %>%
  filter(CHI > 1, obs3w == F) %>%
  distinct(subj)                            # 14 kids


## METHODS SECTION  ---------------------------------------------
## Interrater reliability
rater1 = c(10,12,14,14,10,12,14,11)
rater2 = c(10,12,14,14,10,12,14,13)
#raters asked to find first observed point in both in-lab videos and in home videos, because the infant's first point could have happened in either
#percent agreement and cohens weighted kappa calculated based on 8 possible values: first in-lab point and first home point for four randomly selected subjects

point_rel <-kappa2(cbind(rater1,rater2), weight = "squared") 
point_agree <- agree(cbind(rater1,rater2))

## How much of each CHI file was double-annotated?
all_bl <- read_feather("data/all_basiclevel.feather")
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

chi_counts <- all_bl %>% 
  filter(speaker == "CHI") %>%
  group_by(subj, month, audio_video) %>% 
  count()

range_chi <- range(chi_counts$n)
median_chi <- median(chi_counts$n)
modal_chi <- getmode(chi_counts$n)

full_chi_count <- chi_counts %>% nrow()#number of files with any CHIs

chi_count_less10 <- chi_counts %>% #number with fewer than 10 CHIs, indicating that we did more than 10% of the CHIs
  filter(n < 10) %>% 
  nrow()

percent_less10 <- percent(chi_count_less10/full_chi_count) #percentage of files with <10 CHIs

output <- tibble(x = 1:nrow(chi_counts)) %>%
  mutate(value = 0,
         total = chi_counts$n)

for (i in 1:nrow(chi_counts)) {
  output$value[i] <- ifelse(chi_counts$n[i] <= 10, 1, ceiling(chi_counts$n[i]/10))
}
output <- output %>% 
  mutate(percdouble = value/total)

avgamt_doublecode <- percent(mean(output$percdouble))

min_doublecode <- percent(min(output$percdouble), accuracy = 1)
max_doublecode <- percent(max(output$percdouble), accuracy = 1)

getmode(output$value)
range(output$value)

## Correlating CDI noun vocab and CDI total vocab
cor_noun_to_total_prod <- cor.test(month_level_pwt_vocab$CDIprod, month_level_pwt_vocab$CDInounprod) %>% tidy()
cor_noun_to_total_comp <- cor.test(month_level_pwt_vocab$CDIcomp, month_level_pwt_vocab$CDInouncomp) %>% tidy()

month_level_pwt_vocab <- month_level_pwt_vocab %>% 
  mutate(noun_prop_prod = CDInounprod/CDIprod,
         noun_prop_comp = CDInouncomp/CDIcomp)

mean_prop_noun_prod <- mean(na.omit(month_level_pwt_vocab$noun_prop_prod))
mean_prop_noun_comp <- mean(na.omit(month_level_pwt_vocab$noun_prop_comp))

## Missing questionnaires
# Total number missing: 27 from CDI, 27 from motor
missing_surveys %>% group_by(questionnaire)%>% summarise(num_missing = n())

# CDI: missing 27 surveys from 12 subjs
missing_cdi_count <- as.data.frame(table(month_level_pwt_vocab$noCDI))$Freq[2]
missing_surveys %>% filter(questionnaire=="cdi") %>% group_by(subj) %>% summarize(num_surveys=n())

# Motor: missing 27 surveys from from 12 subjs
missing_motor_count <- nrow(missing_surveys %>% filter(questionnaire == "motor"))
missing_surveys %>% filter(questionnaire=="motor") %>% group_by(questionnaire,subj) %>% summarize(num_subjs=n())


# SUPPLEMENTAL MATERIALS  ---------------------------------------------
## Predicting Vocabulary with Age and Walk Experience: A Replication of Walle & Campos (2014)
mod0_comp <- lmer(data = (point_walk_experience %>% filter(noCDI == FALSE)),
                  CDIcomp ~ month + (1|subj), REML = FALSE)

# W&C models of walk experience on receptive vocabulary
mod_walk_comp <- lmer(data = (point_walk_experience %>% filter(noCDI == FALSE)),
                      CDIcomp ~ month + walk_experience_zeros + (1|subj), REML = FALSE)
summary(mod_walk_comp)
mod_walk_comp_aov <- anova(mod_walk_comp) %>% tidy()
anova(mod0_comp,mod_walk_comp)
mod_walk_log <- lmer(data = (point_walk_experience %>% filter(noCDI == FALSE)),
                     CDIcomp ~ month + walk_experience_zeros + log(walk_experience_zeros+0.001) + (1|subj), REML = FALSE)
summary(mod_walk_log)
mod_walk_log_aov <- anova(mod_walk_comp,mod_walk_log) %>% tidy()

# W&C models of walk experience on productive vocabulary
mod_walk_prod <- lmer(data = (point_walk_experience %>% filter(noCDI == FALSE)),
                      CDIprod ~ month + walk_experience_zeros + (1|subj), REML = FALSE)
summary(mod_walk_prod)
mod_walk_prod_square <- lmer(data = (point_walk_experience %>% filter(noCDI == FALSE)),
                             CDIprod ~ month + poly(walk_experience_zeros, 2) + (1|subj), REML = FALSE)
summary(mod_walk_prod_square)
mod_walk_prod_cube <- lmer(data = (point_walk_experience %>% filter(noCDI == FALSE)),
                           CDIprod ~ month + poly(walk_experience_zeros, 3) + (1|subj), REML = FALSE)
summary(mod_walk_prod_cube)
mod_walk_prod_square_aov <- anova(mod_walk_prod, mod_walk_prod_square) %>% tidy()
mod_walk_prod_cube_aov <- anova(mod_walk_prod_square, mod_walk_prod_cube) %>% tidy()
mod_walk_prod_cube_tidy <- anova(mod_walk_prod_cube) %>% tidy()
