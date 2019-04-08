library(tidyverse)
library(cowplot)
month_level_pwt_vocab <- read_csv("data/month_level_pwt_vocab_ransubj.csv")
subj_level_pwt_obsrep_mo <- read_csv("data/subjlevel_pwt_obsrep_mo_ransubj.csv")
gathervocab_month_level_pwt <- read_csv("data/gathervocab_month_level_pwt_ransubj.csv")

# obs vs. rep dist first walk ---------------------------------------------
obsrep_mo_firstwalk_hist <- ggplot(data= subj_level_pwt_obsrep_mo %>%
                                dplyr::select(subj,obs_walk3_mo, rep_walk3_mo)%>%
                                gather(key = obs_rep, value = age_months, obs_walk3_mo, rep_walk3_mo),
                              aes(x=age_months, fill=obs_rep)) +
  geom_density(alpha=.7, position="identity", size=1, stat = "count")+
  geom_vline(data = subj_level_pwt_obsrep_mo, aes(xintercept=mean(obs_walk3_mo, na.rm=T)),
             color="#1173fc", linetype="dashed", size=1)+
  geom_vline(data = subj_level_pwt_obsrep_mo, aes(xintercept=mean(rep_walk3_mo, na.rm=T)),
             color="#aacaf7", linetype="dashed", size=1)+
  theme_bw(base_size=18)+
  scale_x_continuous(breaks = seq(6,18,1), limits = c(6,18), name = "Age at First Steps (months)")+
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20))+
  #theme(legend.justification=c(1,1), legend.position="none", legend.text = element_text(size=10))+
  scale_fill_manual(name="",
                      labels=c("Observed", "Reported"),
                      values = c("#1173fc", "#aacaf7"))
 

# obs vs. rep dist first point ---------------------------------------------
obsrep_mo_firstpoint_hist <- ggplot(data= subj_level_pwt_obsrep_mo %>%
                                 dplyr::select(subj,obs_1stpoint_mo, rep_1stpoint_mo)%>%
                                 gather(key = obs_rep, value = age_months, obs_1stpoint_mo, rep_1stpoint_mo),
                               aes(x=age_months, fill=obs_rep)) +
  geom_density(alpha=.7, position="identity", size=1, stat = "count")+
  geom_vline(data = subj_level_pwt_obsrep_mo, aes(xintercept=mean(obs_1stpoint_mo, na.rm=T)),
             color="#f23a02", linetype="dashed", size=1)+
  geom_vline(data = subj_level_pwt_obsrep_mo, aes(xintercept=mean(rep_1stpoint_mo, na.rm=T)),
             color="#FFAC59", linetype="dashed", size=1)+
  theme_bw(base_size=18)+
  scale_x_continuous(breaks = seq(6,18,1), limits = c(6,18),  name = "Age at First Point (months)")+
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,15))+
  #theme(legend.justification=c(1,1), legend.position="none")+
  scale_fill_manual(name="",
                      labels=c("Observed", "Reported"),
                      values=c("#f23a02", "#FFAC59"))


# obs vs. rep dist first word ---------------------------------------------
obsrep_mo_firstword_hist <- ggplot(data= subj_level_pwt_obsrep_mo %>%
                                dplyr::select(subj, rep_1stprod_any_mo, obs_1stprod_any_mo)%>%
                                #dplyr::select(subj, rep_1stprodN_mo, obs_1stprodN_mo)%>%
                                gather(key = obs_rep, value = age_months, rep_1stprod_any_mo, obs_1stprod_any_mo),
                                #gather(key = obs_rep, value = age_months, rep_1stprodN_mo, obs_1stprodN_mo),
                              aes(x=age_months, fill=obs_rep)) +
  geom_density(alpha=.7, position="identity", size=1, stat="count") +
  geom_vline(data = subj_level_pwt_obsrep_mo, aes(xintercept=mean(obs_1stprod_any_mo, na.rm=T)),
  #geom_vline(data = subj_level_pwt_obsrep_mo, aes(xintercept=mean(obs_1stprodN_mo, na.rm=T)),
             color="#00B59B", linetype="dashed", size=1)+
  geom_vline(data = subj_level_pwt_obsrep_mo, aes(xintercept=mean(rep_1stprod_any_mo, na.rm=T)),
  #geom_vline(data = subj_level_pwt_obsrep_mo, aes(xintercept=mean(rep_1stprodN_mo, na.rm=T)),
             color="#9ae298", linetype="dashed", size=1)+
  theme_bw(base_size=18)+
  scale_x_continuous(breaks = seq(6,18,1), limits = c(6,18), name = "Age at First Word (months)")+
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,15))+
  #theme(legend.justification=c(1,1), legend.position="none")+
  scale_fill_manual(name="",
                      labels=c("Observed", "Reported"),
                      values=c("#00B59B","#9ae298"))

# composite density plots IN PAPER ----
composite_density <- plot_grid(obsrep_mo_firstwalk_hist + ylab(""),
          obsrep_mo_firstpoint_hist,
          obsrep_mo_firstword_hist + ylab(""), 
          nrow = 3, 
          labels = NULL,
          hjust=-.1,
          vjust=.5, 
          rel_widths = c(1,1,1),
          rel_heights = c(1,1,1))
ggsave(composite_density, filename = "composite_density.png", dpi = 350, height = 8, width = 7, units = "in")

# corr obs vs. rep vocab, by month IN PAPER ----
obsrep_mo_vocab <- ggplot(month_level_pwt_vocab%>%filter(!is.na(CHItypes) & month>11), 
                          aes(CHItypes, CDInounprod, color = subj))+
  geom_point(size = 3, shape = 1)+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~month, scales = "free", nrow=2)+
  theme_bw(base_size = 18)+
  ylab("Reported Productive Noun Vocabulary (CDI)")+
  xlab("Observed Productive Noun Vocabulary (Home Recordings)")+
  stat_smooth(method = "lm", aes(group=1), fill = "#9AE298", color = "#00B59B")+
  guides(color=F)
ggsave(obsrep_mo_vocab, filename = "obsrep_mo_vocab.png", dpi = 350, height = 8, width = 7, units = "in")

# corr obs vs. rep walk, by month ----
obsrep_mo_walk_cor <- ggplot(subj_level_pwt_obsrep_mo, aes(obs_walk3_mo, rep_walk3_mo, group = subj, color = subj))+
  geom_jitter(width=.25, height=.25, size=3, shape=1)+
  stat_smooth(method = "lm", aes(group=1), fill = "#AACAF7", color = "#1173FC")+
  guides(color=F)+
  theme_bw(base_size=16)+
  theme(plot.margin = margin(t = 0.01, b = 0.01, l = 0.01, r = 0.01, "cm")) +
  scale_x_continuous(breaks = seq(6,18,2))+
  scale_y_continuous(breaks = seq(6,18,2), limits = c(6,17))+
  labs(x= "Observed Walk", y="Reported Walk")

# corr obs vs. rep point, by month ----
obsrep_mo_point_cor <- ggplot(subj_level_pwt_obsrep_mo, aes(obs_1stpoint_mo, rep_1stpoint_mo,group = subj, color = subj))+
  geom_jitter(width=.2, height=.2, size=3, shape = 1)+
  stat_smooth(method = "lm", aes(group=1), fill = "#FFAC59", color = "#F23A02")+
  guides(color=F)+
  theme_bw(base_size=16)+
  theme(plot.margin = margin(t = 0.01, b = 0.01, l = 0.01, r = 0.02, "cm"))+
        #axis.text.y = element_blank(),
        #axis.ticks = element_blank()) +
  scale_x_continuous(breaks = seq(6,18,2))+
  scale_y_continuous(breaks = seq(6,18,2), limits = c(6,17)) +
  labs(x= "Observed Point", y="Reported Point")

# corr obs vs. rep first word, by month ----
obsrep_mo_prodN_cor <- ggplot(subj_level_pwt_obsrep_mo, aes(obs_1stprodN_mo, rep_1stprodN_mo,group = subj, color = subj))+
  geom_jitter(width=.2, height=.2, size=3, shape = 1)+
  stat_smooth(method = "lm", aes(group=1), fill = "#9AE298", color = "#00B59B")+
  guides(color=F)+
  theme_bw(base_size=16)+
  theme(plot.margin = margin(t = 0.01, b = 0.01, l = 0.01, r = 0.01, "cm"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  scale_x_continuous(breaks = seq(6,18,2))+
  scale_y_continuous(breaks = seq(6,18,2), limits = c(6,17))+
  labs(x= "Observed 1st Noun",y="Reported 1st Noun")

obsrep_mo_prod_any_cor <- ggplot(subj_level_pwt_obsrep_mo, aes(obs_1stprod_any_mo, rep_1stprod_any_mo,group = subj, color = subj))+
  geom_jitter(width=.2, height=.2, size=3, shape = 1)+
  stat_smooth(method = "lm", aes(group=1), fill = "#9AE298", color = "#00B59B")+
  guides(color=F)+
  theme_bw(base_size=16)+
  theme(plot.margin = margin(t = 0.01, b = 0.01, l = 0.01, r = 0.01, "cm"))+
        #axis.text.y = element_blank(),
        #axis.ticks = element_blank()) +
  scale_x_continuous(breaks = seq(6,18,2))+
  scale_y_continuous(breaks = seq(6,18,2), limits = c(6,17))+
  labs(x= "Observed 1st word",y="Reported 1st word")

# composite obs vs. rep correlations IN PAPER ----
composite_corr <- plot_grid(obsrep_mo_point_cor + ylab("Reported First Event of Each Type"),
          obsrep_mo_walk_cor + ylab(""),
          #obsrep_mo_prodN_cor+ ylab(""), 
          obsrep_mo_prod_any_cor+ ylab(""),
          nrow = 1) +
  coord_fixed(ratio = 1/2)
ggsave(composite_corr, filename = "composite_corr.png", dpi = 350, height = 7, width = 8, units = "in")

#reported walk & CDI----
report_walk_cdicomp <- ggplot(gathervocab_month_level_pwt%>%
                                filter(vocab_type=="CDIcomp") %>% 
                                filter(month<14 & month>9),
                              aes(rep3w, wordcount, fill = rep3w))+
  facet_grid(vocab_type~ month+rep3w_label, scales ="free_y", labeller = labeller(vocab_type=c(`CDIcomp` = "Receptive CDI")))+
  stat_summary(fun.y="mean", geom = "bar", position = "dodge", color = "black")+ 
  guides(fill=F)+
  stat_summary(fun.data="mean_se", position = position_dodge(width = .9))+
  theme_bw(base_size=14)+
  theme(axis.text.x=element_text(size=9),
        strip.text.x = element_blank(),
        strip.text.y = element_blank())+
  scale_y_log10()+
  xlab("Reported As Walker")+ylab("log Receptive Vocab\n(CDI)") +
  scale_fill_manual(name="",
                    labels="",
                    values=c("grey90","#AACAF7"))

report_walk_cdiprod <- ggplot(gathervocab_month_level_pwt%>%
                                filter(vocab_type=="CDIprod") %>% 
                                filter(month<14 & month>9),
                              aes(rep3w, wordcount, fill = rep3w))+
  facet_grid(vocab_type~ month+rep3w_label, scales ="free_y", 
             labeller = labeller(vocab_type=c(`CDIprod` = "Productive CDI")))+
  stat_summary(fun.y="mean", geom = "bar", position = "dodge", color = "black")+ 
  guides(fill=F)+
  stat_summary(fun.data="mean_se", position = position_dodge(width = .9))+
  theme_bw(base_size=14)+
  theme(axis.text.x=element_blank(),
        strip.text.x = element_text(size=14),
        strip.text.y = element_blank())+
  xlab(NULL)+
  ylab("Productive Vocab\n(CDI)") +
  scale_fill_manual(name="",
                    labels="",
                    values=c("grey90","#AACAF7"))

#obs walk and chitypes----
obswalk_chitypes <- ggplot(gathervocab_month_level_pwt%>%filter(vocab_type%in%c("CHItypes") & month<15 & month>10),
                           aes(obs3w,wordcount, fill = obs3w))+
  facet_grid(vocab_type~month+obs3w_label, scales ="free", 
             labeller = labeller(vocab_type=c(`CHItypes`="# CHI types", `CHI` = "# CHI tokens")))+
  stat_summary(fun.y=mean, na.rm=T, geom = "bar", position = "dodge", color = "black")+guides(fill=F)+
  theme_bw(base_size=14)+
  theme(axis.text.x=element_text(size=9),
        strip.text.x = element_text(size=14),
        strip.text.y = element_blank())+
  #scale_y_log10()+
  stat_summary(fun.data="mean_se", position = position_dodge(width = .9))+
  xlab("Observed as Walker")+ylab("Productive Vocab\n(observed)") +
  scale_fill_manual(name="",
                    labels="",
                    values=c("grey90","#1173FC"))

#composite walk graph IN PAPER ----
obs_rep_walks <- plot_grid(report_walk_cdiprod,
                           report_walk_cdicomp,
                           obswalk_chitypes, ncol = 1)
ggsave(obs_rep_walks, filename = "obs_rep_walks.png", dpi = 350, height = 10, width = 8, units = "in")

#reported point & CDI comp----
report_point_cdicomp <- ggplot(gathervocab_month_level_pwt %>%
                                 filter(vocab_type=="CDIcomp") %>% 
                                 filter(month<13 & month>8),
                               aes(repp, wordcount, fill = repp))+
  facet_grid(vocab_type ~ month+repp_label, scales ="free_y", labeller = labeller(vocab_type=c(`CDIcomp` = "Receptive CDI")))+
  stat_summary(fun.y="mean", geom = "bar", position = "dodge", color = "black")+guides(fill=F)+
  stat_summary(fun.data="mean_se", position = position_dodge(width = .9))+
  scale_y_log10()+
  theme_bw(base_size=16)+
  theme(axis.text.x=element_blank(),
        strip.text.x = element_text(size=14),
        strip.text.y = element_blank())+
  xlab(NULL)+ylab("Receptive Vocab\n(CDI)") +
  scale_fill_manual(name="",
                    labels="",
                    values=c("grey90","#FFAC59"))

#reported point & CDI prod----
report_point_cdiprod <- ggplot(gathervocab_month_level_pwt %>%
                                 filter(vocab_type=="CDIprod") %>% 
                                 filter(month<13 & month>8),
                               aes(repp, wordcount, fill = repp))+
  facet_grid(vocab_type ~ month+repp_label, scales ="free_y", 
             labeller = labeller(vocab_type=c(`CDIprod` = "Productive CDI")))+
  stat_summary(fun.y="mean", geom = "bar", position = "dodge", color = "black")+guides(fill=F)+
  stat_summary(fun.data="mean_se", position = position_dodge(width = .9))+
 # scale_y_log10()+
  theme_bw(base_size=16)+
  theme(axis.text.x=element_text(size=10),
        strip.text.x = element_blank(),
        strip.text.y = element_blank())+
  xlab("Reported as Pointer")+ylab("Productive Vocab\n(CDI)\n") +
  scale_fill_manual(name="",
                    labels="",
                    values=c("grey90","#FFAC59"))

#obs point and chitypes----
obspoint_chitypes <- ggplot(gathervocab_month_level_pwt%>%filter(vocab_type == "CHItypes" & month<15 & month>10),
                            aes(obsp, wordcount, fill = obsp))+
  facet_grid(vocab_type~month+obsp_label, 
            scales ="free", 
             labeller = labeller(vocab_type=c(`CHItypes`="# CHI types", `CHI` = "# CHI tokens")))+
  stat_summary(fun.y=mean, na.rm=T, geom = "bar", position = "dodge", color = "black")+guides(fill=F)+
  stat_summary(fun.data="mean_se", position = position_dodge(width = .9))+
  #scale_y_log10()+
  theme_bw(base_size=16)+
  theme(axis.text.x=element_text(size=9),
        strip.text.x = element_text(size=14),
        strip.text.y = element_blank()) +
  xlab("Observed as Pointer")+ ylab("Productive Vocab\n(observed)") +
  scale_fill_manual(name="",
                    labels="",
                    values=c("grey90","#F23A02"))

#to find descriptive numbers for which months vocab was higher from 10%-90% of kids having/not having the skill -----
obswalk_vocab_descriptive <- (gathervocab_month_level_pwt%>%filter(vocab_type == "CHItypes"))%>%
  group_by(month, obs3w) %>% summarise(mean = mean(wordcount, na.rm=TRUE))

repwalk_vocab_descriptive_comp <- (gathervocab_month_level_pwt%>%filter(vocab_type == "CDIcomp"))%>%
  group_by(month, rep3w) %>% summarise(mean = mean(wordcount, na.rm=TRUE))

repwalk_vocab_descriptive_prod <- (gathervocab_month_level_pwt%>%filter(vocab_type == "CDIprod"))%>%
  group_by(month, rep3w) %>% summarise(mean = mean(wordcount, na.rm=TRUE))

obspoint_vocab_descriptive <- (gathervocab_month_level_pwt%>%filter(vocab_type == "CHItypes"))%>%
  group_by(month, obsp) %>% summarise(mean = mean(wordcount, na.rm=TRUE))

reppoint_vocab_descriptive_comp <- (gathervocab_month_level_pwt%>%filter(vocab_type == "CDIcomp"))%>%
  group_by(month, repp) %>% summarise(mean = mean(wordcount, na.rm=TRUE))

reppoint_vocab_descriptive_prod <- (gathervocab_month_level_pwt%>%filter(vocab_type == "CDIprod"))%>%
  group_by(month, repp) %>% summarise(mean = mean(wordcount, na.rm=TRUE))

#composite point graph IN PAPER ----
obs_rep_points <- plot_grid(report_point_cdicomp,
                            report_point_cdiprod,
                            obspoint_chitypes, ncol = 1)
ggsave(obs_rep_points, filename = "obs_rep_points.png", dpi = 350, height = 10, width = 8, units = "in")

#kinda cool graph----
ggplot(month_level_pwt_vocab, aes(month, CHItypes, colour = subj)) + geom_point() +
  facet_wrap( ~ subj, scales = 'free_y') + geom_vline(aes(xintercept = obs_1stpoint_mo, color = "red")) +
  geom_vline(
    aes(xintercept = obs_walk3_mo, linetype = "solid"),
    color = "darkgreen",
    linetype = "longdash"
  ) + theme_bw(base_size = 14) + guides(color = F)


# walk and point experience, age held constant linear regs ----

point_walk_experience <- read_csv("data/point_walk_experience_ransubj.csv")

# reported 
rep_point_exp_CDIcomp <- ggplot(point_walk_experience %>% 
                              filter(month == "17", noCDI == F), mapping = aes(x = point_experience, 
                                                                               y = log(CDIcomp), color = subj)) +
  geom_jitter(width=.25, height=.25, size=3, shape=21)+
  stat_smooth(method = "lm", fill = "#FFAC59", color = "#f23a02")+
  guides(color=F)+
  theme_bw(base_size=16)+
  #theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(0,12,1)) +
  #scale_y_continuous(breaks = seq(1,6,1), limits = c(3,6.5)) +
  labs(x= "Months of Point Experience", y = "log of Receptive Vocab (CDI)")

rep_point_exp_CDIprod <- ggplot(point_walk_experience %>% 
                              filter(month == "17", noCDI == F), mapping = aes(x = point_experience, 
                                                                               y = log(CDIprod), color = subj)) +
  geom_jitter(width=.25, height=.25, size=3, shape=21)+
  stat_smooth(method = "lm", fill = "#FFAC59", color = "#f23a02")+
  guides(color=F)+
  theme_bw(base_size=16)+
  #theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(0,12,1)) +
  #scale_y_continuous(breaks = seq(1,6,1), limits = c(3,6.5)) +
  labs(x= "Months of Point Experience", y = "log of Productive Vocab (CDI)")

rep_walk_exp_CDIcomp <- ggplot(point_walk_experience %>% 
                             filter(month == "17", noCDI == F), mapping = aes(x = walk_experience, 
                                                                              y = log(CDIcomp), color = subj)) +
  geom_jitter(width=.25, height=.25, size=3, shape=21)+
  stat_smooth(method = "lm", fill = "#AACAF7", color = "#1173FC")+
  guides(color=F)+
  theme_bw(base_size=16)+
  #theme(plot.margin = margin(t = 0.01, b = 0.01, l = 0.01, r = 0.01, "cm")) +
  scale_x_continuous(breaks = seq(0,12,1)) +
  labs(x= "Months of Walk Experience", y = "log of Receptive Vocab (CDI)")

rep_walk_exp_CDIprod <- ggplot(point_walk_experience %>% 
                             filter(month == "17", noCDI == F), mapping = aes(x = walk_experience, 
                                                                              y = log(CDIprod), color = subj)) +
  geom_jitter(width=.25, height=.25, size=3, shape=21)+
  stat_smooth(method = "lm", fill = "#AACAF7", color = "#1173FC")+
  guides(color=F)+
  theme_bw(base_size=16)+
  #theme(plot.margin = margin(t = 0.01, b = 0.01, l = 0.01, r = 0.01, "cm")) +
  scale_x_continuous(breaks = seq(0,12,1)) +
  labs(x= "Months of Walk Experience", y = "log of Productive Vocab (CDI)")

# some hacky nonsense to trick cowplot into doing what I want
pointwalk <- read_csv("data/tinyfakedata.csv")

tricking_cowplot_into_giving_me_a_legend <- ggplot(data = pointwalk, mapping = aes(x = Value, fill = Type)) +
  geom_histogram(stat = "count") +
  scale_fill_manual(values = c("#F23A02", "#1173fc"))

blankplot <- ggplot(data = pointwalk, mapping = aes(fill = Type))
## combined -----

rep_receptive_pw_experience <- ggplot(point_walk_experience %>% 
                                        filter(month == "17", noCDI == F)) +
  geom_jitter(mapping = aes(x = point_experience, 
                            y = CDIcomp),
              color = "#FFAC59", width=0, height=.25, size=4, shape=1)+
  geom_jitter(mapping = aes(x = walk_experience, 
                            y = CDIcomp),
              color = "#AACAF7", width=0, height=.25, size=4, shape=1) +
  stat_smooth(mapping = aes(x = point_experience, 
              y = CDIcomp), method = "lm", fill = "#FFAC59", color = "#f23a02") +
  stat_smooth(mapping = aes(x = walk_experience, 
                            y = CDIcomp), method = "lm", fill = "#AACAF7", color = "#1173FC") +
  theme_bw(base_size=16)+
  #theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(0,12,1)) +
  scale_y_log10() +
  #scale_y_continuous(breaks = seq(1,6,1), limits = c(3,6.5)) +
  labs(x= "Months of Experience", y = "log Receptive Vocab (CDI)") 

rep_productive_pw_experience <- ggplot(point_walk_experience %>% 
                                        filter(month == "17", noCDI == F)) +
  geom_jitter(mapping = aes(x = point_experience, 
                            y = CDIprod),
              color = "#FFAC59", width=0, height=.25, size=4, shape=1)+
  geom_jitter(mapping = aes(x = walk_experience, 
                            y = CDIprod),
              color = "#AACAF7", width=0, height=.25, size=4, shape=1) +
  stat_smooth(mapping = aes(x = point_experience, 
                            y = CDIprod), method = "lm", fill = "#FFAC59", color = "#f23a02") +
  stat_smooth(mapping = aes(x = walk_experience, 
                            y = CDIprod), method = "lm", fill = "#AACAF7", color = "#1173FC") +
  theme_bw(base_size=16)+
  #theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(0,12,1)) +
  scale_y_log10() +
  #scale_y_continuous(breaks = seq(1,6,1), limits = c(3,6.5)) +
  labs(x= "Months of Experience", y = "log Productive Vocab (CDI)") 

# observed
obs_point_exp_types <- ggplot(point_walk_experience %>% 
                                  filter(month == "17", noCDI == F), mapping = aes(x = obs_point_exp, 
                                                                                   y = log(CHItypes), color = subj)) +
  geom_jitter(width=.25, height=.25, size=3, shape=21)+
  stat_smooth(method = "lm", fill = "#FFAC59", color = "#f23a02")+
  guides(color=F)+
  theme_bw(base_size=16)+
  #theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(0,12,1)) +
  #scale_y_continuous(breaks = seq(1,6,1), limits = c(3,6.5)) +
  labs(x= "Months of Point Experience", y = "log of Productive Vocab (types)")

obs_walk_exp_types <- ggplot(point_walk_experience %>% 
                                 filter(month == "17", noCDI == F), mapping = aes(x = obs_walk_exp, 
                                                                                  y = log(CHItypes), color = subj)) +
  geom_jitter(width=.25, height=.25, size=3, shape=21)+
  stat_smooth(method = "lm", fill = "#AACAF7", color = "#1173FC")+
  guides(color=F)+
  theme_bw(base_size=16)+
  #theme(plot.margin = margin(t = 0.01, b = 0.01, l = 0.01, r = 0.01, "cm")) +
  scale_x_continuous(breaks = seq(0,12,1)) +
  labs(x= "Months of Walk Experience", y = "log of Productive Vocab (types)")

## combined
obs_pw_experience_types <- ggplot(point_walk_experience %>% 
                                filter(month == "17", noCDI == F)) +
  geom_jitter(mapping = aes(x = obs_point_exp, 
                            y = CHItypes), color = "#f23a02", width=.25, height=.25, size=4, shape=1)+
  stat_smooth(mapping = aes(x = obs_point_exp, 
                            y = CHItypes), method = "lm", fill = "#FFAC59", color = "#f23a02")+
  geom_jitter(mapping = aes(x = obs_walk_exp, 
                            y = CHItypes), color = "#1173FC", width=.25, height=.25, size=4, shape=1)+
  stat_smooth(mapping = aes(x = obs_walk_exp, 
                            y = CHItypes), method = "lm", fill = "#AACAF7", color = "#1173FC")+
  theme_bw(base_size=16)+
  #theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(0,12,1)) +
  #scale_y_continuous(breaks = seq(1,6,1), limits = c(3,6.5)) +
  scale_y_log10() +
  labs(x= "Months of Experience", y = "log Productive Vocab (types)")

## IN PAPER----
legend <- get_legend(tricking_cowplot_into_giving_me_a_legend)

age_held_constant_pw_exp <- plot_grid(rep_receptive_pw_experience,
                                      NULL,
                                      rep_productive_pw_experience,
                                      legend,
                                      obs_pw_experience_types,
                                      NULL,
                                      nrow = 3,
                                      rel_widths = c(3,.3))
ggsave(age_held_constant_pw_exp, filename = "age_held_constant_pw_exp.png", dpi = 350, height = 10, width = 7, units = "in")


##### SUPPLEMENTARY MATERIALS #####
# vocab by month, walk experience, point experience
point_walk_experience <- read_csv("data/point_walk_experience_ransubj.csv")
wallecampos <- read_csv("data/wallecampos.csv")

vocab_by_month <- ggplot(point_walk_experience,aes(month,(CDIprod+1))) +
  geom_jitter(color="#DCDCDC",shape=4,height = 0.5,width=0)+
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = .4)+
  stat_summary(fun.y="mean", colour="black", geom="line")+
  scale_y_log10(limits=c(1,110))+
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks=seq(6,18,2))+
  guides(color = F)+
  labs(x=NULL,y="Productive vocab (log)")

vocab_by_point_exp <- ggplot((point_walk_experience %>% filter(point_experience>-7 & point_experience<7)),aes(x=point_experience,y=(CDIprod+1))) +
  scale_y_log10(limits=c(1,110))+
  geom_jitter(color="#DCDCDC",shape=4,height = 0.5,width=0)+
  stat_summary(fun.data = "mean_cl_boot", colour = "#F23A02", size = .4)+
  stat_summary(fun.y="mean", colour="#F23A02", geom="line")+
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks=seq(-6,6,2))+
  guides(color = F)+
  labs(x=NULL,y=NULL)+
  geom_vline(xintercept = 0, linetype = 2)

vocab_by_walk_exp <- ggplot((point_walk_experience %>% filter(walk_experience>-7  & walk_experience<7)
                             ),aes(walk_experience,(CDIprod+1))) +
  geom_jitter(color="#DCDCDC",shape=4,height = 0.5,width=0)+
  stat_summary(fun.data = "mean_cl_boot", colour = "#1173FC", size = .4)+
  stat_summary(fun.y="mean", colour="#1173FC", geom="line")+
  scale_y_log10(limits=c(1,110))+
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks=seq(-6,6,2))+
  guides(color = F)+
  labs(x=NULL,y=NULL)+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_point(data=wallecampos,aes(walk_experience,(CDIprod+1)),color="purple")+
  geom_line(data=wallecampos,aes(walk_experience,(CDIprod+1)),color="purple")

vocab_by_exp <- plot_grid(vocab_by_month,
                            vocab_by_point_exp,
                            vocab_by_walk_exp, nrow = 1)

# SAME THING BUT COMP
vocab_comp_by_month <- ggplot(point_walk_experience,aes(month,(CDIcomp+1))) +
  geom_jitter(color="#DCDCDC",shape=4,height = 0.5,width=0)+
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = .4)+
  stat_summary(fun.y="mean", colour="black", geom="line")+
  scale_y_log10(limits=c(1,410))+
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks=seq(6,18,2))+
  guides(color = F)+
  labs(x="Age in mos \n",y="Receptive vocab (log)")

vocab_comp_by_point_exp <- ggplot((point_walk_experience%>% filter(point_experience>-7 & point_experience<7)),
                                  aes(x=point_experience,y=(CDIcomp+1))) +
  geom_jitter(color="#DCDCDC",shape=4,height = 0.5,width=0)+
  stat_summary(fun.y="mean", colour="#F23A02", geom="line")+
  scale_y_log10(limits=c(1,410))+
  stat_summary(fun.data = "mean_cl_boot", colour = "#F23A02", size = .4)+
  theme_bw(base_size = 14) +
  geom_vline(xintercept = 0, linetype = 2)+
  scale_x_continuous(breaks=seq(-6,6,2))+
  guides(color = F)+
  labs(x="Mos since \npoint onset",y=NULL)

vocab_comp_by_walk_exp <- ggplot((point_walk_experience%>% filter(walk_experience>-7 & walk_experience<7)),
                                 aes(x=walk_experience,y=(CDIcomp+1))) +
  geom_jitter(color="#DCDCDC",shape=4,height = 0.5,width=0)+
  stat_summary(fun.data = "mean_cl_boot", colour = "#1173FC", size = .4)+
  stat_summary(fun.y="mean", colour="#1173FC", geom="line")+
  scale_y_log10(limits=c(1,410))+
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks=seq(-6,6,2))+
  guides(color = F)+
  geom_vline(xintercept = 0, linetype = 2)+
  labs(x="Mos since \nwalk onset",y=NULL)+
  geom_point(data=wallecampos,aes(walk_experience,(CDIcomp+1)), color = "purple")+
  geom_line(data=wallecampos,aes(walk_experience,(CDIcomp+1)), color = "purple")

vocab_comp_by_exp <- plot_grid(vocab_comp_by_month,
                          vocab_comp_by_point_exp,
                          vocab_comp_by_walk_exp, nrow = 1)


all_vocab_by_exp <- plot_grid(vocab_by_exp,
                              vocab_comp_by_exp,
                              nrow=2)