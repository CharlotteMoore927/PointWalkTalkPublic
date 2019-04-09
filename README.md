# PointWalkTalkPublic
Scripts associated with "Point, Walk, Talk: Links Between Three Early Milestones, from Observation and Parental Report"

- pwt_stats.R will reproduce all of our analyses, using our data.
- pwt_graphs.R will reproduce all of the figures we present in our manuscript, using our data.

inside the /data folder, you will find the following datasets necesssary to reproduce our manuscripts:

### gathervocab_month_level_pwt_ransubj.csv
This file contains the following columns:
 - **month:** subject age in months
 - **noCDI:** logical. TRUE indicates that subject is missing their CDI for that month. 	
 - **rep_1stcomp_any_mo:** subject age in months at first reported comprehension of a word, according to the CDI
 - **rep_1stcompN_mo:** subject age in months at first reported comprehesion of a _noun_, according to the CDI
 - **obs3w:** logical. TRUE indicates that 3 unassisted steps have been observed for that subject at or before that age.
 - **rep3w:** logical. TRUE indicates that 3 unassisted steps have been reported for that subject at or before that age.
 - **obsp:** logical. TRUE indicates that a single-finger extended point has been observed for that subject at or before that age.
 - **repp:** logical. TRUE indicates that a single-finger extended point has been reported for that subject at or before that age.
 - **obst:** logical. TRUE indicates that a spoken word has been observed for that subject at or before that age.
 - **rept:** logical. TRUE indicates that a spoken word has been reported for that subject at or before that age.
 - **vocab_type:** source of vocabulary data. Levels are:
  - CDIcomp (reported receptive vocabulary)
  - CDIprod (reported productive vocabulary)
  - CHItypes (number of unique words the child was observed to say)
  - CHI (number of total words the child was observed to say)
  - CDInounprod (reported receptive vocabulary, nouns only)
  - CDInouncomp (reported productive vocabulary, nouns only)
 - **wordcount:** number of words reported to be in the vocabulary, by vocab_type
 - **obs3w_label:** summary of all infants in the dataset and whether they have been observed to walk three steps or not. "c" is for crawlers. "w" is for walkers. "44c, 0w" means that all 44 subjects are crawlers and none are walkers.
 - **rep3w_label:**	summary of all infants in the dataset and whether they have been reported to walk three steps or not.
 - **obsp_label:**	summary of all infants in the dataset and whether they have been observed to point or not. "np" is for non-pointers. "p" is for pointers.
 - **repp_label:** summary of all infants in the dataset and whether they have been reported to point or not. "np" is for non-pointers. "p" is for pointers.	
 - **subj:** subject number

### missing_surveys_ransubj.csv
This file contains the following columns:
**AgeMonthMotor_Uncorrected:** subject's age according to the motor survey they filled out
**questionnaire:** which type of questionnaire is missing, CDI or the motor survey
**subj:** subject number

### month_level_pwt_vocab_ransubj.csv
### point_walk_experience_ransubj.csv
### subjlevel_pwt_obsrep_mo_ransubj.csv
