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
- **AgeMonthMotor_Uncorrected:** subject's age according to the motor survey they filled out
- **questionnaire:** which type of questionnaire is missing, CDI or the motor survey
- **subj:** subject number

### month_level_pwt_vocab_ransubj.csv
This file contains the following columns:
 - **month:** subject age in months
 - **CDIcomp:** reported receptive vocabulary
 - **CDIprod:** reported productive vocabulary
 - **CDInounprod:** reported receptive vocabulary, nouns only
 - **noCDI:** logical. TRUE indicates that subject is missing their CDI for that month.
 - **CDInouncomp:** (reported productive vocabulary, nouns only)
 - **CHI:** (number of total words the child was observed to say)
 - **CHItypes:** (number of unique words the child was observed to say)
 - **obs_1stpoint_mo:** observed age in months of infants' first point	
 - **obs_1stprodN_mo:**	observed age in months of infants' first produced noun
 - **obs_1stprod_any_mo:** observed age in months of infants' first produced word of any type
 - **obs_walk3_mo:** observed age in months of infants' first three steps	
 - **obs_walk10_mo:** observed age in months of infants' first ten steps	
 - **rep_1stpoint_mo:** reported age in months of infants' first point
 - **rep_1stprodN_mo:** reported age in months of infants' first produced noun
 - **rep_1stprod_any_mo:** reported age in months of infants' first produced word of any type
 - **rep_walk3_mo:** reported age in months of infants' first three steps
 - **rep_1stcomp_any_mo:** subject age in months at first reported comprehension of a word, according to the CDI
 - **rep_1stcompN_mo:** subject age in months at first reported comprehesion of a _noun_, according to the CDI
 - **obs3w:** logical. TRUE indicates that 3 unassisted steps have been observed for that subject at or before that age.
 - **rep3w:** logical. TRUE indicates that 3 unassisted steps have been reported for that subject at or before that age.
 - **obsp:** logical. TRUE indicates that a single-finger extended point has been observed for that subject at or before that age.
 - **repp:** logical. TRUE indicates that a single-finger extended point has been reported for that subject at or before that age.
 - **obst:** logical. TRUE indicates that a spoken word has been observed for that subject at or before that age.
 - **rept:** logical. TRUE indicates that a spoken word has been reported for that subject at or before that age.
 - **subj:** subject number

### point_walk_experience_ransubj.csv
This file contains columns from the above datasets, and the following new ones:
- **firstpoint_mo:**	age at first point from reported sources
- **firstwalk_mo:**	age at first 3 steps from reported sources
- **walk_experience:**	This is a measure of infants' months of experience with walking. The month where an infant started walking is 0. Months before that are negative and months after that are positive.
- **point_experience:**	This is a measure of infants' months of experience with pointing. The month where an infant started pointing is 0. Months before that are negative and months after that are positive.
- **walk_experience_zeros:**	This is a measure of infants' months of experience with walking. The month where an infant started walking is 0. Months before that are all 0s and months after that are positive.
- **point_experience_zeros:**	This is a measure of infants' months of experience with pointing. The month where an infant started pointing is 0. Months before that are all 0s and months after that are positive.
- **firstpoint_obs:**	age at first point from observed sources
- **firstwalk_obs:**	age at first 3 steps from observed sources
- **obs_point_exp:**	same as point_experience but for observed data
- **obs_walk_exp:** same as walk_experience but for observed data

### subjlevel_pwt_obsrep_mo_ransubj.csv
This file is composed of columns that have already been described above.
