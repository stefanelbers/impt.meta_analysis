#preparation R script for Shiny forest plots
#FIRST: LOAD load_data.r and questionnaires_dataframe.R

dat <- dat_clean %>%
  rename(pf_name_measurement_instrument = pf_measurement_name,
         meas_hrqol = measurement_of_hrqol,
         meas_pf = measurements_physical_functioning,
         meas_pinter = measurements_pain_interference,
         meas_dep = measurements_depression,
         meas_anx = measurements_anxiety,
         meas_ef = measurements_general_emotional_functioning,
         meas_ang = measurements_anger,
         meas_se = measurements_self_efficacy,
         meas_srf = measurements_social_role_functioning,
         meas_pintens = measurements_pain_intensity)

# create variables 
dat$char_age_sd <- as.character(dat$age_sd)
dat$char_age_m <- as.character(dat$age_m)
dat$char_age_sd <- paste("(", dat$char_age_sd, ")", sep = "")
dat$age_m_sd <- paste(dat$char_age_m, dat$char_age_sd, sep = " ")
dat$age_m_sd[dat$age_m_sd=="NA (NA)"] <- NA
dat$char_year <- paste("(", dat$year, ")", sep="")
dat$author_year <- paste(dat$author, dat$char_year, sep = " ")

#calculate attrition
dat$attrition_post <- round(100-((dat$sample_size_post/dat$sample_size_pre)*100), 2)
dat$attrition_fu <- round(100-((dat$sample_size_fu/dat$sample_size_post)*100), 2)

#create variable cohort
dat$cohort <- paste(dat$author, dat$year, dat$cohort_id, sep = "_")

#get info from study with cohort_id > 1 from same study with cohort_id = 1
copyVarsFromCohort_id1 <- c("nationality","patient_group") # TO DO: variables need to be added
for (ind1 in which(dat$cohort_id > 1)) {
  #find same study (Author,year) but with cohort_id = 1
  ind2 <- which(dat$author == dat$author[ind1] & dat$year == dat$year[ind1]  & dat$cohort_id == 1)
  for (v in copyVarsFromCohort_id1) {
    eval(parse(text=paste('dat$', v, '[', ind1, ']=dat$', v, '[', ind2, ']', sep = "")))
  }
}

# meetinstrumenten
meetinstrumenten <- c("hrqol","pf","pinter","dep","anx","ef","ang","se","srf","pintens") # TO DO: outcomes need to be added

#determine last follow-up (last_fu)
for (mi in meetinstrumenten) {
  v1 <- paste(mi, '_last_fu', sep = "") 
  dat[v1] = 1 # initialise, <meetinstrument>_last_fu = 1
  
  # Was there even a second follow-up?
  v2 <- paste('meas', mi, sep = "_")
  v3 <- paste(mi, '_measurement_after_fu1', sep = "")
  ind <- which( dat[v2] == "yes" & dat[v3] == "yes" )
  dat[ind,v1] = 2
  
  # Was there even a third follow-up?
  v2 <- paste('meas', mi, sep = "_")
  v3 <- paste(mi, '_measurement_after_fu2', sep = "")
  ind <- which( dat[v2] == "yes" & dat[v3] == "yes" )
  dat[ind,v1] = 3
}

##check
#mi <- "pf"
#dat <- dat[dat$author == "Thieme",] 

#replace missing 'n' from outcomes (pre/post) with generic 'n'
for (mi in meetinstrumenten) {
  v1 <- paste('meas', mi, sep = "_")
  
  # pre/post
  for (p in c("pre","post")) {
    #if <meetinstrument>_<pre/post>_n is missing: <meetinstrument>_<pre/post>_n = sample_size_<pre/post>
    v2 <- paste(mi, p, 'n', sep = "_")
    v3 <- paste('sample_size', p, sep = "_")
    ind <- which(dat[v1] == "yes" & is.na(dat[v2]))
    if (length(ind) > 0) {
      dat[ind,v2] <- dat[ind,v3]
    }
    
    #if <meetinstrument>_<pre/post>_n is (still) missing: <meetinstrument>_<pre/post>_n = sample_size
    ind <- which(dat[v1] == "yes" & is.na(dat[v2]))
    if (length(ind) > 0) {
      dat[ind,v2] <- dat[ind,"sample_size"]
    }
    
    
  }
  
  # follow-up (try to fill missings 'n' of the last follow-up with sample_size_fu)
  for (f in 1:3) {
    v1 <- paste(mi, '_fu', f ,'_n', sep = "")
    v2 <- paste(mi, '_last_fu', sep = "")
    ind <- which( is.na(dat[v1]) & dat[v2] == f )
    dat[ind,v1] <- dat$sample_size_fu[ind]
  }
  
  # follow-up (if still missing, get the general sample_size)
  for (f in 1:3) {
    v1 <- paste(mi, '_fu', f ,'_n', sep = "")
    v2 <- paste(mi, '_last_fu', sep = "")
    ind <- which( is.na(dat[v1]) & dat[v2] == f )
    dat[ind,v1] <- dat$sample_size[ind]
  }
}

## Forest plots
contrasts <- data.frame(left = c("pre","post","pre"), right = c("post","fu","fu"))

# remove data_long if it already exists from previous run
if (exists("data_long")) {
  rm(data_long)
}

for (mi in meetinstrumenten) {
  for (i in 1:dim(contrasts)[1]) {
    contrast <- paste(contrasts$left[i], contrasts$right[i], sep="-")
    
    # add <mi>_last_fu<m/n/sd> based on <mi>_fu<last_fu>
    v1 <- paste(mi, '_last_fu', sep = "") 
    for (ind in 1:dim(dat)[1]){
      for (w in c("m","n","sd","t")) {
        v2 <- paste(mi, '_fu', dat[ind,v1], '_', w, sep="")
        v3 <- paste(mi, '_last_fu_', w, sep="")
        dat[ind,v3] <- dat[ind,v2]     
      }
    }
    
    left <- contrasts$left[i]
    right <- contrasts$right[i]
    
    # rename 'fu' to 'last_fu'
    left <- str_replace(left,"fu","last_fu")
    right <- str_replace(right,"fu","last_fu")
    
    v1 <- paste('meas', mi, sep = "_")
    v2 <- paste(mi, left, 'm', sep = "_")
    v3 <- paste(mi, right, 'm', sep = "_")
    
    logical <- dat[v1] == "yes" & !is.na(dat[v2]) & !is.na(dat[v3])
    data_fp <- dat[logical,]
    
    #data_fp <- as_tibble(data_fp)   # dit is niet nodig?
    data_fp$fu_month <- 0
    data_fp$ri <- .54
    
    v1 <- paste(mi, right, 'm', sep = "_")  # m1i ??? Klopt originele volgorde wel?? m1i=m_post, m2i=m_pre, sd1i=sd_pre, ni=n_post 
    v2 <- paste(mi, left, 'm', sep = "_") # m2i
    v3 <- paste(mi, left, 'sd', sep = "_") # sd1i
    v4 <- paste(mi, right, 'n', sep = "_") # ni
    
    data_fp <- data_fp %>%
      mutate(m1i := eval(parse(text=v1)))
    data_fp <- data_fp %>%
      mutate(m2i := eval(parse(text=v2)))
    data_fp <- data_fp %>%
      mutate(sd1i := eval(parse(text=v3)))
    data_fp <- data_fp %>%
      mutate(ni := eval(parse(text=v4)))
    
    
    
    
    
    ## make dataset to compare with Stefans Excel file
    check_var <- paste('check', mi, contrast, sep="_")
    
    v1 <- paste(mi, 'name_measurement_instrument',sep="_")
    v2 <- paste(mi, 'last_fu_t',sep="_")
    data_fp <- data_fp %>%
      mutate(name_measurement_instrument := eval(parse(text=v1)),
             fu_month := eval(parse(text=v2)))
    
    # left
    v <- paste(mi, left, 'n', sep = "_")
    data_fp <- data_fp %>% mutate(left_n := eval(parse(text=v)))
    v <- paste(mi, left, 'sd', sep = "_")
    data_fp <- data_fp %>% mutate(left_sd := eval(parse(text=v)))
    v <- paste(mi, left, 'm', sep = "_")
    data_fp <- data_fp %>% mutate(left_m := eval(parse(text=v)))
    
    # right
    v <- paste(mi, right, 'n', sep = "_")
    data_fp <- data_fp %>% mutate(right_n := eval(parse(text=v)))
    v <- paste(mi, right, 'sd', sep = "_")
    data_fp <- data_fp %>% mutate(right_sd := eval(parse(text=v)))
    v <- paste(mi, right, 'm', sep = "_")
    data_fp <- data_fp %>% mutate(right_m := eval(parse(text=v)))
    
    data_fp_sub <- data_fp %>%
      select(author, year, cohort_id, cohort_name, name_measurement_instrument, fu_month, left_m, left_sd, left_n, right_m, right_sd, right_n)
    
    # change 1 or 2 into "a" or "b"
    data_fp_sub$cohort_id <-letters[data_fp_sub$cohort_id]
    
    # add column with contrast in first column
    data_fp_sub <- add_column(data_fp_sub, contrast = contrast, .before = 1)
    
    # add column with outcome in first column
    data_fp_sub <- add_column(data_fp_sub, outcome = mi, .before = 1)
    
    # now we can compare the data in e.g. 'check_hrqol_pre-post' with Stefans Excel-sheet
    assign(check_var,data_fp_sub)
    
    ## Eerst had ik de code gemaakt om de verschillende forester plots te maken, later bleek dat je graag de tabel in 'long format' wilde,
    ## daarom zit dat er nu op deze manier in. Achteraf gezien had dit gedaan kunnen worden voordat je de forestplots gaat maken.
    
    # combine all 'check_'-variable into one 'long format' 
    ifelse(exists("data_long"), 
           data_long <- rbind(data_long,data_fp_sub),
           data_long <- data_fp_sub)
    
  }
}


# reversed scoring. Reverse the contrast in case higher scores indicate decreased functioning.  
data_long$rev_scoring <- questionnaires$reverse_scoring[match(as.character(data_long$name_measurement_instrument), as.character(questionnaires$q))]

## reverse scoring procedure ##
data <- data_long
# return: new dataframe with corrected stds and means.
correct_rev_score <- function(data) {
  # Create new copy of data
  rev_corrected_data <- data.frame(data)
  
  # Iterate over each row in data
  for (row in 1:nrow(data)) {
    if (data[row, "rev_scoring"] == 1) {
      # Replace pre by post
      rev_corrected_data[row, "left_m"] <- data[row, "right_m"]
      rev_corrected_data[row, "left_sd"] <- data[row, "right_sd"]
      rev_corrected_data[row, "left_n"] <- data[row, "right_n"]
      
      # Replace post by pre
      rev_corrected_data[row, "right_m"] <- data[row, "left_m"]
      rev_corrected_data[row, "right_sd"] <- data[row, "left_sd"]
      rev_corrected_data[row, "right_n"] <- data[row, "left_n"]
    }
  }
  return(rev_corrected_data)
}
corrected_data <- correct_rev_score(data)

#rename outcomes
corrected_data$outcome <- corrected_data$outcome %>% recode(
  "hrqol" = "health related quality of life",
  "pf" = "physical function",
  "pinter" = "pain interference",
  "dep" =  "depression",
  "anx" = "anxiety",
  "ef" = "general emotional functioning",
  "ang" = "anger",
  "se" = "self-efficacy",
  "srf" = "social functioning",
  "pintens" = "pain intensity"
)

corrected_data <- corrected_data %>%
  mutate(fu_month = if_else(contrast == "pre-post", 0, fu_month))


### static meta analysis ###

static_ma <- corrected_data
static_ma$ri = .59

data_meta <- escalc(measure="SMCR", m1i=right_m, m2i=left_m, sd1i=left_sd, ni=right_n, ri=ri, data=static_ma)
meta_dat <- metafor::summary.escalc(data_meta)
pf_meta_prpo <- filter(meta_dat, contrast == "pre-post" & outcome == "physical function")
pf_meta_pof <- filter(meta_dat, contrast == "post-fu" & outcome == "physical function")
pf_meta_prf <- filter(meta_dat, contrast == "pre-fu" & outcome == "physical function")
pinter_meta_prpo <- filter(meta_dat, contrast == "pre-post" & outcome == "pain interference")
pinter_meta_pof <- filter(meta_dat, contrast == "post-fu" & outcome == "pain interference")
pinter_meta_prf <- filter(meta_dat, contrast == "pre-fu" & outcome == "pain interference")
pintens_meta_prpo <- filter(meta_dat, contrast == "pre-post" & outcome == "pain intensity")
pintens_meta_pof <- filter(meta_dat, contrast == "post-fu" & outcome == "pain intensity")
pintens_meta_prf <- filter(meta_dat, contrast == "pre-fu" & outcome == "pain intensity")
dep_meta_prpo <- filter(meta_dat, contrast == "pre-post" & outcome == "depression")
dep_meta_pof <- filter(meta_dat, contrast == "post-fu" & outcome == "depression")
dep_meta_prf <- filter(meta_dat, contrast == "pre-fu" & outcome == "depression")
anx_meta_prpo <- filter(meta_dat, contrast == "pre-post" & outcome == "anxiety")
anx_meta_pof <- filter(meta_dat, contrast == "post-fu" & outcome == "anxiety")
anx_meta_prf <- filter(meta_dat, contrast == "pre-fu" & outcome == "anxiety")
ef_meta_prpo <- filter(meta_dat, contrast == "pre-post" & outcome == "general emotional functioning")
ef_meta_pof <- filter(meta_dat, contrast == "post-fu" & outcome == "general emotional functioning")
ef_meta_prf <- filter(meta_dat, contrast == "pre-fu" & outcome == "general emotional functioning")
ang_meta_prpo <- filter(meta_dat, contrast == "pre-post" & outcome == "anger")
ang_meta_pof <- filter(meta_dat, contrast == "post-fu" & outcome == "anger")
ang_meta_prf <- filter(meta_dat, contrast == "pre-fu" & outcome == "anger")
se_meta_prpo <- filter(meta_dat, contrast == "pre-post" & outcome == "self-efficacy")
se_meta_pof <- filter(meta_dat, contrast == "post-fu" & outcome == "self-efficacy")
se_meta_prf <- filter(meta_dat, contrast == "pre-fu" & outcome == "self-efficacy")
srf_meta_prpo <- filter(meta_dat, contrast == "pre-post" & outcome == "social functioning")
srf_meta_pof <- filter(meta_dat, contrast == "post-fu" & outcome == "social functioning")
srf_meta_prf <- filter(meta_dat, contrast == "pre-fu" & outcome == "social functioning")

metavars <- c("author", "year", "cohort_id", "yi", "zi", "ci.lb", "ci.ub")
metavars2 <- c("author", "year", "cohort_id", "outcome", "yi", "zi", "ci.lb", "ci.ub")

#statistical significance of all ES combined.
meta_dat_prpo <- filter(meta_dat, contrast == "pre-post")
summarytools::freq(meta_dat_prpo$ci.lb > 0) # + pre-follow-up pattern
summarytools::freq(meta_dat_prpo$ci.lb < 0 & meta_dat_prpo$ci.ub > 0) # 0 pre-follow-up pattern
summarytools::freq(meta_dat_prpo$ci.ub < 0) # - pre follow-up pattern

meta_dat_pof <- filter(meta_dat, contrast == "post-fu")

#pattern total
merge_prpo <- meta_dat_prpo[metavars2]
merge_pof <- meta_dat_pof[metavars2]
meta_merge <- merge(merge_prpo, merge_pof, by = c("author", "year", "cohort_id", "outcome"))

#pattern pre-fu
meta_dat_prf <- filter(meta_dat, contrast == "pre-fu")
summarytools::freq(meta_dat_prf$ci.lb > 0) # + pre-follow-up pattern
summarytools::freq(meta_dat_prf$ci.lb < 0 & meta_dat_prf$ci.ub > 0) # 0 pre-follow-up pattern
summarytools::freq(meta_dat_prf$ci.ub < 0) # - pre follow-up pattern

#statistical significance of all ES combined.

total_patterns <- c(
sum(meta_merge$ci.lb.x > 0 & meta_merge$ci.lb.y > 0, na.rm=TRUE), # ++ pattern
sum(meta_merge$ci.lb.x > 0 & meta_merge$ci.lb.y < 0 & meta_merge$ci.ub.y > 0, na.rm=TRUE), # +0 pattern
sum(meta_merge$ci.lb.x > 0 & meta_merge$ci.ub.y < 0, na.rm=TRUE), # +- pattern
sum(meta_merge$ci.lb.x < 0 & meta_merge$ci.ub.x > 0 & meta_merge$ci.lb.y > 0, na.rm=TRUE), # 0+ pattern
sum(meta_merge$ci.lb.x < 0 & meta_merge$ci.ub.x > 0 & meta_merge$ci.lb.y < 0 & meta_merge$ci.ub.y > 0, na.rm=TRUE), # 00 pattern
sum(meta_merge$ci.lb.x < 0 & meta_merge$ci.ub.x > 0 & meta_merge$ci.ub.y < 0, na.rm=TRUE), # 0- pattern
sum(meta_merge$ci.ub.x < 0 & meta_merge$ci.lb.y > 0, na.rm=TRUE), # -+ pattern
sum(meta_merge$ci.ub.x < 0 & meta_merge$ci.lb.y < 0 & meta_merge$ci.ub.y > 0, na.rm=TRUE), # -0 pattern
sum(meta_merge$ci.ub.x < 0 & meta_merge$ci.ub.y < 0, na.rm=TRUE) # -- pattern 
) %>% print()


#pattern pf
pf_merge_prpo <- pf_meta_prpo[metavars]
pf_merge_pof <- pf_meta_pof[metavars]
pf_merged <- merge(pf_merge_prpo, pf_merge_pof, by = c("author", "year", "cohort_id"))

#pf: count each pattern
sum(pf_merged$ci.lb.x > 0 & pf_merged$ci.lb.y > 0, na.rm=TRUE) # ++ pattern
sum(pf_merged$ci.lb.x > 0 & pf_merged$ci.lb.y < 0 & pf_merged$ci.ub.y > 0, na.rm=TRUE) # +0 pattern
sum(pf_merged$ci.lb.x > 0 & pf_merged$ci.ub.y < 0, na.rm=TRUE) # +- pattern
sum(pf_merged$ci.lb.x < 0 & pf_merged$ci.ub.x > 0 & pf_merged$ci.lb.y > 0, na.rm=TRUE) # 0+ pattern
sum(pf_merged$ci.lb.x < 0 & pf_merged$ci.ub.x > 0 & pf_merged$ci.lb.y < 0 & pf_merged$ci.ub.y > 0, na.rm=TRUE) # 00 pattern
sum(pf_merged$ci.lb.x < 0 & pf_merged$ci.ub.x > 0 & pf_merged$ci.ub.y < 0, na.rm=TRUE) # 0- pattern
sum(pf_merged$ci.ub.x < 0 & pf_merged$ci.lb.y > 0, na.rm=TRUE) # -+ pattern
sum(pf_merged$ci.ub.x < 0 & pf_merged$ci.lb.y < 0 & pf_merged$ci.ub.y > 0, na.rm=TRUE) # -0 pattern
sum(pf_merged$ci.ub.x < 0 & pf_merged$ci.ub.y < 0, na.rm=TRUE) # -- pattern

#pf: pre-fu patterns
summarytools::freq(pf_meta_prf$ci.lb > 0) # + pre-follow-up pattern
summarytools::freq(pf_meta_prf$ci.lb < 0 & pf_meta_prf$ci.ub > 0) # 0 pre-follow-up pattern
summarytools::freq(pf_meta_prf$ci.ub < 0) # - pre follow-up pattern


#pattern pinter
pinter_merge_prpo <- pinter_meta_prpo[metavars]
pinter_merge_pof <- pinter_meta_pof[metavars]
pinter_merged <- merge(pinter_merge_prpo, pinter_merge_pof, by = c("author", "year", "cohort_id"))

#pinter: count each pattern
sum(pinter_merged$ci.lb.x > 0 & pinter_merged$ci.lb.y > 0, na.rm=TRUE) # ++ pattern
sum(pinter_merged$ci.lb.x > 0 & pinter_merged$ci.lb.y < 0 & pinter_merged$ci.ub.y > 0, na.rm=TRUE) # +0 pattern
sum(pinter_merged$ci.lb.x > 0 & pinter_merged$ci.ub.y < 0, na.rm=TRUE) # +- pattern
sum(pinter_merged$ci.lb.x < 0 & pinter_merged$ci.ub.x > 0 & pinter_merged$ci.lb.y > 0, na.rm=TRUE) # 0+ pattern
sum(pinter_merged$ci.lb.x < 0 & pinter_merged$ci.ub.x > 0 & pinter_merged$ci.lb.y < 0 & pinter_merged$ci.ub.y > 0, na.rm=TRUE) # 00 pattern
sum(pinter_merged$ci.lb.x < 0 & pinter_merged$ci.ub.x > 0 & pinter_merged$ci.ub.y < 0, na.rm=TRUE) # 0- pattern
sum(pinter_merged$ci.ub.x < 0 & pinter_merged$ci.lb.y > 0, na.rm=TRUE) # -+ pattern
sum(pinter_merged$ci.ub.x < 0 & pinter_merged$ci.lb.y < 0 & pinter_merged$ci.ub.y > 0, na.rm=TRUE) # -0 pattern
sum(pinter_merged$ci.ub.x < 0 & pinter_merged$ci.ub.y < 0, na.rm=TRUE) # -- pattern

#pinter: pre-fu patterns
summarytools::freq(pinter_meta_prf$ci.lb > 0) # + pre-follow-up pattern
summarytools::freq(pinter_meta_prf$ci.lb < 0 & pinter_meta_prf$ci.ub > 0) # 0 pre-follow-up pattern
summarytools::freq(pinter_meta_prf$ci.ub < 0) # - pre follow-up pattern


#pattern dep
dep_merge_prpo <- dep_meta_prpo[metavars]
dep_merge_pof <- dep_meta_pof[metavars]
dep_merged <- merge(dep_merge_prpo, dep_merge_pof, by = c("author", "year", "cohort_id"))

#dep: count each pattern
sum(dep_merged$ci.lb.x > 0 & dep_merged$ci.lb.y > 0, na.rm=TRUE) # ++ pattern
sum(dep_merged$ci.lb.x > 0 & dep_merged$ci.lb.y < 0 & dep_merged$ci.ub.y > 0, na.rm=TRUE) # +0 pattern
sum(dep_merged$ci.lb.x > 0 & dep_merged$ci.ub.y < 0, na.rm=TRUE) # +- pattern
sum(dep_merged$ci.lb.x < 0 & dep_merged$ci.ub.x > 0 & dep_merged$ci.lb.y > 0, na.rm=TRUE) # 0+ pattern
sum(dep_merged$ci.lb.x < 0 & dep_merged$ci.ub.x > 0 & dep_merged$ci.lb.y < 0 & dep_merged$ci.ub.y > 0, na.rm=TRUE) # 00 pattern
sum(dep_merged$ci.lb.x < 0 & dep_merged$ci.ub.x > 0 & dep_merged$ci.ub.y < 0, na.rm=TRUE) # 0- pattern
sum(dep_merged$ci.ub.x < 0 & dep_merged$ci.lb.y > 0, na.rm=TRUE) # -+ pattern
sum(dep_merged$ci.ub.x < 0 & dep_merged$ci.lb.y < 0 & dep_merged$ci.ub.y > 0, na.rm=TRUE) # -0 pattern
sum(dep_merged$ci.ub.x < 0 & dep_merged$ci.ub.y < 0, na.rm=TRUE) # -- pattern

#dep: pre-fu patterns
summarytools::freq(dep_meta_prf$ci.lb > 0) # + pre-follow-up pattern
summarytools::freq(dep_meta_prf$ci.lb < 0 & dep_meta_prf$ci.ub > 0) # 0 pre-follow-up pattern
summarytools::freq(dep_meta_prf$ci.ub < 0) # - pre follow-up pattern

#pattern anx
anx_merge_prpo <- anx_meta_prpo[metavars]
anx_merge_pof <- anx_meta_pof[metavars]
anx_merged <- merge(anx_merge_prpo, anx_merge_pof, by = c("author", "year", "cohort_id"))

#anx: count each pattern
sum(anx_merged$ci.lb.x > 0 & anx_merged$ci.lb.y > 0, na.rm=TRUE) # ++ pattern
sum(anx_merged$ci.lb.x > 0 & anx_merged$ci.lb.y < 0 & anx_merged$ci.ub.y > 0, na.rm=TRUE) # +0 pattern
sum(anx_merged$ci.lb.x > 0 & anx_merged$ci.ub.y < 0, na.rm=TRUE) # +- pattern
sum(anx_merged$ci.lb.x < 0 & anx_merged$ci.ub.x > 0 & anx_merged$ci.lb.y > 0, na.rm=TRUE) # 0+ pattern
sum(anx_merged$ci.lb.x < 0 & anx_merged$ci.ub.x > 0 & anx_merged$ci.lb.y < 0 & anx_merged$ci.ub.y > 0, na.rm=TRUE) # 00 pattern
sum(anx_merged$ci.lb.x < 0 & anx_merged$ci.ub.x > 0 & anx_merged$ci.ub.y < 0, na.rm=TRUE) # 0- pattern
sum(anx_merged$ci.ub.x < 0 & anx_merged$ci.lb.y > 0, na.rm=TRUE) # -+ pattern
sum(anx_merged$ci.ub.x < 0 & anx_merged$ci.lb.y < 0 & anx_merged$ci.ub.y > 0, na.rm=TRUE) # -0 pattern
sum(anx_merged$ci.ub.x < 0 & anx_merged$ci.ub.y < 0, na.rm=TRUE) # -- pattern

#anx: pre-fu patterns
summarytools::freq(anx_meta_prf$ci.lb > 0) # + pre-follow-up pattern
summarytools::freq(anx_meta_prf$ci.lb < 0 & anx_meta_prf$ci.ub > 0) # 0 pre-follow-up pattern
summarytools::freq(anx_meta_prf$ci.ub < 0) # - pre follow-up pattern

#pattern ef
ef_merge_prpo <- ef_meta_prpo[metavars]
ef_merge_pof <- ef_meta_pof[metavars]
ef_merged <- merge(ef_merge_prpo, ef_merge_pof, by = c("author", "year", "cohort_id"))

#ef: count each pattern
sum(ef_merged$ci.lb.x > 0 & ef_merged$ci.lb.y > 0, na.rm=TRUE) # ++ pattern
sum(ef_merged$ci.lb.x > 0 & ef_merged$ci.lb.y < 0 & ef_merged$ci.ub.y > 0, na.rm=TRUE) # +0 pattern
sum(ef_merged$ci.lb.x > 0 & ef_merged$ci.ub.y < 0, na.rm=TRUE) # +- pattern
sum(ef_merged$ci.lb.x < 0 & ef_merged$ci.ub.x > 0 & ef_merged$ci.lb.y > 0, na.rm=TRUE) # 0+ pattern
sum(ef_merged$ci.lb.x < 0 & ef_merged$ci.ub.x > 0 & ef_merged$ci.lb.y < 0 & ef_merged$ci.ub.y > 0, na.rm=TRUE) # 00 pattern
sum(ef_merged$ci.lb.x < 0 & ef_merged$ci.ub.x > 0 & ef_merged$ci.ub.y < 0, na.rm=TRUE) # 0- pattern
sum(ef_merged$ci.ub.x < 0 & ef_merged$ci.lb.y > 0, na.rm=TRUE) # -+ pattern
sum(ef_merged$ci.ub.x < 0 & ef_merged$ci.lb.y < 0 & ef_merged$ci.ub.y > 0, na.rm=TRUE) # -0 pattern
sum(ef_merged$ci.ub.x < 0 & ef_merged$ci.ub.y < 0, na.rm=TRUE) # -- pattern

#ef: pre-fu patterns
summarytools::freq(ef_meta_prf$ci.lb > 0) # + pre-follow-up pattern
summarytools::freq(ef_meta_prf$ci.lb < 0 & ef_meta_prf$ci.ub > 0) # 0 pre-follow-up pattern
summarytools::freq(ef_meta_prf$ci.ub < 0) # - pre follow-up pattern

#pattern ang
ang_merge_prpo <- ang_meta_prpo[metavars]
ang_merge_pof <- ang_meta_pof[metavars]
ang_merged <- merge(ang_merge_prpo, ang_merge_pof, by = c("author", "year", "cohort_id"))

#ang: count each pattern
sum(ang_merged$ci.lb.x > 0 & ang_merged$ci.lb.y > 0, na.rm=TRUE) # ++ pattern
sum(ang_merged$ci.lb.x > 0 & ang_merged$ci.lb.y < 0 & ang_merged$ci.ub.y > 0, na.rm=TRUE) # +0 pattern
sum(ang_merged$ci.lb.x > 0 & ang_merged$ci.ub.y < 0, na.rm=TRUE) # +- pattern
sum(ang_merged$ci.lb.x < 0 & ang_merged$ci.ub.x > 0 & ang_merged$ci.lb.y > 0, na.rm=TRUE) # 0+ pattern
sum(ang_merged$ci.lb.x < 0 & ang_merged$ci.ub.x > 0 & ang_merged$ci.lb.y < 0 & ang_merged$ci.ub.y > 0, na.rm=TRUE) # 00 pattern
sum(ang_merged$ci.lb.x < 0 & ang_merged$ci.ub.x > 0 & ang_merged$ci.ub.y < 0, na.rm=TRUE) # 0- pattern
sum(ang_merged$ci.ub.x < 0 & ang_merged$ci.lb.y > 0, na.rm=TRUE) # -+ pattern
sum(ang_merged$ci.ub.x < 0 & ang_merged$ci.lb.y < 0 & ang_merged$ci.ub.y > 0, na.rm=TRUE) # -0 pattern
sum(ang_merged$ci.ub.x < 0 & ang_merged$ci.ub.y < 0, na.rm=TRUE) # -- pattern

#ang: pre-fu patterns
summarytools::freq(ang_meta_prf$ci.lb > 0) # + pre-follow-up pattern
summarytools::freq(ang_meta_prf$ci.lb < 0 & ang_meta_prf$ci.ub > 0) # 0 pre-follow-up pattern
summarytools::freq(ang_meta_prf$ci.ub < 0) # - pre follow-up pattern

#pattern se
se_merge_prpo <- se_meta_prpo[metavars]
se_merge_pof <- se_meta_pof[metavars]
se_merged <- merge(se_merge_prpo, se_merge_pof, by = c("author", "year", "cohort_id"))

#se: count each pattern
sum(se_merged$ci.lb.x > 0 & se_merged$ci.lb.y > 0, na.rm=TRUE) # ++ pattern
sum(se_merged$ci.lb.x > 0 & se_merged$ci.lb.y < 0 & se_merged$ci.ub.y > 0, na.rm=TRUE) # +0 pattern
sum(se_merged$ci.lb.x > 0 & se_merged$ci.ub.y < 0, na.rm=TRUE) # +- pattern
sum(se_merged$ci.lb.x < 0 & se_merged$ci.ub.x > 0 & se_merged$ci.lb.y > 0, na.rm=TRUE) # 0+ pattern
sum(se_merged$ci.lb.x < 0 & se_merged$ci.ub.x > 0 & se_merged$ci.lb.y < 0 & se_merged$ci.ub.y > 0, na.rm=TRUE) # 00 pattern
sum(se_merged$ci.lb.x < 0 & se_merged$ci.ub.x > 0 & se_merged$ci.ub.y < 0, na.rm=TRUE) # 0- pattern
sum(se_merged$ci.ub.x < 0 & se_merged$ci.lb.y > 0, na.rm=TRUE) # -+ pattern
sum(se_merged$ci.ub.x < 0 & se_merged$ci.lb.y < 0 & se_merged$ci.ub.y > 0, na.rm=TRUE) # -0 pattern
sum(se_merged$ci.ub.x < 0 & se_merged$ci.ub.y < 0, na.rm=TRUE) # -- pattern

#se: pre-fu patterns
summarytools::freq(se_meta_prf$ci.lb > 0) # + pre-follow-up pattern
summarytools::freq(se_meta_prf$ci.lb < 0 & se_meta_prf$ci.ub > 0) # 0 pre-follow-up pattern
summarytools::freq(se_meta_prf$ci.ub < 0) # - pre follow-up pattern

#pattern srf
srf_merge_prpo <- srf_meta_prpo[metavars]
srf_merge_pof <- srf_meta_pof[metavars]
srf_merged <- merge(srf_merge_prpo, srf_merge_pof, by = c("author", "year", "cohort_id"))

#srf: count each pattern
sum(srf_merged$ci.lb.x > 0 & srf_merged$ci.lb.y > 0, na.rm=TRUE) # ++ pattern
sum(srf_merged$ci.lb.x > 0 & srf_merged$ci.lb.y < 0 & srf_merged$ci.ub.y > 0, na.rm=TRUE) # +0 pattern
sum(srf_merged$ci.lb.x > 0 & srf_merged$ci.ub.y < 0, na.rm=TRUE) # +- pattern
sum(srf_merged$ci.lb.x < 0 & srf_merged$ci.ub.x > 0 & srf_merged$ci.lb.y > 0, na.rm=TRUE) # 0+ pattern
sum(srf_merged$ci.lb.x < 0 & srf_merged$ci.ub.x > 0 & srf_merged$ci.lb.y < 0 & srf_merged$ci.ub.y > 0, na.rm=TRUE) # 00 pattern
sum(srf_merged$ci.lb.x < 0 & srf_merged$ci.ub.x > 0 & srf_merged$ci.ub.y < 0, na.rm=TRUE) # 0- pattern
sum(srf_merged$ci.ub.x < 0 & srf_merged$ci.lb.y > 0, na.rm=TRUE) # -+ pattern
sum(srf_merged$ci.ub.x < 0 & srf_merged$ci.lb.y < 0 & srf_merged$ci.ub.y > 0, na.rm=TRUE) # -0 pattern
sum(srf_merged$ci.ub.x < 0 & srf_merged$ci.ub.y < 0, na.rm=TRUE) # -- pattern

#srf: pre-fu patterns
summarytools::freq(srf_meta_prf$ci.lb > 0) # + pre-follow-up pattern
summarytools::freq(srf_meta_prf$ci.lb < 0 & srf_meta_prf$ci.ub > 0) # 0 pre-follow-up pattern
summarytools::freq(srf_meta_prf$ci.ub < 0) # - pre follow-up pattern

#pattern pintens
pintens_merge_prpo <- pintens_meta_prpo[metavars]
pintens_merge_pof <- pintens_meta_pof[metavars]
pintens_merged <- merge(pintens_merge_prpo, pintens_merge_pof, by = c("author", "year", "cohort_id"))

#pintens: count each pattern
sum(pintens_merged$ci.lb.x > 0 & pintens_merged$ci.lb.y > 0, na.rm=TRUE) # ++ pattern
sum(pintens_merged$ci.lb.x > 0 & pintens_merged$ci.lb.y < 0 & pintens_merged$ci.ub.y > 0, na.rm=TRUE) # +0 pattern
sum(pintens_merged$ci.lb.x > 0 & pintens_merged$ci.ub.y < 0, na.rm=TRUE) # +- pattern
sum(pintens_merged$ci.lb.x < 0 & pintens_merged$ci.ub.x > 0 & pintens_merged$ci.lb.y > 0, na.rm=TRUE) # 0+ pattern
sum(pintens_merged$ci.lb.x < 0 & pintens_merged$ci.ub.x > 0 & pintens_merged$ci.lb.y < 0 & pintens_merged$ci.ub.y > 0, na.rm=TRUE) # 00 pattern
sum(pintens_merged$ci.lb.x < 0 & pintens_merged$ci.ub.x > 0 & pintens_merged$ci.ub.y < 0, na.rm=TRUE) # 0- pattern
sum(pintens_merged$ci.ub.x < 0 & pintens_merged$ci.lb.y > 0, na.rm=TRUE) # -+ pattern
sum(pintens_merged$ci.ub.x < 0 & pintens_merged$ci.lb.y < 0 & pintens_merged$ci.ub.y > 0, na.rm=TRUE) # -0 pattern
sum(pintens_merged$ci.ub.x < 0 & pintens_merged$ci.ub.y < 0, na.rm=TRUE) # -- pattern

#pintens: pre-fu patterns
summarytools::freq(pintens_meta_prf$ci.lb > 0) # + pre-follow-up pattern
summarytools::freq(pintens_meta_prf$ci.lb < 0 & pintens_meta_prf$ci.ub > 0) # 0 pre-follow-up pattern
summarytools::freq(pintens_meta_prf$ci.ub < 0) # - pre follow-up pattern


#PF pre-post#
sum_pf_prpo <- summary(pf_meta_prpo$yi) %>%
  print()

median_pf_prpo <- sum_pf_prpo[3] %>%
  print()


rma(yi, vi, data=pf_meta_prpo)


fp_meta2$tabletext <- cbind(fp_meta_prpo$author, fp_meta_prpo$year, fp_meta_prpo$n_pre, fp_meta_prpo$measure, 
                            fp_meta_prpo$fu_month, fp_meta_prpo$yi, fp_meta_prpo$ci.lb, fp_meta_prpo$ci.ub)

# PF post-fu #
summary(pf_meta_pof$yi)
rma(yi, vi, data=pf_meta_pof)

# PF pre-fu #
summary(pf_meta_prf$yi)
rma(yi, vi, data=pf_meta_prf)

#Pinter pre-post#
sum_pinter_prpo <- summary(pinter_meta_prpo$yi) %>%
  print()

median_pinter_prpo <- sum_pinter_prpo[3] %>%
  print()

rma(yi, vi, data=pinter_meta_prpo)
unique(pinter_meta_prf$name_measurement_instrument)

#Pinter post-fu#
summary(pinter_meta_pof$yi)
rma(yi, vi, data=pinter_meta_pof)

#Pinter post-fu#
summary(pinter_meta_prf$yi)
rma(yi, vi, data=pinter_meta_prf)

#Pintens pre-post#
sum_pintens_prpo <- summary(pintens_meta_prpo$yi) %>%
  print()

median_pintens_prpo <- sum_pintens_prpo[3] %>%
  print()

rma(yi, vi, data=pintens_meta_prpo)
unique(pintens_meta_prf$name_measurement_instrument)

#Pintens post-fu#
summary(pintens_meta_pof$yi)
rma(yi, vi, data=pintens_meta_pof)

#Pintens post-fu#
summary(pintens_meta_prf$yi)
rma(yi, vi, data=pintens_meta_prf)

#dep pre-post#
sum_dep_prpo <- summary(dep_meta_prpo$yi) %>%
  print()

median_dep_prpo <- sum_dep_prpo[3] %>%
  print()

rma(yi, vi, data=dep_meta_prpo)
unique(dep_meta_prf$name_measurement_instrument)

#dep post-fu#
summary(dep_meta_pof$yi)
rma(yi, vi, data=dep_meta_pof)

#dep post-fu#
summary(dep_meta_prf$yi)
rma(yi, vi, data=dep_meta_prf)

#anx pre-post#
sum_anx_prpo <- summary(anx_meta_prpo$yi) %>%
  print()

median_anx_prpo <- sum_anx_prpo[3] %>%
  print()


rma(yi, vi, data=anx_meta_prpo)
unique(anx_meta_prf$name_measurement_instrument)

#anx post-fu#
summary(anx_meta_pof$yi)
rma(yi, vi, data=anx_meta_pof)

#anx post-fu#
summary(anx_meta_prf$yi)
rma(yi, vi, data=anx_meta_prf)

#ef pre-post#
sum_ef_prpo <- summary(ef_meta_prpo$yi) %>%
  print()

median_ef_prpo <- sum_ef_prpo[3] %>%
  print()


rma(yi, vi, data=ef_meta_prpo)
unique(ef_meta_prf$name_measurement_instrument)

#ef post-fu#
summary(ef_meta_pof$yi)
rma(yi, vi, data=ef_meta_pof)

#ef post-fu#
summary(ef_meta_prf$yi)
rma(yi, vi, data=ef_meta_prf)

#ang pre-post#
sum_ang_prpo <- summary(ang_meta_prpo$yi) %>%
  print()

median_ang_prpo <- sum_ang_prpo[3] %>%
  print()

rma(yi, vi, data=ang_meta_prpo)
unique(ang_meta_prf$name_measurement_instrument)

#ang post-fu#
summary(ang_meta_pof$yi)
rma(yi, vi, data=ang_meta_pof)

#ang post-fu#
summary(ang_meta_prf$yi)
rma(yi, vi, data=ang_meta_prf)

#se pre-post#
sum_se_prpo <- summary(se_meta_prpo$yi) %>%
  print()

median_se_prpo <- sum_se_prpo[3] %>%
  print()

rma(yi, vi, data=se_meta_prpo)
unique(se_meta_prf$name_measurement_instrument)

#se post-fu#
summary(se_meta_pof$yi)
rma(yi, vi, data=se_meta_pof)

#se post-fu#
summary(se_meta_prf$yi)
rma(yi, vi, data=se_meta_prf)

#srf pre-post#
sum_srf_prpo <- summary(srf_meta_prpo$yi) %>%
  print()

median_srf_prpo <- sum_srf_prpo[3] %>%
  print()


rma(yi, vi, data=srf_meta_prpo)
unique(srf_meta_prf$name_measurement_instrument)

#srf post-fu#
summary(srf_meta_pof$yi)
rma(yi, vi, data=srf_meta_pof)

#srf post-fu#
summary(srf_meta_prf$yi)
rma(yi, vi, data=srf_meta_prf)


## Re-express median ES on most common scale

#PF prpo
summarytools::freq(pf_meta_prpo$name_measurement_instrument)
pf_es_scale <- filter(pf_meta_prpo, name_measurement_instrument == "SF-36 subscale Physical Functioning") %>%
  select(right_sd, right_n)

pf_es_scale$weighted <- pf_es_scale$right_sd * pf_es_scale$right_n

weighted_sd_pf_prpo <- sum(pf_es_scale$weighted) / sum(pf_es_scale$right_n) # this is the weighted SD of the most commonly used instrument.
print(weighted_sd_pf_prpo)

median_pf_prpo * weighted_sd_pf_prpo # this is the conversion of the median effect size to the most frequently used instrument on that particular outcome.

#Pinter prpo
summarytools::freq(pinter_meta_prpo$name_measurement_instrument)
pinter_es_scale <- filter(pinter_meta_prpo, name_measurement_instrument == "RMDQ") %>%
  select(right_sd, right_n)

pinter_es_scale$weighted <- pinter_es_scale$right_sd * pinter_es_scale$right_n

weighted_sd_pinter_prpo <- sum(pinter_es_scale$weighted) / sum(pinter_es_scale$right_n) # this is the weighted SD of the most commonly used instrument.
print(weighted_sd_pinter_prpo)

median_pinter_prpo * weighted_sd_pinter_prpo # this is the conversion of the median effect size to the most frequently used instrument on that particular outcome.

#dep prpo
summarytools::freq(dep_meta_prpo$name_measurement_instrument)
dep_es_scale <- filter(dep_meta_prpo, name_measurement_instrument == "BDI") %>%
  select(right_sd, right_n)

dep_es_scale$weighted <- dep_es_scale$right_sd * dep_es_scale$right_n

weighted_sd_dep_prpo <- sum(dep_es_scale$weighted) / sum(dep_es_scale$right_n) # this is the weighted SD of the most commonly used instrument.
print(weighted_sd_dep_prpo)

median_dep_prpo * weighted_sd_dep_prpo # this is the conversion of the median effect size to the most frequently used instrument on that particular outcome.

#anx prpo
summarytools::freq(anx_meta_prpo$name_measurement_instrument)
anx_es_scale <- filter(anx_meta_prpo, name_measurement_instrument == "HADS-A") %>%
  select(right_sd, right_n)

anx_es_scale$weighted <- anx_es_scale$right_sd * anx_es_scale$right_n

weighted_sd_anx_prpo <- sum(anx_es_scale$weighted) / sum(anx_es_scale$right_n) # this is the weighted SD of the most commonly used instrument. 
print(weighted_sd_anx_prpo)

median_anx_prpo * weighted_sd_anx_prpo # this is the conversion of the median effect size to the most frequently used instrument on that particular outcome.

#ef prpo
summarytools::freq(ef_meta_prpo$name_measurement_instrument)
ef_es_scale <- filter(ef_meta_prpo, name_measurement_instrument == "SF-36: mental health") %>%
  select(right_sd, right_n)

ef_es_scale$weighted <- ef_es_scale$right_sd * ef_es_scale$right_n

weighted_sd_ef_prpo <- sum(ef_es_scale$weighted) / sum(ef_es_scale$right_n) # this is the weighted SD of the most commonly used instrument.
print(weighted_sd_ef_prpo)

median_ef_prpo * weighted_sd_ef_prpo # this is the conversion of the median effect size to the most frequently used instrument on that particular outcome.

#ang prpo
summarytools::freq(ang_meta_prpo$name_measurement_instrument)
ang_es_scale <- filter(ang_meta_prpo, name_measurement_instrument == "SCL-90: Hostility") %>%
  select(right_sd, right_n)

ang_es_scale$weighted <- ang_es_scale$right_sd * ang_es_scale$right_n

weighted_sd_ang_prpo <- sum(ang_es_scale$weighted) / sum(ang_es_scale$right_n) # this is the weighted SD of the most commonly used instrument.
print(weighted_sd_ang_prpo)

median_ang_prpo * weighted_sd_ang_prpo # this is the conversion of the median effect size to the most frequently used instrument on that particular outcome.

#SE prpo
summarytools::freq(se_meta_prpo$name_measurement_instrument)
se_es_scale <- filter(se_meta_prpo, name_measurement_instrument == "PSEQ") %>%
  select(right_sd, right_n)

se_es_scale$weighted <- se_es_scale$right_sd * se_es_scale$right_n

weighted_sd_se_prpo <- sum(se_es_scale$weighted) / sum(se_es_scale$right_n) # this is the weighted SD of the most commonly used instrument.
print(weighted_sd_se_prpo)

median_se_prpo * weighted_sd_se_prpo # this is the conversion of the median effect size to the most frequently used instrument on that particular outcome.

#SRF prpo
summarytools::freq(srf_meta_prpo$name_measurement_instrument)
srf_es_scale <- filter(srf_meta_prpo, name_measurement_instrument == "SF-36: social functioning") %>%
  select(right_sd, right_n)

srf_es_scale$weighted <- srf_es_scale$right_sd * srf_es_scale$right_n

weighted_sd_srf_prpo <- sum(srf_es_scale$weighted) / sum(srf_es_scale$right_n) # this is the weighted SD of the most commonly used instrument.
print(weighted_sd_srf_prpo)

median_srf_prpo * weighted_sd_srf_prpo # this is the conversion of the median effect size to the most frequently used instrument on that particular outcome.

#Pintens prpo
summarytools::freq(pintens_meta_prpo$name_measurement_instrument)
pintens_es_scale <- filter(pintens_meta_prpo, name_measurement_instrument == "VAS") %>%
  select(right_sd, right_n)

pintens_es_scale$weighted <- pintens_es_scale$right_sd * pintens_es_scale$right_n

weighted_sd_pintens_prpo <- sum(pintens_es_scale$weighted) / sum(pintens_es_scale$right_n) # this is the weighted SD of the most commonly used instrument.
print(weighted_sd_pintens_prpo)

median_pintens_prpo * weighted_sd_pintens_prpo # this is the conversion of the median effect size to the most frequently used instrument on that particular outcome.

## create static forest plot for testing lay-out
data_testfp <- escalc(measure="SMCR", m1i=right_m, m2i=left_m, sd1i=left_sd, ni=right_n, ri=ri, data=static_ma) %>%
  metafor::summary.escalc() %>%
  filter(contrast == "pre-post" & outcome == "physical function")


tabletext <- cbind(c("author", data_testfp$author),
                   c("year", data_testfp$year))

forestplot(tabletext, data_testfp$yi, data_testfp$ci.lb, data_testfp$ci.ub,
           xlab = "<---favors pre---     ---favors post--->",
           txt_gp=fpTxtGp(label=gpar(cex=1),
                          ticks=gpar(cex=.6),
                          xlab=gpar(cex = 1),
                          title=gpar(cex = 1.1)),
           col=fpColors(box="black", lines="black", zero = "gray50"),
           zero=0, cex=0.5, lineheight = unit(1, "cm"), boxsize=0.3,
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = .1, grid=TRUE,
           align = "l",
           graph.pos = "right",
           clip = c(-4, 4),
           alim = c(-4,4))




## create shinyapp ##

#step1: create tibble for corrected data
data_fp <- as_tibble(corrected_data)

#step 2: create Shinyapp
ui <- fluidPage(
  
  # App title ----
  headerPanel("Forest plot systematic review"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "r_value",
                  label = "R Correction Value:",
                  min = 0,
                  max = 1,
                  value = 0,
                  step = 0.1),
      
      selectInput(inputId = "outcome",
                  label = "select ouctome:",
                  choices = c("health related quality of life", "physical function", "pain interference", "depression", "anxiety",   
                              "self-efficacy", "social functioning", "pain intensity", "anger", "general emotional functioning"),
                  selected = "pain interference"),
      
      selectInput(inputId = "contrast",
                  label = "select contrast:",
                  choices = c("pre-post", "post-fu", "pre-fu"),
                  selected = "pre-post")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("forestplot", height = "1300px"), width = 12
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  
  output$forestplot <- renderPlot({
    
    data_fp$ri <- input$r_value
    data_fp_meta <- escalc(measure="SMCR", m1i=right_m, m2i=left_m, sd1i=left_sd, ni=right_n, ri=ri, data=data_fp)
    fp_meta <- metafor::summary.escalc(data_fp_meta)
    fp_meta2 <- filter(fp_meta, contrast == input$contrast & outcome == input$outcome)
    fp_meta2$tabletext <- cbind(fp_meta2$author, fp_meta2$year, fp_meta2$right_n, fp_meta2$name_measurement_instrument, 
                                fp_meta2$fu_month)
    
    forestplot(fp_meta2$tabletext, fp_meta2$yi, fp_meta2$ci.lb, fp_meta2$ci.ub,
               xlab = "<---favors---     ---favors post--->",
               txt_gp=fpTxtGp(label=gpar(cex=1),
                              ticks=gpar(cex=.6),
                              xlab=gpar(cex = 1),
                              title=gpar(cex = 1.1)),
               col=fpColors(box="black", lines="black", zero = "gray50"),
               zero=0, cex=0.5, lineheight = unit(1, "cm"), boxsize=0.3,
               lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = .1, grid=TRUE,
               align = "l",
               graph.pos = "right",
               clip = c(-4, 4),
               alim = c(-4,4)
    )
  })
  
}

shinyApp(ui=ui, server=server)


#test sorting dataset
pf_meta_prpo <- filter(meta_dat, contrast == "pre-post" & outcome == "physical function")
pf_meta_prpo <- pf_meta_prpo[order(pf_meta_prpo$right_n) ,]


