#install.packages
#install.packages("prob")

#load packages
#library(magrittr)
#library(prob)

#create dataframe for all included questionnaires
#Use dataframe as follows: df$max_scale <- questionnaires$q[match(as.character(df$instrument_name), as.numeric(questionnaires$max_scale))]

questionnaires <- structure(list(V1 = c("VAS", "VAS (0-100)", "PRI", "MPI: pain severity (0-6)"),
                                 V2 = c(0, 0, 0, 0),
                                 V3 = c(100, 100, 78, 6),
                                 V4 = c(1, 1, 1, 1)), 
                                 .Names = c("q", "min_scale", "max_scale", "reverse_scoring"), 
                                 class = "data.frame",
                                 row.names = c(NA, -4L))


#V4: if higher scores indicate better functioning, no reverse scoring (0), if lower scores indicate better functioning, reverse scoring (1)


#add additional questionnaires. c1: instrument name on google forms; c2: min_scale; c3 max_scale; c4 reverse_scoring
questionnaires <- questionnaires %>%
  rbind(
    vector_nrs <- c("NRS", 0, 10, 1),            
    vector_nprs <- c("NPRS", 0, 10, 1),
    vector_nrs010 <- c("NRS (0-10)", 0, 10, 1),
    vector_VAS010 <- c("VAS (0-10)", 0, 10, 1),
    vector_NRS0100 <- c("NRS (0-100)", 0, 100, 1),
    vector_likert_pintens <- c("Likert pain intensity", 0, 6, 1),
    vector_rdq <- c("RDQ", 0, 24, 1),
    vector_QBPDS <- c("QBPDS", 0, 100, 1),
    vector_LBPRS <- c("LBPRS", 0, 30, 1),
    vector_DRI <- c("DRI", 0, 1200, 1),
    vector_DRI0100 <- c("DRI (0-100)", 0, 100, 1),
    vector_MPI_PI <- c("MPI: pain interference (0-6)", 0, 6, 1),
    vector_MPI_PI_012 <- c("MPI: pain interference (0-12)", 0, 12, 1),
    vector_MPI_PS_012 <- c("MPI: pain severity (0-12)", 0, 12, 1),
    vector_ODI <- c("ODI", 0, 100, 1),
    vector_ODI01 <- c("ODI (0-1)", 0, 1, 1),
    vector_PDI <- c("PDI", 0, 70, 1),
    vector_RMDQ <- c("RMDQ", 0, 24, 1),
    vector_DPQ_da <- c("DPQ: Daily activities", 0, 100, 1),
    vector_sf36_pf <- c("SF-36 subscale Physical Functioning", 0, 100, 0),
    vector_NHP_pa <- c("NHP: PA", 0, 100, 1),
    vector_hfaq <- c("HFAQ", 0, 100, 0),
    vector_mpi_ga_06 <- c("MPI: GA (0-6)", 0, 6, 0),
    vector_norfunk <- c("Norfunk (0-3)", 0, 3, 1),
    vector_ADS <- c("ADS (german scale of CES-D)", 0, 60, 1),
    vector_haDs <- c("HADS-D", 0, 21, 1),
    vector_dass <- c("DASS", 0, 42, 1),
    vector_bdi2 <- c("BDI-II", 0, 63, 1),
    vector_zung <- c("Zung", 0, 100, 1),
    vector_bdi <- c("BDI", 0, 63, 1),
    vector_deps <- c("Depression index (DEPS)", 0, 30, 1),
    vector_scl90d <- c("SCL90-D", 0, 52, 1),
    vector_hAds <- c("HADS-A", 0, 21, 1),
    vector_vas_a <- c("VAS Anxiety (0-100)", 0, 100, 1),
    vector_scl90a <- c("SCL90-A", 0, 40, 1),
    vector_stai <- c("STAI", 20, 80, 1),
    vector_fri <- c("FRI", 0, 100, 1),
    vector_nhp_er <- c("NHP: Emotional reactions", 0, 100, 1),
    vector_sf36mh <- c("SF-36: mental health", 0, 100, 0),
    vector_MPI_di_012 <- c("MPI: distress (0-12)", 0, 12, 1),
    vector_dpq_ad <- c("DPQ: anxiety/depression", 0, 100, 1),
    vector_scl90h <- c("SCL-90: Hostility", 0, 24, 1),
    vector_pseq <- c("PSEQ", 0, 60, 0),
    vector_dpq_sl <- c("DPQ: social life", 0, 100, 1),
    vector_sf36_sf <- c("SF-36: social functioning", 0, 100, 0),
    vector_sip <- c("SIP", 0, 9608, 1),
    vector_qbprs <- c("QBPRS", 0, 100, 1),
    vector_euroq_vas <- c("EuroQol-5D-3L (VAS 0-100)", 0, 100, 0),
    vector_mean_nhp <- c("mean NHP (0-100)", 0, 100, 1),
    vector_german_lsq <- c("German Life Satisfaction Questionnaire (higher is better)", 7, 49, 0),
    vector_fiq <- c("FIQ (0-100)", 0, 100, 1),
    vector_whoqol_bref <- c("WHOQOL-BREF: global (0-100)", 0, 100, 0),
    vector_lisat11 <- c("LiSat-11: life a a whole (1-6)", 1, 6, 0),
    vector_comi_pain <- c("COMI: pain (0-10)", 0, 10, 1),
    vector_comi_function <- c("COMI: function (0-10)", 0, 10, 1),
    vector_rand_pf <- c("RAND-36 subscale Physical Functioning", 0, 100, 0),
    vector_dass_anx <- c("DASS: anxiety", 0, 42, 1),
    vector_mpi0_pi_0100 <- c("MPI: pain interference (0-100)", 0, 100, 1),
    vector_GSE <- c("GSE (1-4)", 1, 4, 0),
    vector_DSkala <- c("DS (depressivitats-skala)", 1, 10, 1)
    )


#merge additional questionnaires to main set

i_hrqol <- as.tibble(unique(dat_clean$hrqol_name_measurement_instrument))
i_pf <- as.tibble(unique(dat_clean$pf_measurement_name))
i_pinter <- as.tibble(unique(dat_clean$pinter_name_measurement_instrument))
i_anx <- as.tibble(unique(dat_clean$anx_name_measurement_instrument))
i_dep <- as.tibble(unique(dat_clean$dep_name_measurement_instrument))
i_ef <- as.tibble(unique(dat_clean$ef_name_measurement_instrument))
i_ang <- as.tibble(unique(dat_clean$ang_name_measurement_instrument))
i_se <- as.tibble(unique(dat_clean$se_name_measurement_instrument))
i_srf <- as.tibble(unique(dat_clean$srf_name_measurement_instrument))
i_pintens <- as.tibble(unique(dat_clean$pintens_name_measurement_instrument))

i_total <- rbind(i_hrqol, i_pf, i_pinter, i_anx, i_dep, i_ef, i_ang, i_se, i_srf, i_pintens) %>% na.omit

#check if all questionnaires are present in questionnaires dataframe
i_total[!i_total$value %in% questionnaires$q,  ]
                            