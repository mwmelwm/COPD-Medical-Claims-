# import libraries

if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}

if (!require("parallel")) {
  install.packages("parallel")
  library("parallel")
}

#-----------------------------------------------
# read data
#-----------------------------------------------

# read in data
claims <- fread("COPD_MED.csv", stringsAsFactors = TRUE)

#restore R objects
claims_per_patient <- readRDS("claims_per_patient.RData")

#-----------------------------------------------
# feature engineering 
#-----------------------------------------------

# find code that is associated with the emergency room
unique(claims$HIAA_POS_CODE[claims$HIAA_POS_DESC=="EMERGENCY ROOM - HOSPITAL"])
unique(claims$FINANCIAL_SERVICE_SUBGRP_CODE[claims$FINANCIAL_SERVICE_SUBGRP_DESC=="Emergency Room Technical"])

# static variables that indicate ED visits 
ED_FIN_SUB_CAT = 200210
ED_HIAA_code = 23

# function:    createPredictors
# description: create a data frame of one row for each patient with aggregated information on
#              his/her claims and demographics from both halves of the four year time period
# parameter:
#             'df': a data frame with all claims for a patient
#              each row is a claim for the patient
# value:
#              a data frame of one row for a patient
#              with columns 'UNIQUE_ID', ED_X', 'ED_Y',
#             'CLAIM_X', 'CLAIM_X', 'RES_ED_Y', and other predictors
createPredictors <- function(df) {
  #store max date of patient's claims 
  start <- max(as.Date(df$MONTH))
  #store min date of patient's claims
  end <- min(as.Date(df$MONTH))
  #store start of 2nd year date
  pred <- start + 730
  
  #split into x-predictor(first two years), y-response(second two years)
  x <- filter(df, df$MONTH > start, df$MONTH <= pred)
  y <- filter(df, df$MONTH > pred, df$MONTH <= end)
  
  #keep demographic variables: 
  # UNIQUE_ID (col 1)
  # LOB (col 2)
  # GENDER_CODE (col 4)
  # AGE_RANGE (col 5)
  # DECEASED (col 6)
  # TERMED (col 7)
  # ADI (col 14)
  
  out = df[1,c(1:2, 4:7, 14)]
  
  # add predictors 
  
  #total number of COPD ED visits in the first two years
  out$COPD_ED_X = as.numeric(x %>%
                               filter(HIAA_POS_CODE==ED_HIAA_code, 
                                      FINANCIAL_SERVICE_SUBGRP_CODE==ED_FIN_SUB_CAT,
                                      cat=="COPD") %>%
                               summarise(COPD_ED_X = n()))
  
  #total number of COPD ED visits in the second two years
  out$COPD_ED_Y = as.numeric(y %>%
                               filter(HIAA_POS_CODE==ED_HIAA_code, 
                                      FINANCIAL_SERVICE_SUBGRP_CODE==ED_FIN_SUB_CAT,
                                      cat=="COPD") %>%
                               summarise(COPD_ED_Y = n()))
  
  #total number of respiratory non COPD ED visits in the first two years
  out$RES_ED_X = as.numeric(x %>%
                              filter(HIAA_POS_CODE==ED_HIAA_code, 
                                     FINANCIAL_SERVICE_SUBGRP_CODE==ED_FIN_SUB_CAT,
                                     cat=="Respiratory Non COPD") %>%
                              summarise(RES_ED_X = n()))
  
  #total number of respiratory non COPD ED visits in the second two years
  out$RES_ED_Y = as.numeric(y %>%
                              filter(HIAA_POS_CODE==ED_HIAA_code, 
                                     FINANCIAL_SERVICE_SUBGRP_CODE==ED_FIN_SUB_CAT,
                                     cat=="Respiratory Non COPD") %>%
                              summarise(RES_ED_Y = n()))
  
  #total number of images in the first two years
  out$IMAGE_X = as.numeric(x %>%
                             filter(BETOS_CATEGORY=="IMAGING") %>%
                             summarise(IMAGE_X = n()))
  
  #total number of tests in the first two years 
  out$TEST_X = as.numeric(x %>%
                            filter(BETOS_CATEGORY=="TESTS") %>%
                            summarise(TEST_X = n()))
  
  #total number of pulmonary diseases claims in the first two years 
  out$PULM_X = as.numeric(x %>%
                            filter(FINANCIAL_SERVICE_SUBGRP_DESC
                                   =="Pulmonary Diseases") %>%
                            summarise(PULM_X = n()))
  
  #total number of cardic diseases claims in the first two years (comobidities)
  out$CARD_X = as.numeric(x %>%
                            filter(FINANCIAL_SERVICE_SUBGRP_DESC
                                   =="Cardiology") %>%
                            summarise(CARD_X = n()))
  
  #total radiology claims in the first two years 
  out$RAD_X = as.numeric(x %>%
                           filter(FINANCIAL_SERVICE_SUBGRP_DESC
                                  =="Radology") %>%
                           summarise(RAD_X = n()))
  
  #total number of claims in the first two years
  out$CLAIM_X = x %>%
    summarise(CLAIM_X = n())
  
  #total number of ed visits in the first two years
  out$ED_X = x %>%
    filter(HIAA_POS_CODE==ED_HIAA_code, 
           FINANCIAL_SERVICE_SUBGRP_CODE==ED_FIN_SUB_CAT) %>%
    summarise(ED_X = n())
  
  #total number of claims in the second two years
  out$CLAIM_Y = y %>%
    summarise(CLAIM_Y = n())
  
  #total number of ed visits in the second two years
  out$ED_Y = y %>%
    filter(HIAA_POS_CODE==ED_HIAA_code, 
           FINANCIAL_SERVICE_SUBGRP_CODE==ED_FIN_SUB_CAT) %>%
    summarise(ED_Y = n())
  
  out$ED_X <- as.numeric(out$ED_X)
  out$ED_Y <- as.numeric(out$ED_Y)
  out$CLAIM_X <- as.numeric(out$CLAIM_X)
  out$CLAIM_Y <- as.numeric(out$CLAIM_Y)
  out
}

#-----------------------------------------------
# data transformation for modeling 
#-----------------------------------------------

# 'patients' is a data frame with one row per patient 
# each row has demographic and aggregated information for all patients within the claims data
patients <- do.call(rbind, mclapply(claims_per_patient, createPredictors,
                                    mc.cores = floor(detectCores() * .75),
                                    mc.cleanup = TRUE))

# transform ED_Y to binary response variable 
patients$ED_Y=ifelse(patients$ED_Y < 10, "NonCostly", "Costly")
patients$ED_Y=as.factor(patients$ED_Y)

# run more exploratory data analysis on the final dataset in EDA.R

# remove all the unnecessary columns in order to do (ED_Y ~ .)
# final dataset for modeling
data = patients[,-which(names(patients) %in% 
                          c("UNIQUE_ID","DECEASED","TERMED","COPD_ED_Y", 
                            "RES_ED_Y","CLAIM_Y","START_DATE","END_DATE",
                            "PRED_DATE","RAD_X"))]

#-----------------------------------------------
# save data
#-----------------------------------------------

#write R objects to files 
saveRDS(patients, file="patients.RData")
saveRDS(data, file="data.RData")

