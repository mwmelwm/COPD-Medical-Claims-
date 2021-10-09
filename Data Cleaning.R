# import libraries
if (!require("data.table")) {
  install.packages("data.table")
  library("data.table")
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}
if (!require("lubridate")) {
  install.packages("lubridate")
  library("lubridate")
}
if (!require("devtools")) {
  devtools::install_github("wtcooper/icdcoder")
  library("devtools")
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

#-----------------------------------------------
# initial data cleaning
#-----------------------------------------------

# convert year/month to year/month/date
claims$MONTH <- as.Date(paste(claims$MONTH, "01", sep = "/"))

# remove variables that won't be used after doing research
# -SERVICE_CNT (col 9)
# -PRIMARY_ICD_DX_DESC (col 14)
# -CPT4_DESC (col 17)
claims <- claims[,-c(9,14,17)]

# drop patients under age 18 since these patients are /outliers
claims = claims %>%
  filter(! AGE_RANGE %in% c( "1 - 9",  "10 - 17"))

# make a new column category to indicate respiratory related claims
claims$cat = ifelse(grepl("^J", claims$ICD10.1), 
                    "Respiratory Non COPD", "Non Respiratory")

# mark COPD related claims in those of respiratory related claims
claims$cat = ifelse(grepl("^J4[0-7]", claims$ICD10.1), 
                    "COPD", claims$cat)

# drop icd codes related to "Injury, poisoning and certain other consequences of external causes"(S00-T88)
# and "External causes of morbidity" (V00-Y99) 
claims = claims %>%
  filter(!ICD10.1 %in% grep("^S|T|V|W|X|Y", claims$ICD10.1, value=TRUE))

#-----------------------------------------------
# data transformation for data visualization / modeling
#-----------------------------------------------

# idx is a list of integer vectors with one vector per patient unique id
# the vector contains indexes of the claims associated with the unique id 
idx <- split(seq_len(nrow(claims)), claims$UNIQUE_ID)

# claims_per_patient is a list of data frames with one data frame per patient
# each row within the data frame is a claim
# mclapply is the parallelized version of mapply
claims_per_patient <- mclapply(idx, function(x){ 
  claims[x,] 
},
mc.cores = floor(detectCores() * .75),
mc.cleanup = TRUE)

# uniqueid is a data frame with one row per patient
# use this data frame to look at demographics 
uniqueid = claims[!duplicated(claims$UNIQUE_ID),]

#-----------------------------------------------
# save data
#-----------------------------------------------

#write R objects to files
saveRDS(claims_per_patient, file="claims_per_patient.RData")
saveRDS(uniqueid, file="uniqueid.RData")



