# import libraries
if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}

#-----------------------------------------------
# load data
#-----------------------------------------------

# read in data
claims <- fread("COPD_MED.csv", stringsAsFactors = TRUE)

#restore R objects
claims_per_patient <- readRDS("claims_per_patient.RData")
uniqueid <- readRDS("uniqueid.RData")
patients <- readRDS("patients.RData")

#-----------------------------------------------
# exploratory data analysis
#-----------------------------------------------

# bar chat of age distribution of patients
uniqueid$AGE_RANGE <- factor(uniqueid$AGE_RANGE, levels =c("< 1", "1 - 9", "10 - 17", "18 - 25", "26 - 44", "45 - 64", "65 - 74", "75 - 84", ">= 85"))

ggplot(data=uniqueid, aes(x=AGE_RANGE)) +
  geom_bar(stat="count", width=0.7, fill=1:6) + ggtitle("Age Distribution") + xlab("Age") + ylab("Count") + theme(plot.title=element_text(hjust = 0.5),
                                                                                                                  panel.background=element_blank(),
                                                                                                                  axis.line = element_line(colour = "black")) +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# bar chat of gender distribution of patients
uniqueid$sortGender = factor(uniqueid$GENDER_CODE,
                             levels=names(sort(table(uniqueid$GENDER_CODE),
                                               decreasing=TRUE)))

ggplot(data=uniqueid, aes(x=sortGender)) +
  geom_bar(stat="count", width=0.7, fill=1:2) + ggtitle("Gender Distribution") + xlab("Gender") + ylab("Count") + theme(plot.title=element_text(hjust = 0.5),
                                                                                                                        panel.background=element_blank(),
                                                                                                                        axis.line = element_line(colour = "black"))
# bar chat of area deprivation index/socioeconomic status distribution of patients
ggplot(data=uniqueid, aes(x=AREA_DEPRIV_INDEX)) +
  geom_histogram(binwidth=2, fill="lightblue", color="black")  +
  labs(title="ADI Distribution", x="Area Deprivation Index", y="Count") + theme_bw()

# bar chat of line of business distribution of patients
uniqueid$sortLOB = factor(uniqueid$LOB,
                          levels=names(sort(table(uniqueid$LOB),
                                            decreasing=TRUE))) 

ggplot(data=uniqueid, aes(x=sortLOB)) +
  geom_bar(stat="count", width=0.7, fill=1:8) + ggtitle("LOB Distribution") + xlab("LOB") + ylab("Count") + theme(plot.title=element_text(hjust = 0.5),
                                                                                                                  panel.background=element_blank(),
                                                                                                                axis.line = element_line(colour = "black"))


# bar chart of service location of each claim
claims$sortHIAA_POS_DESC = factor(claims$HIAA_POS_DESC,
                          levels=names(sort(table(claims$HIAA_POS_DESC),
                                            decreasing=TRUE))) 
ggplot(data=claims, aes(x=sortHIAA_POS_DESC)) +
  geom_bar(stat="count", width=0.7, fill=1:42) + ggtitle("Service Location Distribution") + xlab("Service Location") + ylab("Count") + theme(plot.title=element_text(hjust = 0.5),
                                                                                                                  panel.background=element_blank(),
                                                                                                                  axis.line = element_line(colour = "black"))

# function:    plotOnePatient  
# description: create a stacked bar chart to visualize claims by icd categories
#              from the id of a patient in the dataset
# parameter:
#             'patient_simplified_id': an integer, the id of a patient 
#              in the dataset
# value:
#              a stacked bar chart of claim count by month by different 
#              icd categories for the patient id given
plotOnePatient = function(patient_simplified_id){
  temp = claims_per_patient[[patient_simplified_id]] %>%
    group_by(MONTH,cat) %>%
    summarise(claim_count = n()) %>%
    arrange(desc(MONTH))
  p_title = paste("Patient",
                  patient_simplified_id, "
                  Claim Distribution by ICD Categories")
  ggplot(data = temp, aes(x = MONTH, 
                          y = claim_count, 
                          fill = cat)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Year-Month having Claims") + 
    labs(y = "Number of Claims") + 
    labs(fill = "ICD Categories") + 
    labs(title = p_title )
}

# stacked bar chart of claim count by month colored by different icd categories 
# for a random patient in the dataset
icdPlot <- plotOnePatient(sample(length(claims_per_patient), 1))
icdPlot

# summary statistics of variables
summary(patients)

# scatterplots of variable combinations
pairs(patients)

# correlations of numeric variables 
cor(patients[sapply(patients, function(x) !is.factor(x))])

