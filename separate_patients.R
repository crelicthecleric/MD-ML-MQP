rm(list = ls())
library(xlsx)
source("functions.R")

isoform_patients <- read.csv("csv/affected_isoforms.csv", header = TRUE)
patients <- isoform_patients$key
encounter <- read.csv("csv/encounters/encounter.csv", sep="\t", header = TRUE)
encounter <- split_cases(patients, encounter)
hospitalization <- read.csv("csv/encounters/hospitalization.csv", sep="\t", header = TRUE)
hospitalization <- split_cases(patients, hospitalization)
pulmonary_device <- read.csv("csv/encounters/pulmonary_device.csv", sep="\t", header = TRUE)
pulmonary_device <- split_cases(patients, pulmonary_device)
icd <- read.csv("csv/encounters/icd.csv", sep="\t", header = TRUE)
icd <- split_cases(patients, icd)
surgery <- read.csv("csv/encounters/surgery.csv", sep="\t", header = TRUE)
surgery <- split_cases(patients, surgery)
assistive_device <- read.csv("csv/encounters/assistive_device.csv", sep="\t", header = TRUE)
assistive_device <- split_cases(patients, assistive_device)
tendon_release <- read.csv("csv/encounters/tendon_release.csv", sep="\t", header = TRUE)
tendon_release <- split_cases(patients, tendon_release)
feeding_tube <- read.csv("csv/encounters/feeding_tube.csv", sep="\t", header = TRUE)
feeding_tube <- split_cases(patients, feeding_tube)
npi <- read.csv("csv/encounters/npi.csv", sep="\t", header = TRUE)
npi <- split_cases(patients, npi)
medication <- read.csv("csv/encounters/medication.csv", sep="\t", header = TRUE)
medication <- split_cases(patients, medication)
trial <- read.csv("csv/encounters/trial.csv", sep="\t", header = TRUE)
trial <- split_cases(patients, trial)
scoliosis <- read.csv("csv/encounters/scoliosis.csv", sep="\t", header = TRUE)
scoliosis <- split_cases(patients, scoliosis)
tracheostomy <- read.csv("csv/encounters/tracheostomy.csv", sep="\t", header = TRUE)
tracheostomy <- split_cases(patients, tracheostomy)
sheets <- c("encounter", "hospitalization", "pulmonary_device", "icd", "surgery", "assistive_device", "tendon_release", "feeding_tube", "npi", "medication", "trial", "scoliosis", "tracheostomy")

for (individual in patients) {
  for (sheet in sheets) {
    df <- get(sheet)
    if (df[individual[1]] %in% df) {
      file_path <- paste0("xlsx/patients/", individual, ".xlsx")
      write.xlsx2(df[[individual[1]]], file_path, sheetName=sheet, append=TRUE)
    }
  }
}
















# visits <- table(unlist(indel_encounters$individual))
# print(length(visits))
# print(length(visits[visits > 1]))
# hist(visits, breaks = seq(0, max(visits), 1), main="Number of Encounters", xlab="Encounters")