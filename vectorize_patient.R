library(tidyverse)
library(naniar)
encounter <- read.csv("csv/encounters/encounter.csv", sep="\t", header = TRUE)
isoforms <- read.csv("csv/affected_isoforms.csv", header = TRUE)
isoforms <- select(isoforms, "key", "Genetic_mutation_frame_type")
colnames(isoforms) <- c("individual", "Genetic_mutation_frame_type")
individuals <- as.vector(isoforms[["individual"]])
metrics <- encounter[, c("key", "individual", "CASE_ID", "Disease.Type", "Patient.Height", "Patient.Weight",
                     "Feeding.Tube.Placement", "ICD.Placement", "Scoliosis.Surgery", "Tendon.Release.Surgery",
                     "Tracheostomy", "Glucocorticoid.Use", "Cushingoid.Features.Diagnosis",
                     "Excessive.Weight.Gain.Diagnosis", "Delayed.Puberty.Diagnosis", "Growth.Suppression.Diagnosis",
                     "Hypertension.Diagnosis", "Behavioral.Disturbance.Diagnosis", "Acne.Diagnosis",
                     "Skin.Fragility.Diagnosis", "Diabetes.Diagnosis", "Cataracts.Diagnosis", "Gastric.Ulcer.Diagnosis", "Fungal.Infection.Diagnosis",
                     "Steroid.induced.fractures", "Steroidal.Complication..Other.Diagnosis", "Brooke.Arms.and.Shoulders",
                     "Brooke.Hips.and.Legs", "Stretching.Exercises", "Wheelchair.Use", "ECG.Result", "Echo.Result",
                     "Cardiomyopathy", "Nutritional.Supplementation", "Non.invasive.ventilation.use",
                     "Tracheostomy.with.invasive.ventilation.use", "Feeding.Tube.Y.N")]
metrics <- filter(metrics, metrics$individual %in% individuals)
names(metrics) <- gsub("[.]", "_", names(metrics))
metrics[metrics == "Unknown" | metrics == "Not Tested" | metrics == "Unable to Evaluate" | metrics == "N/A - patient is under age 3" | metrics == "Patient cannot perform"] <- NA
metrics[is.na(metrics)] <- 0
metrics[metrics == "No" | metrics == "Never" | metrics == "Normal"] <- as.numeric(0)
metrics[metrics == "Yes" | metrics == "Abnormal"] <- as.numeric(1)
# Glucocorticoid
metrics[metrics == "No, but recommended at this visit" | metrics == "No, but previously"] <- as.numeric(0)
metrics[metrics == "Yes, prednisone currently" | metrics == "Yes, Deflazacort currently" | metrics == "Yes, Other"] <- as.numeric(1)
# wheelchair
metrics[metrics == "Yes, intermittent (patient is still able to walk)"] <- as.numeric(1)
metrics[metrics == "Yes, permanent (patient is not able to walk and needs a wheelchair to move)"] <- as.numeric(2)
# ECG
metrics[metrics == "Borderline"] <- as.numeric(1)
metrics[metrics == "Abnormal, not clinically significant"] <- as.numeric(2)
metrics[metrics == "Abnormal, related to muscular dystrophy"] <- as.numeric(3)
metrics[metrics == "Abnormal, clinically significant"] <- as.numeric(4)
# Echo
metrics[metrics == "Yes; only during chest infections"] <- as.numeric(1)
metrics[metrics == "Yes; using only at night"] <- as.numeric(2)
metrics[metrics == "Yes; using at night and during the day"] <- as.numeric(3)
# Brooke
metrics$Brooke_Arms_and_Shoulders <- as.numeric(sub("([0-9]+).*$", "\\1", metrics$Brooke_Arms_and_Shoulders))
metrics$Brooke_Hips_and_Legs <- as.numeric(sub("([0-9]+).*$", "\\1", metrics$Brooke_Hips_and_Legs))

metrics <- merge(metrics, isoforms, by="individual")
patients <- metrics %>% group_by(individual) %>% slice_max(n = 1, CASE_ID)
patients <- as.data.frame(patients)
write.csv(metrics, "csv/metrics.csv", row.names=FALSE)
write.csv(patients, "csv/patients.csv", row.names=FALSE)