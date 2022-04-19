library(tidyverse)
library(naniar)
library(lubridate)
library(lme4)
library(drc)
source("functions.R")

encounter <- read.csv("csv/encounters/encounter.csv", sep="\t", header=TRUE)
isoforms <- read.csv("csv/affected_isoforms.csv", header=TRUE)
dt <- dplyr::select(isoforms, "key", "Disease_Type", "Genetic_mutation_frame_type")
colnames(dt) <- c("individual", "Disease_Type", "Genetic_mutation_frame_type")
isoforms <- dplyr::select(isoforms, "key", "Genetic_mutation_frame_type")
colnames(isoforms) <- c("individual", "Genetic_mutation_frame_type")
isoforms[isoforms == 0] <- as.character("In Frame")
isoforms[isoforms == 1] <- as.character("Out of Frame")
demo <- read.csv("csv/demographics.csv",sep="\t", header=TRUE)
demo <- dplyr::select(demo, "key", "Date.of.Birth")
colnames(demo) <- c("individual", "DOB")
demo$DOB <- do.call("c", lapply(demo$DOB, get_date))
individuals <- as.vector(isoforms[["individual"]])
metrics <- encounter[, c("key", "individual", "CASE_ID", "Disease.Type", "Brooke.Arms.and.Shoulders",
                     "Brooke.Hips.and.Legs", "Encounter.Date")]
metrics <- filter(metrics, metrics$individual %in% individuals)
names(metrics) <- gsub("[.]", "_", names(metrics))
metrics$Encounter_Date <- do.call("c", lapply(metrics$Encounter_Date, get_date))
metrics <- merge(metrics, isoforms, by="individual")
metrics <- merge(metrics, demo, by="individual")
metrics$age <- time_length(difftime(metrics$Encounter_Date, metrics$DOB), "years")

bas <- dplyr::select(metrics, -Brooke_Hips_and_Legs)
bas <- na.omit(bas)
bas <- bas[- grep("Not Tested", bas$Brooke_Arms_and_Shoulders),]
bas <- bas[- grep("Patient cannot perform", bas$Brooke_Arms_and_Shoulders),]
bas <- bas %>% group_by(individual) %>% filter(n() > 1)
bas <- as.data.frame(bas)
bas$Brooke_Arms_and_Shoulders <- as.numeric(sub("([0-9]+).*$", "\\1", bas$Brooke_Arms_and_Shoulders))

bas_fits <- lmList(Brooke_Arms_and_Shoulders ~ age | individual, data=bas)
bas_fits <- coef(bas_fits)
bas_fits <- tibble::rownames_to_column(bas_fits, "individual")
bas_k <- all_ks(bas, "age", "Brooke_Arms_and_Shoulders", "individual")$k
bas_fits$k <- bas_k
colnames(bas_fits) <- c("individual", "intercept", "slope", "k")
bas_fits <- merge(bas_fits, dt, by="individual")

bhl <- dplyr::select(metrics, -Brooke_Arms_and_Shoulders)
bhl <- na.omit(bhl)
bhl <- bhl[- grep("Not Tested", bhl$Brooke_Hips_and_Legs),]
bhl <- bhl[- grep("Patient cannot perform", bhl$Brooke_Hips_and_Legs),]
bhl <- bhl %>% group_by(individual) %>% filter(n() > 1)
bhl <- as.data.frame(bhl)
bhl$Brooke_Hips_and_Legs <- as.numeric(sub("([0-9]+).*$", "\\1", bhl$Brooke_Hips_and_Legs))

bhl_fits <- lmList(Brooke_Hips_and_Legs ~ age | individual, data=bhl)
bhl_fits <- coef(bhl_fits)
bhl_fits <- tibble::rownames_to_column(bhl_fits, "individual")
bhl_k <- all_ks(bhl, "age", "Brooke_Hips_and_Legs", "individual")$k
bhl_fits$k <- bhl_k
colnames(bhl_fits) <- c("individual", "intercept", "slope", "k")
bhl_fits <- merge(bhl_fits, dt, by="individual")

write.csv(bas, "csv/bas.csv", row.names=FALSE)
write.csv(bhl, "csv/bhl.csv", row.names=FALSE)
colnames(bas_fits) <- c("key", "intercept", "slope", "k", "Disease_Type", "Genetic_mutation_frame_type")
write.csv(bas_fits, "csv/bas_fits.csv", row.names=FALSE)
colnames(bhl_fits) <- c("key", "intercept", "slope", "k", "Disease_Type", "Genetic_mutation_frame_type")
write.csv(bhl_fits, "csv/bhl_fits.csv", row.names=FALSE)