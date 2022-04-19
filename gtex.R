library(tidyverse)

# tpm <- read.csv("csv/tissues_SMTSD_tpm_mean_curated.csv")
# transcripts <- c("ENST00000343523", "ENST00000357033", "ENST00000359836",
#                 "ENST00000361471", "ENST00000378677", "ENST00000378680", "ENST00000378702",
#                 "ENST00000378707", "ENST00000474231", "ENST00000541735")
# tpm <- subset(tpm, tpm$Transcript %in% transcripts)
# names(tpm) <- gsub("[.]+", "_", names(tpm))
# write.csv(tpm, "csv/dmd_gtex.csv", col.names=TRUE, row.names=FALSE)

# tissue_tpm <- read.csv("csv/dmd_gtex.csv")
# tissue_tpm <- select(tissue_tpm, -c(Adipose_Subcutaneous, Adipose_Visceral_Omentum_, Adrenal_Gland,
#                                     Artery_Aorta, Artery_Coronary, Artery_Tibial, Bladder, Breast_Mammary_Tissue,
#                                     Cells_Cultured_fibroblasts, Cells_EBV_transformed_lymphocytes,
#                                     Cells_Leukemia_cell_line_CML_, Cervix_Ectocervix, Cervix_Endocervix, Colon_Sigmoid,
#                                     Colon_Transverse, Esophagus_Gastroesophageal_Junction, Esophagus_Mucosa,
#                                     Esophagus_Muscularis, Fallopian_Tube, Kidney_Cortex, Kidney_Medulla, Liver, Lung,
#                                     Minor_Salivary_Gland, Nerve_Tibial, Ovary, Pancreas, Prostate,
#                                     Skin_Not_Sun_Exposed_Suprapubic_, Skin_Sun_Exposed_Lower_leg_,
#                                     Small_Intestine_Terminal_Ileum, Spleen, Stomach, Testis, Thyroid, Uterus, Vagina, Whole_Blood))
# write.csv(tissue_tpm, "csv/tissue_tpm.csv", row.names = FALSE)
affected_isoforms <- read.csv("csv/affected_isoforms.csv")
keys <- select(affected_isoforms, c(key, Disease_Type, Genetic_mutation_frame_type))
isoforms <- as.matrix(select(affected_isoforms, -c(key, Genetic_mutation_frame_type, Disease_Type)))
tissue_tpm <- read.csv("csv/tissue_tpm.csv")
tpms <- as.matrix(select(tissue_tpm, -Transcript))
patient_tpm <- data.frame(isoforms %*% tpms)
patient_tpm$key <- keys$key
patient_tpm$Disease_Type <- keys$Disease_Type
patient_tpm$Genetic_mutation_frame_type <- keys$Genetic_mutation_frame_type
write.csv(patient_tpm, "csv/patient_tpm.csv", row.names = FALSE)

