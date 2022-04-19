rm(list = ls())
source("functions.R")
isoforms <- c("Dp427c", "Dp427m", "Dp427p1", "Dp427p2", "Dp260-1", "Dp260-2", "Dp140", "Dp140ab", "Dp140b", "Dp140bc", "Dp140c", "Dp116", "Dp71", "Dp71ab", "Dp71a", "Dp40")
isoform_exons <- read.csv("csv/Isoform List.csv", header = TRUE, row.names = isoforms)
keys <- c("PATIENT_DISPLAY_ID", "FACILITY_DISPLAY_ID")
exclude <- c("SCHEDULED_FORM_NAME", "UPLOADED_BY", "MODIFIED_DT", "FORM_STATUS")
diagnosis <-  preprocess("xlsx/Diagnosis.xlsx", 14, keys, exclude, 1)

diagnosis$Genetic.mutation.frame.type <- gsub("Inframe", "In frame", diagnosis$Genetic.mutation.frame.type)
diagnosis <- diagnosis[(diagnosis$Genetic.mutation.frame.type == "In frame" | diagnosis$Genetic.mutation.frame.type == "Out of frame"),]
diagnosis$Genetic.mutation.type <- tolower(diagnosis$Genetic.mutation.type)
diagnosis <- diagnosis[(diagnosis$Genetic.mutation.type == "deletion" | diagnosis$Genetic.mutation.type == "duplication"),]
diagnosis <- diagnosis[!is.na(diagnosis$Genetic.mutation.location.type),]
diagnosis <- dplyr::select(diagnosis, "key", "Disease.Type", "Genetic.mutation.type", "Genetic.mutation.location.type", "Genetic.mutation.frame.type", "Mutation.Single.exon", "Mutation.Multiple.exon..start", "Mutation.Multiple.exon..end")
diagnosis <- diagnosis[diagnosis$Genetic.mutation.location.type != "Non-exon region",]
diagnosis <- diagnosis[diagnosis$key != "35 1407",]
diagnosis <- diagnosis[diagnosis$key != "34 1282",]
diagnosis <- diagnosis[diagnosis$key != "10 1148",]

exons <- seq(1, 79)

ind_mut_isoforms <- data.frame(matrix(ncol = 17, nrow = 0))
colnames(ind_mut_isoforms) <- c("key", isoforms)
patient_exons <- list()
for (i in 1:nrow(diagnosis)){
  if (diagnosis[i, "Genetic.mutation.location.type"] == "Multiple Exons"){
    affected_exons <- seq.int(as.integer(diagnosis[i, "Mutation.Multiple.exon..start"]), as.integer(diagnosis[i, "Mutation.Multiple.exon..end"]))
  }
  else{
    affected_exons <- as.integer(diagnosis[i, "Mutation.Single.exon"])
  }
  patient_exons <- append(patient_exons, list(c(diagnosis[i, "key"] ,binarize_list(exons, affected_exons), diagnosis[i, "Genetic.mutation.frame.type"], diagnosis[i, "Disease.Type"])))
  affected_isoforms <- c()
  for (j in affected_exons){
    exon <- paste0("X", as.character(j))
    affected_isoforms <- unique(append(affected_isoforms, row.names(isoform_exons[isoform_exons[exon] == 1,])))
  }
  row <- append(diagnosis[i, "key"], binarize_list(isoforms, affected_isoforms))
  ind_mut_isoforms[nrow(ind_mut_isoforms) + 1,] <- row
}
labels <- dplyr::select(diagnosis, "key", "Genetic.mutation.frame.type", "Disease.Type")
ind_mut_isoforms <- merge(ind_mut_isoforms, labels, by="key")
names(ind_mut_isoforms) <- gsub("[.]", "_", names(ind_mut_isoforms))
write.table(ind_mut_isoforms,"csv/affected_isoforms.csv",sep=",",col.names=TRUE, row.names = FALSE)
patient_exons <- data.frame(do.call(rbind, patient_exons))
colnames(patient_exons) <- c("key", seq(1, 79), "Genetic.mutation.frame.type", "Disease.Type")
names(patient_exons) <- gsub("[.]", "_", names(patient_exons))
write.csv(patient_exons,"csv/affected_exons.csv", row.names = FALSE)
