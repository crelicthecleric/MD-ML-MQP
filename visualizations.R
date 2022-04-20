library(tidyverse)
library(reshape)
library(umap)
source("functions.R")

mutation_types <- read.csv("csv/mutation_types.csv")
encounters <- read.csv("csv/encounters/encounter.csv", sep = "\t")
visits <- encounters %>% group_by(individual, Disease.Type) %>% summarise(n = n())

affected_exons <- read.csv("csv/affected_exons.csv")
affected_exons <- merge(affected_exons, dplyr::select(mutation_types, key, Genetic.mutation.type), "key")
affected_exons <- melt(affected_exons, id=c("key", "Genetic_mutation_frame_type", "Disease_Type", "Genetic.mutation.type"))
colnames(affected_exons) <- c("individual", "Genetic_mutation_frame_type", "Disease_Type", "Genetic.mutation.type", "exon", "affected")
affected_exons <- affected_exons[affected_exons$affected == 1, ]
affected_exons$exon <- as.numeric(gsub(".*?([0-9]+).*", "\\1", affected_exons$exon))

tissue_tpm <- read.csv("csv/tissue_tpm.csv")
tissue_tpm <- dplyr::select(tissue_tpm, -Transcript)
tissue_tpm <- data.frame(TPM=unlist(tissue_tpm))
tissue_tpm <- subset(tissue_tpm, TPM != 0)

bas <- read.csv("csv/bas.csv")
bas_fits <- read.csv("csv/bas_fits.csv")
bhl <- read.csv("csv/bhl.csv")
bhl_fits <- read.csv("csv/bhl_fits.csv")

exon_int <- read.csv("csv/exons_interest.csv")
exon_int <- exon_int[exon_int$type != "walk", ]

mutsum <- ggplot(data=mutation_types, aes(Genetic.mutation.type, fill=Genetic.mutation.frame.type)) +
  geom_bar(position=position_dodge()) +
  labs(title="Mutations by Type and Frame", x ="Mutation Type", y = "Count", fill = "Frame Type") +
  theme(axis.text.x = element_text(angle = 60, hjust=1))


visitsum <- ggplot(visits, aes(n, fill = Disease.Type)) + geom_histogram(binwidth=1) +
  labs(title="Distribution of Visit Counts\nby Individual and Disease Type", x ="Number of Visits", y = "Count", fill = "Disease Type")

locsum <- ggplot(affected_exons, aes(exon, fill = Genetic_mutation_frame_type)) + geom_histogram(binwidth=1) +
  labs(title="Distribution of Affected Exons", x ="Exon", y = "Count", fill = "Frame Type") +
  facet_grid(~Disease_Type)

tpmsum <- ggplot(tissue_tpm) + geom_histogram(aes(x=TPM), binwidth=0.1) +
  labs(title="Distribution of TPM Values", y = "Count")

basage <- ggplot(bas, aes(x=age, y=Brooke_Arms_and_Shoulders)) +
  geom_line(aes(group=individual, color=Disease_Type)) +
  ggtitle("BUE Score vs. Age by Individual") +
  labs(x ="Age", y = "BUE Score", color = "Disease")

basreg <- ggplot(bas_fits, aes(x=slope, y=intercept)) +
  geom_point(alpha = 0.5, aes(shape=Disease_Type, color=Genetic_mutation_frame_type)) +
  ggtitle("BUE Regression\nSlope vs. Intercept by Disease and Frame Type") +
  labs(x ="Slope", y = "Intercept", shape = "Disease", color = "Frame Type")

bhlage <- ggplot(bhl, aes(x=age, y=Brooke_Hips_and_Legs)) +
  geom_line(aes(group=individual, color=Disease_Type)) +
  ggtitle("VLE Score vs. Age\nby Individual") +
  labs(x ="Age", y = "VLE Score", color = "Disease")

bhlreg <- ggplot(bhl_fits, aes(x=slope, y=intercept)) +
  geom_point(alpha = 0.5, aes(shape=Disease_Type, color=Genetic_mutation_frame_type)) +
  ggtitle("VLE\nSlope vs. Intercept\nby Disease and Frame Type") +
  labs(x ="Slope", y = "Intercept", shape = "Disease", color = "Frame Type")

eoidist <- ggplot(exon_int, aes(exon, fill = type)) + geom_histogram(binwidth=1) +
  labs(title="Distribution of Best Model Exons", x ="Exon", y = "Number of Metrics", fill = "Measure")
  facet_grid(~response)

# mutsum
# visitsum
# locsum
# tpmsum
# basage
# basreg
# bhlage
# bhlreg
# eoidist

save_img(mutsum)
save_img(visitsum)
save_img(locsum)
save_img(tpmsum)
save_img(basage)
save_img(basreg)
save_img(bhlage)
save_img(bhlreg)
save_img(eoidist)