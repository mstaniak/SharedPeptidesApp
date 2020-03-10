library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(BiocManager)
options(repos = BiocManager::repositories())
library(BRAIN)
library(MALDIquant)

# all_ms1 = readRDS("www/all_precursor_spectra.RDS")
all_ms1 = readRDS("www/all_ms1_max.RDS")

peptides_summary = all_ms1 %>%
    select(precursor_scan, target_precursor_mz, charge, sequence, proteins, rep) %>%
    group_by(proteins, sequence, target_precursor_mz, precursor_scan, charge) %>%
    summarise(rep = unique(rep)) %>%
    mutate(present = 1L) %>%
    spread(proteins, present, fill = 0L) %>%
    ungroup()

in_brief = peptides_summary %>%
    mutate(is_shared = HPRR4190190 + HPRR590037 == 2L) %>%
    select(sequence, rep, target_precursor_mz, charge, is_shared, precursor_scan) %>%
    group_by(sequence, charge) %>%
    summarise(n_reps = n_distinct(rep),
              n_scans = n_distinct(precursor_scan),
              is_shared = unique(is_shared),
              n_targets = n_distinct(target_precursor_mz))

