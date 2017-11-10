twoConditions <- read.delim('RAFE_temp_2017-06-22_15_57_03.1D', header=FALSE)

keep_ind <- seq(1, 240*42, by=42)
keep_ind %>% head(10)

twoConditions <- twoConditions[keep_ind,]

singleCondition <- read.delim('RAFE_temp_2017-06-22_16_01_49.1D', header=FALSE)

singleCondition <- singleCondition[keep_ind,]
ocps <- singleCondition %>% {2 * (1 - pt(., 189))}

is_sig <- which(p.adjust(ocps, method='fdr') < 0.05)
length(is_sig)


tcps <- twoConditions[is_sig] %>% {2 * (1 - pt(., 189/2))}

is_sig2 <- which(p.adjust(tcps, method='fdr') < 0.05)
length(is_sig2)

twoConditions[is_sig[is_sig2]] %>% length


