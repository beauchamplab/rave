require(parallel)
require(magrittr)

cl <- makeForkCluster(4)

# check for containment, by default containiment is inclusive of the boundaries: min(rng) <= x <= max(rng)
# specificy your own functions for lower bound and upper bound if you want
# works with vector-valued x for default LB_ and UB_ functions
# also works when length(rng) == 1
is_within = function(x, rng, UB_FUNC=`<=`, LB_FUNC=`>=`) {
  (x %>% UB_FUNC(max(rng))) & 
    (x %>% LB_FUNC(min(rng)))
}

parSapply(cl, 1:240, function(ii) {
  freq_i <- c(50,150)
  time_i <- 0:1
  a <- c("Aclear_Vclear_rain", "Aclear_Vclear_rock", "Aclear_Vnoisy_rain", 
         "Aclear_Vnoisy_rock")
  b <- c("Anoisy_Vclear_rain", "Anoisy_Vclear_rock", "Anoisy_Vnoisy_rain", 
         "Anoisy_Vnoisy_rock")
  load(paste0('data/subject_APX1604/ecog/cache/load_ecog_raw/', ii, '/electrode_collapsed.RData'), e <- new.env())
  tA <- sapply(a, function(ai) {
    mean(e$electrode_ss$content[[ai]][
      e$electrode_ss$frequencies %>% is_within(freq_i),
      e$electrode_ss$time %>% is_within(time_i)])
  })
  
})

stopCluster(cl)