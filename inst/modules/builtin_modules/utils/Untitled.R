# circle correlation plot

require(rave)
require(magrittr)
require(stringr)
require(shiny)

# give us some defaults to play with
# this needs to be ABOVE the call to the *_ui.R script because it needs to environment variables that will be created
rave_prepare(
    subject = 'KC_Congruency1_sliding',
    electrodes = 1:100,
    # electrodes = c(14:15, 22:23, 71:72),
    epoch = 'KCa',
    time_range = c(1, 2)
)
ncond <- sort(unique(trials))

# baseline everyone
cl <- parallel::makeForkCluster(parallel::detectCores()); gc()

system.time({
bl_power <- cache(
    key = list(subject$subject_id, electrodes, has_trials, BASELINE),
    val = baseline(BASELINE[1],  BASELINE[2], electrodes)
)
})

bl_power <- parallel::parLapply(cl, electrodes, function(ei) {
    cache(
        key=list(subject$subject_id, ei, has_trials, BASELINE),
        val = baseline(BASELINE[1],  BASELINE[2], ei)
    )
})
parallel::stopCluster(cl)
cl <- parallel::makeForkCluster(parallel::detectCores()); gc()

# I spent a long time trying to get this to use the matrix t-test function,
# but the bottle neck here is the rowMeans on the bl_power, so there is no appreciable speedup
# the code is also more complex because we have to account for differing sample sizes per trial type (this is a common occurence)
# which leads to NAs in the big matrix which have to be accounted for when you call stats:::C_cov
ttest_per_el <- parallel::parLapply(cl, electrodes, function(ei) {
    vapply(ncond, function(ttype) {
        .fast_one_sample_tscore(
            rowMeans(bl_power[[ei]]$subset(Trial=Trial %in% ttype,
                                     Time=Time %within% TIME_RANGE,
                                     data_only = TRUE))
        )
    }, FUN.VALUE = 0.0)
})
parallel::stopCluster(cl); gc()


hcl <- hclust(dist(t(ttest_per_el)), method='ward.D2')
hclr <- hclust(dist(ttest_per_el), method='ward.D2')
ord <- hcl$order
ord2 <- hclr$order
par(mar=c(1,6,1,1))
image(z=t(ttest_per_el[ord2,ord]) %>% clip_x(c(-6,6)), x=seq_along(electrodes), y=seq_along(ncond), axes=F,
      # zlim=c(-1,1) * max(abs(ttest_per_el)), col=rave_heat_map_colors, ylab='')
      zlim=c(-6,6), col=rave_heat_map_colors, ylab='')
mtext(ncond[ord2], side=2, las=1, at=1:17)
mtext(electrodes[ord], side=3, las=1, at=seq_along(electrodes))

rle(cutree(hcl, k=4)[ord]) -> .rle
abline(v=cumsum(.rle$lengths) %>% remove_tail + 0.5, lwd=2)
dim(ttest_per_el)





