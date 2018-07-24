# correlate all the electrodes

setwd('~/Dropbox/RAVE_DEV/modules/john_ce/')
require(rave)

rave_prepare(
    subject = 'KC_Congruency1_sliding',
    electrodes = c(1:120),
    # electrodes = c(14:15, 22:23, 71:72),
    epoch = 'KCa',
    time_range = c(1, 2)
)

source('rave_calculators.R')
source('condition_explorer_plots.R')
source('across_electrode_correlations_ui.R')
source('draw_shapes.R')

options(future.globals.maxSize= 16*1024*1024^2)

BASELINE <- c(-1,-.3)

bl_power <- cache(
    key = list(subject$subject_id, electrodes, BASELINE, any_trials),
    val = .local_baseline(power, BASELINE)
    # val = baseline(BASELINE[1],  BASELINE[2], electrodes)
)

# type 1: get a single point for each trial and then correlate that for each electrode

collapser <- get_favored_collapsers()


by_trial <- sapply(electrodes, function(ei) {
    collapser$over_frequency_and_time(
        bl_power$subset(Electrode= Electrode == ei), trange=c(-.25, 1)
    )
})

# here we have one point per trial, split into trial types
ttypes <- unique(trials)
t_by_cond <- sapply(ttypes, function(ti) {
  .fast_one_sample_tscore_mat(t(by_trial[ti==trials,]))
}) %>% t

process_tree <- function(hcl, k=4) {
  ord <- hcl$order
  grp <- cumsum(rle(cutree(hcl, k=k)[ord])$length)
  list('ord'=ord, 'grp'=grp)
}


hclust(dist(t(t_by_cond))) %>% cutree(k=4) %>% table

hcl_row <- hclust(dist(t_by_cond)) %>% process_tree
hcl_col <- hclust(dist(t(t_by_cond))) %>% process_tree

par(mar=c(4,8,4,1))
image(t(t_by_cond[hcl_row$ord,hcl_col$ord]) %>% clip_x(c(-8,8)), zlim=c(-8,8), col=rave_heat_map_colors,
      y=seq_along(ttypes), x=seq_along(electrodes), axes=F, ylab='')
axis(2, at=seq_along(ttypes), labels = ttypes[hcl_row$ord], las=1, tcl=0, lwd=0)
box()
abline(v=hcl_col$grp+0.5)
abline(h=hcl_row$grp+0.5)
mtext(electrodes[hcl_col$ord], line=0, at=seq_along(electrodes), cex=0.5)

# elec_ps <- p.adjust(.fast_one_sample_t_pval_mat(t(by_trial), sided = 2), method='fdr')
# table(elec_ps %>% round(5))
rowMeans(by_trial) %>% barplot

tlen <- sum(bl_power$dimnames$Time %within% TIME_RANGE)
by_cond <- sapply(electrodes, function(ei) {
    # split by trial type
    vapply(unique(trials), function(ttype) {
        collapser$over_frequency_and_trial(
            bl_power$subset(Trial=Trial==ttype,
                            Time=Time %within% TIME_RANGE,
                            Electrode= Electrode == ei), include_standard_error=FALSE
        )
    }, FUN.VALUE = rep(0, tlen)) %>% c
})

el_cor <- cor(by_trial, method='spearman')

el_keep <- subject$meta$electrode$Channel %in% electrodes
el_lbls <- subject$meta$electrode$Label[el_keep]
subject$meta$electrode[el_keep,5:7] %>% dist %>% cmdscale -> el_mds
plot(el_mds, type='n')
text(el_mds[,1], el_mds[,2], labels=el_lbls)

# check the orientation of the mds by ensuring that the max Y coordinate

#if the correlation between the electrodes Y coord is with the X coord, then flip?
cor(el_mds[,1], subject$meta$electrode$Coord_y[el_keep])
cor(el_mds[,2], subject$meta$electrode$Coord_y[el_keep])
.mds <- el_mds[,2:1]
.mds[,1] <- max(.mds[,1]) - .mds[,1]
.mds[,2] <- -.mds[,2]

cor(.mds[,1], subject$meta$electrode$Coord_x[el_keep], method='spear')

seg <- function(from, to, ...) segments(from[1], from[2], to[1], to[2], ...)

cor_ramp <- colorRamp(c('navy', 'white', 'red'))
rgb(cor_ramp(.2), max=255)

rescale_correlation <- function(rho) (rho + 1) / 2
plot(.mds, type='n', axes=F); box()
# connect the regions based on the correlation
for(ii in 1:(nrow(.mds)-1)) {
    for(jj in (ii+1):nrow(.mds)) {
        # print(rescale_correlation(el_cor[ii,jj]))
        if(abs(el_cor[ii,jj]) > 0.2) {
        seg(.mds[ii,], .mds[jj,],
            col=rgb(max=255, cor_ramp(rescale_correlation(el_cor[ii,jj]))))
        }
    }
}

# hist(logp, breaks=50)
# abline(v=-log(b=10, 0.001), lwd=2)
# gray_ramp <- colorRamp(c('black', 'gray80'))
# elec_ps %<>% clip_x(lim=c(1e-10, 1))
# logp <- -log(b=10, elec_ps)
text(.mds[,1], .mds[,2], labels=el_lbls)#, col=rgb(max=255, gray_ramp(logp / max(logp) )))





by_cond[,10] %>% smooth %>% plot(type='l')
abline(1:17)



