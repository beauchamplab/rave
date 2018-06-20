require(rave)
require(magrittr)

# check your rave_options()

rave_options('module_lookup_file' = '~/Dropbox/RAVE_DEV/module_dev_john.csv',
             'data_dir' = '/Volumes/data/rave_data/data/')

setwd('~/Dropbox/RAVE_DEV/modules/john_ce/')
source('condition_explorer_plots.R')


# give us some defaults to play with
# this needs to be ABOVE the call to the *_ui.R script because it needs to environment variables that will be created
rave_prepare(
    subject = 'KC_Congruency1_sliding',
    electrodes = c(6:7, 14:15, 22:23, 30:31),
    epoch = 'KCa',
    time_range = c(1, 2)
)

# rave_ignore({
# })

BASELINE <- c(-1.0, -0.3)


# 1. baseline the data
bl_power <- baseline(BASELINE[1], BASELINE[2], electrodes)

# 2. select only the trials that you want and the time period that you want
all_trials <- trials %>% unique
TRIALS <- c('drive_a', 'meant_a', 'last_a', 'known_a')
TIME_RANGE <- c(-0.201, 1.001)
FREQUENCY <- c(75,150)

bl_power_analysis <- bl_power$subset(Trial = Trial %in% TRIALS,
                                     Time = Time %within% TIME_RANGE)

# 3. collapse over frequency
source('rave_calculators.R')
collapsers <- get_favored_collapsers()

# because we have multiple electrodes (NB: check that we actually do!), we need to be a little careful with out collapsing
electrodes
ei <- electrodes[1]

flat_electrodes <- sapply(electrodes, function(ei) {
    c(collapsers$over_frequency(bl_power_analysis$subset(Electrode = Electrode==ei)))
})

elec_by_trial <- sapply(electrodes, function(ei) {
    c(collapsers$over_frequency_and_time(bl_power_analysis$subset(Electrode = Electrode==ei)))
})


par(mfrow=c(2,2), mar=rep(2,4))
image(
    cor(flat_electrodes, method = 'spearman')[, 8:1],
    col = rave_heat_map_colors,
    zlim = c(-1, 1),
    asp = 1, main='Full time series'
)

image(
    cor(elec_by_trial, method = 'spearman')[, 8:1],
    col = rave_heat_map_colors,
    zlim = c(-1, 1),
    asp = 1, main='Mean only', axes=F, y=1:8, x=1:8
)
axis(2, at=1:8, labels=electrodes %>% rev)


cmd <- cmdscale(1-cor(flat_electrodes, method='spearman'))
plot(cmd, type='n'); text(cmd, labels=electrodes, asp=1)

plot(hclust(dist(t(flat_electrodes), p=1), method='ward.D2'))


prcomp(t(flat_electrodes)) -> pc
dim(pc$rotation)
# %>% image(col=rave_heat_map_colors)
range(pc$rotation)


elec_by_trial <- sapply(electrodes, function(ei) {
    c(collapsers$over_frequency_and_trial(bl_power_analysis$subset(Electrode = Electrode==ei)))
})

dim(elec_by_trial)
image(
    cor(elec_by_trial)[, 8:1],
    col = rev(rave_heat_map_colors),
    zlim = c(-1, 1),
    asp = 1
)
colnames(elec_by_trial) <- electrodes
pairs(elec_by_trial[,1:3], panel=function(x,y,...){
    points(x,y, pch=19, col=get_color(as.integer(factor(rownames(elec_by_trial)))))
    abline(lm(y~x))
})

plot(scale(elec_by_trial[,1]), type='l', col='dodgerblue3')
lines(scale(elec_by_trial[,2]), type='l', col='orange')


# plot(scale(elec_by_trial[,4]), scale(elec_by_trial[,2]))
# pairs(elec_by_trial[,3:6] %>% apply(2,scale), pch=19)
