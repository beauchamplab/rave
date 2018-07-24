# correlation across every electrode



# types of correlation -- average timeseries for each trial and then correlate

# get a single time point for each trial and then correlate (sort by trial type first?)

# what about lags?

BASELINE <- c(-1, -.2)

# baseline 
bl_power <- cache(
    key = list(subject$subject_id, electrodes, BASELINE, any_trials),
    val = baseline(BASELINE[1],  BASELINE[2], electrodes)
)


# type 1: get a single point for each trial and then correlate that for each electrode

collapser <- get_favored_collapsers()

by_trial <- sapply(electrodes, function(ei) {
    collapser$over_frequency_and_time(
        bl_power$subset(Electrode= Electrode == ei)
    )
})

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

plot(by_cond[,1], type='l')
abline(v=1:16 * 201)

zcond <- apply(by_cond, 2, function(x) scale(smooth.spline(x, df=3*tlen/4)$y))

th <- seq(0,2*pi, length.out=nrow(by_cond)+1)[seq_len(nrow(by_cond))]


lim <- c(-1,1)*ceiling(1+max(abs(zcond)))

par(mfrow=c(3,3), mar=rep(1,4))
for(ii in 1:8) {
    y <- zcond[,ii]
    plot.clean(lim, lim); box()
    text(0,0, electrodes[ii])
    for(ri in 1:6){
        lines(ri*cos(th), ri*sin(th), col='gray70', lwd=0.75)
    }
    text(rep(0,4), c(2,4,6), c(-1, +1, +3), font=1, cex=2., col='gray40')
    
    aa <- matrix(1:nrow(zcond), nrow=17, byrow = TRUE)
    for(ii in 1:nrow(aa)) {
        
        offset <- cbind(3*cos(th), 3*sin(th))
        
        lines(offset[aa[ii,],1] + y[aa[ii,]]*cos(th[aa[ii,]]),
              offset[aa[ii,],2] + y[aa[ii,]]*sin(th[aa[ii,]]), col=get_color(ii))
    }
    
    segments(1,0,lim[2],0, lwd=2)
    
    
}
plot.clean(1:10,1:10)
legend('topleft', abbreviate(unique(trials), minlength = 7),
       text.col=get_color(1:17), ncol=2, cex=1, bty='n', y.intersp = 0.65)





plot(by_cond[,1])
plot(by_cond[,2])
