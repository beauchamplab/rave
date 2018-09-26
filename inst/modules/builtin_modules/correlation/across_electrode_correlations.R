#rm(list=ls()); gc()
require(rave)
require(magrittr)
require(stringr)
require(shiny)

# give us some defaults to play with while we're working on the module code
rave_prepare(subject = 'Words/PAA',
             electrodes = 1:16,
             epoch = 'PAAaudonset',
             time_range = c(2, 4))


source('../utils/rave_calculators.R')
source('condition_explorer_plots.R')
source('across_electrode_correlations_ui.R')
source('../utils/draw_shapes.R')

# setup the groups and some data for us
rave_ignore({

    GROUPS = list(
        list(
            GROUP_NAME = 'G1',
            GROUP = unique(epoch_data$Condition)#c('moth_D', 'moth_D_a')
            )
        # ,
        # list(
        #     GROUP_NAME = 'DL_V',
        #     GROUP = c('drive_v', 'last_v', 'meant_v', 'known_v')
        # ),
        # list(
        #     GROUP_NAME = 'DL_AV',
        #     GROUP = c('drive_av', 'last_av', 'meant_av', 'known_av')
        # )
    )

    FROM_ELEC_GROUPS = list(
        list(
            GROUP_NAME ='FE1',
            GROUP = 3:4
        ),
        list(
            GROUP_NAME = 'FE2',
            GROUP=6:7
        )
    )

    TO_ELEC_GROUPS = list(
        list(
            GROUP_NAME='TE_AV',
            GROUP = 12:13
        )
    )

    BASELINE <- c(-2.00, -0.64)
    TIME_RANGE <- c(-0.48, 1.8)
})


aec_plot_connections <- function() {
    validate(need(exists('res') && dim(res[[1]]$time_series)[2L] > 1, "Not enough for calculation"))

    fnames <- names(res[[1]][[1]])
    to_name <- names(res[[1]])[1]
    ntrials <- length(res)

    # here we need to figure out the possibility of multiple TO groups, right?
    # or maybe we shouldn't allow that... too much to plot

    par(mfrow=c(1,ntrials), mar=rep(1,4))

    # helper function to extract a specified connection value and it's p-value

    for(ii in seq_len(ntrials)) {
        get_r <- function(val) {
            res[[ii]][[1]] %>%
                sapply(function(ri) 'r=' %&% paste0(ri[[val]], collapse='\np='))
        }

        # we need to figure out which ones are significant and change lty/lwd
        ul <- get_r('conditional')
        ll <- get_r('unconditional')

        plot_connectivity(fnames, to_name, names(res)[ii], ul, ll)
    }
}

aec_plot_timeseries <- function() {
    validate(need((exists('over_time_data') && any_trials), "Not enough for calculation"))

    par(mfrow=c(1,length(over_time_data)))
    mapply(function(otd, nm) {
        # to use the builtin time series plot, we need to supply a list of lists
        # each element is its own timeseries
        # reset the name of each electrode to be it's e# rather than the trial type
        for(ii in seq_along(otd)) {
            if(is.null(otd[[ii]])) {
                otd[[ii]] <- list('has_trials'=FALSE)
            }
            otd[[ii]] <- list(
                'data' = otd[[ii]],
                'range' = .fast_range(plus_minus(otd[[ii]][, 1], otd[[ii]][, 2])),
                'name' = names(otd)[ii],
                'has_trials' = TRUE
            )
        }

        cat('TR: ', TIME_RANGE, '\n')
        cat('dim otd$data: ', dim(otd[[1]]$data), '\n')
        cat('length time_points: ', length(time_points[time_points %within% TIME_RANGE]), '\n')

        time_series_plot(otd,
                         x=time_points[time_points %within% TIME_RANGE], title='Condition: ' %&% nm,
                        SHADER = function(...){}, DECORATOR = function(...){})
        legend('topright',
               legend = sapply(otd, '[[', 'name'),
               text.col = get_color(seq_len(length(otd))), bty='n', ncol=2)
        abline(h=0, col='gray70')

    }, over_time_data, names(over_time_data))
}


aec_plot_trial_means_without_time <- function() {
    validate(need(exists('res') && dim(res[[1]]$time_series)[2L] > 1, "Not enough for calculation"))

    # par(mfrow=c(length(res), 1), mar=rep(2.5,4))
    par(mfcol=c(ncol(res[[1]]$time_series)-1, length(res)), mar=c(4, 5, 2, 1))

    lapply(seq_along(res), function(ri) {
        ts <- res[[ri]]$time_series

        #we need to make room for the legend in the top left
        yrange <- pretty(c(ts, 1.1*max(c(ts))))

        for(ii in 2:ncol(ts)) {
            .label = paste(colnames(ts)[c(1,ii)], collapse=' ~ ')
            plot.clean(yrange, yrange, xlab=colnames(ts)[ii], ylab=colnames(ts)[1])
            rave_main(names(res)[ri] %&% ': ' %&% .label)
            abline(0,1, col='lightgray')

            points(ts[,ii], ts[,1], pch=16)

            rave_axis(1, at=pretty(yrange))
            rave_axis(2, at=pretty(yrange))
            if(ri == 1) {
                 legend('topleft', legend=c('Y=X', .label), inset=c(0,0), horiz=FALSE,
                        bty='n', lty=1:2, cex=rave_cex.lab, col = c('lightgray', 'black'))
            }

            abline(lm(ts[,1] ~ ts[,ii]), lty=2)
        }


        # for(ii in seq_len(dim(ts)[2L])) {
            # lines(ts[,ii], type='o', col=get_color(ii), pch=16)
        # }
    })
}


aec_plot_trial_means <- function () {
    validate(need(exists('res') && dim(res[[1]]$time_series)[2L] > 1, "Not enough for calculation"))

    # par(mfrow=c(length(res), 1), mar=rep(2.5,4))
    par(mfrow=c(length(res),1), mar=c(3, 3, 2, 1))

    lapply(seq_along(res), function(ri) {
        ts <- res[[ri]]$time_series

        #we need to make room for the legend in the top left
        yrange <- pretty(c(ts, 1.1*max(c(ts))))

        plot.clean(1:nrow(ts), yrange)
        rave_main(names(res)[ri])

        for(ii in seq_len(dim(ts)[2L])) {
            lines(ts[,ii], type='o', col=get_color(ii), pch=16)
        }
        rave_axis(1, at=pretty(1:nrow(ts)))
        rave_axis(2, at=pretty(c(ts)))

        legend('topleft', legend=colnames(ts), inset=c(0,0), horiz=TRUE,
               bty='n', text.col=get_color(seq_len(ncol(ts))), cex=rave_cex.lab)
    })
}


#
# we could z-score before | after collapse
collapse_electrodes <- function(els) {
    if(is.list(els) && length(els)>1) {
        # check the apprpriate state variable for the desired collapse technique
        collapse <- rowMeans

        els <- collapse(do.call(cbind, els))
    } else {
        els <- unlist(els)
    }

    return(scale(els))
}

#tack on a letter and sentinel string so we can go back to the original name later
# I think we should add the sep as an attribute onto the name for easy removal?
fix_names <- function(nms, prfx) {
    nm_prfx <- prfx %&% seq_along(nms)
    ii <- which(is.null(nms))
    nms[ii] <- nm_prfx[ii]

    return(nms)
}

# connectivity measures take x, y, and Z time series, with Z being a list of time-series
# must return a list with and without Z being partialled, whatever that
# means for a given connectivity measure. Each list must contain a numeric vector
# with numeric values for 'connection' and 'p.value'
#
correlation_conn <- function(x,y,Z,method) {

    uncond <- cor.test(y, x, method=method) %>% format_r
    if(length(Z)<1) {
        cond <- format_r(list("estimate"=NA, 'p.value'=NaN))
    } else {
        y_wo_ii <- resid(lm(y ~ do.call(cbind, Z) -1))
        cond <- cor.test(y_wo_ii, x, method=method) %>% format_r
    }

    list('conditional' = cond,
         'unconditional'= uncond)
}

spearman_conn <- function(x,y,Z) {
    correlation_conn(x,y,Z,method='spearman')
}

pearson_conn <- function(x,y,Z) {
    correlation_conn(x,y,Z,method='pearson')
}

format_r <- function(cor_res, prfx='') {
    cor_res %$% c(estimate, p.value) %>% round(3) %>%
        set_names(prfx %&% c('connection', 'p.value'))
}

rave_execute({

    assertthat::assert_that(length(electrodes) > 2,msg = 'Need > 1 electrode loaded')

    assertthat::assert_that(length(FROM_ELEC_GROUPS) > 0 & length(FROM_ELEC_GROUPS) > 0,
                            msg = 'Need to specify FROM and TO electrode(s)')

    logger('Electrodes: ', paste0(electrodes, collapse=', '))

    #we only need to get the electrodes that are in one of the groups
    from <- FROM_ELEC_GROUPS %>% lapply('[[', 'GROUP')
    to <- TO_ELEC_GROUPS %>% lapply('[[', 'GROUP')
    elecs <- unique(unlist(from) %>% c(unlist(to)))

    #set connectivity measure
    CONN <- spearman_conn

    # 1. baseline
    #using the local basline here as it is much faster for multiple electrodes than using the cumsum file from the network
    # this function is in rave_calculators
    GROUPS = lapply(GROUPS, function(g){ g$Trial_num = epoch_data$Trial[epoch_data$Condition %in% g$GROUP]; g })
    has_trials <- vapply(GROUPS, function(g) length(g$GROUP) > 0, TRUE)
    any_trials = any(has_trials)

    bl_power <- cache(
        key = list(subject$id, electrodes, BASELINE, any_trials, preload_info),
        #having problems with rutabaga
        val = baseline(power$subset(Electrode = Electrode == electrode), from = BASELINE[1],  to = BASELINE[2], hybrid = F, unit = '%')
        # module_tools$baseline(BASELINE[1],  BASELINE[2], electrodes)
        # val = .local_baseline(tmp, BASELINE)
    )
    collapsers <- get_favored_collapsers()

    gnames <- sapply(GROUPS, '[[', 'GROUP_NAME') %>% fix_names(prfx = 'TTYPE_')

    #2. select out the trials for each analysis
    #2b. we also want the mean timeseries for plotting, so grab that while we're at it
    power_by_ttype <- lapply(GROUPS, function(group) {
        if(length(group$GROUP) > 0) {
            # get collapsed data for all the electrodes
                lapply(elecs, function(ei) {
                c(collapsers$over_frequency_and_time(bl_power$subset(Trial=Trial %in% group$Trial_num,
                                                                     Time=Time %within% TIME_RANGE,
                                                                     Electrode = Electrode==ei)))
            }) %>% set_names('e' %&% elecs)
        } else {
            NULL
        }
    }) %>% set_names(gnames)

    by_f_and_t <- lapply(GROUPS, function(group) {
        if(length(group$GROUP) > 0) {
            # get collapsed data for all the electrodes
            lapply(elecs, function(ei) {
                data = collapsers$over_frequency_and_trial(bl_power$subset(Trial=Trial %in% group$Trial_num,
                                                                     Time=Time %within% TIME_RANGE,
                                                                     Electrode = Electrode==ei))
                list('data'=data, 'name'=group$GROUP_NAME)
            }) %>% set_names('e' %&% elecs)
        } else {
            list('data'=NULL, 'range'=NULL, 'has_trials'=FALSE, 'name'=group$GROUP_NAME)
        }
    }) %>% set_names(gnames)

    elf_names <- FROM_ELEC_GROUPS %>% sapply('[[', 'GROUP_NAME') %>% fix_names(prfx='F_')
    elt_names <- TO_ELEC_GROUPS %>% sapply('[[', 'GROUP_NAME') %>% fix_names(prfx='T_')

    # now we need to combine the FROM / TO electrode groups
    mse_over_elec <- function(els) {
        if(length(els) == 1) return(els[[1]]$data)

        emat <- do.call(rbind, lapply(els, function(e) e$data[,1]))

        cbind(.colMeans(emat, nrow(emat), ncol(emat)), .fast_column_se(emat))
    }

    # otd <- by_f_and_t[[1]]
    over_time_data <- by_f_and_t %>% lapply(function(otd) {
        if(is.null(otd)) return (otd)
        # feg <- FROM_ELEC_GROUPS[[1]]
        from <- FROM_ELEC_GROUPS %>% lapply(function(feg) {
            otd['e' %&% feg$GROUP] %>% mse_over_elec
        }) %>% set_names(elf_names)

        to <- TO_ELEC_GROUPS %>% lapply(function(teg) {
            otd['e' %&% teg$GROUP] %>% mse_over_elec
        }) %>% set_names(elt_names)

        append(to, from)
    })

    # we need to collapse the groups if necessary,
    # here we know that we don't need that
    res <- lapply(power_by_ttype, function(pbtt) {
        # collapse the FROM electrodes -- we should probably have a choice in
        # how we do the collapsing, this can be solved inside the collapse function
        el_f <- FROM_ELEC_GROUPS %>% lapply(function(grp) collapse_electrodes(pbtt['e' %&% grp$GROUP]))
        el_to <- TO_ELEC_GROUPS %>% lapply(function(grp) collapse_electrodes(pbtt['e' %&% grp$GROUP]))

        # for each of the electrodes in the TO group, find the connection (switch out CONN(...) based on user-selected method) FROM_i -> TO_j, partialling out From_k...From_l
        el_to %>% lapply(function(y) {
            # go through each of the FROMs so we can partial them
            val <- lapply(seq_along(el_f), function(ii) {
                # y and x go in as vector, Z as a list
                CONN(y, el_f[[ii]], el_f[-ii])
            }) %>% set_names(elf_names)


        })  %>% set_names(elt_names) ->
            .res

        # we need to add the time-series data into the res list
        all_els <- append(el_to, el_f)
        all_els_mat <- do.call(cbind, all_els) %>% set_colnames(c(elt_names, elf_names))

        if(identical(CONN, spearman_conn)) {
            # save the ranked times series
            all_els_mat %<>% apply(2, rank)
        }

        .res$time_series <- all_els_mat

        return(.res)
    })

    res
},
{
    if (.is_async) {
        async_msg = 'Running in the background'
    }
}, async = {

    # put cross validation in here because it will take a while
    logger('Runnning CV')
    Sys.sleep(0.15)
})

async_out = function(){
    validate(need(exists('async_msg', envir = environment()), 'Press "Force run" Button.'))
    return (async_msg)
}

msg_out = function() {
    # put analysis information in here
    if(not_null(get0('lda_msg')))
        return('From LDA: ' %&% lda_msg)

    return ('')
}
# we need to think about what we should send to suma -- CV accuracies?
niml_default <- function() {
    return (1)
}


if(FALSE) {

    require(rave)
    setwd('~/Dropbox/RAVE_DEV/modules/john_ce/')

    rave_options(data_dir = '/Volumes/data/rave_data/data/', module_lookup_file='~/Dropbox/RAVE_DEV/module_dev_john.csv')
    rave_options(data_dir='~/rave_data/data_dir/')

    init_app()

    # system('open ~/Dropbox/RAVE_DEV/module_dev_john.csv')


    module = ModuleEnvir$new('id', 'LABEL', './across_electrode_correlations.R')
    init_app(list(module))
}

