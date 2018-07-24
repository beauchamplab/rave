# rm(list=ls()); gc()
require(rave)
require(magrittr)
require(stringr)
require(shiny)

# give us some defaults to play with while we're working on the module code
rave_prepare(subject = 'Words/PAA',
             electrodes = 1:6,
             epoch = 'PAAaudonset',
             time_range = c(1, 2))

source('rave_calculators.R')
source('within_trial_comparison_ui.R')
source('condition_explorer_plots.R')

rave_ignore({

    GROUPS = list(
        list(
            GROUP_NAME = 'A Only',
            GROUP = c('moth_D_a', 'vacuum_H_a')
        )#,
        # list(
        #     GROUP_NAME = 'AV',
        #     GROUP = c('drive_av', 'known_av', 'last_av', 'meant_av')
        # ),
        #  list(
        #      GROUP_NAME = '',
        #      GROUP=c()
        #  ),
        # list(
        #     GROUP_NAME = 'V',
        #     GROUP = c('drive_v', 'known_v', 'last_v', 'meant_v')
        # ),
        # list(
        #     GROUP_NAME = '',
        #     GROUP=c()
        # )
    )

    TIMES = list(
        list(
            TIME_NAME = 'V',
            RANGE = list(-.5,-.01)
        ),
        list(
            TIME_NAME='A',
            RANGE = list(0.01,0.5)
        ),
         list(
             TIME_NAME = 'G',
             RANGE = c(0.6, 1.0)
         )
    )

    max_zlim=500
    log_scale=FALSE
    sort_trials_by_type=TRUE
    collapse_using_median=FALSE
    electrode=1
})

# time series plot
wtc_over_time_plot <- function() {
    validate(need((exists('has_data') && (has_data)), "Not enough specified for analysis"))

    multi_window <- function(ylim, draw_labels, ...) {
        # clrs <- RColorBrewer::brewer.pal(9, 'Set1')
        # clrs <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
        #           "#A65628", "#F781BF", "#999999")

        clrs <- rep('gray30', length(TIMES))

        for(ti in seq_along(TIMES)){
            x <- unlist(TIMES[[ti]]$RANGE)
            if(diff(x) > 0) {
                vertical_borders(
                    x, ylim, TIMES[[ti]]$TIME_NAME,
                    clrs[ti], lty=1 + (ti-1)%%6
                )
            }
        }
        vertical_borders(BASELINE, ylim,
                         'baseline', rave_colors$BASELINE_WINDOW)
        abline(h=0, col='gray70')
    }

    time_series_plot(line_plot_data,
                     SHADER=multi_window)
}

# by trial plot with statistics
wtc_windowed_comparison_plot <- function(){
    validate(need((exists('has_data') && (has_data)), "Not enough specified for analysis"))

    # trial_scatter_plot(scatter_bar_data)
}

random_effects_plot <- function() {
    validate(need((exists('.lmer')), "Not enough specified for analysis"))

    ranef_mtx <- lme4::ranef(.lmer)
    xlim <- range(ranef_mtx$trial_number)
    xlim <- c(xlim[1] - .1*diff(xlim), xlim[2] + .1*diff(xlim))

    par(mfrow=c(1, ncol(ranef_mtx$trial_number)))
    for(ii in 1:ncol(ranef_mtx$trial_number)) {
        hist(ranef_mtx$trial_number[,ii], xlim = xlim,
             breaks=seq(min(xlim), max(xlim), length.out = 20),
             main = '', xlab='Value', las=1,
             col=getAlphaRGB(get_color(ii),200), border=get_color(ii))
        rave_main(colnames(ranef_mtx$trial_number)[ii] %&% ' Random Effects')
    }
}

# a little helper that comes in handy, especially in %>%'d contexts
which.equal <- function(x, Xs) which(x==Xs)

summary_effect_plot <- function() {
    validate(need((exists('flat_data') && (has_data)), "Not enough specified for analysis"))

    mses <- aggregate(y ~ time_period + trial_type, m_se, data=flat_data)
    ttypes <- unique(mses$trial_type)

    # this can produce wonky results, if the increment is, say, 20 but starts at 40, then it will produce (0,40,60,80,100)
    #FIXME do we care to fix this?
    yp <- unique(c(0, pretty(mses$y[,1] %>% plus_minus(mses$y[,2]), n=3)))


    # getting the colors here is difficult because we've factor()'d the data and this may have collapsed over empty
    # trial types. People leave trial types empty to allow them to choose colors, so we need to handle this
    tt_names <- sapply(GROUPS, '[[', 'GROUP_NAME')
    clrs <- get_color(unique(mses$trial_type) %>% sapply(which.equal, tt_names))

    xp <- barplot(matrix(mses$y[,1], byrow=TRUE, ncol=length(unique(mses$time_period))), beside=TRUE,
            col=getAlphaRGB(clrs, 180), border=NA, las=1, axes=F, ylim=range(yp))

    legend('topleft', text.col=clrs,
           legend=unique(mses$trial_type), bty='n', cex = rave_cex.lab)

    rave_axis(1, at=colMeans(xp),
              labels = sapply(TIMES,  '[[', 'TIME_NAME') %&% '\n' %&% sapply(TIMES,  format_trange),
              tcl=0, lwd=0, mgpx=c(3,2,0))

    rave_axis(2, at=yp)

    if(min(yp)<0) abline(h=0, col='gray70')

    # add the error bars
    ntt <- length(unique(mses$trial_type))
    ebars(t(xp), mses$y, code=0, col=rep(clrs, each=nrow(mses)/ntt), type='n')#, pch=16)
}

format_trange <- function(ti) {
    paste0(ti$RANGE %>% sapply(formatC, digits=2, format='f'),
           collapse=':')
}

fixed_effects_plot <- function () {
    validate(need((exists('flat_data') && (has_data)), "Not enough specified for analysis"))

    yax <- pretty(flat_data$y, n = 3)

    xlim <- length(levels(flat_data$time_period))
    xlim <- c(1-.1*(xlim-1), xlim+.1*(xlim-1))

    plot.clean(xlim, yax)
    if(min(flat_data$y)<0) abline(h=0, col='gray80')

    tt_names <- sapply(GROUPS, '[[', 'GROUP_NAME')

    flat_data %>% split(list((.)$trial_number, (.)$trial_type)) %>%
        sapply(function(ti) {

            # as with the summary plot, the colors here have to be calculated carefully
            clr <- getAlphaRGB(get_color(which.equal(ti$trial_type[1],tt_names)), 50)

            lines(x=as.integer(ti$time_period), ti$y, col=clr)
        })

    # now we want to apply the mse shade
    xpos <- flat_data$time_period %>% unique %>% as.integer

    mses <- aggregate(y ~ time_period + trial_type, m_se, data=flat_data)
    ntt <- length(levels(mses$trial_type))
    ntp <- length(levels(mses$time_period))

    for(ii in 1:ntt) {
        yi <- (ii-1)*ntp + 1:ntp
        ebar_polygon(xpos,
                     mses$y[yi,1], mses$y[yi,2], col=getAlphaRGB(get_color(which.equal(
                         levels(mses$trial_type)[ii], tt_names
                     )), alpha=210), lwd=2)
    }

    rave_axis(1, mgpx = c(3, 2, 0), at=xpos, labels =
                  sapply(TIMES,  '[[', 'TIME_NAME') %&% '\n' %&%
                  sapply(TIMES,  format_trange)
    )
    rave_axis(2, at=yax)
}


matrix_to_table <- function(mat, row_label=' ') {
    cnms <- colnames(mat)
    rnms <- rownames(mat)

    str <- '<div style="width:100%;overflow-x:scroll;"><table style = "width:900px">'

    #header row
    str <- str %&%
        '<tr style="border-bottom:1px solid #333"><td style="font-weight:bold"> '%&%
            paste0(c(row_label, cnms), collapse='</td><td style="font-weight:bold">') %&%
        '</td><tr>'

    # all the rows
    for(ii in seq_len(nrow(mat))) {
        #one of the things we want to do is fix the row names so that instead of A:B they are A &times; B
        str <- str %&%
            '<tr><td>' %&%
                paste0(c(str_replace_all(rnms[ii], ':', '&times;'), formatC(mat[ii,], digits=3)), collapse='</td><td>') %&%
            '</td></tr>'
    }
    str <- str %&% '</table></div>'

    return(str)
}

msg_out = function() {

    # put analysis information in here

    if(exists('.lmer')) {
        # put a description row
        txt <- '<p>LME Call: ' %&% format(summary(.lmer)$call$formula) %&% '<br/>' %&%
            (summary(.lmer)$methTitle %>% str_replace_all('\n', '<br/>')) %&% '</p>'

        # fancy up the variable printing a bit, put the str_rep in parentheses so we don't mess up the description lines above
        # we also want to give people a hint about the reference level
        ref_tt <- levels(flat_data$trial_type)[1]
        ref_tp <- levels(flat_data$time_period)[1]
        intercept <- sprintf('Intercept=%s:%s', ref_tt, ref_tp)
        txt <- txt %&% (
            (summary(.lmer)$coefficients %>% matrix_to_table(row_label='Coef')) %>%
                str_replace_all('trial_type', 'Trial=') %>%
                str_replace_all('time_period', 'Time=') %>%
                str_replace_all('(Intercept)', intercept)
        )

        return(
            HTML(
                # '<iframe width="420" height="345" src="https://www.youtube.com/embed/gEDYR2N7wCM">
                #     </iframe>'
                txt)
        )
    }

    return('no calculations yet')
}

async_out = function(){
    validate(need(exists('async_msg', envir = environment()), 'Press "Force run" Button.'))
    async_msg
}

rave_execute({
    assertthat::assert_that(length(electrode) == 1,msg = 'No electrode selected')

    electrode = as.integer(electrode)

    #baseline all available trials
    GROUPS = lapply(GROUPS, function(g){ g$Trial_num = epoch_data$Trial[epoch_data$Condition %in% g$GROUP]; g })
    has_trials <- vapply(GROUPS, function(g) length(g$GROUP) > 0, TRUE)
    any_trials = any(has_trials)

    bl_power <- cache(
        key = list(subject$subject_id, electrode, BASELINE, any_trials, preload_info),
        val = module_tools$baseline(BASELINE[1],  BASELINE[2], electrode)
    )

    # we were repeating a lot of calculations and looping over GROUPS too many times
    # let's clean that up
    has_trials <- unlist(lapply(GROUPS, function(g) length(g$GROUP) > 0))

    #helper file to build lists with common elements pre-populated
    build_list <- function() {
        ##NB: this is faster than using replicate(length(has_trials))
        lapply(seq_len(length(has_trials)), function(ii)
            list('has_trials' = has_trials[ii],
                 'name' = GROUPS[[ii]]$GROUP_NAME))
    }

    # declare all our variables with pre-populated 'has_trials' and 'name' variables
    line_plot_data <- build_list()

    flat_data <- vector('list', length(GROUPS))

    # load up our collapsers into a list, this allows us to swap them out as needed
    collapse <- get_favored_collapsers()

    # if they didn't provide names for GROUPS or TIMES, fill them in
    for(gi in seq_along(GROUPS)) {
        if('' == GROUPS[[gi]]$GROUP_NAME)
            GROUPS[[gi]]$GROUP_NAME <- 'RAVE.GROUP_' %&% LETTERS[gi]
    }
    for(ti in seq_along(TIMES)) {
        if('' == TIMES[[ti]]$TIME_NAME)
            TIMES[[ti]]$TIME_NAME <- 'RAVE.TIME_' %&% LETTERS[ti]
    }

    # now we loop through only those groups with data
    for(ii in which(has_trials)) {
        power = bl_power$subset(Trial = Trial %in% GROUPS[[ii]]$Trial_num)

        #helper function to clean up the syntax below, value here should be a function
        # we're relying on power being defined above, so don't move this function out of this scope
        `add_data<-` <- function(x, value) {
            x[c('data', 'range', 'N', 'trials')] <- list(value, .fast_range(value), dim(power)[1L], power$dimnames$Trial)
            x
        }

        add_data(line_plot_data[[ii]]) <- collapse$over_frequency_and_trial(power)
        # we want to make a special range for the line plot data that takes into account mean +/- SE
        line_plot_data[[ii]]$range <- .fast_range(plus_minus(line_plot_data[[ii]]$data[,1],
                                                     line_plot_data[[ii]]$data[,2]))
        # this is our data matrix, we need y, time, and group

        # get the data for each of the times provided
        data <- lapply(TIMES, function(ti) {
            .data <- rep(NA, line_plot_data[[ii]]$N)
            if(diff(unlist(ti$RANGE))>0) {
                .data <- collapse$over_frequency_and_time(power, trange = unlist(ti$RANGE))
            }
            return(data.frame('y'=.data, 'time_name'=ti$TIME_NAME, 'trial_number'=seq_along(.data)))
        }) %>% {do.call(rbind, (.))}

        flat_data[[ii]] <- data.frame('trial_type' = GROUPS[[ii]]$GROUP_NAME,
                                      'trial_number' = data$trial_number,
                                      'time_period' = data$time_name,
                                      'y' = data$y
        )
    }
    flat_data %<>% {do.call(rbind, (.))}

    # check if we have data from at least 2 time windows
    .has_data <- function(x) any(!is.na(x))
    if(sum(tapply(flat_data$y, list(flat_data$time_period), .has_data, simplify = TRUE)) > 1) {
        # if we made it here, then there are > 1 time period with data
        has_data <- TRUE

        tnames <- as.character(unique(flat_data$trial_type))
        tpnames <- as.character(unique(flat_data$time_period))
        flat_data$trial_type %<>% factor(levels = tnames)
        flat_data$trial_number %<>% factor
        flat_data$time_period %<>% factor(levels = tpnames)

        # do we have >1 trial_type ?
        ntt <- length(unique(flat_data$trial_type))
        if(ntt > 1) {
            .lmer <- lmerTest::lmer(y ~ trial_type*time_period + (0+trial_type|trial_number), data=flat_data)

            # compare without time_period
            .lmer_no_tp <- lmerTest::lmer(y ~ trial_type + (1|trial_number), data=flat_data)
            .lmer_no_itx <- lmerTest::lmer(y ~ trial_type + time_period + (1|trial_number), data=flat_data)

            # BIC(.lmer, .lmer_no_itx, .lmer_no_tp)

        } else {
            .lmer <- lmerTest::lmer(y ~ time_period + (1|trial_number), data=flat_data)
            .lmer_no_tp <- lmerTest::lmer(y ~ (1|trial_number), data=flat_data)

            BIC(.lmer, .lmer_no_tp)
        }
        # calculate some statistics from the LMER that we can send to SUMA / csv
        result_for_suma <- 0

    } else {
        has_data <- FALSE
        rm('.lmer')
    }

    # attr(scatter_bar_data, 'stats') <- result_for_suma

},{
    if(.is_async){
        async_msg = 'Running in the background'
    }
}, async = {
    logger('Async test')
    # rm(list = ls(all.names = T))
    Sys.sleep(0.15)
    nms = ls(all.names = T)
    async_msg = paste(search(), collapse = ', ')
}
)

niml_default = function(){
    return(result_for_suma)
}

if(FALSE) {
    setwd('~/Dropbox/RAVE_DEV/modules/john_ce/')
    require(rave)
    # require(shiny)

    # rave_options(data_dir = '/Volumes/data/rave_data/data/', module_lookup_file='~/Dropbox/RAVE_DEV/module_dev_john.csv')
    # rave_options(data_dir='~/rave_data/data_dir/')

    # rave_options(data_dir='/Volumes/data/Dipterix/Fertilizer/', raw_data_dir = '/Volumes/data/rave_data/raw/')
    # YAB_JUNK - YABa

    mod <- ModuleEnvir$new(module_id = 'tmp', label_name='Within Trial Comparison',
                           script_path = '~/Dropbox/RAVE_DEV/modules/john_ce/within_trial_comparison.R')
    init_app(mod)
}

