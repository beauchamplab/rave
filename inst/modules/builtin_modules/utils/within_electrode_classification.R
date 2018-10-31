#rm(list=ls()); gc()

# setwd('~/Dropbox/RAVE_DEV/modules/john_ce/')

require(rave)
require(magrittr)
require(stringr)
require(shiny)
require(MASS)

# give us some defaults to play with
# this needs to be ABOVE the call to the *_ui.R script because it needs to environment variables that will be created
rave_prepare(
    subject = 'KC_Congruency1_sliding',
    electrodes = c(6,7,23),
    epoch = 'KCa',
    time_range = c(1, 2)
)

source('rave_calculators.R')
source('condition_explorer_plots.R')
source('within_electrode_classification_ui.R')

# setup the groups and some data for us
rave_ignore({

    GROUPS = list(
        list(
            GROUP_NAME = 'G1',
            GROUP = c('drive_a', 'drive_av', 'drive_v')
        ),
        list(
            GROUP_NAME = 'G2',
            GROUP = c('known_av', 'known_a', 'known_v')
        ),
        list(
            GROUP_NAME = 'G3',
            GROUP = c('meant_a', 'meant_av', 'meant_v')
        ),
        list(
            GROUP_NAME = 'G4',
            GROUP=c('last_a', 'last_av', 'last_v')
        )
    )

    TIME_RANGE <- c(-.25, 1.0)

    # prior_probability_scheme = c('equal_priors', 'match_to_sample_size')

    # cross_validation_scheme

    # input_variable_scheme

    # classifer -- c('max_correlation', 'linear DA', 'quadratic DA', 'SVM', ...)

    # reject time point outliers
    # collapse_using_median

    # put cross validation work into an .async block?

})

wec_by_trial_heat_map <- function() {
    by_trial_heat_map()
}

rave_execute({
    #get the baseline corrected data
    assertthat::assert_that(length(electrode) == 1,msg = 'No electrode selected')

    electrode = as.integer(electrode)

    #baseline all available trials
    has_trials <- vapply(GROUPS, function(g) length(g$GROUP) > 0, TRUE)
    any_trials = any(has_trials)

    bl_power <- cache(
        key = list(subject$subject_id, electrode, BASELINE, any_trials),
        val = baseline(power$subset(Electrode = Electrode == electrode), from = BASELINE[1],  to = BASELINE[2], hybrid = F, unit = '%')#baseline(BASELINE[1],  BASELINE[2], electrode)
    )

    has_trials <- unlist(lapply(GROUPS, function(g) length(g$GROUP) > 0))

    #helper file to build lists with common elements pre-populated
    build_list <- function() {
        ##NB: this is faster than using replicate(length(has_trials))
        lapply(seq_len(length(has_trials)), function(ii)
            list('has_trials' = has_trials[ii],
                 'name' = GROUPS[[ii]]$GROUP_NAME))
    }

    # declare all our variables with pre-populated 'has_trials' and 'name' variables
    build_list() -> by_trial_heat_map_data -> training_data

    collapse <- get_favored_collapsers()

    # build up the data that we need
    for(ii in which(has_trials)) {
        power = bl_power$subset(Trial = Trial %in% GROUPS[[ii]]$GROUP)

        #helper function to clean up the syntax below, value here should be a function
        # we're relying on power being defined above, so don't move this function out of this scope
        `add_data<-` <- function(x, value) {
            x[c('data', 'range', 'N', 'trials')] <- list(value, .fast_range(value), dim(power)[1L], power$dimnames$Trial)
            x
        }

        # collapse over frequency -- maybe this should be a
        add_data(by_trial_heat_map_data[[ii]]) <- collapse$over_frequency(power)
        add_data(training_data[[ii]]) <- collapse$over_frequency(power$subset(Time=Time %within% TIME_RANGE))
    }

    # we really need class labels, so just create our own
    groupn <- training_data[has_trials] %>% sapply('[[', 'N')

    lda_result <- NULL
    lda_msg <- NULL
    if(length(groupn) > 1) {
        .y <- rep(LETTERS[which(has_trials)], groupn)
        .X <- training_data[has_trials] %>%
            lapply(function(x) t(x$data)) %>% {do.call(rbind, (.))}

        # get the value needed for smoothing to washout autocorrelation
        knots <- 15#ceiling(0.1*dim(.X)[2L])

        .smooth <- function(X, knots) {
            t(vapply(1:nrow(X), function(ii) {
                spline(X[ii,], n = knots)$y
            }, FUN.VALUE = rep(0, length = knots)))
        }
        .Xsm <- .X %>% .smooth(knots)
        lda_result <- tryCatch({lda(.Xsm, .y, CV=TRUE)},
                               warning=function(w)lda_msg<<-w, error=function(e) lda_msg<<-e)

        if('simpleWarning' %in% class(lda_result)) lda_result <- NULL

        #is there a generalization dataset?
        test_set <- get0('novel_generalization')
        test_results <- NULL
        if(not_null(test_set) & not_null(lda_result)) {
            power = bl_power$subset(Trial = Trial %in% test_set, Time = Time %within% TIME_RANGE)
            Xtest <- collapse$over_frequency(power) %>% .smooth(knots=knots)
            test_results <- predict(lda(.Xsm, .y), Xtest)

            lda_mgs <- table(lda_result$class, .y)
        }


    } else {
        lda_msg <- 'Need more than 1 group for classification'
    }



    has_data <- sum(has_trials)
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
    setwd('~/Dropbox/RAVE_DEV/modules/john_ce/')

    rave_options(data_dir = '/Volumes/data/rave_data/data/', module_lookup_file='~/Dropbox/RAVE_DEV/module_dev_john.csv')
    rave_options(data_dir='~/rave_data/data_dir/')

    module = ModuleEnvir$new('id', 'LABEL', './within_electrode_classification.R')
    init_app(list(module))
}



