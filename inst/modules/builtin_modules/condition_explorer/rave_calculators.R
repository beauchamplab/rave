# fast ways to do common operations (this should get folded into the electrode class at some point?)
# some of them depend on RAVE globals (FREQUENCIES, TIME_RANGE, etc)

.fast_z <- function(x) {
    .l <- length(x)
    .m <- sum(x) / .l

    (x - .m) / sqrt((sum(x^2) - sum(x)^2 / .l)/(.l-1))
}

### Methods to aggregate vectors
electrode_transform <- function(method='none') {
    switch(method,
           'z-score' = .fast_z,
           'max-scale' = function(x) {
               x/max(x)
           },
           '0-1 scale' = function(x) {
               (x - min(x)) / diff(range(x))
           },
           'rank' = rank,
           IDENTITY_TRANSFORM
    )
}
IDENTITY_TRANSFORM <- function(x) x


# this can be better than baselining using cumsum over the network
.local_baseline <- function(e_tensor, baseline_range, data_only=FALSE) {
    bl_data <- e_tensor$data

    bsl <- e_tensor$dimnames$Time %within% baseline_range

    for(ei in seq_len(dim(bl_data)[4L])) {
        bl <- vapply(seq_along(e_tensor$dimnames$Frequency), function(fi) {
            rowMeans(e_tensor$data[,fi,bsl,ei])
        }, FUN.VALUE = array(0, dim=dim(e_tensor$data)[1]))

        bl_data[,,,ei] <- (100 * (e_tensor$data[,,,ei] / as.vector(bl) -1))
    }

    if(data_only) return (bl_data)

    return (ECoGTensor$new(
        data = bl_data,
        dimnames = e_tensor$dimnames,
        varnames = names(e_tensor$dimnames)
    ))
}

# mean for each time/frequency across trials
collapse_over_trial <- function(el) {
    # ~ 230 ms
    #res <- apply(el$data[,,,1], 2, colMeans)

    # ~ 72 ms
    el <- el$data
    vapply(seq_len(dim(el)[2L]), FUN = function(ii) {
        .colMeans(el[,ii,,1], dim(el)[1L], dim(el)[3L])
    }, FUN.VALUE = rep(0.0, dim(el)[3L]))
}

#keep time/freq, median over trial
collapse_over_trial.median <- function(el) {
    # ~ 708 ms
    # el <- el$data
    # vapply(seq_len(dim(el)[2L]), FUN = function(ii) {
    #     apply(el[,ii,,1], 2, median.default)
    # }, FUN.VALUE = rep(0.0, dim(el)[3L])) -> r1

    # 12 ms
    el <- el$data
    vapply(seq_len(dim(el)[2L]), FUN = function(ii) {
        Biobase::rowMedians(t(el[,ii,,1]))
    }, FUN.VALUE = rep(0.0, dim(el)[3L])) -> r2
}

#FIXME: track down references to mean_o_trial
mean_o_trial <- collapse_over_trial

collapse_over_freq_and_time <- function(el, trange, frange){
    if(prod(dim(el))<1) return(NULL)

    if(missing(trange)) trange=TIME_RANGE
    if(missing(frange)) frange=FREQUENCY

    # ~ 38ms
    el$subset(Frequency = (Frequency %within% frange),
              Time = (Time %within% trange),
              data_only = TRUE) %>%
        rowMeans

    # not much of speedup to be had by trying out the usual suspects
}

collapse_over_freq_and_time.median <- function(el, trange, frange){
    if(prod(dim(el))<1) return(NULL)

    if(missing(trange)) trange=TIME_RANGE
    if(missing(frange)) frange=FREQUENCY

    el$subset(
        Frequency = (Frequency %within% frange),
        Time = (Time %within% trange),
        data_only = TRUE
    ) -> ee

    # this flattening takes too much time?
    # vapply(1:200, function(ii) {
    #     median.default(c(ee[ii,,,1]))
    # }, 0.0)

    #data are stored such that we can create indices that represent each trial's data
    # across Freq and Time
    nt <- seq_len(dim(ee)[1L])
    nel <- dim(ee)[1L]*(seq_len(prod(dim(ee)[2:3]))-1)

    # ind <- outer(nt, nel, `+`)
    # this beats out the comparable call to outer
    ind <- vapply(nt, `+`, nel, FUN.VALUE = rep(0.0, length(nel)))

    # apply(aa.outer, 1, function(ii) median.default(ee[ii]))
    #this beats out the apply
    vapply(seq_len(dim(ind)[2L]), function(ei) {
        median.default(ee[ind[,ei]])
    }, 0.0)
}
collapse_over_freq <- function(el, frange) {
    if(prod(dim(el))<1) return(matrix(NA, ncol=2, nrow=1))

    if(missing(frange)) frange = FREQUENCY

    # ~ 50ms
    # (el$subset(Frequency = (Frequency %within% FREQUENCY))) %>%
    #     content %>%
    #     apply(1, colMeans) %>%
    #     t

    # ~ 24ms
    eCon <- el$subset(Frequency=Frequency %within% frange, data_only = TRUE, drop=TRUE)

    # make sure how this should (not) be transposed so that TIME is down the rows, rather than across the columns
    # when we draw_img TIME will end up on the x axis
    vapply(seq_len(dim(eCon)[1L]), FUN = function(ii) {
        colMeans(eCon[ii,,])
    }, FUN.VALUE = rep(0.0, dim(eCon)[3L]))
}

collapse_over_freq.median <- function(el) {
    if(prod(dim(el))<1) return(matrix(NA, ncol=2, nrow=1))

    eCon <- el$subset(Frequency=Frequency %within% FREQUENCY, data_only = TRUE, drop=TRUE)

    # make sure how this should (not) be transposed so that TIME is down the rows, rather than across the columns
    # when we draw_img TIME will end up on the x axis
    t(vapply(seq_len(dim(eCon)[3L]), FUN = function(ii) {
        Biobase::rowMedians(eCon[,,ii])
    }, FUN.VALUE = rep(0.0, dim(eCon)[1L])))
}


# first take mean over frequency
# then grab mean and SE across trials
collapse_over_freq_and_trial <- function(el, frange, include_standard_error=TRUE){
    if(prod(dim(el))<1) return(matrix(NA, ncol=2, nrow=1))

    # ~ 278ms
    # (el$subset(Frequency = (Frequency %within% FREQUENCY))) %>%
    #     content %>%
    #     apply(3, rowMeans) %>%
    #     apply(2, m_se) %>%
    #     t

    if(missing(frange)) frange = FREQUENCY

    # ~ 159 ms
    eCon <- el$subset(Frequency = (Frequency %within% frange), data_only = TRUE, drop=TRUE)

    # not sure if we can avoid the transpose here
    if(include_standard_error) {
        return (
            t(vapply(seq_len(dim(eCon)[3L]), function(ii) {
                .fast_mse(.rowMeans(eCon[,,ii], dim(eCon)[1L], dim(eCon)[2L]))
            }, rep(0, 2)))
        )
    }

    # if we just want the mean, then flatten out the trial/freq axes and
    # just take the column means--way faster
    dim(eCon) <- c(prod(dim(eCon)[1:2]), dim(eCon)[3])
    colMeans(eCon)
}

.fast_mean <- function(x) sum(x)/length(x)

collapse_over_freq_and_trial.median <- function(el){
    if(prod(dim(el))<1) return(matrix(NA, ncol=2, nrow=1))

    # ~ 278ms
    # (el$subset(Frequency = (Frequency %within% FREQUENCY))) %>%
    #     content %>%
    #     apply(3, rowMeans) %>%
    #     apply(2, m_se) %>%
    #     t

    # ~ 159 ms
    eCon <- el$subset(Frequency = (Frequency %within% FREQUENCY), data_only = TRUE, drop=TRUE)

    # not sure if we can avoid the transpose here
    t(vapply(seq_len(dim(eCon)[3L]), function(ii) {
        .fast_mse(Biobase::rowMedians(eCon[,,ii]))
    }, c(0, 0)))
}

# putting the functions into a list allows users
# to swap out collapsers whole sale, or pass them into functions
rave_collapsers.mean <- function() {
    list(
        'over_trial' = collapse_over_trial,
        'over_frequency' = collapse_over_freq,
        'over_frequency_and_time' = collapse_over_freq_and_time,
        'over_frequency_and_trial' = collapse_over_freq_and_trial
    )
}
# by default, people we'll expect mean collapsing, but do we want to make this
# opaque?
# rave_collapsers <- rave_collapsers.mean

rave_collapsers.median <- function() {
    list(
        'over_trial' = collapse_over_trial.median,
        'over_frequency' = collapse_over_freq.median,
        'over_frequency_and_time' = collapse_over_freq_and_time.median,
        'over_frequency_and_trial' = collapse_over_freq_and_trial.median
    )
}


# which collapsers do you want? This function allows modules
# to swap out collapsers wholesale without changing later function calls
get_favored_collapsers <- function(swap_var = 'collapse_using_median') {
    if (isTRUE(get0(swap_var, ifnotfound = FALSE))) {
        return (rave_collapsers.median())
    }

    return (rave_collapsers.mean())
}




# don't do NA checking here to keep the speed
# if you're doing this on a matrix, check out .fast_column_se and .colMeans
.fast_mse <- function(x) {
    # apparently sum(x) / length(x) is faster than mean(x) -- because of the use of generics and input checking?
    c(sum(x)/length(x),
      sqrt(.Call(stats:::C_cov, x, NULL, 4, FALSE)/length(x)))
}

# no NA checking
.fast_range <- function(x) c(min(x), max(x))

.fast_pearson <- function(x,y) {
    .Call(stats:::C_cor, x, y, 4, FALSE)
}

.fast_one_sample_t <- function(y, sided=1) {
    a <- .fast_mse(y)
    sided*(pt(a[1]/a[2], length(y)-1, lower.tail = FALSE))
}


.fast_one_sample_tscore <- function(y) {
    a <- .fast_mse(y)
    return(a[1]/a[2])
}


# returns the tail probability from a t-test (against 0) for each *ROW* in the supplied
# matrix
# Because we usually want  t > t*, we take the upper tail probability, lower.tail=FALSE
# by default we return a one-sided p-value, but you can multiply by 2 using sided=2
.fast_one_sample_t_pval_mat <- function(ymat, sided=1, lower.tail=FALSE) {
    sided*pt(.fast_one_sample_tscore_mat(ymat), dim(ymat)[2L]-1, lower.tail=lower.tail)
}

# returns the t-score from a t-test (against 0) for each *ROW* in the supplied matrix
.fast_one_sample_tscore_mat <- function(ymat) {
    .rowMeans(ymat, nrow(ymat), ncol(ymat)) / .fast_column_se(t(ymat))
}



# selecting out the diagonal from the cov matrix is faster than
# per-column se calculation,
# even if it means a transpose from the calling function
.fast_column_se <- function(y) {
    sqrt(
        .fast_diagonal(.Call(stats:::C_cov, y, NULL, 4, FALSE))/dim(y)[1L]
    )
}

.fast_column_sd <- function(y) {
    sqrt(
        .fast_diagonal(.Call(stats:::C_cov, y, NULL, 4, FALSE))
    )
}

#ripped this from base::diag, thanks!
.fast_diagonal <- function(y) {
    y[1 + 0L:(min(dim(y)) - 1L) * (dim(y)[1L] + 1)]
}

.fast_se <- function(x) {
    sqrt(.Call(stats:::C_cov, x, NULL, 4, FALSE)/length(x))
}



