# fast ways to do common operations (this should get folded into the electrode class at some point)
# some of them depend on RAVE globals (FREQUENCIES, TIME_RANGE, etc)


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

#FIXME: track down references to mean_o_trial
mean_o_trial <- collapse_over_trial

collapse_over_freq_and_time <- function(el){
    if(prod(dim(el))<1) return(NULL)

    # ~ 38ms
     el$subset(Frequency = (Frequency %within% FREQUENCY),
               Time = (Time %within% TIME_RANGE),
               data_only = TRUE) %>%
        rowMeans

    # not much of speedup to be had by trying out the usual suspects
}


collapse_over_freq <- function(el) {
    if(prod(dim(el))<1) return(matrix(NA, ncol=2, nrow=1))

    # ~ 50ms
    # (el$subset(Frequency = (Frequency %within% FREQUENCY))) %>%
    #     content %>%
    #     apply(1, colMeans) %>%
    #     t

    # ~ 24ms
    eCon <- el$subset(Frequency=Frequency %within% FREQUENCY, data_only = TRUE, drop=TRUE)

    # I think we want this to be transposed so that TIME is down the rows, rather than across the columns
    # when we draw_img TIME will end up on the x axis
    vapply(seq_len(dim(eCon)[1L]), FUN = function(ii) {
        colMeans(eCon[ii,,])
    }, FUN.VALUE = rep(0.0, dim(eCon)[3L]))
}

# first take mean over frequency
# then grab mean and SE across trials
collapse_over_freq_and_trial <- function(el){
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
        .fast_mse(.rowMeans(eCon[,,ii], dim(eCon)[1L], dim(eCon)[2L]))
    }, rep(0, 2)))
}

#
.fast_mse <- function(x) {
    # apparently sum(x) / length(x) is faster than mean(x) -- because of the use of generics and input checking?
    c(sum(x)/length(x),
      sqrt(.Call(stats:::C_cov, x, NULL, 4, FALSE)/length(x)))
}

.fast_range <- function(x) c(min(x), max(x))


# some ideas to consider if these still aren't fast enough

# cycling through a different dimension depending on the relative size of dimensions

# add optional parameters to these functions that take an index into the actual Time/Freq dimensions
# so we can avoid the call to subset -- doesn't seem to really help that much and it adds code burden to module writer, rolling back

