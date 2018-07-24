


Xpc <- prcomp(.X)

pairs(Xpc$x[,1:5], col=get_color(as.integer(factor(.y))), pch=19)

length(.y)
dim(Xpc$x)

par(mfrow=c(2,2))
for(ii in 1:4) lda(Xpc$x[,1:20], .y) %$% plot(means[ii,], type='l', main=ii)

lda(Xpc$x[,1:20], .y, CV=TRUE) %>% get_accuracy -> fitAcc


lda(Xpc$x[,1:20], .y) %>% pairs(col=get_color(as.integer(factor(.y))), cex=1.5, lwd=2)



cumsum(Xpc$sdev^2 / sum(Xpc$sdev^2)) %>% {which((.)>=0.95)}

# plot(cumsum(Xpc$sdev^2 / sum(Xpc$sdev^2)), Xpc$sdev^2/sum(Xpc$sdev^2), type='l')

?prcomp

Xpc$sdev


Xpc$rotation[,1:10] %>% image(zlim=c(-.31,.31), col=rave_heat_map_colors,
                               x=as.numeric(rownames(Xpc$rotation)), y=1:10, las=1, ylab='PC')
range(Xpc$rotation[,1:10])


get_accuracy <- function(res) {
    tbl <- table(.y, res$class)
    acc <- sum(diag(tbl))/length(res$class)
    by_group <- diag(tbl) / rowSums(tbl)
    list('by_group'=by_group, 'conf_matrix'=tbl, 'accuracy'=acc)
}



lapply_async(1:1000, function(ii) {
    lda(Xpc$x[,1:20], sample(.y), CV=TRUE) %>% get_accuracy
}, .call_back = function(i){print(i)}, .packages = 'MASS') -> accs2



require(parallel)
cl <- makeForkCluster(8); gc()
parLapply(cl, 1:1000, function(ii) {
    lda(Xpc$x[,1:20], sample(.y), CV=TRUE) %>% get_accuracy
}) -> accs


msd_null <- sapply(accs, '[[', 'accuracy') %>% quantile(0.95)
# by_group_null <- sapply(accs, '[[', 'by_group') %>% apply(1, quantile, 0.95)
fitAcc

# col_msd <- function(x) {
#     cbind(.colMeans(x, nrow(x), ncol(x)),
#           .fast_column_sd(x)) %>% set_colnames(c('m', 'sd'))
# }

accs %>% sapply('[[', 'accuracy') %>% hist(xlim=c(0.0,.5), breaks=0:100/100, border=NA, col='gray70')
abline(v=fitAcc$accuracy, col='orange')
abline(v=quantile(accs %>% sapply('[[', 'accuracy'), c(2.5, 97.5)/100), col='gray30')

with(fitAcc,{
barplot(c(accuracy, diag(conf_matrix)/rowSums(conf_matrix)), ylim=c(0,.6))
})

abline(h=quantile(sapply(accs, '[[', 'accuracy'), 0.95))

max(table(.y))
image(t(fitAcc$conf_matrix)[,4:1], col=rev(rave_heat_map_colors), zlim=c(-30,30))




