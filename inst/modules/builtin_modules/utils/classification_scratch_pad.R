rave:::rave_lapply(1:50, function(x){
    lda_result <- lda(.X[,xi], sample(.y), CV=TRUE)
    lda_result$class %>% table(.y) %>% {diag(.)/(sum(.)/4)}

}) -> res

lda_result$posterior

lda_result$posterior
diag(table(.y, lda_result$class))/40
#
# sapply(res, mean) %>% quantile(90:99/100)

# with(lda_result, svd^2 / sum(svd^2))
# with(lda_result,
#      draw_img(scaling, tps, 1:3, zlim=c(-1,1)*max(abs(scaling)), DECORATOR = function(...) {
#          abline(h=1:2+0.5); box(); abline(v=0, lty=2)
#          rave_axis(1, pretty(tps))
#          rave_axis(2, 1:3, labels='LDA' %&% 1:3, tcl=0)
#      }, ylab='')
# )
#
# }
#
#
# .Xl %>% cor %>% t %>% image(zlim=c(-1,1))
#
#
# ar(.X[1,])
#
# pairs(lda_result, col=rep(get_color(which(has_trials)), times=groupn), cex=2, pch=19)
#
# predict(lda_result, newdata=data.frame(.X))
#
# MASS:::pairs.lda
#
# model.frame(lda_result)
#
# plot(lda_result$means[1,], type='l')
#      lines(lda_result$means[2,])
#            lines(lda_result$means[3,])
#                  lines(lda_result$means[4,])
