# what if we use FDR instead of first checking the ANOVA

source('~/Dropbox/scripts/basic_helper.R')
require(parallel)



ltri <- function(m)m[lower.tri(m)]

nt=40
ng=4
sd.y=5
eff.size=.5
cl <- makeForkCluster(3); gc()
compare <- parReplicate(1000, cl=cl, function() {
              
    d <- data.frame(g=letters[1:ng] %>% rep(nt), y=rnorm(ng*nt, sd=sd.y))
    
    d$y[d$g=='a'] = d$y[d$g=='a'] + eff.size*sd.y
    
    m <- aov(y ~ g, data=d)
    p <- summary(m)[[1]][1,5]

    c(p<0.05,
      any(TukeyHSD(m)$g[,4]<0.05),
      any(ltri(pairwise.t.test(d$y, d$g, 'fdr')$p.value)<0.05),
      mean(d$y[d$g=='a']))
}) %>% set_rownames(c('F', 'Tukey', 'fdr', 'm_g1'))

rowMeans(compare)

table(compare[1,], compare[2,], dnn = c('F', 'Tukey'))
table(compare[1,], compare[3,], dnn = c('F', 'fdr'))
table(compare[2,], compare[3,], dnn = c('Tukey', 'fdr'))


table(compare[1,], compare[4,] %>% round, dnn = c('F', 'Mean'))
table(compare[2,], compare[4,] %>% round, dnn = c('Tukey', 'Mean'))
table(compare[3,], compare[4,] %>% round, dnn = c('fdr', 'Mean'))


