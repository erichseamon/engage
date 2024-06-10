
matA <- 
  as.matrix(
    read.table(text=
                 "  E     NE           
      S             14    4      
      NS            76    400",
               header=TRUE,
               row.names=1)
  )

names(dimnames(matA)) <- c('solution','stakeholder')
matA

original <- as.table(matA)

summary(original)

fisher.test(original)



matA <- 
  as.matrix(
    read.table(text=
                 "  E     NE      
      S             11    7      
      NS            377   94",
               header=TRUE,
               row.names=1)
  )

names(dimnames(matA)) <- c('solution','model')
matA

original2 <- as.table(matA)

summary(original2)

fisher.test(original2)

















apply(DTA, 1, function(x) chisq.test(matrix(x,ncol=3), simulate.p.value = TRUE))



library(MASS)
Cars93$Type

##  [1] Small   Midsize Compact Midsize Midsize Midsize Large   Large  
##  [9] Midsize Large   Midsize Compact Compact Sporty  Midsize Van    
## [17] Van     Large   Sporty  Large   Compact Large   Small   Small  
## [25] Compact Van     Midsize Sporty  Small   Large   Small   Small  
## [33] Compact Sporty  Sporty  Van     Midsize Large   Small   Sporty
## [41] Sporty  Small   Compact Small   Small   Sporty  Midsize Midsize
## [49] Midsize Midsize Midsize Large   Small   Small   Compact Van    
## [57] Sporty  Compact Midsize Sporty  Midsize Small   Midsize Small  
## [65] Compact Van     Midsize Compact Midsize Van     Large   Sporty
## [73] Small   Compact Sporty  Midsize Large   Compact Small   Small  
## [81] Small   Compact Small   Small   Sporty  Midsize Van     Small  
## [89] Van     Compact Sporty  Compact Midsize
## Levels: Compact Large Midsize Small Sporty Van

table(Cars93$Type)

##
## Compact   Large Midsize   Small  Sporty     Van
##      16      11      22      21      14       9

prop.table(table(Cars93$Type))

##
##    Compact      Large    Midsize      Small     Sporty        Van
## 0.17204301 0.11827957 0.23655914 0.22580645 0.15053763 0.09677419

table(Cars93$Origin)

##
##     USA non-USA
##      48      45

prop.table(table(Cars93$Origin))

##
##      USA  non-USA
## 0.516129 0.483871

table(Cars93$Type, Cars93$Origin)

##          
##           USA non-USA
##   Compact   7       9
##   Large    11       0
##   Midsize  10      12
##   Small     7      14
##   Sporty    8       6
##   Van       5       4

(tab1<-table(Cars93$Type, Cars93$Origin))

##          
##           USA non-USA
##   Compact   7       9
##   Large    11       0
##   Midsize  10      12
##   Small     7      14
##   Sporty    8       6
##   Van       5       4

rowSums(tab1)

## Compact   Large Midsize   Small  Sporty     Van
##      16      11      22      21      14       9

colSums(tab1)

##     USA non-USA
##      48      45

prop.table(table(Cars93$Type, Cars93$Origin))

##          
##                  USA    non-USA
##   Compact 0.07526882 0.09677419
##   Large   0.11827957 0.00000000
##   Midsize 0.10752688 0.12903226
##   Small   0.07526882 0.15053763
##   Sporty  0.08602151 0.06451613
##   Van     0.05376344 0.04301075

prop.table(table(Cars93$Type, Cars93$Origin))*100

##          
##                 USA   non-USA
##   Compact  7.526882  9.677419
##   Large   11.827957  0.000000
##   Midsize 10.752688 12.903226
##   Small    7.526882 15.053763
##   Sporty   8.602151  6.451613
##   Van      5.376344  4.301075

prop.table(table(Cars93$Type, Cars93$Origin), margin=2)*100

##          
##                 USA   non-USA
##   Compact 14.583333 20.000000
##   Large   22.916667  0.000000
##   Midsize 20.833333 26.666667
##   Small   14.583333 31.111111
##   Sporty  16.666667 13.333333
##   Van     10.416667  8.888889

(tab2<-prop.table(table(Cars93$Type, Cars93$Origin), margin=2)*100)

##          
##                 USA   non-USA
##   Compact 14.583333 20.000000
##   Large   22.916667  0.000000
##   Midsize 20.833333 26.666667
##   Small   14.583333 31.111111
##   Sporty  16.666667 13.333333
##   Van     10.416667  8.888889

colSums(tab2)

##     USA non-USA
##     100     100

chisq.test(Cars93$Type, Cars93$Origin)

## Warning in chisq.test(Cars93$Type, Cars93$Origin): Chi-squared
## approximation may be incorrect

##
##  Pearson's Chi-squared test
##
## data:  Cars93$Type and Cars93$Origin
## X-squared = 14.08, df = 5, p-value = 0.01511

fisher.test(Cars93$Type, Cars93$Origin)

##
##  Fisher's Exact Test for Count Data
##
## data:  Cars93$Type and Cars93$Origin
## p-value = 0.007248
## alternative hypothesis: two.sided


library(DescTools)
GTest(Cars93$Type, Cars93$Origin)

##
##  Log likelihood ratio (G-test) test of independence without
##  correction
##
## data:  Cars93$Type and Cars93$Origin
## G = 18.362, X-squared df = 5, p-value = 0.002526


(tab3<-table(Cars93$Man.trans.avail, Cars93$Origin))

##      
##       USA non-USA
##   No   26       6
##   Yes  22      39

chisq.test(tab3)

##
##  Pearson's Chi-squared test with Yates' continuity correction
##
## data:  tab3
## X-squared = 15.397, df = 1, p-value = 8.712e-05


table(Cars93$Man.trans.avail, Cars93$Origin, Cars93$Type)

## , ,  = Compact
##
##      
##       USA non-USA
##   No    2       0
##   Yes   5       9
##
## , ,  = Large
##
##      
##       USA non-USA
##   No   11       0
##   Yes   0       0
##
## , ,  = Midsize
##
##      
##       USA non-USA
##   No    9       4
##   Yes   1       8
##
## , ,  = Small
##
##      
##       USA non-USA
##   No    0       0
##   Yes   7      14
##
## , ,  = Sporty
##
##      
##       USA non-USA
##   No    0       0
##   Yes   8       6
##
## , ,  = Van
##
##      
##       USA non-USA
##   No    4       2
##   Yes   1       2

ftable(Cars93$Man.trans.avail, Cars93$Origin, Cars93$Type)

##              Compact Large Midsize Small Sporty Van
##                                                    
## No  USA            2    11       9     0      0   4
##     non-USA        0     0       4     0      0   2
## Yes USA            5     0       1     7      8   1
##     non-USA        9     0       8    14      6   2

mantelhaen.test(Cars93$Man.trans.avail, Cars93$Origin, Cars93$Type)

##
##  Mantel-Haenszel chi-squared test with continuity correction
##
## data:  Cars93$Man.trans.avail and Cars93$Origin and Cars93$Type
## Mantel-Haenszel X-squared = 8.0153, df = 1, p-value = 0.004638
## alternative hypothesis: true common odds ratio is not equal to 1
## 95 percent confidence interval:
##   2.226531 76.891307
## sample estimates:
## common odds ratio
##          13.08438


CramerV(Cars93$Type, Cars93$Origin)

## [1] 0.3890967

table(Cars93$Type, Cars93$Origin)

##          
##           USA non-USA
##   Compact   7       9
##   Large    11       0
##   Midsize  10      12
##   Small     7      14
##   Sporty    8       6
##   Van       5       4

rowSums(table(Cars93$Type, Cars93$Origin))

## Compact   Large Midsize   Small  Sporty     Van
##      16      11      22      21      14       9


Lambda(Cars93$Type, Cars93$Origin, direction='row')

## [1] 0.04225352

(lambdaTab<-cbind(c(0,100), c(49,51)))

##      [,1] [,2]
## [1,]    0   49
## [2,]  100   51

chisq.test(lambdaTab)

##
##  Pearson's Chi-squared test with Yates' continuity correction
##
## data:  lambdaTab
## X-squared = 62.279, df = 1, p-value = 2.981e-15
Lambda(lambdaTab, direction='row')

## [1] 0


