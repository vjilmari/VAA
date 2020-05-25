---
title: "Analysis 2019"
output: 
  pdf_document: 
    keep_tex: yes
    keep_md: yes
    toc: yes
    toc_depth: 5
---



\newpage

# Preparations

Load packages


```r
library(here)
library(dplyr)
library(labelled)
library(ggplot2)
library(tidyr)
library(stringr)
library(psych)
library(lavaan)
library(semTools)
library(semPlot)
library(haven)
library(sjlabelled)
#library(robumeta)
```

Read data file


```r
df2019 <- readRDS("data/final/candsurvey_vaa_2019.rds")
```

Select variables used in the analysis


```r
VAA_LR_items<-c("h26","h27","h25","h28","y19")
VAA_LR_items %in% names(df2019)
```

```
## [1] TRUE TRUE TRUE TRUE TRUE
```

```r
VAA_GT_items<-c("h21","h22","h13","h29","h24","y25")
VAA_GT_items %in% names(df2019)
```

```
## [1] TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
CS_LR_items<-c("C2b","C2g","C2h")
CS_LR_items %in% names(df2019)
```

```
## [1] TRUE TRUE TRUE
```

```r
CS_GT_items<-c("C2a","C2c","C2d","C2e","C2f","C2i","C2j")
CS_GT_items %in% names(df2019)
```

```
## [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
#LR Self-placement
CS_LR_SP<-c("C5a")
CS_LR_SP %in% names(df2019)
```

```
## [1] TRUE
```

```r
#LR imagined voter placement
CS_LR_IP<-c("C5c")
CS_LR_IP %in% names(df2019)
```

```
## [1] TRUE
```

```r
Party_item<-c("puolue")
Party_item %in% names(df2019)
```

```
## [1] TRUE
```

```r
#vector for all item names

all_items<-c(Party_item,
             VAA_LR_items,
             VAA_GT_items,
             CS_LR_items,
             CS_GT_items,
             CS_LR_SP,
             CS_LR_IP)

#vector for observed variables in CFA (and party)

obs_items<-c(Party_item,
             VAA_LR_items,
             VAA_GT_items,
             CS_LR_items,
             CS_GT_items)
```

Print the responses to the observed items


```r
for (i in 1:length(obs_items)){
  print(obs_items[i])
  print(table(df2019[,obs_items[i]],useNA="always"))
  }
```

```
## [1] "puolue"
## 
##  EOP   FP   IP   KD KESK  KOK   KP  KTP LIBE   LN Muut  PIR   PS  RKP  SDP  SIN 
##   17   38   47  190  216  211   50   32   36  108   28   87  213   98  216  152 
##  SKE  SKP  STL  VAS VIHR <NA> 
##   34   88  175  216  216    0 
## [1] "h26"
## 
##    1    2    3    4    5 <NA> 
##  193  469   91  724  569  422 
## [1] "h27"
## 
##    1    2    3    4    5 <NA> 
##  643  573   49  635  145  423 
## [1] "h25"
## 
##    1    2    3    4    5 <NA> 
##  909  722   40  330   45  422 
## [1] "h28"
## 
##    1    2    3    4    5 <NA> 
##  535  582   48  654  227  422 
## [1] "y19"
## 
##    1    2    4    5 <NA> 
##   37  254  796 1172  209 
## [1] "h21"
## 
##    1    2    3    4    5 <NA> 
##  281  251  106  281 1127  422 
## [1] "h22"
## 
##    1    2    3    4    5 <NA> 
##  453  354  130  559  550  422 
## [1] "h13"
## 
##    1    2    3    4    5 <NA> 
##  272  307   82  619  766  422 
## [1] "h29"
## 
##    1    2    3    4    5 <NA> 
##  744  703   93  418   88  422 
## [1] "h24"
## 
##    1    2    3    4    5 <NA> 
##  380  421   60  558  627  422 
## [1] "y25"
## 
##    1    2    4    5 <NA> 
##  453  700  645  419  251 
## [1] "C2b"
## 
##    1    2    3    4    5 <NA> 
##  250  314   59  101   24 1720 
## [1] "C2g"
## 
##    1    2    3    4    5 <NA> 
##   29   97   84  299  242 1717 
## [1] "C2h"
## 
##    1    2    3    4    5 <NA> 
##   48   94   77  220  313 1716 
## [1] "C2a"
## 
##    1    2    3    4    5 <NA> 
##   15   49   69  324  294 1717 
## [1] "C2c"
## 
##    1    2    3    4    5 <NA> 
##   36   96   94  229  298 1715 
## [1] "C2d"
## 
##    1    2    3    4    5 <NA> 
##  461   79   74   51   86 1717 
## [1] "C2e"
## 
##    1    2    3    4    5 <NA> 
##  183  164  249  125   31 1716 
## [1] "C2f"
## 
##    1    2    3    4    5 <NA> 
##   37  142  156  267  148 1718 
## [1] "C2i"
## 
##    1    2    3    4    5 <NA> 
##   87  103  127  277  158 1716 
## [1] "C2j"
## 
##    1    2    3    4    5 <NA> 
##   49   64   72  144  424 1715
```

Data looks as it should.

Exclude completely missing cases


```r
df2019$completely_missing<-
  rowSums(is.na(df2019[,obs_items[2:length(obs_items)]]))==length(obs_items)-1

table(df2019$completely_missing)
```

```
## 
## FALSE  TRUE 
##  2365   103
```

```r
dat2019<-df2019 %>%
  filter(!completely_missing)
```

Transform/Reverse code high scores on observed variable to indicate right and TAN positioning


```r
reverse_items<-c("h26","y19",
                 "h21","h22","h13",
                 "C2g","C2h",
                 "C2c","C2e","C2i","C2j")

reverse_items %in% names(df2019)
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
for (i in 1:length(reverse_items)){
  dat2019[,reverse_items[i]]<-6-dat2019[,reverse_items[i]]
}
```

\newpage

# Analysis

## H1 and H2

H1. Left-Right placement as computed from responses to the pre-election public Voting Advice Applications (VAAs) is positively associated with Left-Right placement as computed from responses to the privately administered post-election Candidate Survey (CS). This association is stronger than any associations between the Left-Right and GAL-TAN dimensions.

H2. GAL-TAN placement as computed from responses to the pre-election public Voting Advice Applications (VAAs) is positively associated with GAL-TAN placement as computed from responses to the privately administered post-election Candidate Survey (CS). This association is stronger than any associations between the Left-Right and GAL-TAN dimensions.

### Define the model


```r
model_H1H2<-"
#loadings
VAA_LR=~h26+h27+h25+h28+y19
VAA_GT=~h21+h22+h13+h29+h24+y25
CS_LR=~C2b+C2g+C2h
CS_GT=~C2a+C2c+C2d+C2e+C2f+C2i+C2j

#latent correlations

#cross-dimension same-method
VAA_LR~~r.VAA*VAA_GT
CS_LR~~r.CS*CS_GT

#concurrent validity
VAA_LR~~r.LR*CS_LR
VAA_GT~~r.GT*CS_GT

#cross-dimension cross-method correlations
VAA_LR~~r.d1*CS_GT
VAA_GT~~r.d2*CS_LR

#custom parameters
test.H1:=r.LR-max(r.VAA,r.CS,r.d1,r.d2)
test.H2:=r.GT-max(r.VAA,r.CS,r.d1,r.d2)

"
```

### Fit the model


```r
fit_H1H2<-cfa(model=model_H1H2,
              data=dat2019,
              missing="fiml")
```

```
## Warning in lav_object_post_check(object): lavaan WARNING: covariance matrix of latent variables
##                 is not positive definite;
##                 use lavInspect(fit, "cov.lv") to investigate.
```

Some problems with latent variable covariance structure


```r
lavInspect(fit_H1H2, "cov.lv")
```

```
##        VAA_LR VAA_GT CS_LR CS_GT
## VAA_LR 1.038                    
## VAA_GT 0.475  1.222             
## CS_LR  0.475  0.189  0.249      
## CS_GT  0.237  0.677  0.105 0.343
```

```r
#examine standardized estimates
std.est_H1H2<-standardizedsolution(fit_H1H2)
std.est_H1H2[std.est_H1H2$op=="~~" & 
               std.est_H1H2$lhs!=std.est_H1H2$rhs,]
```

```
##       lhs op    rhs est.std    se       z pvalue ci.lower ci.upper
## 22 VAA_LR ~~ VAA_GT   0.422 0.023  18.686      0    0.377    0.466
## 23  CS_LR ~~  CS_GT   0.360 0.037   9.725      0    0.288    0.433
## 24 VAA_LR ~~  CS_LR   0.934 0.020  45.885      0    0.894    0.973
## 25 VAA_GT ~~  CS_GT   1.045 0.010 101.601      0    1.025    1.065
## 26 VAA_LR ~~  CS_GT   0.397 0.029  13.540      0    0.339    0.454
## 27 VAA_GT ~~  CS_LR   0.342 0.035   9.704      0    0.273    0.411
```

There is an impossible correlation between GAL-TAN latent variables (absolute value > 1)

\newpage

#### Respecify the model by introducing the three preregistered residual correlations


```r
model_H1H2.re<-paste0(model_H1H2,
                      "h27~~C2h\n",
                      "h21~~C2d\n",
                      "h29~~C2c\n")
```

#### Fit the respecified model


```r
fit_H1H2.re<-cfa(model=model_H1H2.re,
              data=dat2019,
              missing="fiml")
```

Inspect fit of the model


```r
round(inspect(fit_H1H2,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##     npar       df    chisq   pvalue      cfi      tli    rmsea     srmr 
##   69.000  183.000 2090.910    0.000    0.847    0.824    0.066    0.080
```

```r
round(inspect(fit_H1H2.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##     npar       df    chisq   pvalue      cfi      tli    rmsea     srmr 
##   72.000  180.000 1743.580    0.000    0.874    0.853    0.061    0.076
```

The fit of the model is adequate.

Hypotheses 1 and 2

Print standardized estimates to test the difference between correlations


```r
std.est_H1H2<-standardizedsolution(fit_H1H2.re)
std.est_H1H2[std.est_H1H2$op==":=" | 
               std.est_H1H2$op=="~~" & 
               std.est_H1H2$lhs!=std.est_H1H2$rhs,]
```

```
##        lhs op                            rhs est.std    se      z pvalue
## 22  VAA_LR ~~                         VAA_GT   0.424 0.022 18.855      0
## 23   CS_LR ~~                          CS_GT   0.355 0.037  9.604      0
## 24  VAA_LR ~~                          CS_LR   0.915 0.020 44.726      0
## 25  VAA_GT ~~                          CS_GT   0.990 0.010 96.968      0
## 26  VAA_LR ~~                          CS_GT   0.407 0.029 14.175      0
## 27  VAA_GT ~~                          CS_LR   0.339 0.035  9.680      0
## 28     h27 ~~                            C2h   0.283 0.053  5.353      0
## 29     h21 ~~                            C2d   0.661 0.024 27.725      0
## 30     h29 ~~                            C2c   0.272 0.040  6.849      0
## 81 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.492 0.030 16.340      0
## 82 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.566 0.025 23.080      0
##    ci.lower ci.upper
## 22    0.380    0.468
## 23    0.282    0.427
## 24    0.875    0.956
## 25    0.970    1.010
## 26    0.350    0.463
## 27    0.270    0.408
## 28    0.179    0.387
## 29    0.615    0.708
## 30    0.194    0.350
## 81    0.433    0.551
## 82    0.518    0.614
```

H1: There is very strong (.915, p < .001) correlation between VAA-LR and CS-LR, and it is notably stronger (difference in correlations .491, p < .001) than the strongest of correlations between different dimensions (.42 between VAA_LR and VAA_GT, p < .001)

H2: There is very strong (.990, p < .001) correlation between VAA-GT and CS-GT, and it is notably stronger (difference in correlations .566, p < .001) than the strongest of correlations between different dimensions (.42 between VAA_LR and VAA_GT, p < .001)

\newpage

#### Exploratory analysis for H1 and H2: Seek misspecification to improve the overall model fit

Factor loadings


```r
mis.load_H1H2<-miPowerFit(fit_H1H2.re,stdLoad=.40)
mis.load_H1H2<-mis.load_H1H2[mis.load_H1H2$op=="=~",]
#see summary of the decisions
table(mis.load_H1H2$decision.pow)
```

```
## 
##  EPC:M EPC:NM      I      M     NM 
##      1     27     10      7     18
```

```r
#there are 8 loadings that were detected as misspecifications

rounded.vars<-c("mi","epc","target.epc",
                "std.epc","se.epc")

num.round<-function(var){
  var<-as.numeric(var)
  var<-round(var,2)
  return(var)
}

mis.load_H1H2[,rounded.vars]<-sapply(mis.load_H1H2[,rounded.vars],num.round)

printed.vars<-c("lhs","op","rhs","mi","epc","target.epc",
                "std.epc","std.target.epc","significant.mi",
                "high.power","decision.pow","se.epc")

#print the output

mis.load_H1H2 %>%
  filter(mis.load_H1H2$decision.pow=="M" | 
                mis.load_H1H2$decision.pow=="EPC:M") %>%
  dplyr::select(all_of(printed.vars)) 
```

```
##      lhs op rhs    mi   epc target.epc std.epc std.target.epc significant.mi
## 1 VAA_LR =~ C2h 14.72 -1.68       0.49   -1.38            0.4           TRUE
## 2 VAA_GT =~ C2f  4.41  1.44       0.43    1.35            0.4           TRUE
## 3  CS_LR =~ h27 12.05 -1.77       1.11   -0.64            0.4           TRUE
## 4  CS_LR =~ h25 11.81  1.02       0.91    0.45            0.4           TRUE
## 5  CS_GT =~ h22  6.05  3.23       1.00    1.29            0.4           TRUE
## 6  CS_GT =~ h13 29.34  7.89       0.94    3.35            0.4           TRUE
## 7  CS_GT =~ h29 13.02 -4.22       0.82   -2.07            0.4           TRUE
## 8  CS_GT =~ h24 10.63 -4.17       1.00   -1.67            0.4           TRUE
##   high.power decision.pow se.epc
## 1      FALSE            M   0.44
## 2      FALSE            M   0.69
## 3      FALSE            M   0.51
## 4       TRUE        EPC:M   0.30
## 5      FALSE            M   1.31
## 6      FALSE            M   1.46
## 7      FALSE            M   1.17
## 8      FALSE            M   1.28
```

All the proposed loadings would be cross-loadings across methods (from VAA to CS or vice versa), and therefore not applicable for the present approach. Also, the expected parameter changes are indicative that most of these respecification would be Heywood -cases (standardized loadings that would be larger than 1 in absolute magnitude).

Residual correlations


```r
mis.rescor_H1H2<-miPowerFit(fit_H1H2.re,cor=.20)
mis.rescor_H1H2<-mis.rescor_H1H2[mis.rescor_H1H2$op=="~~" & 
                                   mis.rescor_H1H2$lhs!=mis.rescor_H1H2$rhs,]
#see summary of the decisions
table(mis.rescor_H1H2$decision.pow)
```

```
## 
##  EPC:M EPC:NM     NM 
##      1     68    138
```

```r
#there are 1 residual correlation that is a misspecification

rounded.vars<-c("mi","epc","target.epc",
                "std.epc","se.epc")

num.round<-function(var){
  var<-as.numeric(var)
  var<-round(var,2)
  return(var)
}

mis.rescor_H1H2[,rounded.vars]<-sapply(mis.rescor_H1H2[,rounded.vars],num.round)

printed.vars<-c("lhs","op","rhs","mi","epc","target.epc",
                "std.epc","std.target.epc","significant.mi",
                "high.power","decision.pow","se.epc")

#print the output

mis.rescor_H1H2 %>%
  filter(mis.rescor_H1H2$decision.pow=="M" | 
                mis.rescor_H1H2$decision.pow=="EPC:M") %>%
  dplyr::select(all_of(printed.vars)) 
```

```
##   lhs op rhs     mi  epc target.epc std.epc std.target.epc significant.mi
## 1 h25 ~~ y19 313.89 0.31       0.23    0.27            0.2           TRUE
##   high.power decision.pow se.epc
## 1       TRUE        EPC:M   0.02
```

There was one misspecified residual correlation in VAA-LR, between
h25. Public services should be outsourced more than they are now for private companies and y19. Public authorities should be the main provider of social and healthcare services (r.) 

##### Exploratory respecification


```r
model_H1H2.exp.re<-paste0(model_H1H2.re,
                      "h25~~y19")
```


```r
fit_H1H2.exp.re<-cfa(model=model_H1H2.exp.re,
              data=dat2019,
              missing="fiml")
```

Inspect fit of the model


```r
round(inspect(fit_H1H2.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##     npar       df    chisq   pvalue      cfi      tli    rmsea     srmr 
##   72.000  180.000 1743.580    0.000    0.874    0.853    0.061    0.076
```

```r
round(inspect(fit_H1H2.exp.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##     npar       df    chisq   pvalue      cfi      tli    rmsea     srmr 
##   73.000  179.000 1439.326    0.000    0.899    0.881    0.055    0.073
```

The fit of the model is improved by additional residual correlation.

Retest Hypotheses 1 and 2

Print standardized estimates to test the difference between correlations


```r
std.est_H1H2.exp<-standardizedsolution(fit_H1H2.exp.re)
std.est_H1H2.exp[std.est_H1H2.exp$op==":=" | 
               std.est_H1H2.exp$op=="~~" & 
               std.est_H1H2.exp$lhs!=std.est_H1H2.exp$rhs,]
```

```
##        lhs op                            rhs est.std    se      z pvalue
## 22  VAA_LR ~~                         VAA_GT   0.470 0.022 21.658      0
## 23   CS_LR ~~                          CS_GT   0.366 0.036 10.031      0
## 24  VAA_LR ~~                          CS_LR   0.932 0.021 45.253      0
## 25  VAA_GT ~~                          CS_GT   0.990 0.010 97.117      0
## 26  VAA_LR ~~                          CS_GT   0.441 0.028 15.520      0
## 27  VAA_GT ~~                          CS_LR   0.353 0.034 10.254      0
## 28     h27 ~~                            C2h   0.237 0.056  4.266      0
## 29     h21 ~~                            C2d   0.662 0.024 27.808      0
## 30     h29 ~~                            C2c   0.273 0.040  6.876      0
## 31     h25 ~~                            y19   0.426 0.020 20.857      0
## 82 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.462 0.030 15.375      0
## 83 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.520 0.024 21.777      0
##    ci.lower ci.upper
## 22    0.428    0.513
## 23    0.294    0.437
## 24    0.891    0.972
## 25    0.970    1.010
## 26    0.386    0.497
## 27    0.285    0.420
## 28    0.128    0.346
## 29    0.615    0.708
## 30    0.195    0.351
## 31    0.386    0.466
## 82    0.403    0.520
## 83    0.473    0.566
```

The results are virtually identical to those without the additional residual correlation. 

H1.exp: There is very strong (.932, p < .001) correlation between VAA-LR and CS-LR, and it is notably stronger (difference in correlations .462, p < .001) than the strongest of correlations between different dimensions (.470 between VAA_LR and VAA_GT, p < .001)

H2.exp: There is very strong (.990, p < .001) correlation between VAA-GT and CS-GT, and it is notably stronger (difference in correlations .520, p < .001) than the strongest of correlations between different dimensions (.470 between VAA_LR and VAA_GT, p < .001)

\newpage

## H5

H5. Left-Right self-placement in the privately administered post-election Candidate Survey (CS) is positively associated with Left-Right as computed from responses to the pre-election public Voting Advice Applications (VAAs). This association is stronger than the association between placement of an imagined party voter in the privately administered post-election Candidate Survey (CS) and Left-Right as computed from responses to the pre-election public Voting Advice Applications (VAAs).

### Add placement variables and their correlations with latent factors to the model used for H1 and H2


```r
model_H5<-"
#loadings
VAA_LR=~h26+h27+h25+h28+y19
VAA_GT=~h21+h22+h13+h29+h24+y25
CS_LR=~C2b+C2g+C2h
CS_GT=~C2a+C2c+C2d+C2e+C2f+C2i+C2j

#latent correlations

#cross-dimension same-method
VAA_LR~~r.VAA*VAA_GT
CS_LR~~r.CS*CS_GT

#concurrent validity
VAA_LR~~r.LR*CS_LR
VAA_GT~~r.GT*CS_GT

#cross-dimension cross-method correlations
VAA_LR~~r.d1*CS_GT
VAA_GT~~r.d2*CS_LR

#residual correlations
h27~~C2h
h21~~C2d
h29~~C2c

#placement variables (defined as quasi-latent variables)

SP_LR=~C5a
IP_LR=~C5c

VAA_LR~~r.self.LR*SP_LR
VAA_LR~~r.ideal.LR*IP_LR

#custom parameters
test.H1:=r.LR-max(r.VAA,r.CS,r.d1,r.d2)
test.H2:=r.GT-max(r.VAA,r.CS,r.d1,r.d2)
test.H5:=r.self.LR-r.ideal.LR
"
```

### Fit the model


```r
fit_H5<-cfa(model=model_H5,
            data=dat2019,
            missing="fiml")
```


Inspect fit of the model


```r
round(inspect(fit_H1H2.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##     npar       df    chisq   pvalue      cfi      tli    rmsea     srmr 
##   72.000  180.000 1743.580    0.000    0.874    0.853    0.061    0.076
```

```r
round(inspect(fit_H5,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##     npar       df    chisq   pvalue      cfi      tli    rmsea     srmr 
##   85.000  214.000 1876.855    0.000    0.883    0.861    0.057    0.076
```

The fit of the model is adequate.

Hypotheses 3

Print standardized estimates to test the difference between correlations


```r
std.est_H5<-standardizedsolution(fit_H5)
std.est_H5[std.est_H5$op==":=" | 
               std.est_H5$op=="~~" & 
               std.est_H5$lhs!=std.est_H5$rhs,]
```

```
##         lhs op                            rhs est.std    se      z pvalue
## 22   VAA_LR ~~                         VAA_GT   0.427 0.022 19.111      0
## 23    CS_LR ~~                          CS_GT   0.353 0.037  9.567      0
## 24   VAA_LR ~~                          CS_LR   0.917 0.020 45.304      0
## 25   VAA_GT ~~                          CS_GT   0.990 0.010 96.735      0
## 26   VAA_LR ~~                          CS_GT   0.409 0.029 14.356      0
## 27   VAA_GT ~~                          CS_LR   0.338 0.035  9.708      0
## 28      h27 ~~                            C2h   0.278 0.052  5.349      0
## 29      h21 ~~                            C2d   0.659 0.024 27.382      0
## 30      h29 ~~                            C2c   0.274 0.040  6.921      0
## 33   VAA_LR ~~                          SP_LR   0.829 0.015 55.090      0
## 34   VAA_LR ~~                          IP_LR   0.739 0.020 37.659      0
## 64   VAA_GT ~~                          SP_LR   0.540 0.025 21.566      0
## 65   VAA_GT ~~                          IP_LR   0.497 0.028 17.840      0
## 66    CS_LR ~~                          SP_LR   0.753 0.022 34.247      0
## 67    CS_LR ~~                          IP_LR   0.645 0.026 25.199      0
## 68    CS_GT ~~                          SP_LR   0.528 0.027 19.680      0
## 69    CS_GT ~~                          IP_LR   0.494 0.029 17.106      0
## 70    SP_LR ~~                          IP_LR   0.828 0.011 76.807      0
## 100 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.490 0.030 16.372      0
## 101 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.563 0.024 23.020      0
## 102 test.H5 :=           r.self.LR-r.ideal.LR   0.090 0.016  5.475      0
##     ci.lower ci.upper
## 22     0.383    0.471
## 23     0.281    0.426
## 24     0.877    0.956
## 25     0.970    1.010
## 26     0.353    0.465
## 27     0.270    0.407
## 28     0.176    0.380
## 29     0.612    0.706
## 30     0.196    0.351
## 33     0.800    0.859
## 34     0.700    0.777
## 64     0.490    0.589
## 65     0.443    0.552
## 66     0.710    0.796
## 67     0.595    0.695
## 68     0.476    0.581
## 69     0.438    0.551
## 70     0.807    0.849
## 100    0.431    0.548
## 101    0.515    0.611
## 102    0.058    0.122
```

H5. The correlation between VAA_LR and CS Self-placement on LR is large (.829, p < .001) and larger than the association between VAA_LR and placement of imagined party voter (.739, p < .001; difference .09, p < .001)

### Exploratory H5: Seek misspecifications

Residual correlations


```r
mis.rescor_H5<-miPowerFit(fit_H5,cor=.20)
mis.rescor_H5<-mis.rescor_H5[mis.rescor_H5$op=="~~" & 
                                   mis.rescor_H5$lhs!=mis.rescor_H5$rhs,]
#see summary of the decisions
table(mis.rescor_H5$decision.pow)
```

```
## 
##  EPC:M EPC:NM     NM 
##      1     81    167
```

```r
#there are 1 residual correlation that is a misspecification

rounded.vars<-c("mi","epc","target.epc",
                "std.epc","se.epc")

num.round<-function(var){
  var<-as.numeric(var)
  var<-round(var,2)
  return(var)
}

mis.rescor_H5[,rounded.vars]<-sapply(mis.rescor_H5[,rounded.vars],num.round)

printed.vars<-c("lhs","op","rhs","mi","epc","target.epc",
                "std.epc","std.target.epc","significant.mi",
                "high.power","decision.pow","se.epc")

#print the output

mis.rescor_H5 %>%
  filter(mis.rescor_H5$decision.pow=="M" | 
                mis.rescor_H5$decision.pow=="EPC:M") %>%
  dplyr::select(all_of(printed.vars)) 
```

```
##   lhs op rhs     mi  epc target.epc std.epc std.target.epc significant.mi
## 1 h25 ~~ y19 297.11 0.29       0.23    0.25            0.2           TRUE
##   high.power decision.pow se.epc
## 1       TRUE        EPC:M   0.02
```

There was one misspecified residual correlation in VAA-LR, between
h25. Public services should be outsourced more than they are now for private companies and y19. Public authorities should be the main provider of social and healthcare services (r.) 


#### Exploratory respecification


```r
model_H5.exp<-paste0(model_H5,
                      "h25~~y19")
```


```r
fit_H5.exp<-cfa(model=model_H5.exp,
              data=dat2019,
              missing="fiml")
```

Inspect fit of the model


```r
round(inspect(fit_H5,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##     npar       df    chisq   pvalue      cfi      tli    rmsea     srmr 
##   85.000  214.000 1876.855    0.000    0.883    0.861    0.057    0.076
```

```r
round(inspect(fit_H5.exp,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##     npar       df    chisq   pvalue      cfi      tli    rmsea     srmr 
##   86.000  213.000 1582.463    0.000    0.903    0.885    0.052    0.073
```

The fit of the model is improved.

Retest Hypothesis 5

Print standardized estimates to test the difference between correlations


```r
std.est_H1H2.exp<-standardizedsolution(fit_H5.exp)
std.est_H1H2.exp[std.est_H1H2.exp$op==":=" | 
               std.est_H1H2.exp$op=="~~" & 
               std.est_H1H2.exp$lhs!=std.est_H1H2.exp$rhs,]
```

```
##         lhs op                            rhs est.std    se      z pvalue
## 22   VAA_LR ~~                         VAA_GT   0.472 0.022 21.792      0
## 23    CS_LR ~~                          CS_GT   0.364 0.036  9.966      0
## 24   VAA_LR ~~                          CS_LR   0.931 0.020 45.491      0
## 25   VAA_GT ~~                          CS_GT   0.990 0.010 96.827      0
## 26   VAA_LR ~~                          CS_GT   0.443 0.028 15.650      0
## 27   VAA_GT ~~                          CS_LR   0.351 0.034 10.234      0
## 28      h27 ~~                            C2h   0.238 0.054  4.423      0
## 29      h21 ~~                            C2d   0.659 0.024 27.486      0
## 30      h29 ~~                            C2c   0.274 0.040  6.916      0
## 33   VAA_LR ~~                          SP_LR   0.845 0.015 56.713      0
## 34   VAA_LR ~~                          IP_LR   0.751 0.020 38.037      0
## 35      h25 ~~                            y19   0.418 0.021 20.345      0
## 65   VAA_GT ~~                          SP_LR   0.544 0.025 21.883      0
## 66   VAA_GT ~~                          IP_LR   0.502 0.028 18.073      0
## 67    CS_LR ~~                          SP_LR   0.756 0.022 34.778      0
## 68    CS_LR ~~                          IP_LR   0.648 0.025 25.565      0
## 69    CS_GT ~~                          SP_LR   0.532 0.027 19.916      0
## 70    CS_GT ~~                          IP_LR   0.498 0.029 17.287      0
## 71    SP_LR ~~                          IP_LR   0.830 0.011 77.269      0
## 101 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.460 0.030 15.378      0
## 102 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.518 0.024 21.741      0
## 103 test.H5 :=           r.self.LR-r.ideal.LR   0.094 0.017  5.662      0
##     ci.lower ci.upper
## 22     0.429    0.514
## 23     0.292    0.435
## 24     0.891    0.972
## 25     0.970    1.010
## 26     0.387    0.498
## 27     0.284    0.419
## 28     0.133    0.344
## 29     0.612    0.706
## 30     0.196    0.351
## 33     0.816    0.874
## 34     0.712    0.790
## 35     0.378    0.458
## 65     0.495    0.593
## 66     0.448    0.556
## 67     0.713    0.799
## 68     0.599    0.698
## 69     0.480    0.585
## 70     0.442    0.554
## 71     0.809    0.851
## 101    0.401    0.518
## 102    0.471    0.565
## 103    0.061    0.126
```

The results are virtually identical to those without the additional residual correlation.

H5.exp. The correlation between VAA_LR and CS Self-placement on LR is large (.845, p < .001) and larger than the association between VAA_LR and placement of imagined party voter (.751, p < .001; difference .094, p < .001)


\newpage

## H3 and H4

Exclude other than members of the eight parties that have multiple members in the parliament


```r
dat2019.party<-dat2019 %>%
  filter(puolue=="KD" |
           puolue=="KESK" |
           puolue=="KOK" |
           puolue=="PS" |
           puolue=="RKP" |
           puolue=="SDP" |
           puolue=="VAS" |
           puolue=="VIHR")

table(dat2019.party$puolue)
```

```
## 
##   KD KESK  KOK   PS  RKP  SDP  VAS VIHR 
##  188  213  211  212   97  213  211  214
```


### Define the model


```r
model_H3H4<-"
#loadings
VAA_LR=~h26+h27+h25+h28+y19
VAA_GT=~h21+h22+h13+h29+h24+y25
CS_LR=~C2b+C2g+C2h
CS_GT=~C2a+C2c+C2d+C2e+C2f+C2i+C2j

#latent correlations

#cross-dimension same-method
VAA_LR~~c(r.VAA.KD,r.VAA.KESK,r.VAA.KOK,r.VAA.PS,r.VAA.RKP,r.VAA.SDP,r.VAA.VAS,r.VAA.VIHR)*VAA_GT
CS_LR~~c(r.CS.KD,r.CS.KESK,r.CS.KOK,r.CS.PS,r.CS.RKP,r.CS.SDP,r.CS.VAS,r.CS.VIHR)*CS_GT

#concurrent validity
VAA_LR~~c(r.LR.KD,r.LR.KESK,r.LR.KOK,r.LR.PS,r.LR.RKP,r.LR.SDP,r.LR.VAS,r.LR.VIHR)*CS_LR
VAA_GT~~c(r.GT.KD,r.GT.KESK,r.GT.KOK,r.GT.PS,r.GT.RKP,r.GT.SDP,r.GT.VAS,r.GT.VIHR)*CS_GT

#cross-dimension cross-method correlations
VAA_LR~~c(r.d1.KD,r.d1.KESK,r.d1.KOK,r.d1.PS,r.d1.RKP,r.d1.SDP,r.d1.VAS,r.d1.VIHR)*CS_GT
VAA_GT~~c(r.d2.KD,r.d2.KESK,r.d2.KOK,r.d2.PS,r.d2.RKP,r.d2.SDP,r.d2.VAS,r.d2.VIHR)*CS_LR

#custom parameters
mean.r.VAA:=mean(r.VAA.KD,r.VAA.KESK,r.VAA.KOK,r.VAA.PS,r.VAA.RKP,r.VAA.SDP,r.VAA.VAS,r.VAA.VIHR)
mean.r.CS:=mean(r.CS.KD,r.CS.KESK,r.CS.KOK,r.CS.PS,r.CS.RKP,r.CS.SDP,r.CS.VAS,r.CS.VIHR)
mean.r.LR:=mean(r.LR.KD,r.LR.KESK,r.LR.KOK,r.LR.PS,r.LR.RKP,r.LR.SDP,r.LR.VAS,r.LR.VIHR)
mean.r.GT:=mean(r.GT.KD,r.GT.KESK,r.GT.KOK,r.GT.PS,r.GT.RKP,r.GT.SDP,r.GT.VAS,r.GT.VIHR)
mean.r.d1:=mean(r.d1.KD,r.d1.KESK,r.d1.KOK,r.d1.PS,r.d1.RKP,r.d1.SDP,r.d1.VAS,r.d1.VIHR)
mean.r.d2:=mean(r.d2.KD,r.d2.KESK,r.d2.KOK,r.d2.PS,r.d2.RKP,r.d2.SDP,r.d2.VAS,r.d2.VIHR)

test.H3:=mean.r.LR-max(mean.r.VAA,mean.r.CS,mean.r.d1,mean.r.d2)
test.H4:=mean.r.GT-max(mean.r.VAA,mean.r.CS,mean.r.d1,mean.r.d2)

"
```


### Fit the configural model


```r
fit_H3H4<-cfa(model=model_H3H4,
              data=dat2019.party,
              group=c("puolue"),
              group.label=c("KD","KESK","KOK","PS","RKP","SDP","VAS","VIHR"),
              missing="fiml")
```

```
## Warning in lav_model_estimate(lavmodel = lavmodel, lavpartable = lavpartable, : lavaan WARNING: the optimizer (NLMINB) claimed the model converged,
##                   but not all elements of the gradient are (near) zero;
##                   the optimizer may not have found a local solution
##                   use check.gradient = FALSE to skip this check.
```

Problems with finding a converging model. Add preregistered residual correlations.



```r
model_H3H4.re<-paste0(model_H3H4,
                      "h27~~C2h\n",
                      "h21~~C2d\n",
                      "h29~~C2c\n")
```

#### Fit the respecified model


```r
fit_H3H4.re<-cfa(model=model_H3H4.re,
              data=dat2019.party,
              group=c("puolue"),
              group.label=c("KD","KESK","KOK","PS","RKP","SDP","VAS","VIHR"),
              missing="fiml")
```

```
## Warning in lav_model_estimate(lavmodel = lavmodel, lavpartable = lavpartable, : lavaan WARNING: the optimizer (NLMINB) claimed the model converged,
##                   but not all elements of the gradient are (near) zero;
##                   the optimizer may not have found a local solution
##                   use check.gradient = FALSE to skip this check.
```

The problem still persists



```r
summary(fit_H3H4.re,fit=T,standardized=T,rsquare=T)
```

```
## lavaan 0.6-5 did NOT end normally after 2380 iterations
## ** WARNING ** Estimates below are most likely unreliable
## 
##   Estimator                                         ML
##   Optimization method                           NLMINB
##   Number of free parameters                        576
##                                                       
##   Number of observations per group:                   
##     KD                                             188
##     KESK                                           213
##     KOK                                            211
##     PS                                             212
##     RKP                                             97
##     SDP                                            213
##     VAS                                            211
##     VIHR                                           214
##   Number of missing patterns per group:               
##     KD                                              12
##     KESK                                             9
##     KOK                                             10
##     PS                                               8
##     RKP                                              6
##     SDP                                              9
##     VAS                                             10
##     VIHR                                             9
##                                                       
## Model Test User Model:
##                                                       
##   Test statistic                                    NA
##   Degrees of freedom                                NA
##   Test statistic for each group:
##     KD                                              NA
##     KESK                                            NA
##     KOK                                             NA
##     PS                                              NA
##     RKP                                             NA
##     SDP                                             NA
##     VAS                                             NA
##     VIHR                                            NA
```

```
## Warning in .local(object, ...): lavaan WARNING: fit measures not available if model did not converge
```

```
## Warning in sqrt(ETA2): NaNs produced

## Warning in sqrt(ETA2): NaNs produced

## Warning in sqrt(ETA2): NaNs produced

## Warning in sqrt(ETA2): NaNs produced

## Warning in sqrt(ETA2): NaNs produced

## Warning in sqrt(ETA2): NaNs produced

## Warning in sqrt(ETA2): NaNs produced

## Warning in sqrt(ETA2): NaNs produced
```

```
## 
## Parameter Estimates:
## 
##   Information                                 Observed
##   Observed information based on                Hessian
##   Standard errors                             Standard
## 
## 
## Group 1 [KD]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR =~                                                             
##     h26               1.000                               0.782    0.699
##     h27               0.779       NA                      0.610    0.495
##     h25               0.793       NA                      0.621    0.610
##     h28               0.782       NA                      0.612    0.511
##     y19               0.721       NA                      0.564    0.548
##   VAA_GT =~                                                             
##     h21               1.000                               0.304    0.364
##     h22               2.546       NA                      0.773    0.583
##     h13               0.602       NA                      0.183    0.169
##     h29               1.469       NA                      0.446    0.378
##     h24               0.433       NA                      0.131    0.232
##     y25               2.704       NA                      0.821    0.651
##   CS_LR =~                                                              
##     C2b               1.000                               0.469    0.468
##     C2g               1.581       NA                      0.741    0.697
##     C2h               1.849       NA                      0.867    0.783
##   CS_GT =~                                                              
##     C2a               1.000                               0.113    0.167
##     C2c               5.409       NA                      0.610    0.525
##     C2d               6.884       NA                      0.777    0.645
##     C2e               3.030       NA                      0.342    0.317
##     C2f               0.163       NA                      0.018    0.020
##     C2i               3.150       NA                      0.356    0.329
##     C2j               5.638       NA                      0.636    0.482
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR ~~                                                             
##     VAA_GT  (r.VA)    0.073       NA                      0.308    0.308
##   CS_LR ~~                                                              
##     CS_GT   (r.CS)    0.012       NA                      0.218    0.218
##   VAA_LR ~~                                                             
##     CS_LR   (r.LR)    0.307       NA                      0.837    0.837
##   VAA_GT ~~                                                             
##     CS_GT   (r.GT)    0.024       NA                      0.715    0.715
##   VAA_LR ~~                                                             
##     CS_GT   (r.1.)    0.045       NA                      0.509    0.509
##   VAA_GT ~~                                                             
##     CS_LR   (r.2.)    0.038       NA                      0.267    0.267
##  .h27 ~~                                                                
##    .C2h               0.288       NA                      0.288    0.391
##  .h21 ~~                                                                
##    .C2d               0.206       NA                      0.206    0.288
##  .h29 ~~                                                                
##    .C2c               0.547       NA                      0.547    0.506
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               2.575       NA                      2.575    2.300
##    .h27               2.924       NA                      2.924    2.377
##    .h25               2.040       NA                      2.040    2.004
##    .h28               3.218       NA                      3.218    2.687
##    .y19               1.988       NA                      1.988    1.931
##    .h21               4.434       NA                      4.434    5.312
##    .h22               2.729       NA                      2.729    2.059
##    .h13               2.564       NA                      2.564    2.374
##    .h29               2.380       NA                      2.380    2.017
##    .h24               4.790       NA                      4.790    8.461
##    .y25               3.221       NA                      3.221    2.554
##    .C2b               2.227       NA                      2.227    2.220
##    .C2g               2.293       NA                      2.293    2.156
##    .C2h               2.322       NA                      2.322    2.097
##    .C2a               4.477       NA                      4.477    6.634
##    .C2c               2.671       NA                      2.671    2.296
##    .C2d               3.873       NA                      3.873    3.213
##    .C2e               3.682       NA                      3.682    3.409
##    .C2f               3.722       NA                      3.722    3.972
##    .C2i               2.815       NA                      2.815    2.608
##    .C2j               3.743       NA                      3.743    2.837
##     VAA_LR            0.000                               0.000    0.000
##     VAA_GT            0.000                               0.000    0.000
##     CS_LR             0.000                               0.000    0.000
##     CS_GT             0.000                               0.000    0.000
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               0.641       NA                      0.641    0.511
##    .h27               1.142       NA                      1.142    0.754
##    .h25               0.651       NA                      0.651    0.628
##    .h28               1.059       NA                      1.059    0.739
##    .y19               0.742       NA                      0.742    0.700
##    .h21               0.605       NA                      0.605    0.868
##    .h22               1.159       NA                      1.159    0.660
##    .h13               1.133       NA                      1.133    0.971
##    .h29               1.193       NA                      1.193    0.857
##    .h24               0.303       NA                      0.303    0.946
##    .y25               0.917       NA                      0.917    0.576
##    .C2b               0.786       NA                      0.786    0.781
##    .C2g               0.582       NA                      0.582    0.514
##    .C2h               0.475       NA                      0.475    0.387
##    .C2a               0.443       NA                      0.443    0.972
##    .C2c               0.981       NA                      0.981    0.725
##    .C2d               0.849       NA                      0.849    0.584
##    .C2e               1.049       NA                      1.049    0.900
##    .C2f               0.877       NA                      0.877    1.000
##    .C2i               1.038       NA                      1.038    0.891
##    .C2j               1.336       NA                      1.336    0.767
##     VAA_LR            0.612       NA                      1.000    1.000
##     VAA_GT            0.092       NA                      1.000    1.000
##     CS_LR             0.220       NA                      1.000    1.000
##     CS_GT             0.013       NA                      1.000    1.000
## 
## R-Square:
##                    Estimate
##     h26               0.489
##     h27               0.246
##     h25               0.372
##     h28               0.261
##     y19               0.300
##     h21               0.132
##     h22               0.340
##     h13               0.029
##     h29               0.143
##     h24               0.054
##     y25               0.424
##     C2b               0.219
##     C2g               0.486
##     C2h               0.613
##     C2a               0.028
##     C2c               0.275
##     C2d               0.416
##     C2e               0.100
##     C2f               0.000
##     C2i               0.109
##     C2j               0.233
## 
## 
## Group 2 [KESK]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR =~                                                             
##     h26               1.000                               0.420    0.366
##     h27               1.054       NA                      0.443    0.367
##     h25               1.701       NA                      0.715    0.737
##     h28               0.876       NA                      0.368    0.300
##     y19               0.730       NA                      0.307    0.356
##   VAA_GT =~                                                             
##     h21               1.000                               1.021    0.735
##     h22               0.432       NA                      0.441    0.343
##     h13               0.281       NA                      0.287    0.341
##     h29               0.297       NA                      0.304    0.291
##     h24               0.528       NA                      0.539    0.516
##     y25               0.527       NA                      0.537    0.433
##   CS_LR =~                                                              
##     C2b               1.000                                 NaN      NaN
##     C2g               4.350       NA                        NaN      NaN
##     C2h            -285.327       NA                        NaN      NaN
##   CS_GT =~                                                              
##     C2a               1.000                               0.196    0.261
##     C2c               1.011       NA                      0.198    0.189
##     C2d               5.084       NA                      0.994    0.750
##     C2e              -0.296       NA                     -0.058   -0.052
##     C2f               0.278       NA                      0.054    0.047
##     C2i               1.402       NA                      0.274    0.279
##     C2j               3.227       NA                      0.631    0.513
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR ~~                                                             
##     VAA_GT  (r.VA)   -0.051       NA                     -0.119   -0.119
##   CS_LR ~~                                                              
##     CS_GT   (r.CS)   -0.000       NA                     -0.017   -0.017
##   VAA_LR ~~                                                             
##     CS_LR   (r.LR)   -0.000       NA                     -0.032   -0.032
##   VAA_GT ~~                                                             
##     CS_GT   (r.GT)    0.155       NA                      0.777    0.777
##   VAA_LR ~~                                                             
##     CS_GT   (r.1.)   -0.010       NA                     -0.126   -0.126
##   VAA_GT ~~                                                             
##     CS_LR   (r.2.)   -0.000       NA                     -0.015   -0.015
##  .h27 ~~                                                                
##    .C2h               0.133       NA                      0.133    0.020
##  .h21 ~~                                                                
##    .C2d               0.805       NA                      0.805    0.973
##  .h29 ~~                                                                
##    .C2c               0.199       NA                      0.199    0.194
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               2.928       NA                      2.928    2.552
##    .h27               2.719       NA                      2.719    2.250
##    .h25               2.231       NA                      2.231    2.299
##    .h28               3.243       NA                      3.243    2.643
##    .y19               1.943       NA                      1.943    2.259
##    .h21               2.354       NA                      2.354    1.695
##    .h22               2.894       NA                      2.894    2.247
##    .h13               1.862       NA                      1.862    2.209
##    .h29               2.354       NA                      2.354    2.258
##    .h24               4.044       NA                      4.044    3.876
##    .y25               3.075       NA                      3.075    2.477
##    .C2b               2.058       NA                      2.058    2.467
##    .C2g               2.449       NA                      2.449    2.380
##    .C2h               2.483       NA                      2.483    2.565
##    .C2a               4.276       NA                      4.276    5.700
##    .C2c               2.555       NA                      2.555    2.439
##    .C2d               2.048       NA                      2.048    1.544
##    .C2e               3.571       NA                      3.571    3.184
##    .C2f               3.675       NA                      3.675    3.194
##    .C2i               2.433       NA                      2.433    2.478
##    .C2j               2.066       NA                      2.066    1.681
##     VAA_LR            0.000                               0.000    0.000
##     VAA_GT            0.000                               0.000    0.000
##     CS_LR             0.000                                 NaN      NaN
##     CS_GT             0.000                               0.000    0.000
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               1.139       NA                      1.139    0.866
##    .h27               1.265       NA                      1.265    0.866
##    .h25               0.431       NA                      0.431    0.457
##    .h28               1.370       NA                      1.370    0.910
##    .y19               0.646       NA                      0.646    0.873
##    .h21               0.888       NA                      0.888    0.460
##    .h22               1.464       NA                      1.464    0.883
##    .h13               0.628       NA                      0.628    0.884
##    .h29               0.995       NA                      0.995    0.915
##    .h24               0.799       NA                      0.799    0.734
##    .y25               1.252       NA                      1.252    0.813
##    .C2b               0.696       NA                      0.696    1.001
##    .C2g               1.067       NA                      1.067    1.007
##    .C2h              33.824       NA                     33.824   36.098
##    .C2a               0.524       NA                      0.524    0.932
##    .C2c               1.058       NA                      1.058    0.964
##    .C2d               0.771       NA                      0.771    0.438
##    .C2e               1.255       NA                      1.255    0.997
##    .C2f               1.321       NA                      1.321    0.998
##    .C2i               0.888       NA                      0.888    0.922
##    .C2j               1.112       NA                      1.112    0.736
##     VAA_LR            0.177       NA                      1.000    1.000
##     VAA_GT            1.041       NA                      1.000    1.000
##     CS_LR            -0.000       NA                        NaN      NaN
##     CS_GT             0.038       NA                      1.000    1.000
## 
## R-Square:
##                    Estimate
##     h26               0.134
##     h27               0.134
##     h25               0.543
##     h28               0.090
##     y19               0.127
##     h21               0.540
##     h22               0.117
##     h13               0.116
##     h29               0.085
##     h24               0.266
##     y25               0.187
##     C2b              -0.001
##     C2g              -0.007
##     C2h             -35.098
##     C2a               0.068
##     C2c               0.036
##     C2d               0.562
##     C2e               0.003
##     C2f               0.002
##     C2i               0.078
##     C2j               0.264
## 
## 
## Group 3 [KOK]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR =~                                                             
##     h26               1.000                               0.506    0.544
##     h27               0.536       NA                      0.272    0.307
##     h25               1.222       NA                      0.619    0.650
##     h28               0.879       NA                      0.445    0.439
##     y19               1.095       NA                      0.555    0.491
##   VAA_GT =~                                                             
##     h21               1.000                               0.672    0.636
##     h22               1.004       NA                      0.675    0.519
##     h13               0.254       NA                      0.170    0.267
##     h29               0.630       NA                      0.423    0.400
##     h24               0.656       NA                      0.441    0.481
##     y25               0.731       NA                      0.491    0.390
##   CS_LR =~                                                              
##     C2b               1.000                               0.560    0.463
##     C2g               1.234       NA                      0.691    0.652
##     C2h               0.807       NA                      0.452    0.409
##   CS_GT =~                                                              
##     C2a               1.000                               0.335    0.565
##     C2c              -0.793       NA                     -0.266   -0.249
##     C2d               0.362       NA                      0.121    0.152
##     C2e              -0.775       NA                     -0.260   -0.255
##     C2f               2.011       NA                      0.675    0.793
##     C2i               0.213       NA                      0.072    0.079
##     C2j              -0.369       NA                     -0.124   -0.161
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR ~~                                                             
##     VAA_GT  (r.VA)   -0.055       NA                     -0.161   -0.161
##   CS_LR ~~                                                              
##     CS_GT   (r.CS)   -0.126       NA                     -0.672   -0.672
##   VAA_LR ~~                                                             
##     CS_LR   (r.LR)    0.115       NA                      0.405    0.405
##   VAA_GT ~~                                                             
##     CS_GT   (r.GT)    0.031       NA                      0.140    0.140
##   VAA_LR ~~                                                             
##     CS_GT   (r.1.)    0.006       NA                      0.037    0.037
##   VAA_GT ~~                                                             
##     CS_LR   (r.2.)    0.122       NA                      0.325    0.325
##  .h27 ~~                                                                
##    .C2h               0.323       NA                      0.323    0.381
##  .h21 ~~                                                                
##    .C2d               0.141       NA                      0.141    0.218
##  .h29 ~~                                                                
##    .C2c              -0.029       NA                     -0.029   -0.029
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               4.019       NA                      4.019    4.317
##    .h27               3.915       NA                      3.915    4.423
##    .h25               3.566       NA                      3.566    3.746
##    .h28               3.931       NA                      3.931    3.873
##    .y19               3.014       NA                      3.014    2.671
##    .h21               1.758       NA                      1.758    1.663
##    .h22               2.978       NA                      2.978    2.294
##    .h13               1.399       NA                      1.399    2.191
##    .h29               2.472       NA                      2.472    2.336
##    .h24               4.077       NA                      4.077    4.444
##    .y25               3.387       NA                      3.387    2.689
##    .C2b               2.677       NA                      2.677    2.213
##    .C2g               3.215       NA                      3.215    3.034
##    .C2h               3.765       NA                      3.765    3.413
##    .C2a               4.463       NA                      4.463    7.515
##    .C2c               2.415       NA                      2.415    2.261
##    .C2d               1.513       NA                      1.513    1.889
##    .C2e               3.829       NA                      3.829    3.756
##    .C2f               4.030       NA                      4.030    4.734
##    .C2i               2.413       NA                      2.413    2.651
##    .C2j               1.777       NA                      1.777    2.317
##     VAA_LR            0.000                               0.000    0.000
##     VAA_GT            0.000                               0.000    0.000
##     CS_LR             0.000                               0.000    0.000
##     CS_GT             0.000                               0.000    0.000
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               0.610       NA                      0.610    0.704
##    .h27               0.710       NA                      0.710    0.906
##    .h25               0.523       NA                      0.523    0.578
##    .h28               0.832       NA                      0.832    0.807
##    .y19               0.966       NA                      0.966    0.758
##    .h21               0.666       NA                      0.666    0.596
##    .h22               1.231       NA                      1.231    0.730
##    .h13               0.378       NA                      0.378    0.929
##    .h29               0.940       NA                      0.940    0.840
##    .h24               0.647       NA                      0.647    0.769
##    .y25               1.346       NA                      1.346    0.848
##    .C2b               1.150       NA                      1.150    0.786
##    .C2g               0.645       NA                      0.645    0.575
##    .C2h               1.013       NA                      1.013    0.832
##    .C2a               0.240       NA                      0.240    0.681
##    .C2c               1.070       NA                      1.070    0.938
##    .C2d               0.627       NA                      0.627    0.977
##    .C2e               0.972       NA                      0.972    0.935
##    .C2f               0.269       NA                      0.269    0.372
##    .C2i               0.824       NA                      0.824    0.994
##    .C2j               0.572       NA                      0.572    0.974
##     VAA_LR            0.256       NA                      1.000    1.000
##     VAA_GT            0.451       NA                      1.000    1.000
##     CS_LR             0.313       NA                      1.000    1.000
##     CS_GT             0.113       NA                      1.000    1.000
## 
## R-Square:
##                    Estimate
##     h26               0.296
##     h27               0.094
##     h25               0.422
##     h28               0.193
##     y19               0.242
##     h21               0.404
##     h22               0.270
##     h13               0.071
##     h29               0.160
##     h24               0.231
##     y25               0.152
##     C2b               0.214
##     C2g               0.425
##     C2h               0.168
##     C2a               0.319
##     C2c               0.062
##     C2d               0.023
##     C2e               0.065
##     C2f               0.628
##     C2i               0.006
##     C2j               0.026
## 
## 
## Group 4 [PS]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR =~                                                             
##     h26               1.000                               0.701    0.560
##     h27               1.065       NA                      0.747    0.584
##     h25               0.585       NA                      0.410    0.438
##     h28               0.828       NA                      0.581    0.486
##     y19               0.422       NA                      0.296    0.331
##   VAA_GT =~                                                             
##     h21               1.000                               0.293    0.221
##     h22               0.703       NA                      0.206    0.438
##     h13               2.000       NA                      0.587    0.622
##     h29               1.315       NA                      0.386    0.307
##     h24               0.452       NA                      0.133    0.233
##     y25               1.068       NA                      0.313    0.366
##   CS_LR =~                                                              
##     C2b               1.000                                 NaN      NaN
##     C2g               1.512       NA                        NaN      NaN
##     C2h            -173.590       NA                        NaN      NaN
##   CS_GT =~                                                              
##     C2a               1.000                                 NaN      NaN
##     C2c               3.834       NA                        NaN      NaN
##     C2d               1.047       NA                        NaN      NaN
##     C2e              -1.128       NA                        NaN      NaN
##     C2f               0.364       NA                        NaN      NaN
##     C2i              -3.265       NA                        NaN      NaN
##     C2j               0.868       NA                        NaN      NaN
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR ~~                                                             
##     VAA_GT  (r.VA)    0.023       NA                      0.110    0.110
##   CS_LR ~~                                                              
##     CS_GT   (r.CS)    0.000       NA                      0.024    0.024
##   VAA_LR ~~                                                             
##     CS_LR   (r.LR)   -0.003       NA                     -0.106   -0.106
##   VAA_GT ~~                                                             
##     CS_GT   (r.GT)    0.030       NA                      1.052    1.052
##   VAA_LR ~~                                                             
##     CS_GT   (r.1.)   -0.021       NA                     -0.319   -0.319
##   VAA_GT ~~                                                             
##     CS_LR   (r.2.)    0.000       NA                      0.018    0.018
##  .h27 ~~                                                                
##    .C2h               0.138       NA                      0.138    0.019
##  .h21 ~~                                                                
##    .C2d               0.799       NA                      0.799    0.482
##  .h29 ~~                                                                
##    .C2c              -0.226       NA                     -0.226   -0.149
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               3.386       NA                      3.386    2.705
##    .h27               3.342       NA                      3.342    2.610
##    .h25               1.884       NA                      1.884    2.014
##    .h28               3.606       NA                      3.606    3.017
##    .y19               1.659       NA                      1.659    1.857
##    .h21               3.615       NA                      3.615    2.719
##    .h22               4.839       NA                      4.839   10.261
##    .h13               4.230       NA                      4.230    4.482
##    .h29               3.432       NA                      3.432    2.732
##    .h24               4.646       NA                      4.646    8.151
##    .y25               4.410       NA                      4.410    5.146
##    .C2b               2.113       NA                      2.113    2.140
##    .C2g               2.567       NA                      2.567    2.220
##    .C2h               2.651       NA                      2.651    2.420
##    .C2a               4.941       NA                      4.941   23.832
##    .C2c               3.223       NA                      3.223    2.652
##    .C2d               2.665       NA                      2.665    2.091
##    .C2e               3.993       NA                      3.993    4.243
##    .C2f               4.291       NA                      4.291    6.187
##    .C2i               4.499       NA                      4.499    4.796
##    .C2j               1.897       NA                      1.897    1.787
##     VAA_LR            0.000                               0.000    0.000
##     VAA_GT            0.000                               0.000    0.000
##     CS_LR             0.000                                 NaN      NaN
##     CS_GT             0.000                                 NaN      NaN
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               1.076       NA                      1.076    0.686
##    .h27               1.080       NA                      1.080    0.659
##    .h25               0.707       NA                      0.707    0.808
##    .h28               1.091       NA                      1.091    0.764
##    .y19               0.711       NA                      0.711    0.890
##    .h21               1.682       NA                      1.682    0.951
##    .h22               0.180       NA                      0.180    0.809
##    .h13               0.547       NA                      0.547    0.614
##    .h29               1.429       NA                      1.429    0.906
##    .h24               0.307       NA                      0.307    0.946
##    .y25               0.636       NA                      0.636    0.866
##    .C2b               0.976       NA                      0.976    1.002
##    .C2g               1.340       NA                      1.340    1.003
##    .C2h              48.112       NA                     48.112   40.112
##    .C2a               0.052       NA                      0.052    1.213
##    .C2c               1.611       NA                      1.611    1.091
##    .C2d               1.635       NA                      1.635    1.006
##    .C2e               0.897       NA                      0.897    1.013
##    .C2f               0.482       NA                      0.482    1.003
##    .C2i               0.978       NA                      0.978    1.111
##    .C2j               1.135       NA                      1.135    1.006
##     VAA_LR            0.492       NA                      1.000    1.000
##     VAA_GT            0.086       NA                      1.000    1.000
##     CS_LR            -0.002       NA                        NaN      NaN
##     CS_GT            -0.009       NA                        NaN      NaN
## 
## R-Square:
##                    Estimate
##     h26               0.314
##     h27               0.341
##     h25               0.192
##     h28               0.236
##     y19               0.110
##     h21               0.049
##     h22               0.191
##     h13               0.386
##     h29               0.094
##     h24               0.054
##     y25               0.134
##     C2b              -0.002
##     C2g              -0.003
##     C2h             -39.112
##     C2a              -0.213
##     C2c              -0.091
##     C2d              -0.006
##     C2e              -0.013
##     C2f              -0.003
##     C2i              -0.111
##     C2j              -0.006
## 
## 
## Group 5 [RKP]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR =~                                                             
##     h26               1.000                               0.823    0.704
##     h27               0.983       NA                      0.809    0.660
##     h25               0.814       NA                      0.670    0.595
##     h28               0.474       NA                      0.390    0.338
##     y19               0.439       NA                      0.362    0.364
##   VAA_GT =~                                                             
##     h21               1.000                               0.327    0.562
##     h22               1.146       NA                      0.375    0.454
##     h13               0.440       NA                      0.144    0.174
##     h29               1.537       NA                      0.503    0.485
##     h24               2.379       NA                      0.779    0.609
##     y25               2.271       NA                      0.744    0.567
##   CS_LR =~                                                              
##     C2b               1.000                               0.393    0.393
##     C2g               1.297       NA                      0.509    0.518
##     C2h               2.252       NA                      0.884    0.773
##   CS_GT =~                                                              
##     C2a               1.000                               0.819    0.747
##     C2c               0.298       NA                      0.244    0.321
##     C2d               0.424       NA                      0.348    0.363
##     C2e               0.335       NA                      0.274    0.257
##     C2f               0.915       NA                      0.750    0.760
##     C2i               0.401       NA                      0.329    0.434
##     C2j               0.085       NA                      0.070    0.170
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR ~~                                                             
##     VAA_GT  (r.VA)    0.070       NA                      0.261    0.261
##   CS_LR ~~                                                              
##     CS_GT   (r.CS)   -0.023       NA                     -0.071   -0.071
##   VAA_LR ~~                                                             
##     CS_LR   (r.LR)    0.281       NA                      0.870    0.870
##   VAA_GT ~~                                                             
##     CS_GT   (r.GT)    0.267       NA                      0.994    0.994
##   VAA_LR ~~                                                             
##     CS_GT   (r.1.)    0.069       NA                      0.103    0.103
##   VAA_GT ~~                                                             
##     CS_LR   (r.2.)   -0.007       NA                     -0.054   -0.054
##  .h27 ~~                                                                
##    .C2h               0.268       NA                      0.268    0.402
##  .h21 ~~                                                                
##    .C2d               0.242       NA                      0.242    0.564
##  .h29 ~~                                                                
##    .C2c               0.215       NA                      0.215    0.329
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               2.895       NA                      2.895    2.474
##    .h27               3.003       NA                      3.003    2.452
##    .h25               2.494       NA                      2.494    2.216
##    .h28               2.948       NA                      2.948    2.550
##    .y19               1.938       NA                      1.938    1.952
##    .h21               1.194       NA                      1.194    2.050
##    .h22               1.670       NA                      1.670    2.019
##    .h13               1.410       NA                      1.410    1.707
##    .h29               2.114       NA                      2.114    2.037
##    .h24               2.937       NA                      2.937    2.294
##    .y25               2.639       NA                      2.639    2.012
##    .C2b               1.998       NA                      1.998    2.000
##    .C2g               2.040       NA                      2.040    2.074
##    .C2h               2.419       NA                      2.419    2.113
##    .C2a               3.660       NA                      3.660    3.338
##    .C2c               1.546       NA                      1.546    2.031
##    .C2d               1.367       NA                      1.367    1.430
##    .C2e               3.867       NA                      3.867    3.626
##    .C2f               3.345       NA                      3.345    3.393
##    .C2i               1.634       NA                      1.634    2.157
##    .C2j               1.134       NA                      1.134    2.763
##     VAA_LR            0.000                               0.000    0.000
##     VAA_GT            0.000                               0.000    0.000
##     CS_LR             0.000                               0.000    0.000
##     CS_GT             0.000                               0.000    0.000
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               0.691       NA                      0.691    0.505
##    .h27               0.846       NA                      0.846    0.564
##    .h25               0.818       NA                      0.818    0.646
##    .h28               1.184       NA                      1.184    0.886
##    .y19               0.855       NA                      0.855    0.867
##    .h21               0.232       NA                      0.232    0.684
##    .h22               0.543       NA                      0.543    0.794
##    .h13               0.662       NA                      0.662    0.970
##    .h29               0.824       NA                      0.824    0.765
##    .h24               1.032       NA                      1.032    0.630
##    .y25               1.169       NA                      1.169    0.679
##    .C2b               0.844       NA                      0.844    0.846
##    .C2g               0.709       NA                      0.709    0.732
##    .C2h               0.528       NA                      0.528    0.403
##    .C2a               0.531       NA                      0.531    0.442
##    .C2c               0.520       NA                      0.520    0.897
##    .C2d               0.794       NA                      0.794    0.868
##    .C2e               1.062       NA                      1.062    0.934
##    .C2f               0.410       NA                      0.410    0.422
##    .C2i               0.466       NA                      0.466    0.812
##    .C2j               0.163       NA                      0.163    0.971
##     VAA_LR            0.678       NA                      1.000    1.000
##     VAA_GT            0.107       NA                      1.000    1.000
##     CS_LR             0.154       NA                      1.000    1.000
##     CS_GT             0.671       NA                      1.000    1.000
## 
## R-Square:
##                    Estimate
##     h26               0.495
##     h27               0.436
##     h25               0.354
##     h28               0.114
##     y19               0.133
##     h21               0.316
##     h22               0.206
##     h13               0.030
##     h29               0.235
##     h24               0.370
##     y25               0.321
##     C2b               0.154
##     C2g               0.268
##     C2h               0.597
##     C2a               0.558
##     C2c               0.103
##     C2d               0.132
##     C2e               0.066
##     C2f               0.578
##     C2i               0.188
##     C2j               0.029
## 
## 
## Group 6 [SDP]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR =~                                                             
##     h26               1.000                               0.235    0.320
##     h27               2.164       NA                      0.509    0.663
##     h25               0.428       NA                      0.101    0.219
##     h28               2.338       NA                      0.550    0.560
##     y19               0.372       NA                      0.087    0.237
##   VAA_GT =~                                                             
##     h21               1.000                               0.310    0.391
##     h22               2.346       NA                      0.727    0.617
##     h13               1.131       NA                      0.351    0.414
##     h29               1.530       NA                      0.474    0.470
##     h24               1.867       NA                      0.579    0.449
##     y25               1.971       NA                      0.611    0.474
##   CS_LR =~                                                              
##     C2b               1.000                               0.014    0.018
##     C2g             -32.358       NA                     -0.451   -0.550
##     C2h             -22.085       NA                     -0.307   -0.567
##   CS_GT =~                                                              
##     C2a               1.000                               0.586    0.634
##     C2c               0.677       NA                      0.397    0.508
##     C2d               0.654       NA                      0.384    0.374
##     C2e               0.338       NA                      0.198    0.177
##     C2f               0.898       NA                      0.526    0.461
##     C2i               1.076       NA                      0.630    0.716
##     C2j               0.196       NA                      0.115    0.130
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR ~~                                                             
##     VAA_GT  (r.VA)    0.049       NA                      0.674    0.674
##   CS_LR ~~                                                              
##     CS_GT   (r.CS)   -0.006       NA                     -0.750   -0.750
##   VAA_LR ~~                                                             
##     CS_LR   (r.LR)   -0.002       NA                     -0.727   -0.727
##   VAA_GT ~~                                                             
##     CS_GT   (r.GT)    0.205       NA                      1.127    1.127
##   VAA_LR ~~                                                             
##     CS_GT   (r.1.)    0.071       NA                      0.517    0.517
##   VAA_GT ~~                                                             
##     CS_LR   (r.2.)   -0.001       NA                     -0.316   -0.316
##  .h27 ~~                                                                
##    .C2h               0.038       NA                      0.038    0.146
##  .h21 ~~                                                                
##    .C2d               0.422       NA                      0.422    0.609
##  .h29 ~~                                                                
##    .C2c               0.082       NA                      0.082    0.137
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               1.731       NA                      1.731    2.356
##    .h27               1.578       NA                      1.578    2.058
##    .h25               1.237       NA                      1.237    2.690
##    .h28               1.856       NA                      1.856    1.893
##    .y19               1.163       NA                      1.163    3.152
##    .h21               1.296       NA                      1.296    1.635
##    .h22               2.269       NA                      2.269    1.925
##    .h13               1.534       NA                      1.534    1.811
##    .h29               2.041       NA                      2.041    2.022
##    .h24               2.712       NA                      2.712    2.105
##    .y25               2.493       NA                      2.493    1.932
##    .C2b               1.703       NA                      1.703    2.194
##    .C2g               1.919       NA                      1.919    2.343
##    .C2h               1.383       NA                      1.383    2.549
##    .C2a               4.036       NA                      4.036    4.363
##    .C2c               1.793       NA                      1.793    2.293
##    .C2d               1.425       NA                      1.425    1.391
##    .C2e               3.332       NA                      3.332    2.979
##    .C2f               3.452       NA                      3.452    3.026
##    .C2i               2.140       NA                      2.140    2.432
##    .C2j               1.520       NA                      1.520    1.717
##     VAA_LR            0.000                               0.000    0.000
##     VAA_GT            0.000                               0.000    0.000
##     CS_LR             0.000                               0.000    0.000
##     CS_GT             0.000                               0.000    0.000
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               0.485       NA                      0.485    0.898
##    .h27               0.329       NA                      0.329    0.560
##    .h25               0.201       NA                      0.201    0.952
##    .h28               0.660       NA                      0.660    0.686
##    .y19               0.129       NA                      0.129    0.944
##    .h21               0.532       NA                      0.532    0.847
##    .h22               0.861       NA                      0.861    0.619
##    .h13               0.594       NA                      0.594    0.828
##    .h29               0.794       NA                      0.794    0.779
##    .h24               1.325       NA                      1.325    0.798
##    .y25               1.291       NA                      1.291    0.776
##    .C2b               0.602       NA                      0.602    1.000
##    .C2g               0.468       NA                      0.468    0.697
##    .C2h               0.200       NA                      0.200    0.679
##    .C2a               0.512       NA                      0.512    0.599
##    .C2c               0.454       NA                      0.454    0.742
##    .C2d               0.902       NA                      0.902    0.860
##    .C2e               1.212       NA                      1.212    0.969
##    .C2f               1.024       NA                      1.024    0.787
##    .C2i               0.377       NA                      0.377    0.487
##    .C2j               0.770       NA                      0.770    0.983
##     VAA_LR            0.055       NA                      1.000    1.000
##     VAA_GT            0.096       NA                      1.000    1.000
##     CS_LR             0.000       NA                      1.000    1.000
##     CS_GT             0.344       NA                      1.000    1.000
## 
## R-Square:
##                    Estimate
##     h26               0.102
##     h27               0.440
##     h25               0.048
##     h28               0.314
##     y19               0.056
##     h21               0.153
##     h22               0.381
##     h13               0.172
##     h29               0.221
##     h24               0.202
##     y25               0.224
##     C2b               0.000
##     C2g               0.303
##     C2h               0.321
##     C2a               0.401
##     C2c               0.258
##     C2d               0.140
##     C2e               0.031
##     C2f               0.213
##     C2i               0.513
##     C2j               0.017
## 
## 
## Group 7 [VAS]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR =~                                                             
##     h26               1.000                               0.365    0.689
##     h27               0.516       NA                      0.189    0.327
##     h25               0.244       NA                      0.089    0.341
##     h28               0.578       NA                      0.211    0.297
##     y19               0.345       NA                      0.126    0.320
##   VAA_GT =~                                                             
##     h21               1.000                               0.191    0.364
##     h22               2.576       NA                      0.491    0.472
##     h13               0.828       NA                      0.158    0.146
##     h29               2.917       NA                      0.556    0.652
##     h24               2.619       NA                      0.499    0.540
##     y25               2.941       NA                      0.560    0.489
##   CS_LR =~                                                              
##     C2b               1.000                               0.005    0.005
##     C2g              77.455       NA                      0.386    0.695
##     C2h              39.688       NA                      0.198    0.441
##   CS_GT =~                                                              
##     C2a               1.000                               0.531    0.554
##     C2c               0.739       NA                      0.393    0.535
##     C2d               0.108       NA                      0.058    0.139
##     C2e               0.553       NA                      0.294    0.308
##     C2f               1.225       NA                      0.651    0.596
##     C2i               1.117       NA                      0.593    0.704
##     C2j               0.090       NA                      0.048    0.061
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR ~~                                                             
##     VAA_GT  (r.VA)    0.040       NA                      0.573    0.573
##   CS_LR ~~                                                              
##     CS_GT   (r.CS)    0.001       NA                      0.415    0.415
##   VAA_LR ~~                                                             
##     CS_LR   (r.LR)    0.001       NA                      0.378    0.378
##   VAA_GT ~~                                                             
##     CS_GT   (r.GT)    0.104       NA                      1.027    1.027
##   VAA_LR ~~                                                             
##     CS_GT   (r.1.)    0.116       NA                      0.595    0.595
##   VAA_GT ~~                                                             
##     CS_LR   (r.2.)    0.000       NA                      0.436    0.436
##  .h27 ~~                                                                
##    .C2h               0.039       NA                      0.039    0.177
##  .h21 ~~                                                                
##    .C2d               0.106       NA                      0.106    0.531
##  .h29 ~~                                                                
##    .C2c               0.081       NA                      0.081    0.202
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               1.219       NA                      1.219    2.298
##    .h27               1.253       NA                      1.253    2.177
##    .h25               1.062       NA                      1.062    4.075
##    .h28               1.274       NA                      1.274    1.792
##    .y19               1.073       NA                      1.073    2.721
##    .h21               1.155       NA                      1.155    2.205
##    .h22               1.815       NA                      1.815    1.746
##    .h13               2.449       NA                      2.449    2.264
##    .h29               1.455       NA                      1.455    1.707
##    .h24               1.743       NA                      1.743    1.888
##    .y25               2.040       NA                      2.040    1.779
##    .C2b               1.477       NA                      1.477    1.560
##    .C2g               1.422       NA                      1.422    2.564
##    .C2h               1.129       NA                      1.129    2.519
##    .C2a               3.636       NA                      3.636    3.793
##    .C2c               1.357       NA                      1.357    1.851
##    .C2d               1.074       NA                      1.074    2.602
##    .C2e               2.888       NA                      2.888    3.029
##    .C2f               2.903       NA                      2.903    2.658
##    .C2i               1.839       NA                      1.839    2.181
##    .C2j               1.325       NA                      1.325    1.684
##     VAA_LR            0.000                               0.000    0.000
##     VAA_GT            0.000                               0.000    0.000
##     CS_LR             0.000                               0.000    0.000
##     CS_GT             0.000                               0.000    0.000
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               0.148       NA                      0.148    0.526
##    .h27               0.296       NA                      0.296    0.893
##    .h25               0.060       NA                      0.060    0.883
##    .h28               0.461       NA                      0.461    0.912
##    .y19               0.140       NA                      0.140    0.898
##    .h21               0.238       NA                      0.238    0.868
##    .h22               0.840       NA                      0.840    0.777
##    .h13               1.145       NA                      1.145    0.979
##    .h29               0.418       NA                      0.418    0.575
##    .h24               0.604       NA                      0.604    0.708
##    .y25               1.001       NA                      1.001    0.761
##    .C2b               0.896       NA                      0.896    1.000
##    .C2g               0.159       NA                      0.159    0.516
##    .C2h               0.162       NA                      0.162    0.806
##    .C2a               0.637       NA                      0.637    0.693
##    .C2c               0.383       NA                      0.383    0.713
##    .C2d               0.167       NA                      0.167    0.981
##    .C2e               0.823       NA                      0.823    0.905
##    .C2f               0.769       NA                      0.769    0.645
##    .C2i               0.359       NA                      0.359    0.505
##    .C2j               0.616       NA                      0.616    0.996
##     VAA_LR            0.134       NA                      1.000    1.000
##     VAA_GT            0.036       NA                      1.000    1.000
##     CS_LR             0.000       NA                      1.000    1.000
##     CS_GT             0.282       NA                      1.000    1.000
## 
## R-Square:
##                    Estimate
##     h26               0.474
##     h27               0.107
##     h25               0.117
##     h28               0.088
##     y19               0.102
##     h21               0.132
##     h22               0.223
##     h13               0.021
##     h29               0.425
##     h24               0.292
##     y25               0.239
##     C2b               0.000
##     C2g               0.484
##     C2h               0.194
##     C2a               0.307
##     C2c               0.287
##     C2d               0.019
##     C2e               0.095
##     C2f               0.355
##     C2i               0.495
##     C2j               0.004
## 
## 
## Group 8 [VIHR]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR =~                                                             
##     h26               1.000                               0.452    0.549
##     h27               0.974       NA                      0.441    0.443
##     h25               0.769       NA                      0.348    0.533
##     h28               1.276       NA                      0.577    0.523
##     y19               0.808       NA                      0.366    0.547
##   VAA_GT =~                                                             
##     h21               1.000                               0.077    0.357
##     h22               6.003       NA                      0.464    0.576
##     h13               3.693       NA                      0.285    0.392
##     h29               1.746       NA                      0.135    0.373
##     h24               7.250       NA                      0.560    0.545
##     y25               7.361       NA                      0.569    0.600
##   CS_LR =~                                                              
##     C2b               1.000                               0.501    0.626
##     C2g               0.748       NA                      0.375    0.465
##     C2h               1.314       NA                      0.658    0.830
##   CS_GT =~                                                              
##     C2a               1.000                               0.011    0.015
##     C2c              22.241       NA                      0.241    0.815
##     C2d              21.408       NA                      0.232    0.801
##     C2e               1.822       NA                      0.020    0.020
##     C2f               1.427       NA                      0.015    0.015
##     C2i              20.135       NA                      0.218    0.383
##     C2j              28.543       NA                      0.310    0.501
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR ~~                                                             
##     VAA_GT  (r.VA)    0.021       NA                      0.595    0.595
##   CS_LR ~~                                                              
##     CS_GT   (r.CS)    0.002       NA                      0.381    0.381
##   VAA_LR ~~                                                             
##     CS_LR   (r.LR)    0.207       NA                      0.912    0.912
##   VAA_GT ~~                                                             
##     CS_GT   (r.GT)    0.000       NA                      0.095    0.095
##   VAA_LR ~~                                                             
##     CS_GT   (r.1.)    0.001       NA                      0.186    0.186
##   VAA_GT ~~                                                             
##     CS_LR   (r.2.)    0.004       NA                      0.116    0.116
##  .h27 ~~                                                                
##    .C2h               0.115       NA                      0.115    0.293
##  .h21 ~~                                                                
##    .C2d               0.001       NA                      0.001    0.043
##  .h29 ~~                                                                
##    .C2c               0.029       NA                      0.029    0.502
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               1.743       NA                      1.743    2.114
##    .h27               1.892       NA                      1.892    1.903
##    .h25               1.623       NA                      1.623    2.487
##    .h28               2.282       NA                      2.282    2.067
##    .y19               1.486       NA                      1.486    2.220
##    .h21               1.038       NA                      1.038    4.799
##    .h22               1.572       NA                      1.572    1.952
##    .h13               1.327       NA                      1.327    1.824
##    .h29               1.114       NA                      1.114    3.075
##    .h24               1.915       NA                      1.915    1.861
##    .y25               1.713       NA                      1.713    1.805
##    .C2b               1.701       NA                      1.701    2.125
##    .C2g               1.812       NA                      1.812    2.247
##    .C2h               1.530       NA                      1.530    1.930
##    .C2a               3.580       NA                      3.580    4.900
##    .C2c               1.054       NA                      1.054    3.560
##    .C2d               1.057       NA                      1.057    3.648
##    .C2e               3.130       NA                      3.130    3.205
##    .C2f               2.710       NA                      2.710    2.613
##    .C2i               1.608       NA                      1.608    2.818
##    .C2j               1.230       NA                      1.230    1.992
##     VAA_LR            0.000                               0.000    0.000
##     VAA_GT            0.000                               0.000    0.000
##     CS_LR             0.000                               0.000    0.000
##     CS_GT             0.000                               0.000    0.000
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               0.475       NA                      0.475    0.699
##    .h27               0.794       NA                      0.794    0.804
##    .h25               0.305       NA                      0.305    0.716
##    .h28               0.885       NA                      0.885    0.726
##    .y19               0.314       NA                      0.314    0.701
##    .h21               0.041       NA                      0.041    0.872
##    .h22               0.434       NA                      0.434    0.668
##    .h13               0.448       NA                      0.448    0.846
##    .h29               0.113       NA                      0.113    0.861
##    .h24               0.745       NA                      0.745    0.703
##    .y25               0.577       NA                      0.577    0.641
##    .C2b               0.390       NA                      0.390    0.608
##    .C2g               0.510       NA                      0.510    0.784
##    .C2h               0.196       NA                      0.196    0.311
##    .C2a               0.534       NA                      0.534    1.000
##    .C2c               0.029       NA                      0.029    0.335
##    .C2d               0.030       NA                      0.030    0.358
##    .C2e               0.954       NA                      0.954    1.000
##    .C2f               1.075       NA                      1.075    1.000
##    .C2i               0.278       NA                      0.278    0.853
##    .C2j               0.285       NA                      0.285    0.749
##     VAA_LR            0.205       NA                      1.000    1.000
##     VAA_GT            0.006       NA                      1.000    1.000
##     CS_LR             0.251       NA                      1.000    1.000
##     CS_GT             0.000       NA                      1.000    1.000
## 
## R-Square:
##                    Estimate
##     h26               0.301
##     h27               0.196
##     h25               0.284
##     h28               0.274
##     y19               0.299
##     h21               0.128
##     h22               0.332
##     h13               0.154
##     h29               0.139
##     h24               0.297
##     y25               0.359
##     C2b               0.392
##     C2g               0.216
##     C2h               0.689
##     C2a               0.000
##     C2c               0.665
##     C2d               0.642
##     C2e               0.000
##     C2f               0.000
##     C2i               0.147
##     C2j               0.251
## 
## Defined Parameters:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##     mean.r.VAA        0.073                               0.308    0.308
##     mean.r.CS         0.012                               0.218    0.218
##     mean.r.LR         0.307                               0.837    0.837
##     mean.r.GT         0.024                               0.715    0.715
##     mean.r.d1         0.045                               0.509    0.509
##     mean.r.d2         0.038                               0.267    0.267
##     test.H3           0.234                               0.327    0.327
##     test.H4          -0.049                               0.206    0.206
```

\newpage

From the above output it is difficult to see what is the problem.

Try to fit the model separately for each group

\newpage

#### Model for KD


```r
fit_H3H4.re.KD<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("KD"),
                    missing="fiml")
```

Model for KD converges


```r
round(inspect(fit_H3H4.re.KD,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  72.000 180.000 295.998   0.000   0.705   0.656   0.059   0.122
```

Fit is poor

Print standardized estimates to test the difference between correlations


```r
std.est_H3H4.re.KD<-standardizedsolution(fit_H3H4.re.KD)
std.est_H3H4.re.KD[std.est_H3H4.re.KD$op==":=" | 
               std.est_H3H4.re.KD$op=="~~" & 
               std.est_H3H4.re.KD$lhs!=std.est_H3H4.re.KD$rhs,]
```

```
##        lhs op                            rhs est.std    se     z pvalue
## 22  VAA_LR ~~                         VAA_GT   0.308 0.114 2.709  0.007
## 23   CS_LR ~~                          CS_GT   0.219 0.191 1.147  0.251
## 24  VAA_LR ~~                          CS_LR   0.836 0.094 8.897  0.000
## 25  VAA_GT ~~                          CS_GT   0.714 0.183 3.897  0.000
## 26  VAA_LR ~~                          CS_GT   0.511 0.191 2.675  0.007
## 27  VAA_GT ~~                          CS_LR   0.267 0.199 1.340  0.180
## 28     h27 ~~                            C2h   0.391 0.147 2.656  0.008
## 29     h21 ~~                            C2d   0.288 0.154 1.873  0.061
## 30     h29 ~~                            C2c   0.506 0.118 4.297  0.000
## 81 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.326 0.219 1.487  0.137
## 82 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.204 0.294 0.693  0.488
##    ci.lower ci.upper
## 22    0.085    0.530
## 23   -0.155    0.594
## 24    0.652    1.021
## 25    0.355    1.074
## 26    0.137    0.885
## 27   -0.123    0.657
## 28    0.103    0.680
## 29   -0.013    0.589
## 30    0.275    0.736
## 81   -0.103    0.755
## 82   -0.372    0.779
```


\newpage

#### Model for KESK


```r
fit_H3H4.re.KESK<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("KESK"),
                    missing="fiml")
```

```
## Warning in lav_model_estimate(lavmodel = lavmodel, lavpartable = lavpartable, : lavaan WARNING: the optimizer (NLMINB) claimed the model converged,
##                   but not all elements of the gradient are (near) zero;
##                   the optimizer may not have found a local solution
##                   use check.gradient = FALSE to skip this check.
```

Model for KESK does not converge



Fit is poor

Print standardized estimates to test the difference between correlations


```r
std.est_H3H4.re.KESK<-standardizedsolution(fit_H3H4.re.KESK)
```

```
## Warning in sqrt(ETA2): NaNs produced
```

```
## Warning in computeOmega(Sigma.hat = Sigma.hat, Mu.hat = Mu.hat, lavsamplestats = lavsamplestats, : lav_model_gradient: Sigma.hat is not positive definite
```

```
## Error in chol.default(S) : 
##   the leading minor of order 14 is not positive definite
```

```r
std.est_H3H4.re.KESK[std.est_H3H4.re.KESK$op==":=" | 
               std.est_H3H4.re.KESK$op=="~~" & 
               std.est_H3H4.re.KESK$lhs!=std.est_H3H4.re.KESK$rhs,]
```

```
##        lhs op                            rhs est.std se  z pvalue ci.lower
## 22  VAA_LR ~~                         VAA_GT  -0.119 NA NA     NA       NA
## 23   CS_LR ~~                          CS_GT  -0.007 NA NA     NA       NA
## 24  VAA_LR ~~                          CS_LR  -0.013 NA NA     NA       NA
## 25  VAA_GT ~~                          CS_GT   0.777 NA NA     NA       NA
## 26  VAA_LR ~~                          CS_GT  -0.126 NA NA     NA       NA
## 27  VAA_GT ~~                          CS_LR  -0.006 NA NA     NA       NA
## 28     h27 ~~                            C2h   0.008 NA NA     NA       NA
## 29     h21 ~~                            C2d   0.972 NA NA     NA       NA
## 30     h29 ~~                            C2c   0.194 NA NA     NA       NA
## 81 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)  -0.007 NA NA     NA       NA
## 82 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.783 NA NA     NA       NA
##    ci.upper
## 22       NA
## 23       NA
## 24       NA
## 25       NA
## 26       NA
## 27       NA
## 28       NA
## 29       NA
## 30       NA
## 81       NA
## 82       NA
```


\newpage

#### Model for KOK


```r
fit_H3H4.re.KOK<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("KOK"),
                    missing="fiml")
```

Model for KOK converges


```r
round(inspect(fit_H3H4.re.KOK,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  72.000 180.000 310.526   0.000   0.632   0.571   0.059   0.137
```

Fit is poor

Print standardized estimates to test the difference between correlations


```r
std.est_H3H4.re.KOK<-standardizedsolution(fit_H3H4.re.KOK)
std.est_H3H4.re.KOK[std.est_H3H4.re.KOK$op==":=" | 
               std.est_H3H4.re.KOK$op=="~~" & 
               std.est_H3H4.re.KOK$lhs!=std.est_H3H4.re.KOK$rhs,]
```

```
##        lhs op                            rhs est.std    se      z pvalue
## 22  VAA_LR ~~                         VAA_GT  -0.161 0.113 -1.428  0.153
## 23   CS_LR ~~                          CS_GT  -0.672 0.201 -3.347  0.001
## 24  VAA_LR ~~                          CS_LR   0.405 0.265  1.529  0.126
## 25  VAA_GT ~~                          CS_GT   0.140 0.256  0.545  0.586
## 26  VAA_LR ~~                          CS_GT   0.037 0.257  0.144  0.885
## 27  VAA_GT ~~                          CS_LR   0.325 0.251  1.297  0.195
## 28     h27 ~~                            C2h   0.381 0.104  3.651  0.000
## 29     h21 ~~                            C2d   0.218 0.162  1.341  0.180
## 30     h29 ~~                            C2c  -0.029 0.146 -0.199  0.842
## 81 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.079 0.388  0.205  0.838
## 82 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)  -0.186 0.416 -0.446  0.655
##    ci.lower ci.upper
## 22   -0.382    0.060
## 23   -1.065   -0.278
## 24   -0.114    0.923
## 25   -0.363    0.642
## 26   -0.467    0.541
## 27   -0.166    0.817
## 28    0.177    0.586
## 29   -0.100    0.536
## 30   -0.315    0.257
## 81   -0.681    0.840
## 82   -1.000    0.629
```

\newpage

#### Model for PS


```r
fit_H3H4.re.PS<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("PS"),
                    missing="fiml")
```

```
## Warning in lav_model_estimate(lavmodel = lavmodel, lavpartable = lavpartable, : lavaan WARNING: the optimizer (NLMINB) claimed the model converged,
##                   but not all elements of the gradient are (near) zero;
##                   the optimizer may not have found a local solution
##                   use check.gradient = FALSE to skip this check.
```

Model for PS does not converge



Fit is poor

Print standardized estimates to test the difference between correlations


```r
std.est_H3H4.re.PS<-standardizedsolution(fit_H3H4.re.PS)
```

```
## Warning in sqrt(ETA2): NaNs produced
```

```
## Warning in computeOmega(Sigma.hat = Sigma.hat, Mu.hat = Mu.hat, lavsamplestats = lavsamplestats, : lav_model_gradient: Sigma.hat is not positive definite
```

```
## Error in chol.default(S) : 
##   the leading minor of order 14 is not positive definite
```

```r
std.est_H3H4.re.PS[std.est_H3H4.re.PS$op==":=" | 
               std.est_H3H4.re.PS$op=="~~" & 
               std.est_H3H4.re.PS$lhs!=std.est_H3H4.re.PS$rhs,]
```

```
##        lhs op                            rhs est.std se  z pvalue ci.lower
## 22  VAA_LR ~~                         VAA_GT   0.110 NA NA     NA       NA
## 23   CS_LR ~~                          CS_GT   0.008 NA NA     NA       NA
## 24  VAA_LR ~~                          CS_LR  -0.037 NA NA     NA       NA
## 25  VAA_GT ~~                          CS_GT   1.052 NA NA     NA       NA
## 26  VAA_LR ~~                          CS_GT  -0.321 NA NA     NA       NA
## 27  VAA_GT ~~                          CS_LR   0.006 NA NA     NA       NA
## 28     h27 ~~                            C2h   0.007 NA NA     NA       NA
## 29     h21 ~~                            C2d   0.481 NA NA     NA       NA
## 30     h29 ~~                            C2c  -0.149 NA NA     NA       NA
## 81 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)  -0.147 NA NA     NA       NA
## 82 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.943 NA NA     NA       NA
##    ci.upper
## 22       NA
## 23       NA
## 24       NA
## 25       NA
## 26       NA
## 27       NA
## 28       NA
## 29       NA
## 30       NA
## 81       NA
## 82       NA
```

Heywood correlation between VAA_GT and CS_GT

\newpage

#### Model for RKP


```r
fit_H3H4.re.RKP<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("RKP"),
                    missing="fiml")
```

```
## Warning in lav_object_post_check(object): lavaan WARNING: covariance matrix of latent variables
##                 is not positive definite;
##                 use lavInspect(fit, "cov.lv") to investigate.
```

Model for RKP converges, but has other problems


```r
round(inspect(fit_H3H4.re.RKP,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  72.000 180.000 278.318   0.000   0.610   0.545   0.075   0.156
```

Fit is poor

Print standardized estimates to test the difference between correlations


```r
std.est_H3H4.re.RKP<-standardizedsolution(fit_H3H4.re.RKP)
std.est_H3H4.re.RKP[std.est_H3H4.re.RKP$op==":=" | 
               std.est_H3H4.re.RKP$op=="~~" & 
               std.est_H3H4.re.RKP$lhs!=std.est_H3H4.re.RKP$rhs,]
```

```
##        lhs op                            rhs est.std    se      z pvalue
## 22  VAA_LR ~~                         VAA_GT   0.261 0.156  1.671  0.095
## 23   CS_LR ~~                          CS_GT  -0.071 0.247 -0.286  0.775
## 24  VAA_LR ~~                          CS_LR   0.870 0.157  5.548  0.000
## 25  VAA_GT ~~                          CS_GT   0.994 0.149  6.650  0.000
## 26  VAA_LR ~~                          CS_GT   0.103 0.194  0.529  0.597
## 27  VAA_GT ~~                          CS_LR  -0.054 0.260 -0.209  0.835
## 28     h27 ~~                            C2h   0.402 0.264  1.524  0.128
## 29     h21 ~~                            C2d   0.564 0.209  2.694  0.007
## 30     h29 ~~                            C2c   0.329 0.158  2.078  0.038
## 81 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.608 0.232  2.626  0.009
## 82 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.733 0.226  3.235  0.001
##    ci.lower ci.upper
## 22   -0.045    0.568
## 23   -0.556    0.414
## 24    0.562    1.177
## 25    0.701    1.287
## 26   -0.278    0.484
## 27   -0.563    0.455
## 28   -0.115    0.918
## 29    0.154    0.975
## 30    0.019    0.638
## 81    0.154    1.062
## 82    0.289    1.176
```


\newpage

#### Model for SDP


```r
fit_H3H4.re.SDP<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("SDP"),
                    missing="fiml")
```

```
## Warning in lav_object_post_check(object): lavaan WARNING: covariance matrix of latent variables
##                 is not positive definite;
##                 use lavInspect(fit, "cov.lv") to investigate.
```

Model for SDP conveges, but has other problems


```r
round(inspect(fit_H3H4.re.SDP,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  72.000 180.000 382.844   0.000   0.633   0.572   0.073   0.127
```

Fit is poor

Print standardized estimates to test the difference between correlations


```r
std.est_H3H4.re.SDP<-standardizedsolution(fit_H3H4.re.SDP)
std.est_H3H4.re.SDP[std.est_H3H4.re.SDP$op==":=" | 
               std.est_H3H4.re.SDP$op=="~~" & 
               std.est_H3H4.re.SDP$lhs!=std.est_H3H4.re.SDP$rhs,]
```

```
##        lhs op                            rhs est.std    se      z pvalue
## 22  VAA_LR ~~                         VAA_GT   0.674 0.092  7.314  0.000
## 23   CS_LR ~~                          CS_GT  -0.743 0.167 -4.451  0.000
## 24  VAA_LR ~~                          CS_LR  -0.720 0.222 -3.249  0.001
## 25  VAA_GT ~~                          CS_GT   1.127 0.062 18.131  0.000
## 26  VAA_LR ~~                          CS_GT   0.518 0.133  3.896  0.000
## 27  VAA_GT ~~                          CS_LR  -0.309 0.194 -1.592  0.111
## 28     h27 ~~                            C2h   0.150 0.180  0.835  0.404
## 29     h21 ~~                            C2d   0.609 0.087  7.030  0.000
## 30     h29 ~~                            C2c   0.137 0.123  1.120  0.263
## 81 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)  -1.394 0.236 -5.915  0.000
## 82 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.453 0.110  4.114  0.000
##    ci.lower ci.upper
## 22    0.493    0.855
## 23   -1.071   -0.416
## 24   -1.155   -0.286
## 25    1.005    1.249
## 26    0.257    0.778
## 27   -0.690    0.072
## 28   -0.203    0.503
## 29    0.439    0.779
## 30   -0.103    0.378
## 81   -1.856   -0.932
## 82    0.237    0.669
```

Heywood correlation between VAA_GT and CS_GT

\newpage

#### Model for VAS


```r
fit_H3H4.re.VAS<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("VAS"),
                    missing="fiml")
```

```
## Warning in lav_model_estimate(lavmodel = lavmodel, lavpartable = lavpartable, :
## lavaan WARNING: the optimizer warns that a solution has NOT been found!
```

Model for VAS does not converge




Print standardized estimates to test the difference between correlations


```r
std.est_H3H4.re.VAS<-standardizedsolution(fit_H3H4.re.VAS)
```

```
## Warning in computeOmega(Sigma.hat = Sigma.hat, Mu.hat = Mu.hat, lavsamplestats = lavsamplestats, : lav_model_gradient: Sigma.hat is not positive definite
```

```
## Error in chol.default(S) : 
##   the leading minor of order 13 is not positive definite
```

```r
std.est_H3H4.re.VAS[std.est_H3H4.re.VAS$op==":=" | 
               std.est_H3H4.re.VAS$op=="~~" & 
               std.est_H3H4.re.VAS$lhs!=std.est_H3H4.re.VAS$rhs,]
```

```
##        lhs op                            rhs est.std se  z pvalue ci.lower
## 22  VAA_LR ~~                         VAA_GT   0.573 NA NA     NA       NA
## 23   CS_LR ~~                          CS_GT   0.413 NA NA     NA       NA
## 24  VAA_LR ~~                          CS_LR   0.376 NA NA     NA       NA
## 25  VAA_GT ~~                          CS_GT   1.027 NA NA     NA       NA
## 26  VAA_LR ~~                          CS_GT   0.595 NA NA     NA       NA
## 27  VAA_GT ~~                          CS_LR   0.435 NA NA     NA       NA
## 28     h27 ~~                            C2h   0.177 NA NA     NA       NA
## 29     h21 ~~                            C2d   0.531 NA NA     NA       NA
## 30     h29 ~~                            C2c   0.202 NA NA     NA       NA
## 81 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)  -0.219 NA NA     NA       NA
## 82 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.432 NA NA     NA       NA
##    ci.upper
## 22       NA
## 23       NA
## 24       NA
## 25       NA
## 26       NA
## 27       NA
## 28       NA
## 29       NA
## 30       NA
## 81       NA
## 82       NA
```


\newpage

#### Model for VIHR


```r
fit_H3H4.re.VIHR<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("VIHR"),
                    missing="fiml")
```

```
## Warning in lav_model_estimate(lavmodel = lavmodel, lavpartable = lavpartable, : lavaan WARNING: the optimizer (NLMINB) claimed the model converged,
##                   but not all elements of the gradient are (near) zero;
##                   the optimizer may not have found a local solution
##                   use check.gradient = FALSE to skip this check.
```

Model for VIHR does not converge



Fit is poor

Print standardized estimates to test the difference between correlations


```r
std.est_H3H4.re.VIHR<-standardizedsolution(fit_H3H4.re.VIHR)
```

```
## Warning in lav_model_hessian(lavmodel = lavmodel, lavsamplestats =
## lavsamplestats, : lavaan WARNING: Hessian is not fully symmetric. Max diff =
## 843.353944406845
```

```
## Warning in lav_model_vcov(lavmodel = lavmodel, lavsamplestats = object@SampleStats, : lavaan WARNING:
##     Could not compute standard errors! The information matrix could
##     not be inverted. This may be a symptom that the model is not
##     identified.
```

```
## Error in JAC %*% OUT : requires numeric/complex matrix/vector arguments
```

```r
std.est_H3H4.re.VIHR[std.est_H3H4.re.VIHR$op==":=" | 
               std.est_H3H4.re.VIHR$op=="~~" & 
               std.est_H3H4.re.VIHR$lhs!=std.est_H3H4.re.VIHR$rhs,]
```

```
##        lhs op                            rhs est.std se  z pvalue ci.lower
## 22  VAA_LR ~~                         VAA_GT   0.595 NA NA     NA       NA
## 23   CS_LR ~~                          CS_GT   0.381 NA NA     NA       NA
## 24  VAA_LR ~~                          CS_LR   0.912 NA NA     NA       NA
## 25  VAA_GT ~~                          CS_GT   0.094 NA NA     NA       NA
## 26  VAA_LR ~~                          CS_GT   0.186 NA NA     NA       NA
## 27  VAA_GT ~~                          CS_LR   0.116 NA NA     NA       NA
## 28     h27 ~~                            C2h   0.293 NA NA     NA       NA
## 29     h21 ~~                            C2d   0.042 NA NA     NA       NA
## 30     h29 ~~                            C2c   0.502 NA NA     NA       NA
## 81 test.H1 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.317 NA NA     NA       NA
## 82 test.H2 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)  -0.502 NA NA     NA       NA
##    ci.upper
## 22       NA
## 23       NA
## 24       NA
## 25       NA
## 26       NA
## 27       NA
## 28       NA
## 29       NA
## 30       NA
## 81       NA
## 82       NA
```

\newpage

### Summary of H3-H4 with MG-CFA approach

The configural model did not converge, even after respecification. Single group models also were non-converging or had other type of problems, except for KD and KOK, for which the fit of the model nevertheless was poor, and therefore not interpretable.

This most likely is an indication that the sample sizes of the parties are too small for this model with 21 indicators and 4 factors. 

The alternative way to test hypotheses 4-6 is presented below. It unconfounds the associations in the model by using party-mean centered observed variables for estimating the similar type of model that was used for H1 and H2, and H5, respectively. Because this approach does not have any grouping structure, it uses the overall sample size for the eight parties, which is 1559. It is nevertheless only conducted among the eight focal parties, and other parties are excluded. Because the misspecification in the model with centered variables might be entirely different to raw score variables, the modeling is again started with no residual correlations and they are examined if the fit of the model is inadequate.

\newpage

## H3 and H4 with group-mean centered variables and no grouping structure

Estimate how much of the variation in each item is between-groups


```r
#there was problems running the mult.icc function to the data structure so 
#data observed data was extracted from one of the previously fitted models
#to get rid of all labels etc.
num.dat.2019<-data.frame(fit_H1H2.exp.re@Data@X,dat2019$puolue)
names(num.dat.2019)<-c(fit_H1H2.exp.re@Data@ov$name,"puolue")
num.dat.2019<-num.dat.2019 %>%
  filter(puolue=="KD" |
           puolue=="KESK" |
           puolue=="KOK" |
           puolue=="PS" |
           puolue=="RKP" |
           puolue=="SDP" |
           puolue=="VAS" |
           puolue=="VIHR")


ICC<-data.frame(multilevel::mult.icc(x=num.dat.2019[,obs_items[2:length(obs_items)]],
                                     grpid=num.dat.2019$puolue))
ICC[,2:3]<-round(ICC[,2:3],3)
ICC
```

```
##    Variable  ICC1  ICC2
## 1       h26 0.484 0.995
## 2       h27 0.443 0.994
## 3       h25 0.483 0.995
## 4       h28 0.415 0.993
## 5       y19 0.354 0.991
## 6       h21 0.647 0.997
## 7       h22 0.490 0.995
## 8       h13 0.552 0.996
## 9       h29 0.327 0.990
## 10      h24 0.600 0.997
## 11      y25 0.345 0.990
## 12      C2b 0.127 0.966
## 13      C2g 0.251 0.985
## 14      C2h 0.444 0.994
## 15      C2a 0.295 0.988
## 16      C2c 0.405 0.993
## 17      C2d 0.501 0.995
## 18      C2e 0.103 0.957
## 19      C2f 0.213 0.981
## 20      C2i 0.515 0.995
## 21      C2j 0.419 0.993
```

```r
describe(ICC$ICC1,fast=T)
```

```
##    vars  n mean   sd min  max range   se
## X1    1 21  0.4 0.14 0.1 0.65  0.54 0.03
```

```r
ICC$label<-get_label(df2019[,as.character(ICC[,1])])

#export to .csv file
write.csv2(ICC,"ICC_2019.csv")
```

ICC1 gives the proportion (%) of variance that is between the parties. There is quite a lot of between-party variance, but the responses are not entire defined by party either.

Center all observed variables


```r
dat2019.gmc<-data.frame(dat2019.party)
na.mean<-function(var){
  mean(var,na.rm=T)
}

group.means<-dat2019.gmc %>%
  group_by(puolue) %>%
  summarise_at(all_items[2:length(all_items)],na.mean)


dat2019.gmc<-left_join(x=dat2019.gmc,
                       y=group.means,
                       by=c("puolue"),
                       suffix=c("",".pm"))



for(i in all_items[2:length(all_items)]){
  dat2019.gmc[i]<-dat2019.gmc[,i]-dat2019.gmc[,which(grepl(i,names(dat2019.gmc)) &
                           grepl("pm",names(dat2019.gmc)) & 
                     !grepl("r",names(dat2019.gmc))) ]
}

describe(dat2019.gmc[,all_items],fast=T)
```

```
## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning Inf
```

```
## Warning in FUN(newX[, i], ...): no non-missing arguments to max; returning -Inf
```

```
##        vars    n mean   sd   min  max range   se
## puolue    1 1559  NaN   NA   Inf -Inf  -Inf   NA
## h26       2 1425    0 0.97 -3.02 3.78  6.80 0.03
## h27       3 1424    0 1.03 -2.92 3.10  6.02 0.03
## h25       4 1425    0 0.81 -2.57 3.11  5.68 0.02
## h28       5 1425    0 1.08 -2.93 3.73  6.66 0.03
## y19       6 1528    0 0.82 -2.02 3.93  5.95 0.02
## h21       7 1425    0 0.94 -3.44 3.86  7.30 0.02
## h22       8 1425    0 1.08 -2.84 3.19  6.03 0.03
## h13       9 1425    0 0.88 -3.22 3.67  6.89 0.02
## h29      10 1425    0 0.99 -2.43 3.55  5.98 0.03
## h24      11 1425    0 0.97 -3.80 3.27  7.06 0.03
## y25      12 1504    0 1.16 -3.41 3.29  6.70 0.03
## C2b      13  475    0 0.94 -1.70 3.52  5.22 0.04
## C2g      14  476    0 0.94 -2.26 3.11  5.37 0.04
## C2h      15  476    0 0.92 -2.72 2.88  5.60 0.04
## C2a      16  477    0 0.78 -3.47 1.48  4.96 0.04
## C2c      17  477    0 0.91 -2.30 3.24  5.53 0.04
## C2d      18  475    0 0.96 -2.87 3.76  6.62 0.04
## C2e      19  477    0 1.04 -2.83 2.12  4.95 0.05
## C2f      20  477    0 1.01 -2.72 2.29  5.01 0.05
## C2i      21  477    0 0.89 -3.45 3.18  6.64 0.04
## C2j      22  477    0 0.95 -2.71 3.77  6.47 0.04
## C5a      23  473    0 1.51 -5.31 5.39 10.71 0.07
## C5c      24  470    0 1.07 -3.15 3.16  6.31 0.05
```


### Define the model


```r
model_H3H4<-"
#loadings
VAA_LR=~h26+h27+h25+h28+y19
VAA_GT=~h21+h22+h13+h29+h24+y25
CS_LR=~C2b+C2g+C2h
CS_GT=~C2a+C2c+C2d+C2e+C2f+C2i+C2j

#latent correlations

#cross-dimension same-method
VAA_LR~~r.VAA*VAA_GT
CS_LR~~r.CS*CS_GT

#concurrent validity
VAA_LR~~r.LR*CS_LR
VAA_GT~~r.GT*CS_GT

#cross-dimension cross-method correlations
VAA_LR~~r.d1*CS_GT
VAA_GT~~r.d2*CS_LR

#custom parameters
test.H3:=r.LR-max(r.VAA,r.CS,r.d1,r.d2)
test.H4:=r.GT-max(r.VAA,r.CS,r.d1,r.d2)

"
```



### Fit the model


```r
fit_H3H4<-cfa(model=model_H3H4,
              data=dat2019.gmc,
              missing="fiml")
```

```
## Warning in lav_object_post_check(object): lavaan WARNING: covariance matrix of latent variables
##                 is not positive definite;
##                 use lavInspect(fit, "cov.lv") to investigate.
```


```r
lavInspect(fit_H3H4, "cov.lv")
```

```
##        VAA_LR VAA_GT CS_LR CS_GT
## VAA_LR 0.290                    
## VAA_GT 0.039  0.167             
## CS_LR  0.116  0.011  0.068      
## CS_GT  0.034  0.157  0.016 0.127
```

```r
summary(fit_H3H4,fit=T,standardized=T,rsquare=T)
```

```
## lavaan 0.6-5 ended normally after 89 iterations
## 
##   Estimator                                         ML
##   Optimization method                           NLMINB
##   Number of free parameters                         69
##                                                       
##   Number of observations                          1559
##   Number of missing patterns                        17
##                                                       
## Model Test User Model:
##                                                       
##   Test statistic                               756.826
##   Degrees of freedom                               183
##   P-value (Chi-square)                           0.000
## 
## Model Test Baseline Model:
## 
##   Test statistic                              2537.225
##   Degrees of freedom                               210
##   P-value                                        0.000
## 
## User Model versus Baseline Model:
## 
##   Comparative Fit Index (CFI)                    0.753
##   Tucker-Lewis Index (TLI)                       0.717
## 
## Loglikelihood and Information Criteria:
## 
##   Loglikelihood user model (H0)             -27534.678
##   Loglikelihood unrestricted model (H1)             NA
##                                                       
##   Akaike (AIC)                               55207.356
##   Bayesian (BIC)                             55576.630
##   Sample-size adjusted Bayesian (BIC)        55357.432
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.045
##   90 Percent confidence interval - lower         0.042
##   90 Percent confidence interval - upper         0.048
##   P-value RMSEA <= 0.05                          0.995
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.069
## 
## Parameter Estimates:
## 
##   Information                                 Observed
##   Observed information based on                Hessian
##   Standard errors                             Standard
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR =~                                                             
##     h26               1.000                               0.539    0.556
##     h27               0.926    0.080   11.569    0.000    0.499    0.486
##     h25               0.835    0.071   11.697    0.000    0.450    0.553
##     h28               0.865    0.079   10.990    0.000    0.466    0.434
##     y19               0.628    0.063    9.964    0.000    0.339    0.413
##   VAA_GT =~                                                             
##     h21               1.000                               0.408    0.434
##     h22               1.319    0.138    9.534    0.000    0.539    0.499
##     h13               0.682    0.088    7.739    0.000    0.278    0.316
##     h29               0.950    0.108    8.822    0.000    0.388    0.390
##     h24               1.055    0.105   10.029    0.000    0.431    0.443
##     y25               1.450    0.148    9.777    0.000    0.592    0.509
##   CS_LR =~                                                              
##     C2b               1.000                               0.260    0.278
##     C2g               1.919    0.403    4.764    0.000    0.499    0.534
##     C2h               2.822    0.547    5.158    0.000    0.734    0.798
##   CS_GT =~                                                              
##     C2a               1.000                               0.357    0.458
##     C2c               1.002    0.176    5.687    0.000    0.357    0.393
##     C2d               1.062    0.188    5.639    0.000    0.378    0.392
##     C2e               0.449    0.168    2.674    0.007    0.160    0.154
##     C2f               1.000    0.174    5.753    0.000    0.357    0.352
##     C2i               1.374    0.193    7.107    0.000    0.490    0.547
##     C2j               0.549    0.161    3.406    0.001    0.196    0.205
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   VAA_LR ~~                                                             
##     VAA_GT  (r.VA)    0.039    0.010    3.788    0.000    0.179    0.179
##   CS_LR ~~                                                              
##     CS_GT   (r.CS)    0.016    0.008    2.084    0.037    0.172    0.172
##   VAA_LR ~~                                                             
##     CS_LR   (r.LR)    0.116    0.024    4.835    0.000    0.826    0.826
##   VAA_GT ~~                                                             
##     CS_GT   (r.GT)    0.157    0.022    7.219    0.000    1.079    1.079
##   VAA_LR ~~                                                             
##     CS_GT   (r.d1)    0.034    0.014    2.457    0.014    0.179    0.179
##   VAA_GT ~~                                                             
##     CS_LR   (r.d2)    0.011    0.008    1.361    0.173    0.099    0.099
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26              -0.001    0.026   -0.059    0.953   -0.001   -0.002
##    .h27              -0.002    0.027   -0.060    0.952   -0.002   -0.002
##    .h25              -0.001    0.021   -0.058    0.953   -0.001   -0.002
##    .h28              -0.001    0.028   -0.046    0.964   -0.001   -0.001
##    .y19              -0.002    0.021   -0.073    0.942   -0.002   -0.002
##    .h21              -0.000    0.025   -0.020    0.984   -0.000   -0.001
##    .h22              -0.001    0.028   -0.022    0.982   -0.001   -0.001
##    .h13              -0.000    0.023   -0.014    0.989   -0.000   -0.000
##    .h29              -0.000    0.026   -0.018    0.986   -0.000   -0.000
##    .h24              -0.001    0.026   -0.020    0.984   -0.001   -0.001
##    .y25              -0.001    0.030   -0.021    0.983   -0.001   -0.001
##    .C2b              -0.005    0.043   -0.116    0.908   -0.005   -0.005
##    .C2g              -0.010    0.041   -0.231    0.817   -0.010   -0.010
##    .C2h              -0.015    0.039   -0.378    0.706   -0.015   -0.016
##    .C2a               0.036    0.034    1.053    0.292    0.036    0.046
##    .C2c               0.036    0.040    0.893    0.372    0.036    0.040
##    .C2d               0.038    0.043    0.872    0.383    0.038    0.039
##    .C2e               0.016    0.048    0.338    0.735    0.016    0.016
##    .C2f               0.036    0.045    0.792    0.429    0.036    0.035
##    .C2i               0.049    0.038    1.287    0.198    0.049    0.055
##    .C2j               0.020    0.044    0.453    0.650    0.020    0.021
##     VAA_LR            0.000                               0.000    0.000
##     VAA_GT            0.000                               0.000    0.000
##     CS_LR             0.000                               0.000    0.000
##     CS_GT             0.000                               0.000    0.000
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .h26               0.650    0.033   19.975    0.000    0.650    0.691
##    .h27               0.806    0.037   21.684    0.000    0.806    0.764
##    .h25               0.459    0.024   18.968    0.000    0.459    0.694
##    .h28               0.939    0.041   23.083    0.000    0.939    0.812
##    .y19               0.557    0.024   23.245    0.000    0.557    0.829
##    .h21               0.717    0.032   22.480    0.000    0.717    0.811
##    .h22               0.874    0.041   21.363    0.000    0.874    0.751
##    .h13               0.699    0.028   24.964    0.000    0.699    0.900
##    .h29               0.839    0.035   23.964    0.000    0.839    0.848
##    .h24               0.759    0.033   22.744    0.000    0.759    0.804
##    .y25               1.003    0.047   21.363    0.000    1.003    0.741
##    .C2b               0.809    0.054   14.954    0.000    0.809    0.923
##    .C2g               0.625    0.049   12.708    0.000    0.625    0.715
##    .C2h               0.307    0.061    5.052    0.000    0.307    0.363
##    .C2a               0.479    0.034   13.899    0.000    0.479    0.790
##    .C2c               0.696    0.048   14.407    0.000    0.696    0.845
##    .C2d               0.790    0.056   14.162    0.000    0.790    0.847
##    .C2e               1.058    0.069   15.298    0.000    1.058    0.976
##    .C2f               0.901    0.062   14.584    0.000    0.901    0.876
##    .C2i               0.561    0.044   12.842    0.000    0.561    0.701
##    .C2j               0.872    0.057   15.170    0.000    0.872    0.958
##     VAA_LR            0.290    0.033    8.686    0.000    1.000    1.000
##     VAA_GT            0.167    0.026    6.398    0.000    1.000    1.000
##     CS_LR             0.068    0.025    2.666    0.008    1.000    1.000
##     CS_GT             0.127    0.029    4.436    0.000    1.000    1.000
## 
## R-Square:
##                    Estimate
##     h26               0.309
##     h27               0.236
##     h25               0.306
##     h28               0.188
##     y19               0.171
##     h21               0.189
##     h22               0.249
##     h13               0.100
##     h29               0.152
##     h24               0.196
##     y25               0.259
##     C2b               0.077
##     C2g               0.285
##     C2h               0.637
##     C2a               0.210
##     C2c               0.155
##     C2d               0.153
##     C2e               0.024
##     C2f               0.124
##     C2i               0.299
##     C2j               0.042
## 
## Defined Parameters:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##     test.H3           0.076    0.025    3.028    0.002    0.647    0.647
##     test.H4           0.118    0.023    5.123    0.000    0.900    0.900
```

There is a Heywood correlation between GAL-TAN latent variables (absolute value > 1)

\newpage

#### Respecify the model by introducing the three preregistered residual correlations


```r
model_H3H4.re<-paste0(model_H3H4,
                      "h27~~C2h\n",
                      "h21~~C2d\n",
                      "h29~~C2c\n")
```

#### Fit the respecified model


```r
fit_H3H4.re<-cfa(model=model_H3H4.re,
              data=dat2019.gmc,
              missing="fiml")
```

Inspect fit of the model


```r
round(inspect(fit_H3H4.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  72.000 180.000 602.219   0.000   0.819   0.788   0.039   0.062
```

The fit of the model is quite poor.

Hypotheses 1 and 2

Print standardized estimates to test the difference between correlations


```r
std.est_H3H4<-standardizedsolution(fit_H3H4.re)
std.est_H3H4[std.est_H3H4$op==":=" | 
               std.est_H3H4$op=="~~" & 
               std.est_H3H4$lhs!=std.est_H3H4$rhs,]
```

```
##        lhs op                            rhs est.std    se      z pvalue
## 22  VAA_LR ~~                         VAA_GT   0.175 0.045  3.911  0.000
## 23   CS_LR ~~                          CS_GT   0.165 0.074  2.215  0.027
## 24  VAA_LR ~~                          CS_LR   0.782 0.055 14.220  0.000
## 25  VAA_GT ~~                          CS_GT   0.956 0.047 20.140  0.000
## 26  VAA_LR ~~                          CS_GT   0.189 0.068  2.772  0.006
## 27  VAA_GT ~~                          CS_LR   0.112 0.069  1.631  0.103
## 28     h27 ~~                            C2h   0.295 0.061  4.837  0.000
## 29     h21 ~~                            C2d   0.550 0.036 15.264  0.000
## 30     h29 ~~                            C2c   0.219 0.048  4.559  0.000
## 81 test.H3 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.593 0.086  6.863  0.000
## 82 test.H4 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.767 0.083  9.274  0.000
##    ci.lower ci.upper
## 22    0.087    0.262
## 23    0.019    0.310
## 24    0.674    0.890
## 25    0.863    1.049
## 26    0.055    0.323
## 27   -0.023    0.247
## 28    0.175    0.414
## 29    0.480    0.621
## 30    0.125    0.313
## 81    0.424    0.763
## 82    0.605    0.929
```

H3: There is strong (.782, p < .001) correlation between VAA-LR and CS-LR, and it is notably stronger (difference in correlations .593, p < .001) than the strongest of correlations between different dimensions (.189 between VAA_LR and VAA_GT, p = .006)

H4: There is very strong (.956, p < .001) correlation between VAA-GT and CS-GT, and it is notably stronger (difference in correlations .767, p < .001) than the strongest of correlations between different dimensions (.189 between VAA_LR and VAA_GT, p = .006)


#### Exploratory for H3 and H4: Seek misspecification to improve the overall model fit

Factor loadings


```r
mis.load_H3H4<-miPowerFit(fit_H3H4.re,stdLoad=.40)
mis.load_H3H4<-mis.load_H3H4[mis.load_H3H4$op=="=~",]
#see summary of the decisions
table(mis.load_H3H4$decision.pow)
```

```
## 
## EPC:NM      I      M     NM 
##     27     11      5     20
```

```r
#there are 5 loadings that were detected as misspecifications

rounded.vars<-c("mi","epc","target.epc",
                "std.epc","se.epc")

num.round<-function(var){
  var<-as.numeric(var)
  var<-round(var,2)
  return(var)
}

mis.load_H3H4[,rounded.vars]<-sapply(mis.load_H3H4[,rounded.vars],num.round)

printed.vars<-c("lhs","op","rhs","mi","epc","target.epc",
                "std.epc","std.target.epc","significant.mi",
                "high.power","decision.pow","se.epc")

#print the output

mis.load_H3H4 %>%
  filter(mis.load_H3H4$decision.pow=="M" | 
                mis.load_H3H4$decision.pow=="EPC:M") %>%
  dplyr::select(all_of(printed.vars)) 
```

```
##      lhs op rhs   mi   epc target.epc std.epc std.target.epc significant.mi
## 1 VAA_GT =~ C2a 4.03  2.37       0.82    1.16            0.4           TRUE
## 2 VAA_GT =~ C2e 4.92 -3.23       1.09   -1.18            0.4           TRUE
## 3 VAA_GT =~ C2j 5.30 -3.07       1.00   -1.23            0.4           TRUE
## 4  CS_GT =~ h21 9.35 -3.69       0.99   -1.50            0.4           TRUE
## 5  CS_GT =~ h22 7.87  4.08       1.12    1.45            0.4           TRUE
##   high.power decision.pow se.epc
## 1      FALSE            M   1.18
## 2      FALSE            M   1.46
## 3      FALSE            M   1.33
## 4      FALSE            M   1.21
## 5      FALSE            M   1.45
```

All the proposed loadings would be cross-loadings across methods (from VAA to CS or vice versa), and therefore not applicable for the present approach. Also, the expected parameter changes are indicative that most of these respecification would be Heywood -cases (standardized loadings that would be larger than 1 in absolute magnitude).

Residual correlations


```r
mis.rescor_H3H4<-miPowerFit(fit_H3H4.re,cor=.20)
mis.rescor_H3H4<-mis.rescor_H3H4[mis.rescor_H3H4$op=="~~" & 
                                   mis.rescor_H3H4$lhs!=mis.rescor_H3H4$rhs,]
#see summary of the decisions
table(mis.rescor_H3H4$decision.pow)
```

```
## 
##  EPC:M EPC:NM      I     NM 
##      2     43      1    161
```

```r
#there are 2 residual correlation that are misspecifications

rounded.vars<-c("mi","epc","target.epc",
                "std.epc","se.epc")

num.round<-function(var){
  var<-as.numeric(var)
  var<-round(var,2)
  return(var)
}

mis.rescor_H3H4[,rounded.vars]<-sapply(mis.rescor_H3H4[,rounded.vars],num.round)

printed.vars<-c("lhs","op","rhs","mi","epc","target.epc",
                "std.epc","std.target.epc","significant.mi",
                "high.power","decision.pow","se.epc")

#print the output

mis.rescor_H3H4 %>%
  filter(mis.rescor_H3H4$decision.pow=="M" | 
                mis.rescor_H3H4$decision.pow=="EPC:M") %>%
  dplyr::select(all_of(printed.vars)) 
```

```
##   lhs op rhs    mi  epc target.epc std.epc std.target.epc significant.mi
## 1 h25 ~~ y19 91.85 0.17       0.13    0.26            0.2           TRUE
## 2 C2a ~~ C2f 26.23 0.17       0.16    0.22            0.2           TRUE
##   high.power decision.pow se.epc
## 1       TRUE        EPC:M   0.02
## 2       TRUE        EPC:M   0.03
```

There were two misspecified residual correlation.

One was between VAA-LR items (same misspecification as was found for H1 and H2)
H25. Public services should be outsourced more than they are now for private companies and y19. Public authorities should be the main provider of social and healthcare services (r.) 

The other misspecification was between C2a. Immigrants should adapt to Finnish habits and C2f. People who break the law should be punished more severely 

Respecify the model to allow these parameters to be freely estimated

##### Exploratory respecification


```r
model_H3H4.exp.re<-paste0(model_H3H4.re,
                      "h25~~y19\n",
                      "C2a~~C2f\n")
```




```r
fit_H3H4.exp.re<-cfa(model=model_H3H4.exp.re,
              data=dat2019.gmc,
              missing="fiml")
```

Inspect fit of the model


```r
round(inspect(fit_H3H4.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  72.000 180.000 602.219   0.000   0.819   0.788   0.039   0.062
```

```r
round(inspect(fit_H3H4.exp.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  74.000 178.000 488.872   0.000   0.866   0.842   0.033   0.059
```

The fit of the model is better 

Retest Hypotheses 4 and 5

Print standardized estimates to test the difference between correlations


```r
std.est_H3H4.exp<-standardizedsolution(fit_H3H4.exp.re)
std.est_H3H4.exp[std.est_H3H4.exp$op==":=" | 
               std.est_H3H4.exp$op=="~~" & 
               std.est_H3H4.exp$lhs!=std.est_H3H4.exp$rhs,]
```

```
##        lhs op                            rhs est.std    se      z pvalue
## 22  VAA_LR ~~                         VAA_GT   0.239 0.045  5.367  0.000
## 23   CS_LR ~~                          CS_GT   0.212 0.075  2.835  0.005
## 24  VAA_LR ~~                          CS_LR   0.831 0.055 15.003  0.000
## 25  VAA_GT ~~                          CS_GT   0.957 0.052 18.422  0.000
## 26  VAA_LR ~~                          CS_GT   0.220 0.072  3.074  0.002
## 27  VAA_GT ~~                          CS_LR   0.127 0.068  1.875  0.061
## 28     h27 ~~                            C2h   0.246 0.065  3.819  0.000
## 29     h21 ~~                            C2d   0.546 0.037 14.956  0.000
## 30     h29 ~~                            C2c   0.215 0.049  4.434  0.000
## 31     h25 ~~                            y19   0.290 0.028 10.525  0.000
## 32     C2a ~~                            C2f   0.253 0.047  5.439  0.000
## 83 test.H3 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.592 0.071  8.324  0.000
## 84 test.H4 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.718 0.067 10.651  0.000
##    ci.lower ci.upper
## 22    0.152    0.327
## 23    0.065    0.359
## 24    0.723    0.940
## 25    0.856    1.059
## 26    0.080    0.360
## 27   -0.006    0.260
## 28    0.120    0.373
## 29    0.475    0.618
## 30    0.120    0.311
## 31    0.236    0.344
## 32    0.162    0.345
## 83    0.452    0.731
## 84    0.586    0.850
```

The results are virtually identical to those without the additional residual correlations.

Put a more strict criterion on the residual correlation misspecification (.15)


Residual correlations


```r
mis.rescor_H3H4<-miPowerFit(fit_H3H4.exp.re,cor=.15)
mis.rescor_H3H4<-mis.rescor_H3H4[mis.rescor_H3H4$op=="~~" & 
                                   mis.rescor_H3H4$lhs!=mis.rescor_H3H4$rhs,]
#see summary of the decisions
table(mis.rescor_H3H4$decision.pow)
```

```
## 
##  EPC:M EPC:NM      I     NM 
##      2     34      1    168
```

```r
#there are two additional residual correlations that are misspecifications

rounded.vars<-c("mi","epc","target.epc",
                "std.epc","se.epc")

num.round<-function(var){
  var<-as.numeric(var)
  var<-round(var,2)
  return(var)
}

mis.rescor_H3H4[,rounded.vars]<-sapply(mis.rescor_H3H4[,rounded.vars],num.round)

printed.vars<-c("lhs","op","rhs","mi","epc","target.epc",
                "std.epc","std.target.epc","significant.mi",
                "high.power","decision.pow","se.epc")

#print the output

mis.rescor_H3H4 %>%
  filter(mis.rescor_H3H4$decision.pow=="M" | 
                mis.rescor_H3H4$decision.pow=="EPC:M") %>%
  dplyr::select(all_of(printed.vars)) 
```

```
##   lhs op rhs    mi  epc target.epc std.epc std.target.epc significant.mi
## 1 h22 ~~ C2i 16.44 0.16       0.14    0.17           0.15           TRUE
## 2 C2d ~~ C2j 24.41 0.17       0.14    0.18           0.15           TRUE
##   high.power decision.pow se.epc
## 1       TRUE        EPC:M   0.04
## 2       TRUE        EPC:M   0.03
```

There were two more misspecified residual correlations.

Between VAA-GAL-TAN h22. If the government proposes to establish a refugee center in my home municipality, the proposal should be accepted (r.) and CS-GAL-TAN C2i. Immigrants are good for the Finnish economy (r.)

And between two CS-GAL-TAN items: C2d. Same Sex Marriages should be prohibited by law and C2j. Deciding on abortion issues should be a women's right (r.) 


```r
model_H3H4.exp.re.2<-paste0(model_H3H4.exp.re,
                      "h22~~C2i\n",
                      "C2d~~C2j\n")
```




```r
fit_H3H4.exp.re.2<-cfa(model=model_H3H4.exp.re.2,
              data=dat2019.gmc,
              missing="fiml")
```

Inspect fit of the model


```r
round(inspect(fit_H3H4.exp.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  74.000 178.000 488.872   0.000   0.866   0.842   0.033   0.059
```

```r
round(inspect(fit_H3H4.exp.re.2,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  76.000 176.000 446.837   0.000   0.884   0.861   0.031   0.056
```

The fit of the model is again improved

Retest Hypotheses 4 and 5

Print standardized estimates to test the difference between correlations


```r
std.est_H3H4.exp<-standardizedsolution(fit_H3H4.exp.re.2)
std.est_H3H4.exp[std.est_H3H4.exp$op==":=" | 
               std.est_H3H4.exp$op=="~~" & 
               std.est_H3H4.exp$lhs!=std.est_H3H4.exp$rhs,]
```

```
##        lhs op                            rhs est.std    se      z pvalue
## 22  VAA_LR ~~                         VAA_GT   0.236 0.045  5.274  0.000
## 23   CS_LR ~~                          CS_GT   0.183 0.079  2.335  0.020
## 24  VAA_LR ~~                          CS_LR   0.830 0.056 14.939  0.000
## 25  VAA_GT ~~                          CS_GT   0.945 0.053 17.802  0.000
## 26  VAA_LR ~~                          CS_GT   0.197 0.074  2.659  0.008
## 27  VAA_GT ~~                          CS_LR   0.121 0.068  1.780  0.075
## 28     h27 ~~                            C2h   0.246 0.065  3.797  0.000
## 29     h21 ~~                            C2d   0.515 0.039 13.210  0.000
## 30     h29 ~~                            C2c   0.214 0.049  4.376  0.000
## 31     h25 ~~                            y19   0.290 0.028 10.522  0.000
## 32     C2a ~~                            C2f   0.243 0.049  5.013  0.000
## 33     h22 ~~                            C2i   0.233 0.054  4.335  0.000
## 34     C2d ~~                            C2j   0.205 0.041  5.013  0.000
## 85 test.H3 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.594 0.071  8.332  0.000
## 86 test.H4 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.709 0.069 10.330  0.000
##    ci.lower ci.upper
## 22    0.148    0.324
## 23    0.029    0.337
## 24    0.721    0.939
## 25    0.841    1.049
## 26    0.052    0.342
## 27   -0.012    0.255
## 28    0.119    0.373
## 29    0.439    0.592
## 30    0.118    0.309
## 31    0.236    0.344
## 32    0.148    0.338
## 33    0.128    0.338
## 34    0.125    0.285
## 85    0.455    0.734
## 86    0.574    0.843
```


Print all the final model parameters


```r
standardizedsolution(fit_H3H4.exp.re.2)
```

```
##        lhs op                            rhs est.std    se      z pvalue
## 1   VAA_LR =~                            h26   0.584 0.030 19.534  0.000
## 2   VAA_LR =~                            h27   0.494 0.031 16.025  0.000
## 3   VAA_LR =~                            h25   0.467 0.030 15.364  0.000
## 4   VAA_LR =~                            h28   0.465 0.030 15.387  0.000
## 5   VAA_LR =~                            y19   0.300 0.034  8.917  0.000
## 6   VAA_GT =~                            h21   0.409 0.032 12.933  0.000
## 7   VAA_GT =~                            h22   0.494 0.030 16.273  0.000
## 8   VAA_GT =~                            h13   0.321 0.032 10.045  0.000
## 9   VAA_GT =~                            h29   0.387 0.031 12.419  0.000
## 10  VAA_GT =~                            h24   0.452 0.031 14.615  0.000
## 11  VAA_GT =~                            y25   0.533 0.029 18.090  0.000
## 12   CS_LR =~                            C2b   0.275 0.051  5.391  0.000
## 13   CS_LR =~                            C2g   0.561 0.045 12.390  0.000
## 14   CS_LR =~                            C2h   0.773 0.045 17.254  0.000
## 15   CS_GT =~                            C2a   0.461 0.052  8.919  0.000
## 16   CS_GT =~                            C2c   0.401 0.052  7.644  0.000
## 17   CS_GT =~                            C2d   0.346 0.050  6.917  0.000
## 18   CS_GT =~                            C2e   0.171 0.058  2.952  0.003
## 19   CS_GT =~                            C2f   0.329 0.056  5.845  0.000
## 20   CS_GT =~                            C2i   0.548 0.049 11.149  0.000
## 21   CS_GT =~                            C2j   0.177 0.058  3.047  0.002
## 22  VAA_LR ~~                         VAA_GT   0.236 0.045  5.274  0.000
## 23   CS_LR ~~                          CS_GT   0.183 0.079  2.335  0.020
## 24  VAA_LR ~~                          CS_LR   0.830 0.056 14.939  0.000
## 25  VAA_GT ~~                          CS_GT   0.945 0.053 17.802  0.000
## 26  VAA_LR ~~                          CS_GT   0.197 0.074  2.659  0.008
## 27  VAA_GT ~~                          CS_LR   0.121 0.068  1.780  0.075
## 28     h27 ~~                            C2h   0.246 0.065  3.797  0.000
## 29     h21 ~~                            C2d   0.515 0.039 13.210  0.000
## 30     h29 ~~                            C2c   0.214 0.049  4.376  0.000
## 31     h25 ~~                            y19   0.290 0.028 10.522  0.000
## 32     C2a ~~                            C2f   0.243 0.049  5.013  0.000
## 33     h22 ~~                            C2i   0.233 0.054  4.335  0.000
## 34     C2d ~~                            C2j   0.205 0.041  5.013  0.000
## 35     h26 ~~                            h26   0.659 0.035 18.840  0.000
## 36     h27 ~~                            h27   0.756 0.030 24.875  0.000
## 37     h25 ~~                            h25   0.782 0.028 27.503  0.000
## 38     h28 ~~                            h28   0.783 0.028 27.822  0.000
## 39     y19 ~~                            y19   0.910 0.020 45.135  0.000
## 40     h21 ~~                            h21   0.833 0.026 32.232  0.000
## 41     h22 ~~                            h22   0.756 0.030 25.218  0.000
## 42     h13 ~~                            h13   0.897 0.021 43.658  0.000
## 43     h29 ~~                            h29   0.850 0.024 35.325  0.000
## 44     h24 ~~                            h24   0.795 0.028 28.405  0.000
## 45     y25 ~~                            y25   0.715 0.031 22.734  0.000
## 46     C2b ~~                            C2b   0.924 0.028 32.888  0.000
## 47     C2g ~~                            C2g   0.686 0.051 13.503  0.000
## 48     C2h ~~                            C2h   0.403 0.069  5.812  0.000
## 49     C2a ~~                            C2a   0.787 0.048 16.520  0.000
## 50     C2c ~~                            C2c   0.839 0.042 19.931  0.000
## 51     C2d ~~                            C2d   0.881 0.035 25.484  0.000
## 52     C2e ~~                            C2e   0.971 0.020 48.730  0.000
## 53     C2f ~~                            C2f   0.892 0.037 24.067  0.000
## 54     C2i ~~                            C2i   0.700 0.054 12.989  0.000
## 55     C2j ~~                            C2j   0.969 0.021 47.025  0.000
## 56  VAA_LR ~~                         VAA_LR   1.000 0.000     NA     NA
## 57  VAA_GT ~~                         VAA_GT   1.000 0.000     NA     NA
## 58   CS_LR ~~                          CS_LR   1.000 0.000     NA     NA
## 59   CS_GT ~~                          CS_GT   1.000 0.000     NA     NA
## 60     h26 ~1                                 -0.002 0.026 -0.068  0.946
## 61     h27 ~1                                 -0.002 0.026 -0.089  0.929
## 62     h25 ~1                                 -0.002 0.026 -0.061  0.951
## 63     h28 ~1                                 -0.001 0.026 -0.054  0.957
## 64     y19 ~1                                 -0.002 0.026 -0.076  0.939
## 65     h21 ~1                                  0.005 0.026  0.173  0.863
## 66     h22 ~1                                  0.000 0.026 -0.007  0.995
## 67     h13 ~1                                 -0.001 0.026 -0.021  0.983
## 68     h29 ~1                                 -0.002 0.026 -0.067  0.947
## 69     h24 ~1                                 -0.001 0.026 -0.030  0.976
## 70     y25 ~1                                 -0.001 0.026 -0.026  0.979
## 71     C2b ~1                                 -0.002 0.045 -0.051  0.960
## 72     C2g ~1                                 -0.005 0.044 -0.111  0.911
## 73     C2h ~1                                 -0.006 0.041 -0.134  0.894
## 74     C2a ~1                                  0.040 0.044  0.905  0.365
## 75     C2c ~1                                  0.037 0.044  0.853  0.394
## 76     C2d ~1                                  0.022 0.041  0.547  0.584
## 77     C2e ~1                                  0.015 0.046  0.326  0.745
## 78     C2f ~1                                  0.029 0.045  0.634  0.526
## 79     C2i ~1                                  0.074 0.042  1.735  0.083
## 80     C2j ~1                                  0.015 0.046  0.337  0.736
## 81  VAA_LR ~1                                  0.000 0.000     NA     NA
## 82  VAA_GT ~1                                  0.000 0.000     NA     NA
## 83   CS_LR ~1                                  0.000 0.000     NA     NA
## 84   CS_GT ~1                                  0.000 0.000     NA     NA
## 85 test.H3 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.594 0.071  8.332  0.000
## 86 test.H4 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.709 0.069 10.330  0.000
##    ci.lower ci.upper
## 1     0.526    0.643
## 2     0.433    0.554
## 3     0.408    0.527
## 4     0.406    0.525
## 5     0.234    0.366
## 6     0.347    0.471
## 7     0.434    0.553
## 8     0.259    0.384
## 9     0.326    0.448
## 10    0.392    0.513
## 11    0.476    0.591
## 12    0.175    0.375
## 13    0.472    0.650
## 14    0.685    0.861
## 15    0.360    0.562
## 16    0.298    0.504
## 17    0.248    0.444
## 18    0.058    0.285
## 19    0.219    0.439
## 20    0.452    0.644
## 21    0.063    0.291
## 22    0.148    0.324
## 23    0.029    0.337
## 24    0.721    0.939
## 25    0.841    1.049
## 26    0.052    0.342
## 27   -0.012    0.255
## 28    0.119    0.373
## 29    0.439    0.592
## 30    0.118    0.309
## 31    0.236    0.344
## 32    0.148    0.338
## 33    0.128    0.338
## 34    0.125    0.285
## 35    0.590    0.727
## 36    0.697    0.816
## 37    0.726    0.837
## 38    0.728    0.839
## 39    0.871    0.950
## 40    0.782    0.884
## 41    0.697    0.815
## 42    0.857    0.937
## 43    0.803    0.898
## 44    0.740    0.850
## 45    0.654    0.777
## 46    0.869    0.979
## 47    0.586    0.785
## 48    0.267    0.538
## 49    0.694    0.881
## 50    0.757    0.922
## 51    0.813    0.948
## 52    0.932    1.010
## 53    0.819    0.964
## 54    0.594    0.805
## 55    0.928    1.009
## 56    1.000    1.000
## 57    1.000    1.000
## 58    1.000    1.000
## 59    1.000    1.000
## 60   -0.054    0.050
## 61   -0.054    0.049
## 62   -0.053    0.050
## 63   -0.053    0.050
## 64   -0.052    0.048
## 65   -0.047    0.056
## 66   -0.052    0.051
## 67   -0.052    0.051
## 68   -0.054    0.050
## 69   -0.053    0.051
## 70   -0.051    0.050
## 71   -0.091    0.087
## 72   -0.091    0.081
## 73   -0.086    0.075
## 74   -0.047    0.127
## 75   -0.048    0.123
## 76   -0.058    0.102
## 77   -0.075    0.105
## 78   -0.060    0.117
## 79   -0.010    0.157
## 80   -0.074    0.105
## 81    0.000    0.000
## 82    0.000    0.000
## 83    0.000    0.000
## 84    0.000    0.000
## 85    0.455    0.734
## 86    0.574    0.843
```

\newpage

## H6 with group mean centered observed variables

H6. Within-party placement on Left-Right as computed from responses to the pre-election public Voting Advice Applications (VAAs) is positively associated with within-party placement on Left-Right as computed from responses to the privately administered post-election Candidate Survey (CS). This association is stronger than any within-party associations between the Left-Right and GAL-TAN dimensions. 

### Add placement variables and their correlations with latent factors to the model used for H3 and H4


```r
model_H6<-paste0(model_H3H4.exp.re.2,
                 "SP_LR=~C5a\n",
                 "IP_LR=~C5c\n",
                 "VAA_LR~~r.self.LR*SP_LR\n",
                 "VAA_LR~~r.ideal.LR*IP_LR\n",
                 "test.H6:=r.self.LR-r.ideal.LR\n")
```

### Fit the model


```r
fit_H6<-cfa(model=model_H6,
            data=dat2019.gmc,
            missing="fiml")
```


Inspect fit of the model


```r
round(inspect(fit_H3H4.exp.re.2,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  76.000 176.000 446.837   0.000   0.884   0.861   0.031   0.056
```

```r
round(inspect(fit_H6,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  89.000 210.000 521.639   0.000   0.878   0.853   0.031   0.056
```

The fit of the model is similar.

Hypothesis 6

Print standardized estimates to test the difference between correlations


```r
std.est_H6<-standardizedsolution(fit_H6)
std.est_H6[std.est_H6$op==":=" | 
               std.est_H6$op=="~~" & 
               std.est_H6$lhs!=std.est_H6$rhs,]
```

```
##         lhs op                            rhs est.std    se      z pvalue
## 22   VAA_LR ~~                         VAA_GT   0.235 0.045  5.251  0.000
## 23    CS_LR ~~                          CS_GT   0.189 0.080  2.371  0.018
## 24   VAA_LR ~~                          CS_LR   0.842 0.054 15.723  0.000
## 25   VAA_GT ~~                          CS_GT   0.946 0.053 17.892  0.000
## 26   VAA_LR ~~                          CS_GT   0.194 0.074  2.605  0.009
## 27   VAA_GT ~~                          CS_LR   0.126 0.069  1.828  0.068
## 28      h27 ~~                            C2h   0.249 0.060  4.129  0.000
## 29      h21 ~~                            C2d   0.514 0.039 13.143  0.000
## 30      h29 ~~                            C2c   0.215 0.049  4.398  0.000
## 31      h25 ~~                            y19   0.290 0.028 10.527  0.000
## 32      C2a ~~                            C2f   0.239 0.049  4.874  0.000
## 33      h22 ~~                            C2i   0.245 0.053  4.641  0.000
## 34      C2d ~~                            C2j   0.205 0.041  5.014  0.000
## 37   VAA_LR ~~                          SP_LR   0.495 0.051  9.630  0.000
## 38   VAA_LR ~~                          IP_LR   0.072 0.062  1.165  0.244
## 68   VAA_GT ~~                          SP_LR   0.229 0.057  3.999  0.000
## 69   VAA_GT ~~                          IP_LR   0.089 0.063  1.418  0.156
## 70    CS_LR ~~                          SP_LR   0.446 0.050  8.866  0.000
## 71    CS_LR ~~                          IP_LR   0.124 0.056  2.205  0.027
## 72    CS_GT ~~                          SP_LR   0.206 0.061  3.359  0.001
## 73    CS_GT ~~                          IP_LR   0.108 0.064  1.686  0.092
## 74    SP_LR ~~                          IP_LR   0.423 0.038 11.199  0.000
## 104 test.H3 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.607 0.070  8.691  0.000
## 105 test.H4 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.711 0.069 10.370  0.000
## 106 test.H6 :=           r.self.LR-r.ideal.LR   0.423 0.062  6.850  0.000
##     ci.lower ci.upper
## 22     0.147    0.323
## 23     0.033    0.345
## 24     0.737    0.947
## 25     0.842    1.049
## 26     0.048    0.339
## 27    -0.009    0.261
## 28     0.131    0.368
## 29     0.437    0.590
## 30     0.119    0.310
## 31     0.236    0.344
## 32     0.143    0.335
## 33     0.141    0.348
## 34     0.125    0.285
## 37     0.394    0.595
## 38    -0.049    0.193
## 68     0.117    0.341
## 69    -0.034    0.211
## 70     0.348    0.545
## 71     0.014    0.235
## 72     0.086    0.327
## 73    -0.018    0.235
## 74     0.349    0.497
## 104    0.470    0.744
## 105    0.576    0.845
## 106    0.302    0.544
```

The correlation between VAA_LR and CS Self-placement on LR is strong (.495, p < .001) and larger than the association between VAA_LR and placement of imagined party voter (.072, p = .244; difference .42, p < .001)

Look for misspecifications


Residual correlations


```r
mis.rescor_H6<-miPowerFit(fit_H6,cor=.20)
mis.rescor_H6<-mis.rescor_H6[mis.rescor_H6$op=="~~" & 
                                   mis.rescor_H6$lhs!=mis.rescor_H6$rhs,]
#see summary of the decisions
table(mis.rescor_H6$decision.pow)
```

```
## 
## EPC:NM      I     NM 
##     37      1    207
```

```r
#there are no misspecification with delta set at .20

#look with .15

mis.rescor_H6<-miPowerFit(fit_H6,cor=.15)
mis.rescor_H6<-mis.rescor_H6[mis.rescor_H6$op=="~~" & 
                                   mis.rescor_H6$lhs!=mis.rescor_H6$rhs,]
#see summary of the decisions
table(mis.rescor_H6$decision.pow)
```

```
## 
##  EPC:M EPC:NM      I     NM 
##      1     36      1    207
```

```r
#there are is a single misspecification with .15 as criterion

rounded.vars<-c("mi","epc","target.epc",
                "std.epc","se.epc")

num.round<-function(var){
  var<-as.numeric(var)
  var<-round(var,2)
  return(var)
}

mis.rescor_H6[,rounded.vars]<-sapply(mis.rescor_H6[,rounded.vars],num.round)

printed.vars<-c("lhs","op","rhs","mi","epc","target.epc",
                "std.epc","std.target.epc","significant.mi",
                "high.power","decision.pow","se.epc")

#print the output

mis.rescor_H6 %>%
  filter(mis.rescor_H6$decision.pow=="M" | 
                mis.rescor_H6$decision.pow=="EPC:M") %>%
  dplyr::select(all_of(printed.vars)) 
```

```
##   lhs op rhs    mi  epc target.epc std.epc std.target.epc significant.mi
## 1 h21 ~~ C2j 19.02 0.18       0.14     0.2           0.15           TRUE
##   high.power decision.pow se.epc
## 1       TRUE        EPC:M   0.04
```

The misspecification is between VAA-GAL-TAN: h21. Gay and lesbian couples should have the same marriage and adoption rights as straight couples (r.)  and C2j. Deciding on abortion issues should be a women's right (r.)

Add it to the model



```r
model_H6.re<-paste0(model_H6,
                      "h21~~C2j\n")
```




```r
fit_H6.re<-cfa(model=model_H6.re,
              data=dat2019.gmc,
              missing="fiml")
```

Inspect fit of the model


```r
round(inspect(fit_H6,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  89.000 210.000 521.639   0.000   0.878   0.853   0.031   0.056
```

```r
round(inspect(fit_H6.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)
```

```
##    npar      df   chisq  pvalue     cfi     tli   rmsea    srmr 
##  90.000 209.000 500.409   0.000   0.886   0.862   0.030   0.054
```

No big differences in fit


Print standardized estimates to test the difference between correlations


```r
std.est_H6.re<-standardizedsolution(fit_H6.re)
std.est_H6.re[std.est_H6.re$op==":=" | 
               std.est_H6.re$op=="~~" & 
               std.est_H6.re$lhs!=std.est_H6.re$rhs,]
```

```
##         lhs op                            rhs est.std    se      z pvalue
## 22   VAA_LR ~~                         VAA_GT   0.233 0.045  5.212  0.000
## 23    CS_LR ~~                          CS_GT   0.183 0.080  2.293  0.022
## 24   VAA_LR ~~                          CS_LR   0.841 0.054 15.703  0.000
## 25   VAA_GT ~~                          CS_GT   0.935 0.052 17.820  0.000
## 26   VAA_LR ~~                          CS_GT   0.194 0.074  2.617  0.009
## 27   VAA_GT ~~                          CS_LR   0.115 0.069  1.671  0.095
## 28      h27 ~~                            C2h   0.249 0.060  4.128  0.000
## 29      h21 ~~                            C2d   0.552 0.036 15.261  0.000
## 30      h29 ~~                            C2c   0.216 0.049  4.442  0.000
## 31      h25 ~~                            y19   0.290 0.028 10.527  0.000
## 32      C2a ~~                            C2f   0.233 0.050  4.696  0.000
## 33      h22 ~~                            C2i   0.243 0.053  4.572  0.000
## 34      C2d ~~                            C2j   0.314 0.043  7.272  0.000
## 37   VAA_LR ~~                          SP_LR   0.495 0.051  9.643  0.000
## 38   VAA_LR ~~                          IP_LR   0.072 0.062  1.168  0.243
## 39      h21 ~~                            C2j   0.236 0.049  4.858  0.000
## 69   VAA_GT ~~                          SP_LR   0.228 0.057  3.990  0.000
## 70   VAA_GT ~~                          IP_LR   0.089 0.062  1.427  0.154
## 71    CS_LR ~~                          SP_LR   0.446 0.050  8.861  0.000
## 72    CS_LR ~~                          IP_LR   0.124 0.056  2.208  0.027
## 73    CS_GT ~~                          SP_LR   0.207 0.061  3.368  0.001
## 74    CS_GT ~~                          IP_LR   0.107 0.064  1.664  0.096
## 75    SP_LR ~~                          IP_LR   0.423 0.038 11.198  0.000
## 105 test.H3 := r.LR-max(r.VAA,r.CS,r.d1,r.d2)   0.608 0.070  8.695  0.000
## 106 test.H4 := r.GT-max(r.VAA,r.CS,r.d1,r.d2)   0.701 0.068 10.297  0.000
## 107 test.H6 :=           r.self.LR-r.ideal.LR   0.423 0.062  6.855  0.000
##     ci.lower ci.upper
## 22     0.146    0.321
## 23     0.027    0.339
## 24     0.736    0.946
## 25     0.832    1.037
## 26     0.049    0.340
## 27    -0.020    0.250
## 28     0.131    0.368
## 29     0.481    0.622
## 30     0.121    0.312
## 31     0.236    0.344
## 32     0.136    0.331
## 33     0.139    0.347
## 34     0.230    0.399
## 37     0.394    0.596
## 38    -0.049    0.193
## 39     0.141    0.331
## 69     0.116    0.340
## 70    -0.033    0.212
## 71     0.347    0.545
## 72     0.014    0.235
## 73     0.086    0.327
## 74    -0.019    0.233
## 75     0.349    0.497
## 105    0.471    0.745
## 106    0.568    0.835
## 107    0.302    0.544
```

Results are virtually identical