#' ---
#' title: "Analysis 2019"
#' output: 
#'   pdf_document: 
#'     keep_tex: yes
#'     keep_md: yes
#'     toc: yes
#'     toc_depth: 5
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' \newpage
#' 
#' # Preparations
#' 
#' Load packages
#' 
## ----message=FALSE, warning=FALSE----------------------------------------------------------------------------------
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

#' 
#' Read data file
#' 
## ------------------------------------------------------------------------------------------------------------------
df2019 <- readRDS("data/final/candsurvey_vaa_2019.rds")

#' 
#' Select variables used in the analysis
#' 
## ------------------------------------------------------------------------------------------------------------------
VAA_LR_items<-c("h26","h27","h25","h28","y19")
VAA_LR_items %in% names(df2019)

VAA_GT_items<-c("h21","h22","h13","h29","h24","y25")
VAA_GT_items %in% names(df2019)

CS_LR_items<-c("C2b","C2g","C2h")
CS_LR_items %in% names(df2019)

CS_GT_items<-c("C2a","C2c","C2d","C2e","C2f","C2i","C2j")
CS_GT_items %in% names(df2019)

#LR Self-placement
CS_LR_SP<-c("C5a")
CS_LR_SP %in% names(df2019)

#LR imagined voter placement
CS_LR_IP<-c("C5c")
CS_LR_IP %in% names(df2019)

Party_item<-c("puolue")
Party_item %in% names(df2019)

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


#' 
#' Print the responses to the observed items
#' 
## ------------------------------------------------------------------------------------------------------------------
for (i in 1:length(obs_items)){
  print(obs_items[i])
  print(table(df2019[,obs_items[i]],useNA="always"))
  }

#' 
#' Data looks as it should.
#' 
#' Exclude completely missing cases
#' 
## ------------------------------------------------------------------------------------------------------------------

df2019$completely_missing<-
  rowSums(is.na(df2019[,obs_items[2:length(obs_items)]]))==length(obs_items)-1

table(df2019$completely_missing)

dat2019<-df2019 %>%
  filter(!completely_missing)


#' 
#' Transform/Reverse code high scores on observed variable to indicate right and TAN positioning
#' 
## ------------------------------------------------------------------------------------------------------------------
reverse_items<-c("h26","y19",
                 "h21","h22","h13",
                 "C2g","C2h",
                 "C2c","C2e","C2i","C2j")

reverse_items %in% names(df2019)

for (i in 1:length(reverse_items)){
  dat2019[,reverse_items[i]]<-6-dat2019[,reverse_items[i]]
}


#' 
#' \newpage
#' 
#' # Analysis
#' 
#' ## H1 and H2
#' 
#' H1. Left-Right placement as computed from responses to the pre-election public Voting Advice Applications (VAAs) is positively associated with Left-Right placement as computed from responses to the privately administered post-election Candidate Survey (CS). This association is stronger than any associations between the Left-Right and GAL-TAN dimensions.
#' 
#' H2. GAL-TAN placement as computed from responses to the pre-election public Voting Advice Applications (VAAs) is positively associated with GAL-TAN placement as computed from responses to the privately administered post-election Candidate Survey (CS). This association is stronger than any associations between the Left-Right and GAL-TAN dimensions.
#' 
#' ### Define the model
#' 
## ------------------------------------------------------------------------------------------------------------------


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


#' 
#' ### Fit the model
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H1H2<-cfa(model=model_H1H2,
              data=dat2019,
              missing="fiml")


#' 
#' Some problems with latent variable covariance structure
#' 
## ------------------------------------------------------------------------------------------------------------------
lavInspect(fit_H1H2, "cov.lv")
#examine standardized estimates
std.est_H1H2<-standardizedsolution(fit_H1H2)
std.est_H1H2[std.est_H1H2$op=="~~" & 
               std.est_H1H2$lhs!=std.est_H1H2$rhs,]




#' 
#' There is an impossible correlation between GAL-TAN latent variables (absolute value > 1)
#' 
#' \newpage
#' 
#' #### Respecify the model by introducing the three preregistered residual correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
model_H1H2.re<-paste0(model_H1H2,
                      "h27~~C2h\n",
                      "h21~~C2d\n",
                      "h29~~C2c\n")

#' 
#' #### Fit the respecified model
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H1H2.re<-cfa(model=model_H1H2.re,
              data=dat2019,
              missing="fiml")


#' 
#' Inspect fit of the model
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H1H2,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

round(inspect(fit_H1H2.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' The fit of the model is adequate.
#' 
#' Hypotheses 1 and 2
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H1H2<-standardizedsolution(fit_H1H2.re)
std.est_H1H2[std.est_H1H2$op==":=" | 
               std.est_H1H2$op=="~~" & 
               std.est_H1H2$lhs!=std.est_H1H2$rhs,]

#' 
#' H1: There is very strong (.915, p < .001) correlation between VAA-LR and CS-LR, and it is notably stronger (difference in correlations .491, p < .001) than the strongest of correlations between different dimensions (.42 between VAA_LR and VAA_GT, p < .001)
#' 
#' H2: There is very strong (.990, p < .001) correlation between VAA-GT and CS-GT, and it is notably stronger (difference in correlations .566, p < .001) than the strongest of correlations between different dimensions (.42 between VAA_LR and VAA_GT, p < .001)
#' 
#' \newpage
#' 
#' #### Exploratory analysis for H1 and H2: Seek misspecification to improve the overall model fit
#' 
#' Factor loadings
#' 
## ------------------------------------------------------------------------------------------------------------------
mis.load_H1H2<-miPowerFit(fit_H1H2.re,stdLoad=.40)
mis.load_H1H2<-mis.load_H1H2[mis.load_H1H2$op=="=~",]
#see summary of the decisions
table(mis.load_H1H2$decision.pow)
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

  

#' 
#' All the proposed loadings would be cross-loadings across methods (from VAA to CS or vice versa), and therefore not applicable for the present approach. Also, the expected parameter changes are indicative that most of these respecification would be Heywood -cases (standardized loadings that would be larger than 1 in absolute magnitude).
#' 
#' Residual correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
mis.rescor_H1H2<-miPowerFit(fit_H1H2.re,cor=.20)
mis.rescor_H1H2<-mis.rescor_H1H2[mis.rescor_H1H2$op=="~~" & 
                                   mis.rescor_H1H2$lhs!=mis.rescor_H1H2$rhs,]
#see summary of the decisions
table(mis.rescor_H1H2$decision.pow)

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

  

#' 
#' There was one misspecified residual correlation in VAA-LR, between
#' h25. Public services should be outsourced more than they are now for private companies and y19. Public authorities should be the main provider of social and healthcare services (r.) 
#' 
#' ##### Exploratory respecification
#' 
## ------------------------------------------------------------------------------------------------------------------
model_H1H2.exp.re<-paste0(model_H1H2.re,
                      "h25~~y19")


#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H1H2.exp.re<-cfa(model=model_H1H2.exp.re,
              data=dat2019,
              missing="fiml")

#' 
#' Inspect fit of the model
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H1H2.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

round(inspect(fit_H1H2.exp.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' The fit of the model is improved by additional residual correlation.
#' 
#' Retest Hypotheses 1 and 2
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H1H2.exp<-standardizedsolution(fit_H1H2.exp.re)
std.est_H1H2.exp[std.est_H1H2.exp$op==":=" | 
               std.est_H1H2.exp$op=="~~" & 
               std.est_H1H2.exp$lhs!=std.est_H1H2.exp$rhs,]

#' 
#' The results are virtually identical to those without the additional residual correlation. 
#' 
#' H1.exp: There is very strong (.932, p < .001) correlation between VAA-LR and CS-LR, and it is notably stronger (difference in correlations .462, p < .001) than the strongest of correlations between different dimensions (.470 between VAA_LR and VAA_GT, p < .001)
#' 
#' H2.exp: There is very strong (.990, p < .001) correlation between VAA-GT and CS-GT, and it is notably stronger (difference in correlations .520, p < .001) than the strongest of correlations between different dimensions (.470 between VAA_LR and VAA_GT, p < .001)
#' 
#' \newpage
#' 
#' ## H5
#' 
#' H5. Left-Right self-placement in the privately administered post-election Candidate Survey (CS) is positively associated with Left-Right as computed from responses to the pre-election public Voting Advice Applications (VAAs). This association is stronger than the association between placement of an imagined party voter in the privately administered post-election Candidate Survey (CS) and Left-Right as computed from responses to the pre-election public Voting Advice Applications (VAAs).
#' 
#' ### Add placement variables and their correlations with latent factors to the model used for H1 and H2
#' 
## ------------------------------------------------------------------------------------------------------------------


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


#' 
#' ### Fit the model
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H5<-cfa(model=model_H5,
            data=dat2019,
            missing="fiml")

#' 
#' 
#' Inspect fit of the model
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H1H2.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

round(inspect(fit_H5,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' The fit of the model is adequate.
#' 
#' Hypotheses 3
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H5<-standardizedsolution(fit_H5)
std.est_H5[std.est_H5$op==":=" | 
               std.est_H5$op=="~~" & 
               std.est_H5$lhs!=std.est_H5$rhs,]

#' 
#' H5. The correlation between VAA_LR and CS Self-placement on LR is large (.829, p < .001) and larger than the association between VAA_LR and placement of imagined party voter (.739, p < .001; difference .09, p < .001)
#' 
#' ### Exploratory H5: Seek misspecifications
#' 
#' Residual correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
mis.rescor_H5<-miPowerFit(fit_H5,cor=.20)
mis.rescor_H5<-mis.rescor_H5[mis.rescor_H5$op=="~~" & 
                                   mis.rescor_H5$lhs!=mis.rescor_H5$rhs,]
#see summary of the decisions
table(mis.rescor_H5$decision.pow)

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

  

#' 
#' There was one misspecified residual correlation in VAA-LR, between
#' h25. Public services should be outsourced more than they are now for private companies and y19. Public authorities should be the main provider of social and healthcare services (r.) 
#' 
#' 
#' #### Exploratory respecification
#' 
## ------------------------------------------------------------------------------------------------------------------
model_H5.exp<-paste0(model_H5,
                      "h25~~y19")


#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H5.exp<-cfa(model=model_H5.exp,
              data=dat2019,
              missing="fiml")

#' 
#' Inspect fit of the model
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H5,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

round(inspect(fit_H5.exp,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' The fit of the model is improved.
#' 
#' Retest Hypothesis 5
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H1H2.exp<-standardizedsolution(fit_H5.exp)
std.est_H1H2.exp[std.est_H1H2.exp$op==":=" | 
               std.est_H1H2.exp$op=="~~" & 
               std.est_H1H2.exp$lhs!=std.est_H1H2.exp$rhs,]

#' 
#' The results are virtually identical to those without the additional residual correlation.
#' 
#' H5.exp. The correlation between VAA_LR and CS Self-placement on LR is large (.845, p < .001) and larger than the association between VAA_LR and placement of imagined party voter (.751, p < .001; difference .094, p < .001)
#' 
#' 
#' \newpage
#' 
#' ## H3 and H4
#' 
#' Exclude other than members of the eight parties that have multiple members in the parliament
#' 
## ------------------------------------------------------------------------------------------------------------------

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

#' 
#' 
#' ### Define the model
#' 
## ------------------------------------------------------------------------------------------------------------------


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


#' 
#' 
#' ### Fit the configural model
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4<-cfa(model=model_H3H4,
              data=dat2019.party,
              group=c("puolue"),
              group.label=c("KD","KESK","KOK","PS","RKP","SDP","VAS","VIHR"),
              missing="fiml")


#' 
#' Problems with finding a converging model. Add preregistered residual correlations.
#' 
#' 
## ------------------------------------------------------------------------------------------------------------------
model_H3H4.re<-paste0(model_H3H4,
                      "h27~~C2h\n",
                      "h21~~C2d\n",
                      "h29~~C2c\n")

#' 
#' #### Fit the respecified model
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.re<-cfa(model=model_H3H4.re,
              data=dat2019.party,
              group=c("puolue"),
              group.label=c("KD","KESK","KOK","PS","RKP","SDP","VAS","VIHR"),
              missing="fiml")


#' 
#' The problem still persists
#' 
#' 
## ------------------------------------------------------------------------------------------------------------------
summary(fit_H3H4.re,fit=T,standardized=T,rsquare=T)


#' 
#' \newpage
#' 
#' From the above output it is difficult to see what is the problem.
#' 
#' Try to fit the model separately for each group
#' 
#' \newpage
#' 
#' #### Model for KD
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.re.KD<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("KD"),
                    missing="fiml")

#' 
#' Model for KD converges
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H3H4.re.KD,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' Fit is poor
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H3H4.re.KD<-standardizedsolution(fit_H3H4.re.KD)
std.est_H3H4.re.KD[std.est_H3H4.re.KD$op==":=" | 
               std.est_H3H4.re.KD$op=="~~" & 
               std.est_H3H4.re.KD$lhs!=std.est_H3H4.re.KD$rhs,]

#' 
#' 
#' \newpage
#' 
#' #### Model for KESK
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.re.KESK<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("KESK"),
                    missing="fiml")

#' 
#' Model for KESK does not converge
#' 
## ----eval=FALSE, include=FALSE-------------------------------------------------------------------------------------
## round(inspect(fit_H3H4.re.KESK,"fit")
##       [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' Fit is poor
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H3H4.re.KESK<-standardizedsolution(fit_H3H4.re.KESK)
std.est_H3H4.re.KESK[std.est_H3H4.re.KESK$op==":=" | 
               std.est_H3H4.re.KESK$op=="~~" & 
               std.est_H3H4.re.KESK$lhs!=std.est_H3H4.re.KESK$rhs,]

#' 
#' 
#' \newpage
#' 
#' #### Model for KOK
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.re.KOK<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("KOK"),
                    missing="fiml")

#' 
#' Model for KOK converges
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H3H4.re.KOK,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' Fit is poor
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H3H4.re.KOK<-standardizedsolution(fit_H3H4.re.KOK)
std.est_H3H4.re.KOK[std.est_H3H4.re.KOK$op==":=" | 
               std.est_H3H4.re.KOK$op=="~~" & 
               std.est_H3H4.re.KOK$lhs!=std.est_H3H4.re.KOK$rhs,]

#' 
#' \newpage
#' 
#' #### Model for PS
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.re.PS<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("PS"),
                    missing="fiml")

#' 
#' Model for PS does not converge
#' 
## ----eval=FALSE, include=FALSE-------------------------------------------------------------------------------------
## round(inspect(fit_H3H4.re.PS,"fit")
##       [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' Fit is poor
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H3H4.re.PS<-standardizedsolution(fit_H3H4.re.PS)
std.est_H3H4.re.PS[std.est_H3H4.re.PS$op==":=" | 
               std.est_H3H4.re.PS$op=="~~" & 
               std.est_H3H4.re.PS$lhs!=std.est_H3H4.re.PS$rhs,]

#' 
#' Heywood correlation between VAA_GT and CS_GT
#' 
#' \newpage
#' 
#' #### Model for RKP
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.re.RKP<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("RKP"),
                    missing="fiml")

#' 
#' Model for RKP converges, but has other problems
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H3H4.re.RKP,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' Fit is poor
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H3H4.re.RKP<-standardizedsolution(fit_H3H4.re.RKP)
std.est_H3H4.re.RKP[std.est_H3H4.re.RKP$op==":=" | 
               std.est_H3H4.re.RKP$op=="~~" & 
               std.est_H3H4.re.RKP$lhs!=std.est_H3H4.re.RKP$rhs,]

#' 
#' 
#' \newpage
#' 
#' #### Model for SDP
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.re.SDP<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("SDP"),
                    missing="fiml")

#' 
#' Model for SDP conveges, but has other problems
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H3H4.re.SDP,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' Fit is poor
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H3H4.re.SDP<-standardizedsolution(fit_H3H4.re.SDP)
std.est_H3H4.re.SDP[std.est_H3H4.re.SDP$op==":=" | 
               std.est_H3H4.re.SDP$op=="~~" & 
               std.est_H3H4.re.SDP$lhs!=std.est_H3H4.re.SDP$rhs,]

#' 
#' Heywood correlation between VAA_GT and CS_GT
#' 
#' \newpage
#' 
#' #### Model for VAS
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.re.VAS<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("VAS"),
                    missing="fiml")

#' 
#' Model for VAS does not converge
#' 
## ----eval=FALSE, include=FALSE-------------------------------------------------------------------------------------
## round(inspect(fit_H3H4.re.VAS,"fit")
##       [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H3H4.re.VAS<-standardizedsolution(fit_H3H4.re.VAS)
std.est_H3H4.re.VAS[std.est_H3H4.re.VAS$op==":=" | 
               std.est_H3H4.re.VAS$op=="~~" & 
               std.est_H3H4.re.VAS$lhs!=std.est_H3H4.re.VAS$rhs,]

#' 
#' 
#' \newpage
#' 
#' #### Model for VIHR
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.re.VIHR<-cfa(model=model_H1H2.re,
                    data=dat2019.party,
                    group=c("puolue"),
                    group.label=c("VIHR"),
                    missing="fiml")

#' 
#' Model for VIHR does not converge
#' 
## ----eval=FALSE, include=FALSE-------------------------------------------------------------------------------------
## round(inspect(fit_H3H4.re.VIHR,"fit")
##       [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' Fit is poor
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H3H4.re.VIHR<-standardizedsolution(fit_H3H4.re.VIHR)
std.est_H3H4.re.VIHR[std.est_H3H4.re.VIHR$op==":=" | 
               std.est_H3H4.re.VIHR$op=="~~" & 
               std.est_H3H4.re.VIHR$lhs!=std.est_H3H4.re.VIHR$rhs,]

#' 
#' \newpage
#' 
#' ### Summary of H3-H4 with MG-CFA approach
#' 
#' The configural model did not converge, even after respecification. Single group models also were non-converging or had other type of problems, except for KD and KOK, for which the fit of the model nevertheless was poor, and therefore not interpretable.
#' 
#' This most likely is an indication that the sample sizes of the parties are too small for this model with 21 indicators and 4 factors. 
#' 
#' The alternative way to test hypotheses 4-6 is presented below. It unconfounds the associations in the model by using party-mean centered observed variables for estimating the similar type of model that was used for H1 and H2, and H5, respectively. Because this approach does not have any grouping structure, it uses the overall sample size for the eight parties, which is `r nrow(dat2019.party)`. It is nevertheless only conducted among the eight focal parties, and other parties are excluded. Because the misspecification in the model with centered variables might be entirely different to raw score variables, the modeling is again started with no residual correlations and they are examined if the fit of the model is inadequate.
#' 
#' \newpage
#' 
#' ## H3 and H4 with group-mean centered variables and no grouping structure
#' 
#' Estimate how much of the variation in each item is between-groups
#' 
## ------------------------------------------------------------------------------------------------------------------
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
describe(ICC$ICC1,fast=T)

ICC$label<-get_label(df2019[,as.character(ICC[,1])])

#export to .csv file
write.csv2(ICC,"ICC_2019.csv")


#' 
#' ICC1 gives the proportion (%) of variance that is between the parties. There is quite a lot of between-party variance, but the responses are not entire defined by party either.
#' 
#' Center all observed variables
#' 
## ------------------------------------------------------------------------------------------------------------------


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

#' 
#' 
#' ### Define the model
#' 
## ------------------------------------------------------------------------------------------------------------------


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


#' 
#' 
#' 
#' ### Fit the model
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4<-cfa(model=model_H3H4,
              data=dat2019.gmc,
              missing="fiml")


#' 
## ------------------------------------------------------------------------------------------------------------------
lavInspect(fit_H3H4, "cov.lv")
summary(fit_H3H4,fit=T,standardized=T,rsquare=T)


#' 
#' There is a Heywood correlation between GAL-TAN latent variables (absolute value > 1)
#' 
#' \newpage
#' 
#' #### Respecify the model by introducing the three preregistered residual correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
model_H3H4.re<-paste0(model_H3H4,
                      "h27~~C2h\n",
                      "h21~~C2d\n",
                      "h29~~C2c\n")

#' 
#' #### Fit the respecified model
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.re<-cfa(model=model_H3H4.re,
              data=dat2019.gmc,
              missing="fiml")


#' 
#' Inspect fit of the model
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H3H4.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' The fit of the model is quite poor.
#' 
#' Hypotheses 1 and 2
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H3H4<-standardizedsolution(fit_H3H4.re)
std.est_H3H4[std.est_H3H4$op==":=" | 
               std.est_H3H4$op=="~~" & 
               std.est_H3H4$lhs!=std.est_H3H4$rhs,]

#' 
#' H3: There is strong (.782, p < .001) correlation between VAA-LR and CS-LR, and it is notably stronger (difference in correlations .593, p < .001) than the strongest of correlations between different dimensions (.189 between VAA_LR and VAA_GT, p = .006)
#' 
#' H4: There is very strong (.956, p < .001) correlation between VAA-GT and CS-GT, and it is notably stronger (difference in correlations .767, p < .001) than the strongest of correlations between different dimensions (.189 between VAA_LR and VAA_GT, p = .006)
#' 
#' 
#' #### Exploratory for H3 and H4: Seek misspecification to improve the overall model fit
#' 
#' Factor loadings
#' 
## ------------------------------------------------------------------------------------------------------------------
mis.load_H3H4<-miPowerFit(fit_H3H4.re,stdLoad=.40)
mis.load_H3H4<-mis.load_H3H4[mis.load_H3H4$op=="=~",]
#see summary of the decisions
table(mis.load_H3H4$decision.pow)
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

  

#' 
#' All the proposed loadings would be cross-loadings across methods (from VAA to CS or vice versa), and therefore not applicable for the present approach. Also, the expected parameter changes are indicative that most of these respecification would be Heywood -cases (standardized loadings that would be larger than 1 in absolute magnitude).
#' 
#' Residual correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
mis.rescor_H3H4<-miPowerFit(fit_H3H4.re,cor=.20)
mis.rescor_H3H4<-mis.rescor_H3H4[mis.rescor_H3H4$op=="~~" & 
                                   mis.rescor_H3H4$lhs!=mis.rescor_H3H4$rhs,]
#see summary of the decisions
table(mis.rescor_H3H4$decision.pow)

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

  

#' 
#' There were two misspecified residual correlation.
#' 
#' One was between VAA-LR items (same misspecification as was found for H1 and H2)
#' H25. Public services should be outsourced more than they are now for private companies and y19. Public authorities should be the main provider of social and healthcare services (r.) 
#' 
#' The other misspecification was between C2a. Immigrants should adapt to Finnish habits and C2f. People who break the law should be punished more severely 
#' 
#' Respecify the model to allow these parameters to be freely estimated
#' 
#' ##### Exploratory respecification
#' 
## ------------------------------------------------------------------------------------------------------------------
model_H3H4.exp.re<-paste0(model_H3H4.re,
                      "h25~~y19\n",
                      "C2a~~C2f\n")


#' 
#' 
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.exp.re<-cfa(model=model_H3H4.exp.re,
              data=dat2019.gmc,
              missing="fiml")

#' 
#' Inspect fit of the model
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H3H4.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

round(inspect(fit_H3H4.exp.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' The fit of the model is better 
#' 
#' Retest Hypotheses 4 and 5
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H3H4.exp<-standardizedsolution(fit_H3H4.exp.re)
std.est_H3H4.exp[std.est_H3H4.exp$op==":=" | 
               std.est_H3H4.exp$op=="~~" & 
               std.est_H3H4.exp$lhs!=std.est_H3H4.exp$rhs,]

#' 
#' The results are virtually identical to those without the additional residual correlations.
#' 
#' Put a more strict criterion on the residual correlation misspecification (.15)
#' 
#' 
#' Residual correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
mis.rescor_H3H4<-miPowerFit(fit_H3H4.exp.re,cor=.15)
mis.rescor_H3H4<-mis.rescor_H3H4[mis.rescor_H3H4$op=="~~" & 
                                   mis.rescor_H3H4$lhs!=mis.rescor_H3H4$rhs,]
#see summary of the decisions
table(mis.rescor_H3H4$decision.pow)

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

  

#' 
#' There were two more misspecified residual correlations.
#' 
#' Between VAA-GAL-TAN h22. If the government proposes to establish a refugee center in my home municipality, the proposal should be accepted (r.) and CS-GAL-TAN C2i. Immigrants are good for the Finnish economy (r.)
#' 
#' And between two CS-GAL-TAN items: C2d. Same Sex Marriages should be prohibited by law and C2j. Deciding on abortion issues should be a women's right (r.) 
#' 
## ------------------------------------------------------------------------------------------------------------------
model_H3H4.exp.re.2<-paste0(model_H3H4.exp.re,
                      "h22~~C2i\n",
                      "C2d~~C2j\n")


#' 
#' 
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H3H4.exp.re.2<-cfa(model=model_H3H4.exp.re.2,
              data=dat2019.gmc,
              missing="fiml")

#' 
#' Inspect fit of the model
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H3H4.exp.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

round(inspect(fit_H3H4.exp.re.2,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' The fit of the model is again improved
#' 
#' Retest Hypotheses 4 and 5
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H3H4.exp<-standardizedsolution(fit_H3H4.exp.re.2)
std.est_H3H4.exp[std.est_H3H4.exp$op==":=" | 
               std.est_H3H4.exp$op=="~~" & 
               std.est_H3H4.exp$lhs!=std.est_H3H4.exp$rhs,]

#' 
#' 
#' Print all the final model parameters
#' 
## ------------------------------------------------------------------------------------------------------------------
standardizedsolution(fit_H3H4.exp.re.2)

#' 
#' \newpage
#' 
#' ## H6 with group mean centered observed variables
#' 
#' H6. Within-party placement on Left-Right as computed from responses to the pre-election public Voting Advice Applications (VAAs) is positively associated with within-party placement on Left-Right as computed from responses to the privately administered post-election Candidate Survey (CS). This association is stronger than any within-party associations between the Left-Right and GAL-TAN dimensions. 
#' 
#' ### Add placement variables and their correlations with latent factors to the model used for H3 and H4
#' 
## ------------------------------------------------------------------------------------------------------------------

model_H6<-paste0(model_H3H4.exp.re.2,
                 "SP_LR=~C5a\n",
                 "IP_LR=~C5c\n",
                 "VAA_LR~~r.self.LR*SP_LR\n",
                 "VAA_LR~~r.ideal.LR*IP_LR\n",
                 "test.H6:=r.self.LR-r.ideal.LR\n")



#' 
#' ### Fit the model
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H6<-cfa(model=model_H6,
            data=dat2019.gmc,
            missing="fiml")

#' 
#' 
#' Inspect fit of the model
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H3H4.exp.re.2,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

round(inspect(fit_H6,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' The fit of the model is similar.
#' 
#' Hypothesis 6
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H6<-standardizedsolution(fit_H6)
std.est_H6[std.est_H6$op==":=" | 
               std.est_H6$op=="~~" & 
               std.est_H6$lhs!=std.est_H6$rhs,]

#' 
#' The correlation between VAA_LR and CS Self-placement on LR is strong (.495, p < .001) and larger than the association between VAA_LR and placement of imagined party voter (.072, p = .244; difference .42, p < .001)
#' 
#' Look for misspecifications
#' 
#' 
#' Residual correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
mis.rescor_H6<-miPowerFit(fit_H6,cor=.20)
mis.rescor_H6<-mis.rescor_H6[mis.rescor_H6$op=="~~" & 
                                   mis.rescor_H6$lhs!=mis.rescor_H6$rhs,]
#see summary of the decisions
table(mis.rescor_H6$decision.pow)

#there are no misspecification with delta set at .20

#look with .15

mis.rescor_H6<-miPowerFit(fit_H6,cor=.15)
mis.rescor_H6<-mis.rescor_H6[mis.rescor_H6$op=="~~" & 
                                   mis.rescor_H6$lhs!=mis.rescor_H6$rhs,]
#see summary of the decisions
table(mis.rescor_H6$decision.pow)

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

  

#' 
#' The misspecification is between VAA-GAL-TAN: h21. Gay and lesbian couples should have the same marriage and adoption rights as straight couples (r.)  and C2j. Deciding on abortion issues should be a women's right (r.)
#' 
#' Add it to the model
#' 
#' 
## ------------------------------------------------------------------------------------------------------------------
model_H6.re<-paste0(model_H6,
                      "h21~~C2j\n")


#' 
#' 
#' 
## ------------------------------------------------------------------------------------------------------------------
fit_H6.re<-cfa(model=model_H6.re,
              data=dat2019.gmc,
              missing="fiml")

#' 
#' Inspect fit of the model
#' 
## ------------------------------------------------------------------------------------------------------------------
round(inspect(fit_H6,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

round(inspect(fit_H6.re,"fit")
      [c("npar","df","chisq","pvalue","cfi","tli","rmsea","srmr")],3)

#' 
#' No big differences in fit
#' 
#' 
#' Print standardized estimates to test the difference between correlations
#' 
## ------------------------------------------------------------------------------------------------------------------
std.est_H6.re<-standardizedsolution(fit_H6.re)
std.est_H6.re[std.est_H6.re$op==":=" | 
               std.est_H6.re$op=="~~" & 
               std.est_H6.re$lhs!=std.est_H6.re$rhs,]

#' 
#' Results are virtually identical
