#########################################################
#### Satorra-Bentler Corrected Chi-Square Comparison ####
####              Created: Sep 27 2012               ####
####              Version: 0.2 (Alpha)               ####
#########################################################

SBchi <- function(model0,model1) {

  # reading the mplus output
  mplus0 <- read.fwf(model0,width=c(80))
  mplus1 <- read.fwf(model1,width=c(80))
  
  begin0 <- grep('MODEL RESULTS',mplus0[,1])
  begin1 <- grep('MODEL RESULTS',mplus1[,1])
  
  # accounting for differences in Mplus versions
  if (sum(grepl('TESTS OF MODEL FIT',mplus0[,1]))!=0) {
    startfit0 <- grep('TESTS OF MODEL FIT',mplus0[,1])[1]
    }
  
  if (sum(grepl('MODEL FIT INFORMATION',mplus0[,1]))!=0) {
    startfit0 <- grep('MODEL FIT INFORMATION',mplus0[,1])[1]
    }
  
  if (sum(grepl('TESTS OF MODEL FIT',mplus1[,1]))!=0) {
    startfit1 <- grep('TESTS OF MODEL FIT',mplus1[,1])[1]
  }
  
  if (sum(grepl('MODEL FIT INFORMATION',mplus1[,1]))!=0) {
    startfit1 <- grep('MODEL FIT INFORMATION',mplus1[,1])[1]
  }
  
  
  fit0 <- read.fwf(model0,
   skip=startfit0,
   n=(begin0[1]-startfit0),
   width=c(38,19),
   col.names=c('Measure','Value'))
  
  fit1 <- read.fwf(model1,
   skip=startfit1,
   n=(begin1[1]-startfit1),
   width=c(38,19),
   col.names=c('Measure','Value'))
  
  
  fit0[,2] <- gsub(' ','',fit0[,2]); fit1[,2] <- gsub(' ','',fit1[,2])
  
  p0 <- as.numeric(fit0[grep('Number of Free Parameters',fit0[,1]),2])
  p1 <- as.numeric(fit1[grep('Number of Free Parameters',fit1[,1]),2])
  
  L0 <- as.numeric(fit0[grep('Loglikelihood',fit0[,1])+2,2])
  L1 <- as.numeric(fit1[grep('Loglikelihood',fit1[,1])+2,2])

  if (sum(grepl('Scaling Corr',fit0[,1]))>0) {
    c0 <- as.numeric(fit0[grep('Scaling Corr',fit0[,1]),2]) }
  else c0 <- 1

  if (sum(grepl('Scaling Corr',fit0[,1]))>0) {
    c1 <- as.numeric(fit1[grep('Scaling Corr',fit1[,1]),2]) }
  else c1 <- 1
  
  RMSEA0 <- as.numeric(fit0[grep('^RMSEA',fit0[,1])+2,2])
  RMSEA1 <- as.numeric(fit1[grep('^RMSEA',fit1[,1])+2,2])

  SRMR0 <- as.numeric(fit0[grep('^SRMR',fit0[,1])+2,2])
  SRMR1 <- as.numeric(fit1[grep('^SRMR',fit1[,1])+2,2])
  
  CFI0 <- as.numeric(fit0[grep('CFI',fit0[,1]),2][2])
  CFI1 <- as.numeric(fit1[grep('CFI',fit1[,1]),2][2])  

  TLI0 <- as.numeric(fit0[grep('TLI',fit0[,1]),2][2])
  TLI1 <- as.numeric(fit1[grep('TLI',fit1[,1]),2][2])  
  
  AIC0 <- as.numeric(fit0[grep('Information Criteria',fit0[,1])+2,2])
  BIC0 <- as.numeric(fit0[grep('Information Criteria',fit0[,1])+3,2])
  aBIC0 <- as.numeric(fit0[grep('Information Criteria',fit0[,1])+4,2])

  AIC1 <- as.numeric(fit1[grep('Information Criteria',fit1[,1])+2,2])
  BIC1 <- as.numeric(fit1[grep('Information Criteria',fit1[,1])+3,2])
  aBIC1 <- as.numeric(fit1[grep('Information Criteria',fit1[,1])+4,2])

  chi0 <- as.numeric(fit0[grep('Chi-Square Test',fit0[,1])+2,2][1])
  df0 <- as.numeric(fit0[grep('Chi-Square Test',fit0[,1])+3,2][1])
  pv0 <- as.numeric(fit0[grep('Chi-Square Test',fit0[,1])+4,2][1])
  
  chi1 <- as.numeric(fit1[grep('Chi-Square Test',fit1[,1])+2,2][1])
  df1 <- as.numeric(fit1[grep('Chi-Square Test',fit1[,1])+3,2][1])
  pv1 <- as.numeric(fit1[grep('Chi-Square Test',fit1[,1])+4,2][1])
  
  
  
  results <- data.frame(c(p0,p1),c(L0,L1),c(c0,c1),
    c(chi0,chi1),c(df0,df1),c(pv0,pv1),
    c(RMSEA0,RMSEA1),c(SRMR0,SRMR1),c(CFI0,CFI1),c(TLI0,TLI1),
    c(AIC0,AIC1),c(BIC0,BIC1),c(aBIC0,aBIC1))
  names(results) <- c('Parameters','LogLikelihood','Correction',
    'ChiSquare','df','pValue',
    'RMSEA','SRMR','CFI','TLI',
    'AIC','BIC','aBIC')
  
  # Taken from statmodel.com, Sep 21 2012
  cd <- (p0 * c0 - p1*c1)/(p0 - p1)
  TRd <- -2*(L0 - L1)/cd
  
  pd <- 1-abs(pchisq(TRd,p1-p0))
  
  results[3,] <- results[1,]-results[2,]
  
  results$ChiSquare[3] <- TRd
  results$df[3] <- p1-p0
  results$pValue[3] <- pd
  
  results[3,1:2] <- NA
  results$Correction[3] <- cd  
  
  rownames(results) <- c('H0','H1','Comparison')
  
  if (sum(grepl('Scaling Corr',fit0[,1]))==0) {
    warning('The models you are trying to compare were not estimated using MLR. The uncorrected Chi-Square comparison is reported.') }

  return(round(results,3))
  
}
