## ---- evaluate_zipf


#==============================================================================#
#                               fitLNRE                                        #
#==============================================================================#
#'  fitLNRE
#' 
#' This function takes as its parameter, a frequency spectrum object and returns
#' a list of fit summaries for each LNRE model
#' 
#' @param documentSpc - the document frequency spectrum object
#' @return fitSummaries - a list of fit summaries
#' @author John James
#' @export
fitLNRE <- function(docSpc) {
  message('zm.chisq')
  zm.chisq.custom       <- lnre('zm', spc=docSpc, cost='chisq', method='Custom', exact = FALSE)
  zm.chisq.custom.exact <- lnre('zm', spc=docSpc, cost='chisq', method='Custom')
#  zm.chisq.nlm       <- lnre('zm', spc=docSpc, cost='chisq', method='NLM', exact = FALSE)
  zm.chisq.nlm.exact <- lnre('zm', spc=docSpc, cost='chisq', method='NLM')
  zm.chisq.nelder       <- lnre('zm', spc=docSpc, cost='chisq', method='Nelder-Mead', exact = FALSE)
  zm.chisq.nelder.exact <- lnre('zm', spc=docSpc, cost='chisq', method='Nelder-Mead')
  zm.chisq.sann       <- lnre('zm', spc=docSpc, cost='chisq', method='SANN', exact = FALSE)
  zm.chisq.sann.exact <- lnre('zm', spc=docSpc, cost='chisq', method='SANN')
  
  message('zm.linear')
  zm.linear.custom       <- lnre('zm', spc=docSpc, cost='linear', method='Custom', exact = FALSE)
  zm.linear.custom.exact <- lnre('zm', spc=docSpc, cost='linear', method='Custom')
  zm.linear.nlm       <- lnre('zm', spc=docSpc, cost='linear', method='NLM', exact = FALSE)
  zm.linear.nelder       <- lnre('zm', spc=docSpc, cost='linear', method='Nelder-Mead', exact = FALSE)
  zm.linear.nelder.exact <- lnre('zm', spc=docSpc, cost='linear', method='Nelder-Mead')
  zm.linear.sann       <- lnre('zm', spc=docSpc, cost='linear', method='SANN', exact = FALSE)
  zm.linear.sann.exact <- lnre('zm', spc=docSpc, cost='linear', method='SANN')
  
  message('zm.smooth')
  zm.smooth.linear.custom       <- lnre('zm', spc=docSpc, cost='smooth.linear', method='Custom', exact = FALSE)
  zm.smooth.linear.custom.exact <- lnre('zm', spc=docSpc, cost='smooth.linear', method='Custom')
  zm.smooth.linear.nlm       <- lnre('zm', spc=docSpc, cost='smooth.linear', method='NLM', exact = FALSE)
  zm.smooth.linear.nlm.exact <- lnre('zm', spc=docSpc, cost='smooth.linear', method='NLM')
  zm.smooth.linear.nelder       <- lnre('zm', spc=docSpc, cost='smooth.linear', method='Nelder-Mead', exact = FALSE)
  zm.smooth.linear.nelder.exact <- lnre('zm', spc=docSpc, cost='smooth.linear', method='Nelder-Mead')
  zm.smooth.linear.sann       <- lnre('zm', spc=docSpc, cost='smooth.linear', method='SANN', exact = FALSE)
  zm.smooth.linear.sann.exact <- lnre('zm', spc=docSpc, cost='smooth.linear', method='SANN')
  
  message('zm.mse')
  zm.mse.custom       <- lnre('zm', spc=docSpc, cost='mse', method='Custom', exact = FALSE)
  zm.mse.custom.exact <- lnre('zm', spc=docSpc, cost='mse', method='Custom')
  zm.mse.nlm       <- lnre('zm', spc=docSpc, cost='mse', method='NLM', exact = FALSE)
  zm.mse.nlm.exact <- lnre('zm', spc=docSpc, cost='mse', method='NLM')
  zm.mse.nelder       <- lnre('zm', spc=docSpc, cost='mse', method='Nelder-Mead', exact = FALSE)
  zm.mse.nelder.exact <- lnre('zm', spc=docSpc, cost='mse', method='Nelder-Mead')
  zm.mse.sann       <- lnre('zm', spc=docSpc, cost='mse', method='SANN', exact = FALSE)
  zm.mse.sann.exact <- lnre('zm', spc=docSpc, cost='mse', method='SANN')
  
  message('zm.exact')
  zm.exact.custom       <- lnre('zm', spc=docSpc, cost='exact', method='Custom', exact = FALSE)
  zm.exact.custom.exact <- lnre('zm', spc=docSpc, cost='exact', method='Custom')
  zm.exact.nlm       <- lnre('zm', spc=docSpc, cost='exact', method='NLM', exact = FALSE)
  zm.exact.nlm.exact <- lnre('zm', spc=docSpc, cost='exact', method='NLM')
  zm.exact.nelder       <- lnre('zm', spc=docSpc, cost='exact', method='Nelder-Mead', exact = FALSE)
  zm.exact.nelder.exact <- lnre('zm', spc=docSpc, cost='exact', method='Nelder-Mead')
  zm.exact.sann       <- lnre('zm', spc=docSpc, cost='exact', method='SANN', exact = FALSE)
  zm.exact.sann.exact <- lnre('zm', spc=docSpc, cost='exact', method='SANN')
  
  message('fzm.chisq')
  fzm.chisq.custom       <- lnre('fzm', spc=docSpc, cost='chisq', method='custom', exact = FALSE)
  fzm.chisq.nelder.exact <- lnre('fzm', spc=docSpc, cost='chisq', method='Nelder-Mead')
  fzm.chisq.sann       <- lnre('fzm', spc=docSpc, cost='chisq', method='SANN', exact = FALSE)
  fzm.chisq.sann.exact <- lnre('fzm', spc=docSpc, cost='chisq', method='SANN')
  
  message('fzm.linear')
  fzm.linear.custom       <- lnre('fzm', spc=docSpc, cost='linear', method='Custom', exact = FALSE)
  fzm.linear.custom.exact <- lnre('fzm', spc=docSpc, cost='linear', method='Custom')
  fzm.linear.nelder       <- lnre('fzm', spc=docSpc, cost='linear', method='Nelder-Mead', exact = FALSE)
  fzm.linear.nelder.exact <- lnre('fzm', spc=docSpc, cost='linear', method='Nelder-Mead')
  fzm.linear.sann       <- lnre('fzm', spc=docSpc, cost='linear', method='SANN', exact = FALSE)
  fzm.linear.sann.exact <- lnre('fzm', spc=docSpc, cost='linear', method='SANN')
  
  message('fzm.smooth')
  fzm.smooth.linear.custom       <- lnre('fzm', spc=docSpc, cost='smooth.linear', method='Custom', exact = FALSE)
  fzm.smooth.linear.custom.exact <- lnre('fzm', spc=docSpc, cost='smooth.linear', method='Custom')
  fzm.smooth.linear.nelder       <- lnre('fzm', spc=docSpc, cost='smooth.linear', method='Nelder-Mead', exact = FALSE)
  fzm.smooth.linear.nelder.exact <- lnre('fzm', spc=docSpc, cost='smooth.linear', method='Nelder-Mead')
  fzm.smooth.linear.sann       <- lnre('fzm', spc=docSpc, cost='smooth.linear', method='SANN', exact = FALSE)
  fzm.smooth.linear.sann.exact <- lnre('fzm', spc=docSpc, cost='smooth.linear', method='SANN')
  
  message('fzm.mse')
  fzm.mse.nelder       <- lnre('fzm', spc=docSpc, cost='mse', method='Nelder-Mead', exact = FALSE)
  fzm.mse.nelder.exact <- lnre('fzm', spc=docSpc, cost='mse', method='Nelder-Mead')
  fzm.mse.sann       <- lnre('fzm', spc=docSpc, cost='mse', method='SANN', exact = FALSE)
  fzm.mse.sann.exact <- lnre('fzm', spc=docSpc, cost='mse', method='SANN')
  
  message('fzm.exact')
  fzm.exact.custom       <- lnre('fzm', spc=docSpc, cost='exact', method='Custom', exact = FALSE)
  fzm.exact.custom.exact <- lnre('fzm', spc=docSpc, cost='exact', method='Custom')
  fzm.exact.nelder       <- lnre('fzm', spc=docSpc, cost='exact', method='Nelder-Mead', exact = FALSE)
  fzm.exact.nelder.exact <- lnre('fzm', spc=docSpc, cost='exact', method='Nelder-Mead')
  fzm.exact.sann       <- lnre('fzm', spc=docSpc, cost='exact', method='SANN', exact = FALSE)
  fzm.exact.sann.exact <- lnre('fzm', spc=docSpc, cost='exact', method='SANN')
  
  message('gigp.chisq')
  gigp.chisq.custom       <- lnre('gigp', spc=docSpc, cost='chisq', method='Custom', exact = FALSE)
  gigp.chisq.custom.exact <- lnre('gigp', spc=docSpc, cost='chisq', method='Custom')
  gigp.chisq.nelder       <- lnre('gigp', spc=docSpc, cost='chisq', method='Nelder-Mead', exact = FALSE)
  gigp.chisq.nelder.exact <- lnre('gigp', spc=docSpc, cost='chisq', method='Nelder-Mead')
  gigp.chisq.sann       <- lnre('gigp', spc=docSpc, cost='chisq', method='SANN', exact = FALSE)
  gigp.chisq.sann.exact <- lnre('gigp', spc=docSpc, cost='chisq', method='SANN')
  
  message('gigp.linear')
  gigp.linear.custom       <- lnre('gigp', spc=docSpc, cost='linear', method='Custom', exact = FALSE)
  gigp.linear.custom.exact <- lnre('gigp', spc=docSpc, cost='linear', method='Custom')
  gigp.linear.nlm       <- lnre('gigp', spc=docSpc, cost='linear', method='NLM', exact = FALSE)
  gigp.linear.nlm.exact <- lnre('gigp', spc=docSpc, cost='linear', method='NLM')
  gigp.linear.nelder       <- lnre('gigp', spc=docSpc, cost='linear', method='Nelder-Mead', exact = FALSE)
  gigp.linear.nelder.exact <- lnre('gigp', spc=docSpc, cost='linear', method='Nelder-Mead')
  gigp.linear.sann       <- lnre('gigp', spc=docSpc, cost='linear', method='SANN', exact = FALSE)
  gigp.linear.sann.exact <- lnre('gigp', spc=docSpc, cost='linear', method='SANN')
  
  message('gigp.smooth')
  gigp.smooth.linear.custom       <- lnre('gigp', spc=docSpc, cost='smooth.linear', method='Custom', exact = FALSE)
  gigp.smooth.linear.custom.exact <- lnre('gigp', spc=docSpc, cost='smooth.linear', method='Custom')
  gigp.smooth.linear.nlm       <- lnre('gigp', spc=docSpc, cost='smooth.linear', method='NLM', exact = FALSE)
  gigp.smooth.linear.nlm.exact <- lnre('gigp', spc=docSpc, cost='smooth.linear', method='NLM')
  gigp.smooth.linear.nelder       <- lnre('gigp', spc=docSpc, cost='smooth.linear', method='Nelder-Mead', exact = FALSE)
  gigp.smooth.linear.nelder.exact <- lnre('gigp', spc=docSpc, cost='smooth.linear', method='Nelder-Mead')
  gigp.smooth.linear.sann       <- lnre('gigp', spc=docSpc, cost='smooth.linear', method='SANN', exact = FALSE)
  gigp.smooth.linear.sann.exact <- lnre('gigp', spc=docSpc, cost='smooth.linear', method='SANN')
  
  message('gigp.mse')
  gigp.mse.sann       <- lnre('gigp', spc=docSpc, cost='mse', method='SANN', exact = FALSE)
  gigp.mse.sann.exact <- lnre('gigp', spc=docSpc, cost='mse', method='SANN')
  
  message('gigp.exact')
  gigp.exact.sann       <- lnre('gigp', spc=docSpc, cost='exact', method='SANN', exact = FALSE)
  gigp.exact.sann.exact <- lnre('gigp', spc=docSpc, cost='exact', method='SANN')
  
  models <- list(zm.chisq.custom  =  zm.chisq.custom,
                 zm.chisq.custom.exact  =  zm.chisq.custom.exact,
 #                zm.chisq.nlm  =  zm.chisq.nlm,
                 zm.chisq.nlm.exact  =  zm.chisq.nlm.exact,
                 zm.chisq.nelder  =  zm.chisq.nelder,
                 zm.chisq.nelder.exact  =  zm.chisq.nelder.exact,
                 zm.chisq.sann  =  zm.chisq.sann,
                 zm.chisq.sann.exact  =  zm.chisq.sann.exact,
                 zm.linear.custom  =  zm.linear.custom,
                 zm.linear.custom.exact  =  zm.linear.custom.exact,
                 zm.linear.nlm  =  zm.linear.nlm,
                 zm.linear.nelder  =  zm.linear.nelder,
                 zm.linear.nelder.exact  =  zm.linear.nelder.exact,
                 zm.linear.sann  =  zm.linear.sann,
                 zm.linear.sann.exact  =  zm.linear.sann.exact,
                 zm.smooth.linear.custom  =  zm.smooth.linear.custom,
                 zm.smooth.linear.custom.exact  =  zm.smooth.linear.custom.exact,
                 zm.smooth.linear.nlm  =  zm.smooth.linear.nlm,
                 zm.smooth.linear.nlm.exact  =  zm.smooth.linear.nlm.exact,
                 zm.smooth.linear.nelder  =  zm.smooth.linear.nelder,
                 zm.smooth.linear.nelder.exact  =  zm.smooth.linear.nelder.exact,
                 zm.smooth.linear.sann  =  zm.smooth.linear.sann,
                 zm.smooth.linear.sann.exact  =  zm.smooth.linear.sann.exact,
                 zm.mse.custom  =  zm.mse.custom,
                 zm.mse.custom.exact  =  zm.mse.custom.exact,
                 zm.mse.nlm  =  zm.mse.nlm,
                 zm.mse.nlm.exact  =  zm.mse.nlm.exact,
                 zm.mse.nelder  =  zm.mse.nelder,
                 zm.mse.nelder.exact  =  zm.mse.nelder.exact,
                 zm.mse.sann  =  zm.mse.sann,
                 zm.mse.sann.exact  =  zm.mse.sann.exact,
                 zm.exact.custom  =  zm.exact.custom,
                 zm.exact.custom.exact  =  zm.exact.custom.exact,
                 zm.exact.nlm  =  zm.exact.nlm,
                 zm.exact.nlm.exact  =  zm.exact.nlm.exact,
                 zm.exact.nelder  =  zm.exact.nelder,
                 zm.exact.nelder.exact  =  zm.exact.nelder.exact,
                 zm.exact.sann  =  zm.exact.sann,
                 zm.exact.sann.exact  =  zm.exact.sann.exact,
                 fzm.chisq.nelder  =  fzm.chisq.nelder,
                 fzm.chisq.nelder.exact  =  fzm.chisq.nelder.exact,
                 fzm.chisq.sann  =  fzm.chisq.sann,
                 fzm.chisq.sann.exact  =  fzm.chisq.sann.exact,
                 fzm.linear.custom  =  fzm.linear.custom,
                 fzm.linear.custom.exact  =  fzm.linear.custom.exact,
                 fzm.linear.nelder  =  fzm.linear.nelder,
                 fzm.linear.nelder.exact  =  fzm.linear.nelder.exact,
                 fzm.linear.sann  =  fzm.linear.sann,
                 fzm.linear.sann.exact  =  fzm.linear.sann.exact,
                 fzm.smooth.linear.custom  =  fzm.smooth.linear.custom,
                 fzm.smooth.linear.custom.exact  =  fzm.smooth.linear.custom.exact,
                 fzm.smooth.linear.nelder  =  fzm.smooth.linear.nelder,
                 fzm.smooth.linear.nelder.exact  =  fzm.smooth.linear.nelder.exact,
                 fzm.smooth.linear.sann  =  fzm.smooth.linear.sann,
                 fzm.smooth.linear.sann.exact  =  fzm.smooth.linear.sann.exact,
                 fzm.mse.nelder  =  fzm.mse.nelder,
                 fzm.mse.nelder.exact  =  fzm.mse.nelder.exact,
                 fzm.mse.sann  =  fzm.mse.sann,
                 fzm.mse.sann.exact  =  fzm.mse.sann.exact,
                 fzm.exact.custom  =  fzm.exact.custom,
                 fzm.exact.custom.exact  =  fzm.exact.custom.exact,
                 fzm.exact.nelder  =  fzm.exact.nelder,
                 fzm.exact.nelder.exact  =  fzm.exact.nelder.exact,
                 fzm.exact.sann  =  fzm.exact.sann,
                 fzm.exact.sann.exact  =  fzm.exact.sann.exact,
                 gigp.chisq.custom  =  gigp.chisq.custom,
                 gigp.chisq.custom.exact  =  gigp.chisq.custom.exact,
                 gigp.chisq.nelder  =  gigp.chisq.nelder,
                 gigp.chisq.nelder.exact  =  gigp.chisq.nelder.exact,
                 gigp.chisq.sann  =  gigp.chisq.sann,
                 gigp.chisq.sann.exact  =  gigp.chisq.sann.exact,
                 gigp.linear.custom  =  gigp.linear.custom,
                 gigp.linear.custom.exact  =  gigp.linear.custom.exact,
                 gigp.linear.nlm  =  gigp.linear.nlm,
                 gigp.linear.nlm.exact  =  gigp.linear.nlm.exact,
                 gigp.linear.nelder  =  gigp.linear.nelder,
                 gigp.linear.nelder.exact  =  gigp.linear.nelder.exact,
                 gigp.linear.sann  =  gigp.linear.sann,
                 gigp.linear.sann.exact  =  gigp.linear.sann.exact,
                 gigp.smooth.linear.custom  =  gigp.smooth.linear.custom,
                 gigp.smooth.linear.custom.exact  =  gigp.smooth.linear.custom.exact,
                 gigp.smooth.linear.nlm  =  gigp.smooth.linear.nlm,
                 gigp.smooth.linear.nlm.exact  =  gigp.smooth.linear.nlm.exact,
                 gigp.smooth.linear.nelder  =  gigp.smooth.linear.nelder,
                 gigp.smooth.linear.nelder.exact  =  gigp.smooth.linear.nelder.exact,
                 gigp.smooth.linear.sann  =  gigp.smooth.linear.sann,
                 gigp.smooth.linear.sann.exact  =  gigp.smooth.linear.sann.exact,
                 gigp.mse.sann  =  gigp.mse.sann,
                 gigp.mse.sann.exact  =  gigp.mse.sann.exact,
                 gigp.exact.sann  =  gigp.exact.sann,
                 gigp.exact.sann.exact  =  gigp.exact.sann.exact)
  
  modelNames <- c("zm.chisq.custom",
                  "zm.chisq.custom.exact",
#                  "zm.chisq.nlm",
                  "zm.chisq.nlm.exact",
                  "zm.chisq.nelder",
                  "zm.chisq.nelder.exact",
                  "zm.chisq.sann",
                  "zm.chisq.sann.exact",
                  "zm.linear.custom",
                  "zm.linear.custom.exact",
                  "zm.linear.nlm",
                  "zm.linear.nelder",
                  "zm.linear.nelder.exact",
                  "zm.linear.sann",
                  "zm.linear.sann.exact",
                  "zm.smooth.linear.custom",
                  "zm.smooth.linear.custom.exact",
                  "zm.smooth.linear.nlm",
                  "zm.smooth.linear.nlm.exact",
                  "zm.smooth.linear.nelder",
                  "zm.smooth.linear.nelder.exact",
                  "zm.smooth.linear.sann",
                  "zm.smooth.linear.sann.exact",
                  "zm.mse.custom",
                  "zm.mse.custom.exact",
                  "zm.mse.nlm",
                  "zm.mse.nlm.exact",
                  "zm.mse.nelder",
                  "zm.mse.nelder.exact",
                  "zm.mse.sann",
                  "zm.mse.sann.exact",
                  "zm.exact.custom",
                  "zm.exact.custom.exact",
                  "zm.exact.nlm",
                  "zm.exact.nlm.exact",
                  "zm.exact.nelder",
                  "zm.exact.nelder.exact",
                  "zm.exact.sann",
                  "zm.exact.sann.exact",
                  "fzm.chisq.nelder",
                  "fzm.chisq.nelder.exact",
                  "fzm.chisq.sann",
                  "fzm.chisq.sann.exact",
                  "fzm.linear.custom",
                  "fzm.linear.custom.exact",
                  "fzm.linear.nelder",
                  "fzm.linear.nelder.exact",
                  "fzm.linear.sann",
                  "fzm.linear.sann.exact",
                  "fzm.smooth.linear.custom",
                  "fzm.smooth.linear.custom.exact",
                  "fzm.smooth.linear.nelder",
                  "fzm.smooth.linear.nelder.exact",
                  "fzm.smooth.linear.sann",
                  "fzm.smooth.linear.sann.exact",
                  "fzm.mse.nelder",
                  "fzm.mse.nelder.exact",
                  "fzm.mse.sann",
                  "fzm.mse.sann.exact",
                  "fzm.exact.custom",
                  "fzm.exact.custom.exact",
                  "fzm.exact.nelder",
                  "fzm.exact.nelder.exact",
                  "fzm.exact.sann",
                  "fzm.exact.sann.exact",
                  "gigp.chisq.custom",
                  "gigp.chisq.custom.exact",
                  "gigp.chisq.nelder",
                  "gigp.chisq.nelder.exact",
                  "gigp.chisq.sann",
                  "gigp.chisq.sann.exact",
                  "gigp.linear.custom",
                  "gigp.linear.custom.exact",
                  "gigp.linear.nlm",
                  "gigp.linear.nlm.exact",
                  "gigp.linear.nelder",
                  "gigp.linear.nelder.exact",
                  "gigp.linear.sann",
                  "gigp.linear.sann.exact",
                  "gigp.smooth.linear.custom",
                  "gigp.smooth.linear.custom.exact",
                  "gigp.smooth.linear.nlm",
                  "gigp.smooth.linear.nlm.exact",
                  "gigp.smooth.linear.nelder",
                  "gigp.smooth.linear.nelder.exact",
                  "gigp.smooth.linear.sann",
                  "gigp.smooth.linear.sann.exact",
                  "gigp.mse.sann",
                  "gigp.mse.sann.exact",
                  "gigp.exact.sann",
                  "gigp.exact.sann.exact")
  
  p <- c(zm.chisq.custom$gof$p,
         zm.chisq.custom.exact$gof$p,
#         zm.chisq.nlm$gof$p,
         zm.chisq.nlm.exact$gof$p,
         zm.chisq.nelder$gof$p,
         zm.chisq.nelder.exact$gof$p,
         zm.chisq.sann$gof$p,
         zm.chisq.sann.exact$gof$p,
         zm.linear.custom$gof$p,
         zm.linear.custom.exact$gof$p,
         zm.linear.nlm$gof$p,
         zm.linear.nelder$gof$p,
         zm.linear.nelder.exact$gof$p,
         zm.linear.sann$gof$p,
         zm.linear.sann.exact$gof$p,
         zm.smooth.linear.custom$gof$p,
         zm.smooth.linear.custom.exact$gof$p,
         zm.smooth.linear.nlm$gof$p,
         zm.smooth.linear.nlm.exact$gof$p,
         zm.smooth.linear.nelder$gof$p,
         zm.smooth.linear.nelder.exact$gof$p,
         zm.smooth.linear.sann$gof$p,
         zm.smooth.linear.sann.exact$gof$p,
         zm.mse.custom$gof$p,
         zm.mse.custom.exact$gof$p,
         zm.mse.nlm$gof$p,
         zm.mse.nlm.exact$gof$p,
         zm.mse.nelder$gof$p,
         zm.mse.nelder.exact$gof$p,
         zm.mse.sann$gof$p,
         zm.mse.sann.exact$gof$p,
         zm.exact.custom$gof$p,
         zm.exact.custom.exact$gof$p,
         zm.exact.nlm$gof$p,
         zm.exact.nlm.exact$gof$p,
         zm.exact.nelder$gof$p,
         zm.exact.nelder.exact$gof$p,
         zm.exact.sann$gof$p,
         zm.exact.sann.exact$gof$p,
         fzm.chisq.nelder$gof$p,
         fzm.chisq.nelder.exact$gof$p,
         fzm.chisq.sann$gof$p,
         fzm.chisq.sann.exact$gof$p,
         fzm.linear.custom$gof$p,
         fzm.linear.custom.exact$gof$p,
         fzm.linear.nelder$gof$p,
         fzm.linear.nelder.exact$gof$p,
         fzm.linear.sann$gof$p,
         fzm.linear.sann.exact$gof$p,
         fzm.smooth.linear.custom$gof$p,
         fzm.smooth.linear.custom.exact$gof$p,
         fzm.smooth.linear.nelder$gof$p,
         fzm.smooth.linear.nelder.exact$gof$p,
         fzm.smooth.linear.sann$gof$p,
         fzm.smooth.linear.sann.exact$gof$p,
         fzm.mse.nelder$gof$p,
         fzm.mse.nelder.exact$gof$p,
         fzm.mse.sann$gof$p,
         fzm.mse.sann.exact$gof$p,
         fzm.exact.custom$gof$p,
         fzm.exact.custom.exact$gof$p,
         fzm.exact.nelder$gof$p,
         fzm.exact.nelder.exact$gof$p,
         fzm.exact.sann$gof$p,
         fzm.exact.sann.exact$gof$p,
         gigp.chisq.custom$gof$p,
         gigp.chisq.custom.exact$gof$p,
         gigp.chisq.nelder$gof$p,
         gigp.chisq.nelder.exact$gof$p,
         gigp.chisq.sann$gof$p,
         gigp.chisq.sann.exact$gof$p,
         gigp.linear.custom$gof$p,
         gigp.linear.custom.exact$gof$p,
         gigp.linear.nlm$gof$p,
         gigp.linear.nlm.exact$gof$p,
         gigp.linear.nelder$gof$p,
         gigp.linear.nelder.exact$gof$p,
         gigp.linear.sann$gof$p,
         gigp.linear.sann.exact$gof$p,
         gigp.smooth.linear.custom$gof$p,
         gigp.smooth.linear.custom.exact$gof$p,
         gigp.smooth.linear.nlm$gof$p,
         gigp.smooth.linear.nlm.exact$gof$p,
         gigp.smooth.linear.nelder$gof$p,
         gigp.smooth.linear.nelder.exact$gof$p,
         gigp.smooth.linear.sann$gof$p,
         gigp.smooth.linear.sann.exact$gof$p,
         gigp.mse.sann$gof$p,
         gigp.mse.sann.exact$gof$p,
         gigp.exact.sann$gof$p,
         gigp.exact.sann.exact$gof$p)
  
  X2 <- c(zm.chisq.custom$gof$X2,
          zm.chisq.custom.exact$gof$X2,
          zm.chisq.nlm$gof$X2,
          zm.chisq.nlm.exact$gof$X2,
          zm.chisq.nelder$gof$X2,
          zm.chisq.nelder.exact$gof$X2,
          zm.chisq.sann$gof$X2,
          zm.chisq.sann.exact$gof$X2,
          zm.linear.custom$gof$X2,
          zm.linear.custom.exact$gof$X2,
          zm.linear.nlm$gof$X2,
          zm.linear.nelder$gof$X2,
          zm.linear.nelder.exact$gof$X2,
          zm.linear.sann$gof$X2,
          zm.linear.sann.exact$gof$X2,
          zm.smooth.linear.custom$gof$X2,
          zm.smooth.linear.custom.exact$gof$X2,
          zm.smooth.linear.nlm$gof$X2,
          zm.smooth.linear.nlm.exact$gof$X2,
          zm.smooth.linear.nelder$gof$X2,
          zm.smooth.linear.nelder.exact$gof$X2,
          zm.smooth.linear.sann$gof$X2,
          zm.smooth.linear.sann.exact$gof$X2,
          zm.mse.custom$gof$X2,
          zm.mse.custom.exact$gof$X2,
          zm.mse.nlm$gof$X2,
          zm.mse.nlm.exact$gof$X2,
          zm.mse.nelder$gof$X2,
          zm.mse.nelder.exact$gof$X2,
          zm.mse.sann$gof$X2,
          zm.mse.sann.exact$gof$X2,
          zm.exact.custom$gof$X2,
          zm.exact.custom.exact$gof$X2,
          zm.exact.nlm$gof$X2,
          zm.exact.nlm.exact$gof$X2,
          zm.exact.nelder$gof$X2,
          zm.exact.nelder.exact$gof$X2,
          zm.exact.sann$gof$X2,
          zm.exact.sann.exact$gof$X2,
          fzm.chisq.nelder$gof$X2,
          fzm.chisq.nelder.exact$gof$X2,
          fzm.chisq.sann$gof$X2,
          fzm.chisq.sann.exact$gof$X2,
          fzm.linear.custom$gof$X2,
          fzm.linear.custom.exact$gof$X2,
          fzm.linear.nelder$gof$X2,
          fzm.linear.nelder.exact$gof$X2,
          fzm.linear.sann$gof$X2,
          fzm.linear.sann.exact$gof$X2,
          fzm.smooth.linear.custom$gof$X2,
          fzm.smooth.linear.custom.exact$gof$X2,
          fzm.smooth.linear.nelder$gof$X2,
          fzm.smooth.linear.nelder.exact$gof$X2,
          fzm.smooth.linear.sann$gof$X2,
          fzm.smooth.linear.sann.exact$gof$X2,
          fzm.mse.nelder$gof$X2,
          fzm.mse.nelder.exact$gof$X2,
          fzm.mse.sann$gof$X2,
          fzm.mse.sann.exact$gof$X2,
          fzm.exact.custom$gof$X2,
          fzm.exact.custom.exact$gof$X2,
          fzm.exact.nelder$gof$X2,
          fzm.exact.nelder.exact$gof$X2,
          fzm.exact.sann$gof$X2,
          fzm.exact.sann.exact$gof$X2,
          gigp.chisq.custom$gof$X2,
          gigp.chisq.custom.exact$gof$X2,
          gigp.chisq.nelder$gof$X2,
          gigp.chisq.nelder.exact$gof$X2,
          gigp.chisq.sann$gof$X2,
          gigp.chisq.sann.exact$gof$X2,
          gigp.linear.custom$gof$X2,
          gigp.linear.custom.exact$gof$X2,
          gigp.linear.nlm$gof$X2,
          gigp.linear.nlm.exact$gof$X2,
          gigp.linear.nelder$gof$X2,
          gigp.linear.nelder.exact$gof$X2,
          gigp.linear.sann$gof$X2,
          gigp.linear.sann.exact$gof$X2,
          gigp.smooth.linear.custom$gof$X2,
          gigp.smooth.linear.custom.exact$gof$X2,
          gigp.smooth.linear.nlm$gof$X2,
          gigp.smooth.linear.nlm.exact$gof$X2,
          gigp.smooth.linear.nelder$gof$X2,
          gigp.smooth.linear.nelder.exact$gof$X2,
          gigp.smooth.linear.sann$gof$X2,
          gigp.smooth.linear.sann.exact$gof$X2,
          gigp.mse.sann$gof$X2,
          gigp.mse.sann.exact$gof$X2,
          gigp.exact.sann$gof$X2,
          gigp.exact.sann.exact$gof$X2)
  
  fitSummaries <- data.frame(modelNames, p, X2)
  
  results <- list(
    modelNames = modelNames,
    models = models,
    fitSummaries = fitSummaries)
  
  return(results)
}

#==============================================================================#
#                             evaluateZipf                                     #
#==============================================================================#
#'  evaluateZipf
#' 
#' This function takes the metadata for document as its parameter then trains 
#' and evaluates a series of LNRE models for goodness of fit.
#' 
#' @param document  - meta data for a document being analyzed
#' @return fitAssessment - a data frame containing goodness of fit estimates
#' @author John James
#' @export
evaluateZipf <- function(document) {
  
  # Calculate diversity measures
  startTime <- Sys.time()
  message(paste('\nEvaluating zipf model parameters at', startTime))
  
  # Load Document
#  document <- readFile(document)
  
  # Create frequency spectrum object and term frequency list
  message('creating SPC')
  docSpc    <- text2spc.fnc(document)
  docSpc$m  <- as.integer(docSpc$m)
  docSpc$Vm <- as.integer(docSpc$Vm)
  
  # Fit LNRE models
  message('fitting models...')
  fitData <- fitLNRE(docSpc)
  
  # Log and Return results
  output <- list()
  output$directory <- schema$directories$logsDir
  output$fileName  <- paste0(sub('\\..*', '', 'evaluate-zipf'), 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'zipfEvaluation'
  output$data  <- fitData
  saveObject(output)
  logR('evaluateZipf', startTime, output$directory, output$fileName)
  
  endTime <- Sys.time()
  message(paste('Lexical Diversity Sample Size Analysis Complete at', endTime))
  message(paste('Elapsed time', difftime(endTime, startTime, units = 'auto')))
}
## ---- end
#document <- readFile(clean$documents[[1]])
#evaluateZipf(document)