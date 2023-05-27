outcomes <- c('Fractures'="frc", 'Type 2 diabetes'="dm2", 'Hypertension'="htn", 'Cataracts'="ctc", 'Dyslipidemia'="dlp")



evidence <- list(
  ocs_coefficients = list(
    frc=c(cur_ocs_low=log(1.048), cur_ocs_high=log(1.442), hist_ocs_low=log(1.089), hist_ocs_high=log(1.138)),
    dm2=c(cur_ocs_low=log(1.048), cur_ocs_high=log(1.442), hist_ocs_low=log(1.089), hist_ocs_high=log(1.138)),
    htn=c(cur_ocs_low=log(1.048), cur_ocs_high=log(1.442), hist_ocs_low=log(1.089), hist_ocs_high=log(1.138)),
    ctc=c(cur_ocs_low=log(1.048), cur_ocs_high=log(1.442), hist_ocs_low=log(1.089), hist_ocs_high=log(1.138)),
    dlp=c(cur_ocs_low=log(1.048), cur_ocs_high=log(1.442), hist_ocs_low=log(1.089), hist_ocs_high=log(1.138))
  ),

 ctc_prevalence_by_sex_age_raw=cbind(
   age=c(40,45,50,55,60,65,70,75,80),
   male=c(0.42633425,1.363140044,2.695579389,5.61113219,9.05482392,16.32278021,30.05728866,51.57442458,67.55008414)/100,
   female=c(1.218035792,2.154841587,4.410908596,8.118018125,13.27573573,21.99560343,38.89489065,61.337392,74.27831633)/100
   )
)


#' @export
get_evidence <- function()
{
  evidence$ctc_prevalence_by_sex_age <- cbind(
    age=40:80,
    male=approx(evidence$ctc_prevalence_by_sex_age_raw[,1],evidence$ctc_prevalence_by_sex_age_raw[,2], xout=40:80)$y,
    female=approx(evidence$ctc_prevalence_by_sex_age_raw[,1],evidence$ctc_prevalence_by_sex_age_raw[,3], xout=40:80)$y)

  evidence$htn_incidence_by_sex_age <- cbind(
    age=40:80,
    male=(((40:80)-20)/80)^2/10,
    female=(((40:80)-20)/80)^2/10)

  evidence
}



#' @export
get_outcomes <- function()
{
  outcomes
}


calculate_risk_ctc <- function(profile=c(female=1, age=65, cur_ocs=1, hist_ocs=2), time_horizon=10, evidence=get_evidence())
{
  i <- which(evidence$ctc_prevalence_by_sex_age[,1]==profile['age'])
  j <- ifelse(profile['female'],'female','male')
  r0 <- evidence$ctc_prevalence_by_sex_age[i+time_horizon,j]-evidence$ctc_prevalence_by_sex_age[i,j]

  coeffs <- evidence$ocs_coefficients$ctc

  x1 <- c(cur_ocs_low=unname(profile['cur_ocs']==1)*1,
                cur_ocs_high=unname(profile['cur_ocs']==2)*1,
                hist_ocs_low=unname(profile['hist_ocs']==1)*1,
                hist_ocs_high=unname(profile['hist_ocs']==2)*1)
  or <- exp(sum(coeffs*x1))

  r1 <- (r0/(1-r0)*or)/(1+r0/(1-r0)*or)

  c(risk=unname(r1-r0), risk0=unname(r0), risk1=unname(r1))
}



calculate_risk_htn <- function(profile=c(female=1, age=65, cur_ocs=1, hist_ocs=2), time_horizon=10, evidence=get_evidence())
{
  i <- which(evidence$htn_incidence_by_sex_age[,1]==profile['age'])
  j <- ifelse(profile['female'],'female','male')
  H0 <- sum(evidence$htn_incidence_by_sex_age[i:(i+time_horizon-1),j])
  r0 <- 1-exp(-H0)

  coeffs <- evidence$ocs_coefficients$ctc

  x1 <- c(cur_ocs_low=unname(profile['cur_ocs']==1)*1,
          cur_ocs_high=unname(profile['cur_ocs']==2)*1,
          hist_ocs_low=unname(profile['hist_ocs']==1)*1,
          hist_ocs_high=unname(profile['hist_ocs']==2)*1)

  hr <- exp(sum(coeffs*x1))

  H1 <- H0*hr
  r1 <- 1-exp(-H1)

  c(risk=unname(r1-r0), risk0=unname(r0), risk1=unname(r1))
}


#' @export
calculate_risk <- function(profile=c(female=1, age=65, cur_ocs=1, hist_ocs=2), outcome, time_horizon=10)
{
  if(outcome=='ctc') return(calculate_risk_ctc(profile=profile,time_horizon=time_horizon))
  if(outcome=='htn') return(calculate_risk_htn(profile=profile,time_horizon=time_horizon))

  covars <- c(1,profile)
  coeffs <- c(int=-5,female=0.1,age=0.01,evidence$ocs_coefficients[[outcome]])

  x0 <- x1 <- c(int=1, female=unname(profile['female']),
         age=unname(profile['age']),
         cur_ocs_low=unname(profile['cur_ocs']==1),
         cur_ocs_high=unname(profile['cur_ocs']==2),
         hist_ocs_low=unname(profile['hist_ocs']==1),
         hist_ocs_high=unname(profile['hist_ocs']==2))

  x0[4:7] <- 0

  lin0 <- sum(coeffs*x0)
  h0 <- exp(lin0)
  r0 <- 1-exp(-h0*time_horizon)

  lin1 <- sum(coeffs*x1)
  h1 <- exp(lin1)
  r1 <- 1-exp(-h1*time_horizon)


  c(risk=r1-r0, risk0=r0, risk1=r1)
}
