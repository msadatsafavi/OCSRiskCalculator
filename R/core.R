outcomes <- c('Fractures'="ost", 'Type 2 diabetes'="dm2", 'Hypertension'="htn", 'Cataracts'="ctc", 'Dyslipidemia'="dlp")

evidence <- list(
  ocs_coefficients = list(
    frc=c(cur_ocs_low=log(1.048), cur_ocs_high=log(1.442), hist_ocs_low_years=log(1.089), hist_ocs_high_years=log(1.138)),
    dm2=c(cur_ocs_low=log(1.048), cur_ocs_high=log(1.442), hist_ocs_low_years=log(1.089), hist_ocs_high_years=log(1.138)),
    htn=c(cur_ocs_low=log(1.048), cur_ocs_high=log(1.442), hist_ocs_low_years=log(1.089), hist_ocs_high_years=log(1.138)),
    ctc=c(cur_ocs_low=log(1.048), cur_ocs_high=log(1.442), hist_ocs_low_years=log(1.089), hist_ocs_high_years=log(1.138)),
    dlp=c(cur_ocs_low=log(1.048), cur_ocs_high=log(1.442), hist_ocs_low_years=log(1.089), hist_ocs_high_years=log(1.138))
  ),

 ctc_prevalence_by_sex_age_raw=cbind(
   age=c(40,45,50,55,60,65,70,75,80),
   male=c(0.42633425,1.363140044,2.695579389,5.61113219,9.05482392,16.32278021,30.05728866,51.57442458,67.55008414)/100,
   female=c(1.218035792,2.154841587,4.410908596,8.118018125,13.27573573,21.99560343,38.89489065,61.337392,74.27831633)/100
   ),

 frc_prevalence_by_sex_age_raw=list(
   male=cbind(
     age=c(40,45.096502702569296,49.97692326677691,55.06108547029642,60.07823876397561,65.0953920576548,70.11402621121997,75.1343880346521,80.09193671792086,85.12093689068783,90.15437964311276),
     incidence=c(0.07834736,0.087955179,0.10195819,0.11885184,0.140124886,0.161397932,0.191413974,0.23163018,0.300997606,0.392214626,0.509660636)),
   female=cbind(
    age=c(40,45.02381716316608,50.10649850679961,55.05516203075252,60.207814003998315,65.2259545376015,70.443023915887,75.13685613446206,80.1005750672557,85.20818421896979,90.24680998099561),
    incidence=c(0.05211837,0.058819755,0.066970407,0.083879853,0.105137103,0.132238813,0.179764543,0.246201841,0.35199842,0.507322852,0.655369351)
 ))

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

  evidence$frc_prevalence_by_sex_age <- cbind(
    age=40:80,
    male=approx(evidence$frc_prevalence_by_sex_age_raw$male[,1],evidence$frc_prevalence_by_sex_age_raw$male[,2], xout=40:80)$y,
    female=approx(evidence$frc_prevalence_by_sex_age_raw$female[,1],evidence$frc_prevalence_by_sex_age_raw$female[,2], xout=40:80)$y)

  evidence
}



#' @export
get_outcomes <- function()
{
  outcomes
}


calculate_risk_ctc <- function(profile=c(female=1, age=65, cur_ocs=1, hist_ocs_low_years=0, hist_ocs_high_years=0), time_horizon=10)
{
  i <- which(evidence$ctc_prevalence_by_sex_age[,1]==profile['age'])
  j <- ifelse(profile['female'],'female','male')
  r0 <- evidence$ctc_prevalence_by_sex_age[i+time_horizon,j]-evidence$ctc_prevalence_by_sex_age[i,j]

  coeffs <- evidence$ocs_coefficients$ctc

  x1 <- c(cur_ocs_low=unname(profile['cur_ocs']==1)*1,
          cur_ocs_high=unname(profile['cur_ocs']==2)*1,
          hist_ocs_low_years=unname(profile['hist_ocs_low_years'])*1,
          hist_ocs_high_years=unname(profile['hist_ocs_high_years'])*1)

  or <- exp(sum(coeffs*x1))

  r1 <- (r0/(1-r0)*or)/(1+r0/(1-r0)*or)

  c(risk=unname(r1-r0), risk0=unname(r0), risk1=unname(r1))
}



calculate_risk_htn <- function(profile=c(female=1, age=65, cur_ocs=1, hist_ocs=2), time_horizon=10)
{
  i <- which(evidence$htn_incidence_by_sex_age[,1]==profile['age'])
  j <- ifelse(profile['female'],'female','male')
  H0 <- sum(evidence$htn_incidence_by_sex_age[i:(i+time_horizon-1),j])
  r0 <- 1-exp(-H0)

  coeffs <- evidence$ocs_coefficients$ctc

  x1 <- c(cur_ocs_low=unname(profile['cur_ocs']==1)*1,
          cur_ocs_high=unname(profile['cur_ocs']==2)*1,
          hist_ocs_low_years=unname(profile['hist_ocs_low_years'])*1,
          hist_ocs_high_years=unname(profile['hist_ocs_high_years'])*1)

  hr <- exp(sum(coeffs*x1))

  H1 <- H0*hr
  r1 <- 1-exp(-H1)

  c(risk=unname(r1-r0), risk0=unname(r0), risk1=unname(r1))
}


calculate_risk_htn <- function(profile=c(female=1, age=65, cur_ocs=1, hist_ocs_low_years=0, hist_ocs_high_years=0), time_horizon=10)
{
  i <- which(evidence$ctc_prevalence_by_sex_age[,1]==profile['age'])
  j <- ifelse(profile['female'],'female','male')
  r0 <- evidence$ctc_prevalence_by_sex_age[i+time_horizon,j]-evidence$ctc_prevalence_by_sex_age[i,j]

  coeffs <- evidence$ocs_coefficients$ctc

  x1 <- c(cur_ocs_low=unname(profile['cur_ocs']==1)*1,
          cur_ocs_high=unname(profile['cur_ocs']==2)*1,
          hist_ocs_low_years=unname(profile['hist_ocs_low_years'])*1,
          hist_ocs_high_years=unname(profile['hist_ocs_high_years'])*1)

  or <- exp(sum(coeffs*x1))

  r1 <- (r0/(1-r0)*or)/(1+r0/(1-r0)*or)

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
         cur_ocs_low=unname(profile['cur_ocs']==1)*1,
         cur_ocs_high=unname(profile['cur_ocs']==2)*1,
         hist_ocs_low_years=unname(profile['hist_ocs_low_years'])*1,
         hist_ocs_high_years=unname(profile['hist_ocs_high_years'])*1)

  x0[4:7] <- 0

  lin0 <- sum(coeffs*x0)
  h0 <- exp(lin0)
  r0 <- 1-exp(-h0*time_horizon)

  lin1 <- sum(coeffs*x1)
  h1 <- exp(lin1)
  r1 <- 1-exp(-h1*time_horizon)


  c(risk=r1-r0, risk0=r0, risk1=r1)
}
