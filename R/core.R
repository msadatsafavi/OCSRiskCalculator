outcomes <- c('Osteoporosis'="ost",
            'Fractures'="frc",
            'Metabolic syndrome'='mbs',
            'Hypertension'="htn",
            'Obesity'="obs",
            'Type 2 diabetes'="dm2",
            'Dyslipidemia'="dlp",
            'Avascular necrosis'="avn",
            'Gastrointestinal ulcers/bleeds'="gib",
            'Tuberculosis'="tbs",
            'Cataracts'="ctc",
            'Glaucoma'="glc")




evidence <- list(
  ocs_coefficients = list(
    ost=c(cur_ocs_low=log(1.048), cur_ocs_high=log(1.442), hist_ocs_low_years=log(1.089), hist_ocs_high_years=log(1.138)),
    frc=c(cur_ocs_low=log(1.076), cur_ocs_high=log(1.208), hist_ocs_low_years=log(1.075), hist_ocs_high_years=log(1.072)),
    mbs=c(cur_ocs_low=log(0.868), cur_ocs_high=log(0.842), hist_ocs_low_years=log(1.038), hist_ocs_high_years=log(0.920)),
    htn=c(cur_ocs_low=log(1.056), cur_ocs_high=log(1.324), hist_ocs_low_years=log(1.063), hist_ocs_high_years=log(1.077)),
    obs=c(cur_ocs_low=log(1.022), cur_ocs_high=log(1.280), hist_ocs_low_years=log(1.071), hist_ocs_high_years=log(1.063)),
    dm2=c(cur_ocs_low=log(0.978), cur_ocs_high=log(1.299), hist_ocs_low_years=log(1.039), hist_ocs_high_years=log(1.031)),
    dlp=c(cur_ocs_low=log(1.035), cur_ocs_high=log(1.037), hist_ocs_low_years=log(1.040), hist_ocs_high_years=log(1.046)),
    avn=c(cur_ocs_low=log(1.077), cur_ocs_high=log(1.462), hist_ocs_low_years=log(1.013), hist_ocs_high_years=log(1.292)),
    gib=c(cur_ocs_low=log(1.070), cur_ocs_high=log(1.330), hist_ocs_low_years=log(1.035), hist_ocs_high_years=log(0.966)),
    tbs=c(cur_ocs_low=log(0.915), cur_ocs_high=log(1.151), hist_ocs_low_years=log(1.030), hist_ocs_high_years=log(1.126)),
    ctc=c(cur_ocs_low=log(0.943), cur_ocs_high=log(1.256), hist_ocs_low_years=log(1.061), hist_ocs_high_years=log(1.026)),
    glc=c(cur_ocs_low=log(0.921), cur_ocs_high=log(1.040), hist_ocs_low_years=log(1.009), hist_ocs_high_years=log(0.959))
  )
)




#' @export
get_evidence <- function()
{
   evidence
}



#' @export
get_outcomes <- function()
{
  outcomes
}


#' @export
calculate_risk <- function(profile, outcome)
{
  coeffs <- evidence$ocs_coefficients[[outcome]]
  profile <- as.list(profile)
  pf <- c(cur_ocs_low=profile$cur_ocs*(profile$ocs_intensity==0),
          cur_ocs_high=profile$cur_ocs*(profile$ocs_intensity==1),
          hist_ocs_low_years=profile$ocs_year*(profile$ocs_intensity==0),
          hist_ocs_high_years=profile$ocs_year*(profile$ocs_intensity==1))

  exp(sum(pf[names(coeffs)]*coeffs))
}
