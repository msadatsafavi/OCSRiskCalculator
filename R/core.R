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



#' @export
terms_of_use <- function()
{
  "The Oral Corticosteroid Risk Calculator (“Calculator”) is a web-based tool that provides an estimate of the long-term risk of exposure to oral corticosteroids. The Calculator was developed by NAPTIA Consultation, with support from GlaxoSmithKline. You (the “End User”) is granted access to use the Calculator only on the condition that the End User agrees to the terms of use set forth below.

The End User acknowledges that the Calculator, including any and all improvements, variations, updates, upgrades, modifications, enhancements or derivative works (the “Calculator”) is owned by NAPTIA COnsultation and that NAPTIA Consultation specifically retains the right to grant licenses of the Calculator to other persons.

The Calculator is provided to the End User for non-commercial purposes only.

CLINICAL USE ADVISORY - THE Calculator IS AN EXPERIMENTAL TECHNOLOGY AND HAS NOT BEEN TESTED OR APPROVED FOR USE IN CONNECTION WITH ANY HUMAN CLINICAL USE. THE Calculator AND ANY CONTENT OR OTHER INFORMATION CONTAINED IN THE Calculator, ACCESSED THROUGH THE Calculator OR PROVIDED BY THE Calculator IS FOR INFORMATIONAL PURPOSES ONLY AND IN NO WAY IS INTENDED TO SERVE AS A SUBSTITUTE FOR PROFESSIONAL MEDICAL ADVICE OR INTENDED TO PROVIDE ANY MEDICAL OR CLINICAL JUDGMENT OR INDIVIDUALIZED PATIENT CARE. THE END USER SHOULD SEEK PROFESSIONAL MEDICAL ADVICE IN EVALUATING THE INFORMATION PROVIDED WITHIN THE Calculator. UBC ALSO RECOMMENDS THAT THE END USER CONSULT WITH MEDICAL PROFESSIONALS FOR HEALTH EVALUATION, DIAGNOSTIC OR TREATMENT DECISIONS.

DISCLAIMER OF WARRANTY – The End User acknowledges and agrees that the Calculator is being provided “as is” and that to the extent that the Calculator or any such updates, upgrades or enhancements to the Calculator are provided by or on behalf of UBC to the End User, the following disclaimers of warranty and limitations of liability shall apply in connection therewith. UBC does not: (a) warrant or guarantee that any content or other information contained in the Calculator or accessed through the Calculator or provided by the Calculator is accurate or error free; (b) undertake to provide the End User with any maintenance or support services related to the Calculator, including, without limitation, any updates, upgrades or enhancements thereto; or © assume any obligation to obtain and update any content or information in the Calculator or accessed through Calculator or provided by the Calculator

The End User further acknowledges and agrees that: (i) the Calculator is intended for use only as a research and educational tool; (ii) it is the sole responsibility of the End User to determine his/her application and use of the Calculator, (iii) the Calculator is not intended as a replacement for professional medical advice or a health professional’s medical and clinical judgment (iv) UBC will not be responsible for any errors, misstatements, inaccuracies or omissions regarding the Calculator or any delays in or interruptions in use of the Calculator, (v) UBC has no control of or responsibility for the End User’s use of the Calculator and has no knowledge of the specific or unique circumstances under which the Calculator or information provided by the Calculator may be used by the End User; and (vi) the End User assumes sole responsibility for any and all use of the Calculator including any use of any content or other information contained Calculator, accessed through the Calculator or provided by the Calculator. Furthermore, the End User acknowledges that the Calculator is being provided WITHOUT REPRESENTATION OR WARRANTY OF ANY KIND. BY WAY OF EXAMPLE AND NOT LIMITATION, UBC MAKES NO REPRESENTATIONS OR WARRANTIES OF COMMERCIAL UTILITY, TITLE, MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. UBC MAKES NO REPRESENTATION THAT THE USE OF THE Calculator WILL NOT INFRINGE ANY PATENT, COPYRIGHT, TRADEMARK OR OTHER PROPERTY OR PROPRIETARY RIGHTS.

LIMITATION OF LIABILITY - NAPTIA Consultation will not be liable to the End User or any other person or entity (including but not limited to persons treated by the End User or on behalf of the End User) for any liability, loss or damages caused or alleged to have been caused, either directly or indirectly, by the use of Calculator. Without limitation, in no event will the NAPTIA Consultation be liable for any tort, personal injury, medical malpractice, misdiagnosis, death, product liability, loss of profit or data, or for special, indirect, consequential, incidental or punitive damages, however caused and regardless of the theory of liability, arising out of or related to the use of or inability to use the Calculator, even if UBC has been advised of the possibility of such loss or damages.

INDEMNITY - TO THE FULLEST EXTENT PERMITTED BY LAW, THE END USER AGREES TO INDEMNIFY, DEFEND AND HOLD HARMLESS NAPTIA CONSULTATION AND ITS EMPLOYERS, EMPLOYEES, AND AGENTS (COLLECTIVELY THE “INDEMNITEES”) FROM AND AGAINST ALL CLAIMS, ACTIONS LIABILITIES, LOSSES, EXPENSES, DAMAGES AND COSTS (INCLUDING, WITHOUT LIMITATION, REASONABLE LEGAL FEES) (“CLAIMS”) THAT MAY AT ANY TIME BE INCURRED BY ANY OF THEM BY REASON OF ANY CLAIMS, SUITS OR PROCEEDINGS ARISING FROM (A) THE END USER’S USE OF THE Calculator OR BY ANY OTHER PARTY AUTHORIZED BY THE END USER, AND (B) ANY BREACH BY END USER OF ANY REPRESENTATION, CONDITION OR TERM OF THIS LICENSE AGREEMENT. WITHOUT LIMITATION, THIS INDEMNITY SHALL INCLUDE ANY CLAIMS BROUGHT BY THIRD PARTIES AGAINST THE INDEMNITEES IN TORT, OR FOR PERSONAL INJURY, MEDICAL MALPRACTICE, MISDIAGNOSIS, DEATH, PRODUCT LIABILITY, LOSS OF PROFIT OR DATA, OR FOR SPECIAL, INDIRECT, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY, ARISING OUT OF OR RELATED TO THE END USER’S USE OF, OR INABILITY TO USE, Calculator GOVERNING LAW – This Agreement will be governed and interpreted according to the laws of British Columbia, Canada. The Licensee agrees that by Calculatoring the terms of this Agreement and using the Calculator, the Licensee has attorned to the exclusive jurisdiction of the courts of competent authority in the Province of British Columbia."
}



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
  ),

  no_ocs_py = 72063*2.9,

  no_ocs_n_events = list(
    ost=4100,
    mbs=531,
    htn=7818,
    obs=5039,
    dm2=3424,
    ctc=1500,
    avn=63,
    dlp=7322,
    gib=3290,
    frc=3459,
    tbs=2023,
    glc=1310
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
calculate_risk <- function(profile, outcome) #THIS IS RR
{
  evidence <- get_evidence()
  coeffs <- evidence$ocs_coefficients[[outcome]]
  profile <- as.list(profile)
  pf <- c(cur_ocs_low=profile$cur_ocs*(profile$ocs_intensity==0),
          cur_ocs_high=profile$cur_ocs*(profile$ocs_intensity==1),
          hist_ocs_low_years=profile$ocs_year*(profile$ocs_intensity==0),
          hist_ocs_high_years=profile$ocs_year*(profile$ocs_intensity==1))

  exp(sum(pf[names(coeffs)]*coeffs))
}




#' @export
calculate_baseline_risk <- function(profile, outcome, years=10)
{
  evidence <- get_evidence()
  rate <- unlist(evidence$no_ocs_n_events)[outcome]/evidence$no_ocs_py

  1-exp(-rate*years)
}



