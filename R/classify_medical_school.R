#' Classify medical school as US_MD, US_DO, CAN_MD, or IMG
#'
#' @name mysterycall_classify_medical_school
NULL

# -- Built-in pattern lists ----------------------------------------------------

# DO-granting schools  --  all contain one of these substrings (case-insensitive)
.mc_do_patterns <- c(
  "osteopathic", "college of osteo",
  "touro", "pcom", "noorda", "atsu", "msucom",
  "kcumb", "lecom", "wvsom", "unecom", "nsucom",
  "azcom", "dmu", "ou-hsc com",
  "burrell college", "campbell university school of osteopathic",
  "lake erie", "liberty university college of osteopathic",
  "lincoln memorial debusk", "marengo college of osteopathic",
  "midwestern university", "ohio university heritage",
  "pacific northwest university", "rocky vista",
  "sam houston", "rowan-shrs", "st. matthias"
)

# Canadian medical schools (subset of most common)
.mc_canadian_patterns <- c(
  "dalhousie", "laval", "mcgill", "mcmaster", "memorial university",
  "university of alberta", "university of british columbia",
  "university of calgary", "university of manitoba",
  "university of montreal", "universite de montreal",
  "universite de sherbrooke", "university of ottawa",
  "university of saskatchewan", "university of toronto",
  "university of western ontario", "western university",
  "queen's university", "queens university", "northern ontario"
)

# International indicators  --  country names, known IMG schools, Spanish prefix
.mc_img_patterns <- c(
  "mexico", "india", "china", "pakistan", "egypt", "philippines",
  "grenada", "ross university", "st\\. george", "\\bsgu\\b",
  "american university of the caribbean", "saba university",
  "\\bcaribbean\\b", "dominica", "universidad", "foreign",
  "international", "manila", "cebu", "iran", "syria", "iraq",
  "korea", "japan", "taiwan", "vietnam", "thailand",
  "nigeria", "ghana", "sudan", "ethiopia", "kenya",
  "brazil", "argentina", "colombia", "peru",
  "united kingdom", "\\buk\\b medical", "ireland", "australia",
  "new zealand", "south africa", "israel", "turkey",
  "poland", "romania", "ukraine", "russia", "czech"
)


#' Classify a medical school name as US_MD, US_DO, CAN_MD, or IMG
#'
#' Uses pattern matching against curated lists of osteopathic programs, Canadian
#' medical schools, and international medical school indicators. Classification
#' priority: DO > CAN_MD > IMG > US_MD. Schools that do not match any pattern
#' are assumed to be US allopathic (US_MD).
#'
#' @param school_name Character vector of medical school names as they appear
#'   in CMS Physician Compare or NPPES data.
#' @param na_label Character scalar returned for `NA` or empty inputs.
#'   Default `"Unknown"`.
#'
#' @return Character vector the same length as `school_name` with values
#'   `"US_MD"`, `"US_DO"`, `"CAN_MD"`, `"IMG"`, or `na_label`.
#'
#' @family provider characteristics
#' @export
#'
#' @examples
#' schools <- c(
#'   "University of Colorado School of Medicine",
#'   "Philadelphia College of Osteopathic Medicine",
#'   "McGill University Faculty of Medicine",
#'   "Ross University School of Medicine",
#'   NA
#' )
#' mysterycall_classify_medical_school(schools)
mysterycall_classify_medical_school <- function(school_name,
                                                 na_label = "Unknown") {

  if (!is.character(school_name)) {
    stop("`school_name` must be a character vector.", call. = FALSE)
  }
  if (!is.character(na_label) || length(na_label) != 1L) {
    stop("`na_label` must be a single character string.", call. = FALSE)
  }

  .classify_one <- function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return(na_label)
    sl <- tolower(s)
    if (any(vapply(.mc_do_patterns,      function(p) grepl(p, sl, perl = TRUE), logical(1L)))) return("US_DO")
    if (any(vapply(.mc_canadian_patterns, function(p) grepl(p, sl, perl = TRUE), logical(1L)))) return("CAN_MD")
    if (any(vapply(.mc_img_patterns,      function(p) grepl(p, sl, perl = TRUE), logical(1L)))) return("IMG")
    "US_MD"
  }

  vapply(school_name, .classify_one, character(1L), USE.NAMES = FALSE)
}
