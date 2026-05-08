#' Taxonomy Codes for Obstetricians and Gynecologists
#'
#' This dataset contains taxonomy codes for Obstetricians and Gynecologists among other healthcare providers.
#'
#' @return A tibble mapping NUCC taxonomy codes to descriptive provider
#'   types relevant to obstetrics and gynecology research.
#' @source <https://www.nucc.org/images/stories/PDF/taxonomy_23_0.pdf>
#' @format A data frame with three columns:
#'   \describe{
#'     \item{Code}{NUCC (National Uniform Claim Committee) taxonomy code.}
#'     \item{Classification}{Provider classification (e.g., Allopathic & Osteopathic Physicians).}
#'     \item{Specialization}{Provider specialization within the classification.}
#'   }
#'
#' @examples
#' \donttest{
#' # Load the taxonomy dataset
#' data(taxonomy)
#'
#' # Explore the dataset
#' print(taxonomy)
#' }
#' @family datasets
#' @name taxonomy
"taxonomy"
