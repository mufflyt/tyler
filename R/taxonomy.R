#' Taxonomy Codes for Obstetricians and Gynecologists
#'
#' This dataset contains taxonomy codes for Obstetricians and Gynecologists among other healthcare providers.
#' @source <https://www.nucc.org/images/stories/PDF/taxonomy_23_0.pdf>
#' @format A data frame with two columns:
#'   \describe{
#'     \item{NUCC Code}{NUCC (National Uniform Claim Committee) code for healthcare providers.}
#'     \item{Provider Type}{The type of healthcare provider corresponding to the NUCC code.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Load the taxonomy dataset
#' data(taxonomy)
#'
#' # Explore the dataset
#' head(taxonomy)
#' }
#' @name taxonomy
"taxonomy"
