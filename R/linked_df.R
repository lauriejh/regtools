#' Example linked individual-level data
#'
#' A simulated dataset including individual information such as unique ID, ICD-10 and diagnosis date (year), year of birth, and reason for immigration
#'
#' @format `linked_df`
#' A data frame with 30,024 rows and 3 columns:
#' \describe{
#'   \item{id}{Unique personal identifier}
#'   \item{code}{ICD-10 codes}
#'   \item{y_diagnosis_first }{Year of first diagnosis}
#'   \item{sex}{Sex (code)}
#'   \item{y_birth}{Year of birth}
#'   \item{innvandringsgrunn}{Reason for immigration (https://www.ssb.no/klass/klassifikasjoner/355)}
#' }
#' @source Code used to simulate this dataset can be found in /data-raw directory
"linked_df"
