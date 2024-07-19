#' Search NPI Numbers for Given Names
#'
#' This function searches for NPI (National Provider Identifier) numbers based on
#' given first and last names. It can accept input data in the form of a dataframe,
#' a CSV file, or an RDS file with columns named 'first' and 'last' representing
#' first names and last names, respectively.
#'
#' @param input_data A dataframe, a file path to a CSV, or an RDS file containing
#'                   first and last names.
#'
#' @return A dataframe containing NPI numbers for the provided names that match
#'         the specified taxonomies.
#'
#' @importFrom npi npi_search npi_flatten
#' @importFrom readr read_csv
#' @importFrom dplyr filter
#'
#' @examples
#' # Input as a dataframe
#' input_df <- data.frame(
#'   first = c("John", "Jane", "Alice"),
#'   last = c("Doe", "Smith", "Johnson")
#' )
#' npi_results <- search_npi(input_df)
#'
#' # Input as a CSV file
#' input_csv <- "path/to/input.csv"
#' npi_results <- search_npi(input_csv)
#'
#' @export
search_npi <- function(input_data) {

  vc <- c("Allergy & Immunology", "Allergy & Immunology, Allergy", "Anesthesiology", "Anesthesiology, Critical Care Medicine", "Anesthesiology, Hospice and Palliative Medicine", "Anesthesiology, Pain Medicine", "Advanced Practice Midwife", "Colon & Rectal Surgery", "Dermatology", "Dermatology, Clinical & Laboratory Dermatological Immunology", "Dermatology, Dermatopathology", "Dermatology, MOHS-Micrographic Surgery", "Dermatology, Pediatric Dermatology", "Dermatology, Procedural Dermatology", "Doula", "Emergency Medicine", "Emergency Medicine, Emergency Medical Services", "Emergency Medicine, Hospice and Palliative Medicine", "Emergency Medicine, Medical Toxicology", "Emergency Medicine, Pediatric Emergency Medicine", "Emergency Medicine, Undersea and Hyperbaric Medicine", "Family Medicine", "Family Medicine, Addiction Medicine", "Family Medicine, Adolescent Medicine", "Family Medicine, Adult Medicine", "Family Medicine, Geriatric Medicine", "Family Medicine, Hospice and Palliative Medicine", "Family Medicine, Sports Medicine", "Internal Medicine", "Internal Medicine, Addiction Medicine", "Internal Medicine, Adolescent Medicine", "Internal Medicine, Advanced Heart Failure and Transplant Cardiology", "Internal Medicine, Allergy & Immunology", "Internal Medicine, Bariatric Medicine", "Internal Medicine, Cardiovascular Disease", "Internal Medicine, Clinical Cardiac Electrophysiology", "Internal Medicine, Critical Care Medicine", "Internal Medicine, Endocrinology, Diabetes & Metabolism", "Internal Medicine, Gastroenterology", "Internal Medicine, Geriatric Medicine", "Internal Medicine, Hematology", "Internal Medicine, Hematology & Oncology", "Internal Medicine, Hospice and Palliative Medicine", "Internal Medicine, Hypertension Specialist", "Internal Medicine, Infectious Disease", "Internal Medicine, Interventional Cardiology", "Internal Medicine, Medical Oncology", "Internal Medicine, Nephrology", "Internal Medicine, Pulmonary Disease", "Internal Medicine, Rheumatology", "Internal Medicine, Sleep Medicine", "Internal Medicine, Sports Medicine", "Lactation Consultant, Non-RN", "Medical Genetics, Clinical Biochemical Genetics", "Medical Genetics, Clinical Genetics (M.D.)", "Medical Genetics, Ph.D. Medical Genetics", "Midwife", "Nuclear Medicine", "Neuromusculoskeletal Medicine, Sports Medicine", "Neuromusculoskeletal Medicine & OMM", "Nuclear Medicine, Nuclear Cardiology", "Obstetrics & Gynecology", "Obstetrics & Gynecology, Complex Family Planning", "Obstetrics & Gynecology, Critical Care Medicine", "Obstetrics & Gynecology, Gynecologic Oncology", "Obstetrics & Gynecology, Gynecology", "Obstetrics & Gynecology, Hospice and Palliative Medicine", "Obstetrics & Gynecology, Maternal & Fetal Medicine", "Obstetrics & Gynecology, Obstetrics", "Obstetrics & Gynecology, Reproductive Endocrinology", "Ophthalmology", "Ophthalmology, Cornea and External Diseases Specialist", "Ophthalmology, Glaucoma Specialist", "Ophthalmology, Ophthalmic Plastic and Reconstructive Surgery", "Ophthalmology, Pediatric Ophthalmology and Strabismus Specialist", "Ophthalmology, Retina Specialist", "Oral & Maxillofacial Surgery", "Orthopaedic Surgery", "Orthopaedic Surgery, Adult Reconstructive Orthopaedic Surgery", "Orthopaedic Surgery, Foot and Ankle Surgery", "Orthopaedic Surgery, Hand Surgery", "Orthopaedic Surgery, Orthopaedic Surgery of the Spine", "Orthopaedic Surgery, Orthopaedic Trauma", "Orthopaedic Surgery, Pediatric Orthopaedic Surgery", "Orthopaedic Surgery, Sports Medicine", "Otolaryngology, Facial Plastic Surgery", "Otolaryngology, Otolaryngic Allergy", "Otolaryngology, Otolaryngology/Facial Plastic Surgery", "Otolaryngology, Otology & Neurotology", "Otolaryngology, Pediatric Otolaryngology", "Otolaryngology, Plastic Surgery within the Head & Neck", "Pain Medicine, Interventional Pain Medicine", "Pain Medicine, Pain Medicine", "Pathology, Anatomic Pathology", "Pathology, Anatomic Pathology & Clinical Pathology", "Pathology, Anatomic Pathology & Clinical Pathology", "Pathology, Blood Banking & Transfusion Medicine")

  bc <- c("Pathology, Chemical Pathology", "Pathology, Clinical Laboratory Director, Non-physician", "Pathology, Clinical Pathology", "Pathology, Clinical Pathology/Laboratory Medicine", "Pathology, Cytopathology", "Pathology, Dermatopathology", "Pathology, Forensic Pathology", "Pathology, Hematology", "Pathology, Medical Microbiology", "Pathology, Molecular Genetic Pathology", "Pathology, Neuropathology", "Pediatrics", "Pediatrics, Adolescent Medicine", "Pediatrics, Clinical & Laboratory Immunology", "Pediatrics, Child Abuse Pediatrics", "Pediatrics, Developmental - Behavioral Pediatrics", "Pediatrics, Hospice and Palliative Medicine", "Pediatrics, Neonatal-Perinatal Medicine", "Pediatrics, Neurodevelopmental Disabilities", "Pediatrics, Pediatric Allergy/Immunology", "Pediatrics, Pediatric Cardiology", "Pediatrics, Pediatric Critical Care Medicine", "Pediatrics, Pediatric Emergency Medicine", "Pediatrics, Pediatric Endocrinology", "Pediatrics, Pediatric Gastroenterology", "Pediatrics, Pediatric Hematology-Oncology", "Pediatrics, Pediatric Infectious Diseases", "Pediatrics, Pediatric Nephrology", "Pediatrics, Pediatric Pulmonology", "Pediatrics, Pediatric Rheumatology", "Pediatrics, Sleep Medicine", "Physical Medicine & Rehabilitation, Neuromuscular Medicine", "Physical Medicine & Rehabilitation, Pain Medicine", "Physical Medicine & Rehabilitation", "Physical Medicine & Rehabilitation, Pediatric Rehabilitation Medicine", "Physical Medicine & Rehabilitation, Spinal Cord Injury Medicine", "Physical Medicine & Rehabilitation, Sports Medicine", "Plastic Surgery", "Plastic Surgery, Plastic Surgery Within the Head and Neck", "Plastic Surgery, Surgery of the Hand", "Preventive Medicine, Aerospace Medicine", "Preventive Medicine, Obesity Medicine", "Preventive Medicine, Occupational Medicine", "Preventive Medicine, Preventive Medicine/Occupational Environmental Medicine", "Preventive Medicine, Undersea and Hyperbaric Medicine", "Preventive Medicine, Public Health & General Preventive Medicine", "Psychiatry & Neurology, Addiction Medicine", "Psychiatry & Neurology, Addiction Psychiatry", "Psychiatry & Neurology, Behavioral Neurology & Neuropsychiatry", "Psychiatry & Neurology, Brain Injury Medicine", "Psychiatry & Neurology, Child & Adolescent Psychiatry", "Psychiatry & Neurology, Clinical Neurophysiology", "Psychiatry & Neurology, Forensic Psychiatry", "Psychiatry & Neurology, Geriatric Psychiatry", "Psychiatry & Neurology, Neurocritical Care", "Psychiatry & Neurology, Neurology", "Psychiatry & Neurology, Neurology with Special Qualifications in Child Neurology", "Psychiatry & Neurology, Psychiatry", "Psychiatry & Neurology, Psychosomatic Medicine", "Psychiatry & Neurology, Sleep Medicine", "Psychiatry & Neurology, Vascular Neurology", "Radiology, Body Imaging", "Radiology, Diagnostic Neuroimaging", "Radiology, Diagnostic Radiology", "Radiology, Diagnostic Ultrasound", "Radiology, Neuroradiology", "Radiology, Nuclear Radiology", "Radiology, Pediatric Radiology", "Radiology, Radiation Oncology", "Radiology, Vascular & Interventional Radiology", "Specialist", "Surgery", "Surgery, Pediatric Surgery", "Surgery, Plastic and Reconstructive Surgery", "Surgery, Surgery of the Hand", "Surgery, Surgical Critical Care", "Surgery, Surgical Oncology", "Surgery, Trauma Surgery", "Surgery, Vascular Surgery", "Urology", "Urology, Female Pelvic Medicine and Reconstructive Surgery", "Urology, Pediatric Urology","Pathology", "Thoracic Surgery (Cardiothoracic Vascular Surgery)" , "Transplant Surgery")

  if (is.data.frame(input_data)) {
    # Input is a dataframe
    df <- input_data
  } else if (is.character(input_data)) {
    # Input is a file path to a CSV
    df <- readr::read_csv(input_data)
  } else {
    stop("Input must be a dataframe or a file path to a CSV.")
  }

  # Extract first and last names
  first_name <- df$first
  last_name <- df$last

  # NPI search logic
  npi <- npi::npi_search(first_name = first_name, last_name = last_name)
  t <- npi::npi_flatten(npi, cols = c("basic", "taxonomies"))

  # Filter results based on specified taxonomies
  t_filtered <- t %>%
    dplyr::filter(taxonomies_desc %in% vc | taxonomies_desc %in% bc)

  return(t_filtered)
}
