#' Search and Process NPI Numbers
#'
#' This function takes an input file in RDS, CSV, or XLS/XLSX format, reads the data,
#' performs NPI search and processing, and returns the results.
#' The function also provides customization options for the user
#' to specify the enumeration type, search limit, and filtering criteria for NPI results.
#'
#' @param input_file The path to the input file (RDS, CSV, or XLS/XLSX) containing the data.
#' @param enumeration_type The enumeration type for NPI search (e.g., "ind", "org", "all").
#'   Default is "ind".
#' @param limit The maximum number of search results to request for each name pair.
#'   Default is 10.
#' @param country_code Filter for only the "US".
#' @param filter_credentials A character vector containing the credentials to filter the
#'   NPI results. Default is c("MD", "DO").
#' @return A list containing the processed result data frame.
#'
#' @examples
#' \dontrun{
#' # Call the function with the input file name
#' input_file <- "data/Sent_to_npi_search.xlsx"
#' output <- search_and_process_npi(input_file)
#'
#' }
#' @import dplyr
#' @import memoise
#' @import readr
#' @import readxl
#' @import npi
#'
#' @export
search_and_process_npi <- function(input_file,
                                   enumeration_type = "ind",
                                   limit = 5L,
                                   country_code = "US",
                                   filter_credentials = c("MD", "DO")) {

  # For testing:
  # input_file <- "/Users/tylermuffly/Dropbox (Personal)/Nomogram/nomogram/data/nppes_search/Lo_R_Author.csv"
  # enumeration_type <- "ind"
  # limit<-5L
  # country_code<- "US"
  # filter_credentials <- c("MD", "DO")
  library(dplyr)
  library(stats)
  library(npi)
  library(tidyverse)
  library(memoise)
  library(humaniformat)
  library(progress)

  # Check if the input file exists
  if (!file.exists(input_file)) {
    stop(
      "The specified file '", input_file, "' does not exist.\n",
      "Please provide the full path to the file."
    )
  }

  # Read data from the input file
  file_extension <- tools::file_ext(input_file)

  if (file_extension == "rds") {
    data <- readRDS(input_file)
  } else if (file_extension %in% c("csv", "xls", "xlsx")) {
    if (file_extension %in% c("xls", "xlsx")) {
      data <- readxl::read_xlsx(input_file)
    } else {
      data <- readr::read_csv(input_file)
    }
  } else {
    stop("Unsupported file format. Please provide an RDS, CSV, or XLS/XLSX file.")
  }

  first_names <- data$first
  last_names <- data$last

  # Define the list of taxonomies to filter
  vc <- c("Allergy & Immunology", "Allergy & Immunology, Allergy", "Anesthesiology", "Anesthesiology, Critical Care Medicine", "Anesthesiology, Hospice and Palliative Medicine", "Anesthesiology, Pain Medicine", "Advanced Practice Midwife", "Colon & Rectal Surgery", "Dermatology", "Dermatology, Clinical & Laboratory Dermatological Immunology", "Dermatology, Dermatopathology", "Dermatology, MOHS-Micrographic Surgery", "Dermatology, Pediatric Dermatology", "Dermatology, Procedural Dermatology", "Doula", "Emergency Medicine", "Emergency Medicine, Emergency Medical Services", "Emergency Medicine, Hospice and Palliative Medicine", "Emergency Medicine, Medical Toxicology", "Emergency Medicine, Pediatric Emergency Medicine", "Emergency Medicine, Undersea and Hyperbaric Medicine", "Family Medicine", "Family Medicine, Addiction Medicine", "Family Medicine, Adolescent Medicine", "Family Medicine, Adult Medicine", "Family Medicine, Geriatric Medicine", "Family Medicine, Hospice and Palliative Medicine", "Family Medicine, Sports Medicine", "Internal Medicine", "Internal Medicine, Addiction Medicine", "Internal Medicine, Adolescent Medicine", "Internal Medicine, Advanced Heart Failure and Transplant Cardiology", "Internal Medicine, Allergy & Immunology", "Internal Medicine, Bariatric Medicine", "Internal Medicine, Cardiovascular Disease", "Internal Medicine, Clinical Cardiac Electrophysiology", "Internal Medicine, Critical Care Medicine", "Internal Medicine, Endocrinology, Diabetes & Metabolism", "Internal Medicine, Gastroenterology", "Internal Medicine, Geriatric Medicine", "Internal Medicine, Hematology", "Internal Medicine, Hematology & Oncology", "Internal Medicine, Hospice and Palliative Medicine", "Internal Medicine, Hypertension Specialist", "Internal Medicine, Infectious Disease", "Internal Medicine, Interventional Cardiology", "Internal Medicine, Medical Oncology", "Internal Medicine, Nephrology", "Internal Medicine, Pulmonary Disease", "Internal Medicine, Rheumatology", "Internal Medicine, Sleep Medicine", "Internal Medicine, Sports Medicine", "Lactation Consultant, Non-RN", "Medical Genetics, Clinical Biochemical Genetics", "Medical Genetics, Clinical Genetics (M.D.)", "Medical Genetics, Ph.D. Medical Genetics", "Midwife", "Nuclear Medicine", "Neuromusculoskeletal Medicine, Sports Medicine", "Neuromusculoskeletal Medicine & OMM", "Nuclear Medicine, Nuclear Cardiology", "Obstetrics & Gynecology", "Obstetrics & Gynecology, Complex Family Planning", "Obstetrics & Gynecology, Critical Care Medicine", "Obstetrics & Gynecology, Gynecologic Oncology", "Obstetrics & Gynecology, Gynecology", "Obstetrics & Gynecology, Hospice and Palliative Medicine", "Obstetrics & Gynecology, Maternal & Fetal Medicine", "Obstetrics & Gynecology, Obstetrics", "Obstetrics & Gynecology, Reproductive Endocrinology", "Ophthalmology", "Ophthalmology, Cornea and External Diseases Specialist", "Ophthalmology, Glaucoma Specialist", "Ophthalmology, Ophthalmic Plastic and Reconstructive Surgery", "Ophthalmology, Pediatric Ophthalmology and Strabismus Specialist", "Ophthalmology, Retina Specialist", "Oral & Maxillofacial Surgery", "Orthopaedic Surgery", "Orthopaedic Surgery, Adult Reconstructive Orthopaedic Surgery", "Orthopaedic Surgery, Foot and Ankle Surgery", "Orthopaedic Surgery, Hand Surgery", "Orthopaedic Surgery, Orthopaedic Surgery of the Spine", "Orthopaedic Surgery, Orthopaedic Trauma", "Orthopaedic Surgery, Pediatric Orthopaedic Surgery", "Orthopaedic Surgery, Sports Medicine", "Otolaryngology, Facial Plastic Surgery", "Otolaryngology, Otolaryngic Allergy", "Otolaryngology, Otolaryngology/Facial Plastic Surgery", "Otolaryngology, Otology & Neurotology", "Otolaryngology, Pediatric Otolaryngology", "Otolaryngology, Plastic Surgery within the Head & Neck", "Pain Medicine, Interventional Pain Medicine", "Pain Medicine, Pain Medicine", "Pathology, Anatomic Pathology", "Pathology, Anatomic Pathology & Clinical Pathology", "Pathology, Anatomic Pathology & Clinical Pathology", "Pathology, Blood Banking & Transfusion Medicine")

  bc <- c("Pathology, Chemical Pathology", "Pathology, Clinical Laboratory Director, Non-physician", "Pathology, Clinical Pathology", "Pathology, Clinical Pathology/Laboratory Medicine", "Pathology, Cytopathology", "Pathology, Dermatopathology", "Pathology, Forensic Pathology", "Pathology, Hematology", "Pathology, Medical Microbiology", "Pathology, Molecular Genetic Pathology", "Pathology, Neuropathology", "Pediatrics", "Pediatrics, Adolescent Medicine", "Pediatrics, Clinical & Laboratory Immunology", "Pediatrics, Child Abuse Pediatrics", "Pediatrics, Developmental - Behavioral Pediatrics", "Pediatrics, Hospice and Palliative Medicine", "Pediatrics, Neonatal-Perinatal Medicine", "Pediatrics, Neurodevelopmental Disabilities", "Pediatrics, Pediatric Allergy/Immunology", "Pediatrics, Pediatric Cardiology", "Pediatrics, Pediatric Critical Care Medicine", "Pediatrics, Pediatric Emergency Medicine", "Pediatrics, Pediatric Endocrinology", "Pediatrics, Pediatric Gastroenterology", "Pediatrics, Pediatric Hematology-Oncology", "Pediatrics, Pediatric Infectious Diseases", "Pediatrics, Pediatric Nephrology", "Pediatrics, Pediatric Pulmonology", "Pediatrics, Pediatric Rheumatology", "Pediatrics, Sleep Medicine", "Physical Medicine & Rehabilitation, Neuromuscular Medicine", "Physical Medicine & Rehabilitation, Pain Medicine", "Physical Medicine & Rehabilitation", "Physical Medicine & Rehabilitation, Pediatric Rehabilitation Medicine", "Physical Medicine & Rehabilitation, Spinal Cord Injury Medicine", "Physical Medicine & Rehabilitation, Sports Medicine", "Plastic Surgery", "Plastic Surgery, Plastic Surgery Within the Head and Neck", "Plastic Surgery, Surgery of the Hand", "Preventive Medicine, Aerospace Medicine", "Preventive Medicine, Obesity Medicine", "Preventive Medicine, Occupational Medicine", "Preventive Medicine, Preventive Medicine/Occupational Environmental Medicine", "Preventive Medicine, Undersea and Hyperbaric Medicine", "Preventive Medicine, Public Health & General Preventive Medicine", "Psychiatry & Neurology, Addiction Medicine", "Psychiatry & Neurology, Addiction Psychiatry", "Psychiatry & Neurology, Behavioral Neurology & Neuropsychiatry", "Psychiatry & Neurology, Brain Injury Medicine", "Psychiatry & Neurology, Child & Adolescent Psychiatry", "Psychiatry & Neurology, Clinical Neurophysiology", "Psychiatry & Neurology, Forensic Psychiatry", "Psychiatry & Neurology, Geriatric Psychiatry", "Psychiatry & Neurology, Neurocritical Care", "Psychiatry & Neurology, Neurology", "Psychiatry & Neurology, Neurology with Special Qualifications in Child Neurology", "Psychiatry & Neurology, Psychiatry", "Psychiatry & Neurology, Psychosomatic Medicine", "Psychiatry & Neurology, Sleep Medicine", "Psychiatry & Neurology, Vascular Neurology", "Radiology, Body Imaging", "Radiology, Diagnostic Neuroimaging", "Radiology, Diagnostic Radiology", "Radiology, Diagnostic Ultrasound", "Radiology, Neuroradiology", "Radiology, Nuclear Radiology", "Radiology, Pediatric Radiology", "Radiology, Radiation Oncology", "Radiology, Vascular & Interventional Radiology", "Specialist", "Surgery", "Surgery, Pediatric Surgery", "Surgery, Plastic and Reconstructive Surgery", "Surgery, Surgery of the Hand", "Surgery, Surgical Critical Care", "Surgery, Surgical Oncology", "Surgery, Trauma Surgery", "Surgery, Vascular Surgery", "Urology", "Urology, Female Pelvic Medicine and Reconstructive Surgery", "Urology, Pediatric Urology","Pathology", "Thoracic Surgery (Cardiothoracic Vascular Surgery)" , "Transplant Surgery")

  # Define a function to search NPI based on first and last names
  search_npi <- function(first_name, last_name) {tryCatch(   # to skip empty results
    {
      #npi search object
      npi <- npi::npi_search(first_name = first_name , last_name = last_name)

      #retrieve basic and taxonomy data from npi objects
      t <- npi::npi_flatten(npi, cols = c("basic", "taxonomies"))

      #subset results with taxonomy that matches taxonomies in the lists
      t <- t %>% dplyr::filter(taxonomies_desc %in% vc | taxonomies_desc %in% bc)
      #t <- subset(t, t$taxonomies_desc %in% vc | t$taxonomies_desc %in% bc)

    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
    return(t)
  }

  # Create empty list to receive the data
  out <- list()

  # Initialize progress bar
  total_names <- length(first_names)
  pb <- progress::progress_bar$new(total = total_names)

  # Search NPI for each name in the input data
  out <- purrr::map2(first_names, last_names, function(first_name, last_name) {
    pb$tick()
    search_npi(first_name, last_name)
  })

  #SECOND LOOP
  # Create a list with two elements, first_name and last_name, where each element is a vector of names.
  arg_list <- list(first_name = first_names, last_name = last_names)

  # Apply the search_npi function to each combination of first_name and last_name in arg_list.
  npi_data <- purrr::pmap(arg_list, search_npi)

  # Filter npi_data to keep only elements that are data frames.
  npi_data  <- Filter(is.data.frame, npi_data)

  # Combine multiple data frames into a single data frame using data.table::rbindlist().  This is a function from the data.table package that is used to concatenate (bind) a list of data frames into one data frame.
  result <- data.table::rbindlist(npi_data, fill = TRUE)

  # Save the result to a file
  # date_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  # filename <- paste("data/results_of_search_and_process_npi", date_time, ".rds", sep = "_")
  # readr::write_rds(result, file = filename)

  # Return the result data frame
  return(result)
}

# library(tyler)
# input_file <- acog_presidents
# input_file <- "data/acog_presidents.csv"
# input_file <- "subspecialists_only.csv" #%>%
                # rename(first = first_name) %>%
                #   rename(last = last_name) %>%
                #   write_csv("subspecialists_only.csv")
# output_result <- search_and_process_npi(input_file)
# readr::write_csv(output_result, "results_of_search_and_process_npi.csv")
