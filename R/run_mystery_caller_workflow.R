#' Run the end-to-end mystery caller workflow
#'
#' This helper orchestrates the core steps required to prepare and execute a
#' mystery caller campaign. It stitches together roster creation, NPI
#' validation, call sheet preparation, workload splitting, and Phase 2 data
#' hygiene checks so teams can focus on execution instead of plumbing.
#'
#' @param taxonomy_terms Character vector of taxonomy descriptions to pass to
#'   [search_by_taxonomy()]. Set to `NULL` to skip taxonomy-based searches.
#' @param name_data Optional data frame containing `first` and `last` columns to
#'   use with [search_and_process_npi()]. Provide `NULL` to skip name-based
#'   searches.
#' @param phase1_data Data frame holding Phase 1 calling roster information to
#'   pass to [clean_phase_1_results()].
#' @param lab_assistant_names Character vector of caller names used when splitting
#'   the cleaned roster via [split_and_save()]. Must contain at least two
#'   entries.
#' @param output_directory Directory where [split_and_save()] should write the
#'   complete and per-caller workbooks.
#' @param phase2_data Data frame or file path consumed by
#'   [clean_phase_2_data()].
#' @param quality_check_path File path where [save_quality_check_table()] should
#'   write the quality check CSV.
#' @param phase1_output_directory Directory where [clean_phase_1_results()]
#'   should write the cleaned Phase 1 CSV. Defaults to `output_directory`.
#' @param split_insurance_order Ordering passed to [split_and_save()]'s
#'   `insurance_order` argument. Defaults to `c("Medicaid", "Blue Cross/Blue Shield")`.
#' @param phase2_required_strings Character vector of substrings used when
#'   standardising Phase 2 column names via [clean_phase_2_data()].
#' @param phase2_standard_names Replacement names corresponding to
#'   `phase2_required_strings`.
#' @param npi_search_args Named list of additional arguments forwarded to
#'   [search_and_process_npi()].
#' @param all_states Optional character vector of all states to supply to
#'   [states_where_physicians_were_NOT_contacted()].
#' @param verbose Logical flag controlling workflow progress messages.
#'
#' @return A list containing intermediate artefacts from each workflow stage:
#'   `roster`, `validated_roster`, `cleaned_phase1`, `cleaned_phase2`,
#'   `coverage_summary`, and `quality_check_table`, plus file paths generated
#'   along the way (`cleaned_phase1_path`, `cleaned_phase2_path`,
#'   `quality_check_path`, and `split_workbook_paths`).
#'
#' @export
#' @importFrom dplyr bind_rows distinct
#' @importFrom tibble tibble
#' @importFrom utils modifyList
run_mystery_caller_workflow <- function(
  taxonomy_terms = NULL,
  name_data = NULL,
  phase1_data,
  lab_assistant_names,
  output_directory,
  phase2_data,
  quality_check_path,
  phase1_output_directory = output_directory,
  split_insurance_order = c("Medicaid", "Blue Cross/Blue Shield"),
  phase2_required_strings = c(
    "physician_information", "able_to_contact_office", "are_we_including",
    "reason_for_exclusions", "appointment_date", "number_of_transfers",
    "call_time", "hold_time", "notes", "person_completing",
    "state", "npi", "name"
  ),
  phase2_standard_names = c(
    "physician_info", "contact_office", "included_in_study",
    "exclusion_reasons", "appt_date", "transfer_count",
    "call_duration", "hold_duration", "notes", "completed_by",
    "state", "npi", "name"
  ),
  npi_search_args = list(),
  all_states = NULL,
  verbose = TRUE
) {
  roster <- build_roster_stage(taxonomy_terms, name_data, npi_search_args, verbose)
  phase1 <- prepare_phase1_stage(
    phase1_data = phase1_data,
    lab_assistant_names = lab_assistant_names,
    output_directory = output_directory,
    phase1_output_directory = phase1_output_directory,
    split_insurance_order = split_insurance_order,
    verbose = verbose
  )

  phase2 <- process_phase2_stage(
    phase2_data = phase2_data,
    required_strings = phase2_required_strings,
    standard_names = phase2_standard_names,
    verbose = verbose
  )

  quality <- export_quality_checks_stage(
    cleaned_phase2 = phase2$data,
    quality_check_path = quality_check_path,
    all_states = all_states,
    verbose = verbose
  )

  list(
    roster = roster$combined,
    validated_roster = roster$validated,
    cleaned_phase1 = phase1$data,
    cleaned_phase1_path = phase1$output_path,
    split_workbook_paths = phase1$workbook_paths,
    cleaned_phase2 = phase2$data,
    cleaned_phase2_path = phase2$output_path,
    coverage_summary = quality$coverage_summary,
    quality_check_table = quality$table,
    quality_check_path = quality$path
  )
}

build_roster_stage <- function(taxonomy_terms, name_data, npi_search_args, verbose) {
  workflow_log("Building provider roster...", verbose = verbose)
  roster_taxonomy <- if (!is.null(taxonomy_terms) && length(taxonomy_terms)) {
    search_by_taxonomy(taxonomy_terms)
  } else {
    tibble::tibble()
  }

  roster_names <- tibble::tibble()
  if (!is.null(name_data)) {
    assert_is_dataframe(name_data, "name_data")
    if (nrow(name_data)) {
      assert_has_columns(name_data, c("first", "last"), "name_data")
      args <- utils::modifyList(list(data = name_data), npi_search_args)
      roster_names <- do.call(search_and_process_npi, args)
    }
  }

  combined_roster <- dplyr::bind_rows(roster_taxonomy, roster_names)
  if ("npi" %in% names(combined_roster) && nrow(combined_roster)) {
    combined_roster <- dplyr::distinct(combined_roster, npi, .keep_all = TRUE)
  }

  validated_roster <- combined_roster
  if ("npi" %in% names(combined_roster) && nrow(combined_roster)) {
    validated_roster <- validate_and_remove_invalid_npi(combined_roster)
  }

  list(combined = combined_roster, validated = validated_roster)
}

prepare_phase1_stage <- function(phase1_data,
                                 lab_assistant_names,
                                 output_directory,
                                 phase1_output_directory,
                                 split_insurance_order,
                                 verbose) {
  workflow_log("Cleaning Phase 1 roster...", verbose = verbose)
  cleaned_phase1 <- clean_phase_1_results(
    phase1_data,
    output_directory = phase1_output_directory,
    verbose = verbose
  )
  output_path <- attr(cleaned_phase1, "output_path")

  workflow_log("Splitting Phase 1 workbook...", verbose = verbose)
  split_result <- split_and_save(
    cleaned_phase1,
    output_directory = output_directory,
    lab_assistant_names = lab_assistant_names,
    insurance_order = split_insurance_order,
    verbose = verbose
  )

  list(
    data = cleaned_phase1,
    output_path = output_path,
    workbook_paths = split_result
  )
}

process_phase2_stage <- function(phase2_data, required_strings, standard_names, verbose) {
  workflow_log("Cleaning Phase 2 call outcomes...", verbose = verbose)
  cleaned_phase2 <- clean_phase_2_data(
    data_or_path = phase2_data,
    required_strings = required_strings,
    standard_names = standard_names,
    verbose = verbose
  )

  list(
    data = cleaned_phase2,
    output_path = attr(cleaned_phase2, "output_path")
  )
}

export_quality_checks_stage <- function(cleaned_phase2, quality_check_path, all_states, verbose) {
  coverage_summary <- NULL
  if (!is.null(cleaned_phase2) && "state" %in% names(cleaned_phase2)) {
    coverage_summary <- states_where_physicians_were_NOT_contacted(
      cleaned_phase2,
      all_states = all_states
    )
  }

  quality_check_table <- NULL
  if (!is.null(cleaned_phase2) && all(c("npi", "name") %in% names(cleaned_phase2))) {
    quality_check_table <- save_quality_check_table(
      cleaned_phase2,
      quality_check_path,
      verbose = verbose
    )
  }

  list(
    coverage_summary = coverage_summary,
    table = quality_check_table,
    path = if (!is.null(quality_check_table)) quality_check_path else NULL
  )
}
