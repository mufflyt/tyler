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
#' @param verbose Logical. When `TRUE`, print stage updates to the console while
#'   running the workflow. Defaults to `interactive()`.
#' @param npi_progress_observer Optional callback that receives progress updates
#'   from [search_and_process_npi()]. It is invoked with the same payload as the
#'   `progress_callback` argument for that function.
#'
#' @return A list containing intermediate artefacts from each workflow stage:
#'   `roster`, `validated_roster`, `cleaned_phase1`, `cleaned_phase2`,
#'   `coverage_summary`, and `quality_check_table`.
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
  verbose = interactive(),
  npi_progress_observer = NULL
) {
  announce <- function(stage) {
    if (isTRUE(verbose)) {
      message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), stage))
    }
  }

  announce("Starting mystery caller workflow")
  roster_taxonomy <- if (!is.null(taxonomy_terms) && length(taxonomy_terms)) {
    announce("Searching NPIs by taxonomy")
    search_by_taxonomy(taxonomy_terms)
  } else {
    tibble::tibble()
  }

  roster_names <- tibble::tibble()
  if (!is.null(name_data)) {
    if (!is.data.frame(name_data)) {
      stop("`name_data` must be a data frame with `first` and `last` columns.")
    }
    if (nrow(name_data)) {
      if (!all(c("first", "last") %in% names(name_data))) {
        stop("`name_data` must contain columns named `first` and `last`.")
      }
      announce("Searching NPIs by name")
      args <- utils::modifyList(list(data = name_data), npi_search_args)
      user_callback <- NULL
      if (!is.null(args$progress_callback)) {
        user_callback <- args$progress_callback
      }
      callbacks <- list()
      if (is.function(user_callback)) {
        callbacks <- c(callbacks, list(user_callback))
      }
      if (is.function(npi_progress_observer)) {
        callbacks <- c(callbacks, list(npi_progress_observer))
      }
      if (isTRUE(verbose)) {
        callbacks <- c(callbacks, list(function(update) {
          if (is.null(update$event)) {
            return(invisible(NULL))
          }
          known_events <- c(
            start = "Starting name searches",
            result = "Retrieved results",
            no_results = "No results",
            skipped = "Skipped",
            heartbeat = "Still processing",
            completed = "Completed name searches"
          )
          label <- known_events[[update$event]]
          if (is.null(label)) {
            label <- update$event
          }
          detail <- if (!is.null(update$search_term)) {
            paste0(" for ", update$search_term)
          } else {
            ""
          }
          message(sprintf("[%s] %s%s", format(Sys.time(), "%H:%M:%S"), label, detail))
          invisible(NULL)
        }))
        if (is.null(args$heartbeat_seconds)) {
          args$heartbeat_seconds <- 30
        }
      }
      if (length(callbacks) == 1) {
        args$progress_callback <- callbacks[[1]]
      } else if (length(callbacks) > 1) {
        args$progress_callback <- function(update) {
          for (cb in callbacks) {
            try(cb(update), silent = TRUE)
          }
          invisible(NULL)
        }
      }
      roster_names <- do.call(search_and_process_npi, args)
    }
  }

  combined_roster <- dplyr::bind_rows(roster_taxonomy, roster_names)
  if ("npi" %in% names(combined_roster) && nrow(combined_roster)) {
    combined_roster <- dplyr::distinct(combined_roster, npi, .keep_all = TRUE)
  }

  validated_roster <- combined_roster
  if ("npi" %in% names(combined_roster) && nrow(combined_roster)) {
    announce("Validating NPI roster")
    validated_roster <- validate_and_remove_invalid_npi(combined_roster)
  }

  announce("Cleaning Phase 1 results")
  cleaned_phase1 <- clean_phase_1_results(
    phase1_data,
    output_directory = phase1_output_directory,
    notify = TRUE
  )
  announce("Splitting Phase 1 workbooks for callers")
  split_and_save(
    cleaned_phase1,
    output_directory = output_directory,
    lab_assistant_names = lab_assistant_names,
    insurance_order = split_insurance_order
  )

  announce("Cleaning Phase 2 results")
  cleaned_phase2 <- clean_phase_2_data(
    data_or_path = phase2_data,
    required_strings = phase2_required_strings,
    standard_names = phase2_standard_names
  )

  coverage_summary <- NULL
  if ("state" %in% names(cleaned_phase2)) {
    announce("Summarising coverage gaps")
    coverage_summary <- states_where_physicians_were_NOT_contacted(cleaned_phase2, all_states = all_states)
  }

  if (!dir.exists(dirname(quality_check_path))) {
    dir.create(dirname(quality_check_path), recursive = TRUE, showWarnings = FALSE)
  }

  quality_check_table <- NULL
  if (all(c("npi", "name") %in% names(cleaned_phase2))) {
    announce("Saving quality check table")
    quality_check_table <- save_quality_check_table(cleaned_phase2, quality_check_path)
  }

  if (isTRUE(verbose)) {
    message(sprintf("[%s] Mystery caller workflow complete", format(Sys.time(), "%H:%M:%S")))
  }

  list(
    roster = combined_roster,
    validated_roster = validated_roster,
    cleaned_phase1 = cleaned_phase1,
    cleaned_phase2 = cleaned_phase2,
    coverage_summary = coverage_summary,
    quality_check_table = quality_check_table
  )
}
