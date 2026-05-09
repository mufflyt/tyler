#' Deprecated functions in tyler
#'
#' These names were used before `tyler` adopted a consistent `tyler_` prefix.
#' They emit deprecation warnings and forward all arguments to the current
#' equivalent.
#'
#' @name tyler-deprecated
#' @keywords internal
NULL

#' @rdname tyler-deprecated
#' @export
arsenal_tables_write2word <- function(...) {
  .Deprecated("tyler_write_arsenal_table", package = "tyler",
              msg = paste0("arsenal_tables_write2word() is deprecated. Use tyler_write_arsenal_table() instead."))
  tyler_write_arsenal_table(...)
}

#' @rdname tyler-deprecated
#' @export
calculate_intersection_overlap_and_save <- function(...) {
  .Deprecated("tyler_calculate_overlap", package = "tyler",
              msg = paste0("calculate_intersection_overlap_and_save() is deprecated. Use tyler_calculate_overlap() instead."))
  tyler_calculate_overlap(...)
}

#' @rdname tyler-deprecated
#' @export
check_normality <- function(...) {
  .Deprecated("tyler_check_normality", package = "tyler",
              msg = paste0("check_normality() is deprecated. Use tyler_check_normality() instead."))
  tyler_check_normality(...)
}

#' @rdname tyler-deprecated
#' @export
clean_phase_1_results <- function(...) {
  .Deprecated("tyler_clean_phase1", package = "tyler",
              msg = paste0("clean_phase_1_results() is deprecated. Use tyler_clean_phase1() instead."))
  tyler_clean_phase1(...)
}

#' @rdname tyler-deprecated
#' @export
clean_phase_2_data <- function(...) {
  .Deprecated("tyler_clean_phase2", package = "tyler",
              msg = paste0("clean_phase_2_data() is deprecated. Use tyler_clean_phase2() instead."))
  tyler_clean_phase2(...)
}

#' @rdname tyler-deprecated
#' @export
create_and_plot_interaction <- function(...) {
  .Deprecated("tyler_plot_interaction", package = "tyler",
              msg = paste0("create_and_plot_interaction() is deprecated. Use tyler_plot_interaction() instead."))
  tyler_plot_interaction(...)
}

#' @rdname tyler-deprecated
#' @export
create_density_plot <- function(...) {
  .Deprecated("tyler_plot_density", package = "tyler",
              msg = paste0("create_density_plot() is deprecated. Use tyler_plot_density() instead."))
  tyler_plot_density(...)
}

#' @rdname tyler-deprecated
#' @export
create_formula <- function(...) {
  .Deprecated("tyler_create_formula", package = "tyler",
              msg = paste0("create_formula() is deprecated. Use tyler_create_formula() instead."))
  tyler_create_formula(...)
}

#' @rdname tyler-deprecated
#' @export
create_individual_isochrone_plots <- function(...) {
  .Deprecated("tyler_plot_isochrones", package = "tyler",
              msg = paste0("create_individual_isochrone_plots() is deprecated. Use tyler_plot_isochrones() instead."))
  tyler_plot_isochrones(...)
}

#' @rdname tyler-deprecated
#' @export
create_isochrones <- function(...) {
  .Deprecated("tyler_create_isochrones", package = "tyler",
              msg = paste0("create_isochrones() is deprecated. Use tyler_create_isochrones() instead."))
  tyler_create_isochrones(...)
}

#' @rdname tyler-deprecated
#' @export
create_isochrones_for_dataframe <- function(...) {
  .Deprecated("tyler_isochrones_for_df", package = "tyler",
              msg = paste0("create_isochrones_for_dataframe() is deprecated. Use tyler_isochrones_for_df() instead."))
  tyler_isochrones_for_df(...)
}

#' @rdname tyler-deprecated
#' @export
create_line_plot <- function(...) {
  .Deprecated("tyler_plot_line", package = "tyler",
              msg = paste0("create_line_plot() is deprecated. Use tyler_plot_line() instead."))
  tyler_plot_line(...)
}

#' @rdname tyler-deprecated
#' @export
create_scatter_plot <- function(...) {
  .Deprecated("tyler_plot_scatter", package = "tyler",
              msg = paste0("create_scatter_plot() is deprecated. Use tyler_plot_scatter() instead."))
  tyler_plot_scatter(...)
}

#' @rdname tyler-deprecated
#' @export
download_large_file <- function(...) {
  .Deprecated("tyler_download_file", package = "tyler",
              msg = paste0("download_large_file() is deprecated. Use tyler_download_file() instead."))
  tyler_download_file(...)
}

#' @rdname tyler-deprecated
#' @export
format_pct <- function(...) {
  .Deprecated("tyler_format_pct", package = "tyler",
              msg = paste0("format_pct() is deprecated. Use tyler_format_pct() instead."))
  tyler_format_pct(...)
}

#' @rdname tyler-deprecated
#' @export
genderize_physicians <- function(...) {
  .Deprecated("tyler_genderize", package = "tyler",
              msg = paste0("genderize_physicians() is deprecated. Use tyler_genderize() instead."))
  tyler_genderize(...)
}

#' @rdname tyler-deprecated
#' @export
geocode_unique_addresses <- function(...) {
  .Deprecated("tyler_geocode", package = "tyler",
              msg = paste0("geocode_unique_addresses() is deprecated. Use tyler_geocode() instead."))
  tyler_geocode(...)
}

#' @rdname tyler-deprecated
#' @export
get_census_data <- function(...) {
  .Deprecated("tyler_get_census_data", package = "tyler",
              msg = paste0("get_census_data() is deprecated. Use tyler_get_census_data() instead."))
  tyler_get_census_data(...)
}

#' @rdname tyler-deprecated
#' @export
hrr <- function(...) {
  .Deprecated("tyler_hrr", package = "tyler",
              msg = paste0("hrr() is deprecated. Use tyler_hrr() instead."))
  tyler_hrr(...)
}

#' @rdname tyler-deprecated
#' @export
hrr_generate_maps <- function(...) {
  .Deprecated("tyler_hrr_maps", package = "tyler",
              msg = paste0("hrr_generate_maps() is deprecated. Use tyler_hrr_maps() instead."))
  tyler_hrr_maps(...)
}

#' @rdname tyler-deprecated
#' @export
map_create_acog_districts_sf <- function(...) {
  .Deprecated("tyler_map_acog_districts", package = "tyler",
              msg = paste0("map_create_acog_districts_sf() is deprecated. Use tyler_map_acog_districts() instead."))
  tyler_map_acog_districts(...)
}

#' @rdname tyler-deprecated
#' @export
map_create_base <- function(...) {
  .Deprecated("tyler_map_base", package = "tyler",
              msg = paste0("map_create_base() is deprecated. Use tyler_map_base() instead."))
  tyler_map_base(...)
}

#' @rdname tyler-deprecated
#' @export
map_create_block_group_overlap <- function(...) {
  .Deprecated("tyler_map_block_group", package = "tyler",
              msg = paste0("map_create_block_group_overlap() is deprecated. Use tyler_map_block_group() instead."))
  tyler_map_block_group(...)
}

#' @rdname tyler-deprecated
#' @export
map_create_leaflet_base <- function(...) {
  .Deprecated("tyler_map_leaflet", package = "tyler",
              msg = paste0("map_create_leaflet_base() is deprecated. Use tyler_map_leaflet() instead."))
  tyler_map_leaflet(...)
}

#' @rdname tyler-deprecated
#' @export
map_create_physician_dot <- function(...) {
  .Deprecated("tyler_map_physicians", package = "tyler",
              msg = paste0("map_create_physician_dot() is deprecated. Use tyler_map_physicians() instead."))
  tyler_map_physicians(...)
}

#' @rdname tyler-deprecated
#' @export
max_table <- function(...) {
  .Deprecated("tyler_max_table", package = "tyler",
              msg = paste0("max_table() is deprecated. Use tyler_max_table() instead."))
  tyler_max_table(...)
}

#' @rdname tyler-deprecated
#' @export
min_table <- function(...) {
  .Deprecated("tyler_min_table", package = "tyler",
              msg = paste0("min_table() is deprecated. Use tyler_min_table() instead."))
  tyler_min_table(...)
}

#' @rdname tyler-deprecated
#' @export
most_common_gender_training_academic <- function(...) {
  .Deprecated("tyler_most_common_gender", package = "tyler",
              msg = paste0("most_common_gender_training_academic() is deprecated. Use tyler_most_common_gender() instead."))
  tyler_most_common_gender(...)
}

#' @rdname tyler-deprecated
#' @export
physician_age <- function(...) {
  .Deprecated("tyler_physician_age", package = "tyler",
              msg = paste0("physician_age() is deprecated. Use tyler_physician_age() instead."))
  tyler_physician_age(...)
}

#' @rdname tyler-deprecated
#' @export
plot_and_save_emmeans <- function(...) {
  .Deprecated("tyler_plot_emmeans", package = "tyler",
              msg = paste0("plot_and_save_emmeans() is deprecated. Use tyler_plot_emmeans() instead."))
  tyler_plot_emmeans(...)
}

#' @rdname tyler-deprecated
#' @export
plot_census_age_distribution <- function(...) {
  .Deprecated("tyler_plot_census_age", package = "tyler",
              msg = paste0("plot_census_age_distribution() is deprecated. Use tyler_plot_census_age() instead."))
  tyler_plot_census_age(...)
}

#' @rdname tyler-deprecated
#' @export
remove_constant_vars <- function(...) {
  .Deprecated("tyler_remove_constants", package = "tyler",
              msg = paste0("remove_constant_vars() is deprecated. Use tyler_remove_constants() instead."))
  tyler_remove_constants(...)
}

#' @rdname tyler-deprecated
#' @export
remove_near_zero_var <- function(...) {
  .Deprecated("tyler_remove_near_zero", package = "tyler",
              msg = paste0("remove_near_zero_var() is deprecated. Use tyler_remove_near_zero() instead."))
  tyler_remove_near_zero(...)
}

#' @rdname tyler-deprecated
#' @export
rename_columns_by_substring <- function(...) {
  .Deprecated("tyler_rename_columns", package = "tyler",
              msg = paste0("rename_columns_by_substring() is deprecated. Use tyler_rename_columns() instead."))
  tyler_rename_columns(...)
}

#' @rdname tyler-deprecated
#' @export
retrieve_clinician_data <- function(...) {
  .Deprecated("tyler_get_clinician_data", package = "tyler",
              msg = paste0("retrieve_clinician_data() is deprecated. Use tyler_get_clinician_data() instead."))
  tyler_get_clinician_data(...)
}

#' @rdname tyler-deprecated
#' @export
run_mystery_caller_workflow <- function(...) {
  .Deprecated("tyler_run_workflow", package = "tyler",
              msg = paste0("run_mystery_caller_workflow() is deprecated. Use tyler_run_workflow() instead."))
  tyler_run_workflow(...)
}

#' @rdname tyler-deprecated
#' @export
run_mystery_caller_workflow_with_logging <- function(...) {
  .Deprecated("tyler_run_workflow_logged", package = "tyler",
              msg = paste0("run_mystery_caller_workflow_with_logging() is deprecated. Use tyler_run_workflow_logged() instead."))
  tyler_run_workflow_logged(...)
}

#' @rdname tyler-deprecated
#' @export
save_quality_check_table <- function(...) {
  .Deprecated("tyler_save_quality_table", package = "tyler",
              msg = paste0("save_quality_check_table() is deprecated. Use tyler_save_quality_table() instead."))
  tyler_save_quality_table(...)
}

#' @rdname tyler-deprecated
#' @export
search_and_process_npi <- function(...) {
  .Deprecated("tyler_search_and_process_npi", package = "tyler",
              msg = paste0("search_and_process_npi() is deprecated. Use tyler_search_and_process_npi() instead."))
  tyler_search_and_process_npi(...)
}

#' @rdname tyler-deprecated
#' @export
search_by_taxonomy <- function(...) {
  .Deprecated("tyler_search_taxonomy", package = "tyler",
              msg = paste0("search_by_taxonomy() is deprecated. Use tyler_search_taxonomy() instead."))
  tyler_search_taxonomy(...)
}

#' @rdname tyler-deprecated
#' @export
split_and_save <- function(...) {
  .Deprecated("tyler_split_and_save", package = "tyler",
              msg = paste0("split_and_save() is deprecated. Use tyler_split_and_save() instead."))
  tyler_split_and_save(...)
}

#' @rdname tyler-deprecated
#' @export
states_where_physicians_were_NOT_contacted <- function(...) {
  .Deprecated("tyler_not_contacted_states", package = "tyler",
              msg = paste0("states_where_physicians_were_NOT_contacted() is deprecated. Use tyler_not_contacted_states() instead."))
  tyler_not_contacted_states(...)
}

#' @rdname tyler-deprecated
#' @export
summarize_census_data <- function(...) {
  .Deprecated("tyler_summarize_census", package = "tyler",
              msg = paste0("summarize_census_data() is deprecated. Use tyler_summarize_census() instead."))
  tyler_summarize_census(...)
}

#' @rdname tyler-deprecated
#' @export
table_calculate_percentages <- function(...) {
  .Deprecated("tyler_table_percentages", package = "tyler",
              msg = paste0("table_calculate_percentages() is deprecated. Use tyler_table_percentages() instead."))
  tyler_table_percentages(...)
}

#' @rdname tyler-deprecated
#' @export
table_calculate_proportion <- function(...) {
  .Deprecated("tyler_table_proportion", package = "tyler",
              msg = paste0("table_calculate_proportion() is deprecated. Use tyler_table_proportion() instead."))
  tyler_table_proportion(...)
}

#' @rdname tyler-deprecated
#' @export
table_generate_overall <- function(...) {
  .Deprecated("tyler_table_overall", package = "tyler",
              msg = paste0("table_generate_overall() is deprecated. Use tyler_table_overall() instead."))
  tyler_table_overall(...)
}

#' @rdname tyler-deprecated
#' @export
table_write_pdf <- function(...) {
  .Deprecated("tyler_write_table_pdf", package = "tyler",
              msg = paste0("table_write_pdf() is deprecated. Use tyler_write_table_pdf() instead."))
  tyler_write_table_pdf(...)
}

#' @rdname tyler-deprecated
#' @export
validate_and_remove_invalid_npi <- function(...) {
  .Deprecated("tyler_validate_npi", package = "tyler",
              msg = paste0("validate_and_remove_invalid_npi() is deprecated. Use tyler_validate_npi() instead."))
  tyler_validate_npi(...)
}

#' @rdname tyler-deprecated
#' @param input_data A data frame or CSV path containing `first` and `last` columns.
#' @export
search_npi <- function(input_data, ...) {
  .Deprecated("tyler_search_and_process_npi", package = "tyler",
              msg = "search_npi() is deprecated. Use tyler_search_and_process_npi().")
  data <- if (is.data.frame(input_data)) {
    input_data
  } else if (is.character(input_data) && length(input_data) == 1) {
    readr::read_csv(input_data, show_col_types = FALSE)
  } else {
    stop("`input_data` must be a data frame or a file path to a CSV.", call. = FALSE)
  }
  required_cols <- c("first", "last")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop("Input data must contain columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  tyler_search_and_process_npi(data = data, ...)
}

#' @rdname tyler-deprecated
#' @export
test_and_process_isochrones <- function(input_file, ...) {
  .Deprecated("tyler_isochrones_for_df", package = "tyler",
              msg = "test_and_process_isochrones() is deprecated. Use tyler_isochrones_for_df().")
  stop("test_and_process_isochrones() is deprecated and has been removed. Use tyler_isochrones_for_df().", call. = FALSE)
}

#' @rdname tyler-deprecated
#' @param chunk_size Deprecated.
#' @export
process_and_save_isochrones <- function(input_file, chunk_size = 25, ...) {
  .Deprecated("tyler_isochrones_for_df", package = "tyler",
              msg = "process_and_save_isochrones() is deprecated. Use tyler_isochrones_for_df().")
  stop("process_and_save_isochrones() is deprecated and has been removed. Use tyler_isochrones_for_df().", call. = FALSE)
}

#' @rdname tyler-deprecated
#' @export
progress_tracker <- function(...) {
  .Deprecated("tyler_progress_tracker", package = "tyler",
              msg = "progress_tracker() is deprecated. Use tyler_progress_tracker() instead.")
  tyler_progress_tracker(...)
}

#' @rdname tyler-deprecated
#' @export
progress_tracker_start <- function(...) {
  .Deprecated("tyler_progress_start", package = "tyler",
              msg = "progress_tracker_start() is deprecated. Use tyler_progress_start() instead.")
  tyler_progress_start(...)
}

#' @rdname tyler-deprecated
#' @export
progress_tracker_finish <- function(...) {
  .Deprecated("tyler_progress_finish", package = "tyler",
              msg = "progress_tracker_finish() is deprecated. Use tyler_progress_finish() instead.")
  tyler_progress_finish(...)
}

#' @rdname tyler-deprecated
#' @export
progress_tracker_fail <- function(...) {
  .Deprecated("tyler_progress_fail", package = "tyler",
              msg = "progress_tracker_fail() is deprecated. Use tyler_progress_fail() instead.")
  tyler_progress_fail(...)
}

#' @rdname tyler-deprecated
#' @export
progress_tracker_update <- function(...) {
  .Deprecated("tyler_progress_update", package = "tyler",
              msg = "progress_tracker_update() is deprecated. Use tyler_progress_update() instead.")
  tyler_progress_update(...)
}

#' @rdname tyler-deprecated
#' @export
progress_tracker_summary <- function(...) {
  .Deprecated("tyler_progress_summary", package = "tyler",
              msg = "progress_tracker_summary() is deprecated. Use tyler_progress_summary() instead.")
  tyler_progress_summary(...)
}
