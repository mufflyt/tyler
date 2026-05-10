#' Compute Disparity Metrics Across Groups
#'
#' For each level of a grouping column (e.g., insurance type), compute
#' acceptance rates, confidence intervals, and disparity metrics relative
#' to a reference group.
#'
#' @param data A data.frame containing the study data.
#' @param outcome_col Character scalar. Name of the binary outcome column
#'   (values must be 0 or 1, NAs are dropped).
#' @param group_col Character scalar. Name of the grouping column
#'   (e.g., insurance type).
#' @param ref_group Character scalar or NULL. The reference group label.
#'   If NULL, the first factor level is used when `group_col` is a factor,
#'   otherwise the most frequent level.
#' @param ci_method Character. Method for computing the rate confidence
#'   interval. One of \code{"wilson"} (default), \code{"exact"}, or
#'   \code{"wald"}.
#' @param alpha Numeric in (0, 1). Significance level; CIs use
#'   \code{1 - alpha} confidence. Default 0.05.
#'
#' @return A data.frame with class
#'   \code{c("mysterycall_disparities_table", "data.frame")} sorted by rate
#'   descending, with the reference group printed first. Columns:
#'   \describe{
#'     \item{group}{Group label.}
#'     \item{n}{Total observations in group.}
#'     \item{n_accepted}{Sum of outcome_col (excluding NAs).}
#'     \item{rate}{Acceptance rate (n_accepted / n).}
#'     \item{lower_ci}{Lower bound of rate CI.}
#'     \item{upper_ci}{Upper bound of rate CI.}
#'     \item{abs_diff}{Absolute difference in percentage points from
#'       ref_group rate (0 for reference).}
#'     \item{rel_risk}{Rate relative to ref_group rate (1 for reference).}
#'     \item{rr_lower}{Lower bound of RR 95% CI (NA for reference).}
#'     \item{rr_upper}{Upper bound of RR 95% CI (NA for reference).}
#'     \item{p_value}{Two-proportion z-test p-value vs. reference (NA for
#'       reference).}
#'     \item{p_value_fmt}{Formatted p-value string.}
#'   }
#'
#' @details
#' Relative risk confidence intervals are computed via the log method:
#' \code{SE_log_rr = sqrt(1/n_accepted - 1/n + 1/ref_n_accepted - 1/ref_n)}.
#' A continuity correction of 0.5 is added when \code{n_accepted} is 0.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   insurance = sample(c("Medicaid", "Private", "Medicare"), 120, replace = TRUE),
#'   accepted  = rbinom(120, 1, 0.5)
#' )
#' mysterycall_disparities_table(df, "accepted", "insurance")
#'
#' @export
mysterycall_disparities_table <- function(
    data,
    outcome_col,
    group_col,
    ref_group  = NULL,
    ci_method  = c("wilson", "exact", "wald"),
    alpha      = 0.05
) {
  # ---- input validation ------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(outcome_col) || length(outcome_col) != 1L) {
    stop("`outcome_col` must be a single character string.", call. = FALSE)
  }
  if (!is.character(group_col) || length(group_col) != 1L) {
    stop("`group_col` must be a single character string.", call. = FALSE)
  }
  if (!outcome_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in `data`.", outcome_col), call. = FALSE)
  }
  if (!group_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in `data`.", group_col), call. = FALSE)
  }
  ov <- data[[outcome_col]]
  ov_nona <- ov[!is.na(ov)]
  if (!all(ov_nona %in% c(0, 1))) {
    stop(
      sprintf("Column '%s' must be binary (values 0 and 1 only).", outcome_col),
      call. = FALSE
    )
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single numeric value in (0, 1).", call. = FALSE)
  }
  ci_method <- match.arg(ci_method)

  # ---- determine reference group ---------------------------------------
  gv <- data[[group_col]]
  if (is.null(ref_group)) {
    if (is.factor(gv)) {
      ref_group <- levels(gv)[1L]
    } else {
      tbl <- sort(table(gv), decreasing = TRUE)
      ref_group <- names(tbl)[1L]
    }
  } else {
    if (!ref_group %in% unique(gv)) {
      stop(
        sprintf("ref_group '%s' not found in column '%s'.", ref_group, group_col),
        call. = FALSE
      )
    }
  }

  # ---- per-group summaries --------------------------------------------
  conf_level <- 1 - alpha
  groups <- if (is.factor(gv)) levels(gv) else sort(unique(as.character(gv)))
  # coerce to character for uniform handling
  gv_chr <- as.character(gv)

  rows <- lapply(groups, function(g) {
    idx <- gv_chr == g & !is.na(gv_chr)
    y   <- ov[idx]
    y   <- y[!is.na(y)]
    n   <- length(y)
    n_acc <- sum(y)
    rate  <- if (n > 0) n_acc / n else NA_real_
    # CI
    ci <- .disp_rate_ci(n_acc, n, ci_method, conf_level, rate, alpha)
    data.frame(
      group      = g,
      n          = n,
      n_accepted = n_acc,
      rate       = rate,
      lower_ci   = ci[1L],
      upper_ci   = ci[2L],
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)

  # ---- reference row values -------------------------------------------
  ref_row <- result[result$group == ref_group, , drop = FALSE]
  ref_rate  <- ref_row$rate
  ref_n     <- ref_row$n
  ref_n_acc <- ref_row$n_accepted

  # ---- compute disparity columns --------------------------------------
  abs_diff   <- numeric(nrow(result))
  rel_risk   <- numeric(nrow(result))
  rr_lower   <- rep(NA_real_, nrow(result))
  rr_upper   <- rep(NA_real_, nrow(result))
  p_value    <- rep(NA_real_, nrow(result))
  p_value_fmt <- rep(NA_character_, nrow(result))

  for (i in seq_len(nrow(result))) {
    g     <- result$group[i]
    ni    <- result$n[i]
    ni_acc <- result$n_accepted[i]
    ratei <- result$rate[i]

    if (g == ref_group) {
      abs_diff[i]    <- 0
      rel_risk[i]    <- 1
      rr_lower[i]    <- NA_real_
      rr_upper[i]    <- NA_real_
      p_value[i]     <- NA_real_
      p_value_fmt[i] <- NA_character_
    } else {
      abs_diff[i] <- (ratei - ref_rate) * 100

      # relative risk
      rr <- if (!is.na(ref_rate) && ref_rate > 0) ratei / ref_rate else NA_real_
      rel_risk[i] <- rr

      # RR CI via log method
      ni_acc_cc  <- if (ni_acc == 0)  ni_acc + 0.5  else ni_acc
      ref_acc_cc <- if (ref_n_acc == 0) ref_n_acc + 0.5 else ref_n_acc
      se_log_rr  <- sqrt(
        1 / ni_acc_cc - 1 / ni +
        1 / ref_acc_cc - 1 / ref_n
      )
      if (!is.na(rr) && rr > 0) {
        log_rr      <- log(rr)
        z_crit      <- stats::qnorm(1 - alpha / 2)
        rr_lower[i] <- exp(log_rr - z_crit * se_log_rr)
        rr_upper[i] <- exp(log_rr + z_crit * se_log_rr)
      }

      # two-proportion z-test p-value
      pv <- tryCatch(
        suppressWarnings(
          stats::prop.test(
            c(ni_acc, ref_n_acc),
            c(ni, ref_n),
            correct = FALSE
          )$p.value
        ),
        error = function(e) NA_real_
      )
      p_value[i] <- pv
      p_value_fmt[i] <- if (is.na(pv)) {
        NA_character_
      } else if (pv < 0.001) {
        "<0.001"
      } else {
        sprintf("%.3f", pv)
      }
    }
  }

  result$abs_diff    <- abs_diff
  result$rel_risk    <- rel_risk
  result$rr_lower    <- rr_lower
  result$rr_upper    <- rr_upper
  result$p_value     <- p_value
  result$p_value_fmt <- p_value_fmt

  # ---- sort: ref group first, then by rate descending -----------------
  is_ref  <- result$group == ref_group
  non_ref <- result[!is_ref, , drop = FALSE]
  non_ref <- non_ref[order(-non_ref$rate), , drop = FALSE]
  result  <- rbind(result[is_ref, , drop = FALSE], non_ref)
  rownames(result) <- NULL

  class(result) <- c("mysterycall_disparities_table", "data.frame")
  result
}

# Internal -- compute rate CI by method
.disp_rate_ci <- function(n_acc, n, method, conf_level, rate, alpha) {
  if (n == 0L) return(c(NA_real_, NA_real_))
  if (method == "wilson") {
    ci <- suppressWarnings(
      stats::prop.test(n_acc, n, conf.level = conf_level, correct = FALSE)$conf.int
    )
    return(as.numeric(ci))
  }
  if (method == "exact") {
    ci <- stats::binom.test(n_acc, n, conf.level = conf_level)$conf.int
    return(as.numeric(ci))
  }
  # wald
  z   <- stats::qnorm(1 - alpha / 2)
  se  <- sqrt(rate * (1 - rate) / n)
  return(c(rate - z * se, rate + z * se))
}

#' Print a mysterycall_disparities_table
#'
#' @param x A \code{mysterycall_disparities_table} object.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
print.mysterycall_disparities_table <- function(x, ...) {
  cat("Disparity table (", nrow(x), " groups)\n", sep = "")
  cat(sprintf(
    "%-20s %6s %6s %8s   %-15s  %8s  %-20s  %s\n",
    "Group", "n", "n_acc", "Rate", "95% CI", "Abs.Diff", "RR (95% CI)", "p-value"
  ))
  cat(strrep("-", 100), "\n")
  for (i in seq_len(nrow(x))) {
    row <- x[i, ]
    rate_fmt <- sprintf("%.1f%%", row$rate * 100)
    ci_fmt   <- sprintf("%.1f%%-%.1f%%", row$lower_ci * 100, row$upper_ci * 100)
    ad_fmt   <- if (row$group == x$group[1L] && row$abs_diff == 0) {
      "(ref)"
    } else {
      sprintf("%+.1f pp", row$abs_diff)
    }
    rr_fmt <- if (is.na(row$rr_lower)) {
      sprintf("%.2f (ref)", row$rel_risk)
    } else {
      sprintf("%.2f (%.2f-%.2f)", row$rel_risk, row$rr_lower, row$rr_upper)
    }
    pv_fmt <- if (is.na(row$p_value_fmt)) "(ref)" else row$p_value_fmt
    cat(sprintf(
      "%-20s %6d %6d %8s   %-15s  %8s  %-20s  %s\n",
      row$group, row$n, row$n_accepted, rate_fmt, ci_fmt, ad_fmt, rr_fmt, pv_fmt
    ))
  }
  invisible(x)
}
