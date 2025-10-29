#' Internal helper to validate and prepare sf inputs for spatial workflows.
#'
#' This function ensures objects share a common CRS, have valid geometries,
#' contain the expected geometry types, possess sensible bounding boxes, and
#' do not contain empty geometries. Invalid geometries are repaired with
#' `sf::st_make_valid()` when possible.
#'
#' @param ... Named sf objects to validate.
#' @param expected_types Named list mapping object names to allowed geometry
#'   types. A character vector applies to all inputs when names are omitted.
#' @param auto_fix Logical indicating whether invalid geometries should be
#'   repaired automatically. Defaults to `TRUE`.
#' @param target_crs Optional CRS (numeric EPSG code, proj4string, or `sf::crs`
#'   object) that all objects should be transformed into before bounding box
#'   checks. When `NULL`, the CRS of the first object is used.
#' @param context Character string identifying the calling context for clearer
#'   error messages.
#'
#' @return A named list of validated sf objects.
#' @keywords internal
validate_sf_inputs <- function(...,
                               expected_types = NULL,
                               auto_fix = TRUE,
                               target_crs = NULL,
                               context = "geospatial operation") {
  objects <- list(...)
  if (!length(objects)) {
    stop("No sf objects supplied for validation.")
  }

  object_names <- names(objects)
  if (is.null(object_names) || any(!nzchar(object_names))) {
    object_names <- paste0("object_", seq_along(objects))
    names(objects) <- object_names
  }

  if (!is.null(expected_types)) {
    if (is.character(expected_types)) {
      expected_types <- rep(list(expected_types), length(objects))
      names(expected_types) <- object_names
    } else if (is.list(expected_types)) {
      missing_names <- setdiff(object_names, names(expected_types))
      if (length(missing_names)) {
        expected_types[missing_names] <- expected_types[[1]]
      }
    } else {
      stop("`expected_types` must be a character vector or named list.")
    }
  }

  ref_crs <- if (!is.null(target_crs)) {
    sf::st_crs(target_crs)
  } else {
    sf::st_crs(objects[[1]])
  }

  if (is.na(ref_crs)) {
    stop(sprintf("Reference CRS is missing for %s.", context))
  }

  # Validate CRS is a recognized coordinate system
  if (!is.null(ref_crs) && !sf::st_is_longlat(sf::st_crs(ref_crs))) {
    # Ensure projected CRS has valid bounds
    tryCatch({
      bbox_test <- sf::st_bbox(sf::st_point(c(0, 0)) %>% sf::st_sfc(crs = ref_crs))
      if (any(is.infinite(bbox_test) | is.na(bbox_test))) {
        stop(sprintf("Invalid CRS bounds detected for %s.", context))
      }
    }, error = function(e) {
      stop(sprintf("CRS validation failed for %s: %s", context, e$message))
    })
  }

  sanitize_object <- function(obj, name) {
    if (!inherits(obj, "sf")) {
      stop(sprintf("`%s` must be an sf object for %s.", name, context))
    }
    if (!nrow(obj)) {
      stop(sprintf("`%s` has no rows; cannot proceed with %s.", name, context))
    }

    obj_crs <- sf::st_crs(obj)
    if (is.na(obj_crs)) {
      stop(sprintf("`%s` is missing a defined CRS for %s.", name, context))
    }

    if (!is.null(target_crs)) {
      obj <- sf::st_transform(obj, ref_crs)
    } else if (!identical(obj_crs, ref_crs)) {
      obj <- sf::st_transform(obj, ref_crs)
    }

    geom <- sf::st_geometry(obj)
    if (is.null(geom)) {
      stop(sprintf("`%s` lacks a geometry column required for %s.", name, context))
    }

    empty_idx <- which(sf::st_is_empty(geom))
    if (length(empty_idx)) {
      stop(sprintf("`%s` contains empty geometries (rows: %s) incompatible with %s.",
                   name, paste(empty_idx, collapse = ", "), context))
    }

    invalid_idx <- which(!sf::st_is_valid(obj))
    if (length(invalid_idx)) {
      if (isTRUE(auto_fix)) {
        obj <- sf::st_make_valid(obj)
        still_invalid <- which(!sf::st_is_valid(obj))
        if (length(still_invalid)) {
          stop(sprintf("`%s` has geometries that remain invalid after repair (rows: %s) during %s.",
                       name, paste(still_invalid, collapse = ", "), context))
        }
      } else {
        stop(sprintf("`%s` contains invalid geometries (rows: %s) disallowed in %s.",
                     name, paste(invalid_idx, collapse = ", "), context))
      }
    }

    if (!is.null(expected_types)) {
      allowed_types <- expected_types[[name]]
      if (is.null(allowed_types)) {
        allowed_types <- expected_types[[1]]
      }
      if (!is.null(allowed_types)) {
        geom_types <- unique(as.character(sf::st_geometry_type(obj)))
        if (!all(geom_types %in% allowed_types)) {
          stop(sprintf(
            "`%s` contains geometry types [%s]; expected only [%s] in %s.",
            name,
            paste(geom_types, collapse = ", "),
            paste(allowed_types, collapse = ", "),
            context
          ))
        }
      }
    }

    bbox <- sf::st_bbox(obj)
    if (any(!is.finite(bbox))) {
      stop(sprintf("`%s` has a bounding box with non-finite values in %s.", name, context))
    }
    if (bbox["xmin"] >= bbox["xmax"] || bbox["ymin"] >= bbox["ymax"]) {
      stop(sprintf("`%s` has a degenerate bounding box incompatible with %s.", name, context))
    }

    obj
  }

  objects <- Map(sanitize_object, objects, object_names)

  if (length(objects) > 1) {
    bbox_sfc <- lapply(objects, function(x) sf::st_as_sfc(sf::st_bbox(x)))
    bbox_intersection <- Reduce(sf::st_intersection, bbox_sfc)
    if (sf::st_is_empty(bbox_intersection)) {
      stop(sprintf("Bounding boxes of supplied objects do not overlap for %s.", context))
    }
  }

  objects
}
