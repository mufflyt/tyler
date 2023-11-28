#' Apply and Save Image Tint
#'
#' @description This function applies a tint to an image and saves the tinted image.
#' The tints are determined from a given color palette.
#'
#' @param image_path Character string specifying the file path of the image to be tinted.
#' @param alpha Numeric value between 0 and 1 specifying the degree of the tint.
#' Higher values will result in stronger tints.
#' @param palette_name Character string specifying the color palette to use for the tint.
#' Default is 'viridis'.
#' @param save_dir Character string specifying the directory to save the tinted images.
#' @param n_colors Integer specifying the number of different colors to use from the color palette.
#'
#' @return Saves tinted images in the specified directory.
#' @import imager
#' @import grDevices
#' @import scales
#' @export
apply_and_save_tint <- function(image_path, alpha = 0.5, palette_name = "viridis", save_dir = ".", n_colors = 5) {
  message("Loading original image...")
  original_image <- imager::load.image(image_path)

  message("Retrieving color palette...")
  colors_to_use <- scales::viridis_pal(option = palette_name)(n_colors)

  message("Getting image dimensions...")
  height <- dim(original_image)[1]
  width <- dim(original_image)[2]

  for (i in seq_along(colors_to_use)) {
    color_name <- colors_to_use[i]
    message(sprintf("Applying %s tint...", color_name))

    color_vector <- grDevices::col2rgb(color_name) / 255
    color_image <- imager::imfill(height, width, val = color_vector)

    tinted_image <- (1 - alpha) * original_image + alpha * color_image

    message("Displaying tinted image...")
    plot(tinted_image)

    save_path <- file.path(save_dir, sprintf("dollar_bill_tint_%d_%s.jpg", i, color_name))
    message(sprintf("Tint %d (%s) image saved at %s", i, color_name, save_path))
    imager::save.image(tinted_image, save_path)
  }
}
