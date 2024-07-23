physician_age <- function(df, age_column) {
  # Calculate the median age
  median_age <- round(median(df[[age_column]], na.rm = TRUE), 2)

  # Calculate the 25th and 75th percentiles
  q25 <- quantile(df[[age_column]], probs = 0.25, na.rm = TRUE)
  q75 <- quantile(df[[age_column]], probs = 0.75, na.rm = TRUE)

  # Round the percentiles to one decimal place
  q25 <- round(q25, 1)
  q75 <- round(q75, 1)

  # Create the sentence
  sentence <- paste0(
    "The median age of the dataset was ", median_age,
    " (IQR 25th percentile ", q25, " to 75th percentile ", q75, ")."
  )

  return(sentence)
}
