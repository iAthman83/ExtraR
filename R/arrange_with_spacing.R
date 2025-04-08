#' Arrange With Spacing
#'
#' This function takes a data frame and column then groups similar rows of that column
#' Arranges the Overall analysis into descending order and then add a blank space between groups
#' For easy readability of the analysis
#'
#' @param df A data frame to be formatted
#' @param col A column to be used for grouping.
#' @return A new data frame with with all operations performed.
#' @export

arrange_with_spacing <- function(df, col) {
  # Compute run-length encoding for the target column
  r <- rle(as.character(df[[col]]))

  # Initialize an empty data frame with the same columns as df
  new_df <- df[0, ]
  idx <- 1

  # Loop over each group and append the rows and an empty row (except after the last group)
  for (i in seq_along(r$lengths)) {
    group_length <- r$lengths[i]
    group_rows <- df[idx:(idx + group_length - 1), ]

    # Sort the group by the stat_overall column in descending order
    group_rows <- group_rows[order(-group_rows$stat_overall), ]

    new_df <- rbind(new_df, group_rows)
    # Insert an empty row if this is not the last group
    if(i < length(r$lengths)) {
      # Create an empty row with NA values
      empty_row <- setNames(as.list(rep(NA, ncol(df))), names(df))

      # Copy values from the first and second columns of the last row
      empty_row[[1]] <- group_rows[nrow(group_rows), 1]
      empty_row[[2]] <- group_rows[nrow(group_rows), 2]

      new_df <- rbind(new_df, empty_row)
    }
    idx <- idx + group_length
  }
  return(new_df)
}
