#' Cleanup
#'
#' Removes the first two rows of the data frame df. Qualtrics data has two extra rows at the top that can mess up data manipulation.
#'
#'@param df A data frame from qualtrics.
#'
#'@return A data frame
#'@export
cleanup <- function(df)
{
    # this just gets rids of the extra rows qualtrics adds
    df <- df[-c(2),]
    df <- df[-c(1),]
}
