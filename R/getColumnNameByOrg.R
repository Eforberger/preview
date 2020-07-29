#' T
#'
#' Q
#'
#'@param org TODO
#'@param group_name TODO
#'@param subgroup_name TODO
#'
#'@importFrom rlang .data
#'
#'@return A data frame
#'@export
getQuestionName <- function(org, group_name, subgroup_name)
{
    # if the subgroup started with a number, we put X_ infront of it. We need to remove that now:
    subgroup_name <- stringr::str_replace(subgroup_name, "X_", "")
    #cat("searching for name with group",group_name,"subgroup",subgroup_name,"...")
    question_name <- (dplyr::filter(dplyr::filter(org, .data$group == group_name), .data$subgroup == subgroup_name))$name
    if (is.na(question_name) | is.null(question_name))
    {
        question_name <- "LABEL NOT FOUND"
    }
    #cat("returning with value", question_name,"\n")
    return(question_name)
}
