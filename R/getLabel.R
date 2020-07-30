
#' Retrieves the label for a graph.
#'
#' getLabelByGrouping returns the label for a graph via a data frame that matches each group and subgroup to a string. The data frame has 3 columns minimum - subgroup, group and label.
#'
#'
#'@param labels TODO
#'@param group_name TODO
#'@param subgroup_name TODO
#'@param linesize When should we wrap our label to a new line?
#'
#'@importFrom rlang .data
#'
#'@return A data frame
#'@export
getLabelByGrouping <- function(labels, group_name, subgroup_name, linesize = 100, ...)
{
    #print("getLabelByGrouping")
    grouped_lbls <- dplyr::filter(labels, .data$group == group_name)
    lbl <- dplyr::filter(grouped_lbls, .data$subgroup == subgroup_name)$label
    if (is.na(lbl) | is.null(lbl))
    {
        lbl <- "LABEL NOT FOUND"
    }
    return(paste(strwrap(lbl, width = linesize), collapse = "\n")) # we use strwrap to add in newlines, otherwise the question prints weird in the graphs.
}

#' Retrieves the label for a graph.
#'
#' getAllLabelsByGrouping returns a multi line label containing all questions for all columns with the matching group and subgroup. This is done by first getting the names of all rows with the specified group and subgroup. We then search the data frame labels for columns with the same names. The contents of each column are placed into a list. Once all columns have been found, the list is turned into one big string and returned as the final label.
#' The idea is that while questions might have the same group and subgroup, they might be phrased differently, and we want to display that difference. For example, "I hate bannanas." and "I hate apples" might both be in the same group and subgroup because we are testing how people feel about fruit - however, the two questions themselves are NOT the same, and we wish to show that.
#' This function is meant to work with qualtrics data. Qualtrics data contains two extra rows at the top, the first of which contains the question asked to get the data for that column. The idea is that you peel that row off the data, and pass it into getLabelByOrg along with a column name, and will get back the question/label. To change when the label breaks, change the argument linesize.
#'
getAllLabelsByGrouping <- function(labels, group_name, subgroup_name, org, linesize = 100, ...)
{
    lbl_list <- list()

    # if the subgroup started with a number, we put X_ infront of it. We need to remove that now:
    subgroup_name <- stringr::str_replace(subgroup_name, "X_", "")

    # get all names in org with the same group and subgroup
    target_org <- dplyr::filter(org, .data$group == group_name, .data$subgroup == subgroup_name)

    # is target_org null?
    if (is.null(target_org))
    {
        lbl <- "LABEL NOT FOUND"
    }

    target_org <- dplyr::mutate(target_org, label = "NOT FOUND")
    for (i in 1:nrow(target_org))
    {
        target_org$label[i] <- dplyr::select(labels, matches(target_org$name[i]))[1,1]
    }

    # remove duplicate labels
    #lbl_list <- unique(lbl_list)

    # label them with the category they go with
    for (lbl_i in 1:nrow(target_org))
    {
        lbl_list[length(lbl_list)+1] <-  paste(target_org$category[lbl_i],":",strwrap(target_org$label[lbl_i], width = linesize), collapse = "\n")
    }
    return(paste(lbl_list, collapse= "\n"))
}

#' Retrieves the label for a graph.
#'
#' getLabelByName returns the label for a graph via a data frame with the same column names as the data df, and the rows contain the label(s) for the question with the same name as that column. This function is meant to work with qualtrics data. Qualtrics data contains two extra rows at the top, the first of which contains the question asked to get the data for that column. The idea is that you peel that row off the data, and pass it into getLabelByName along with a column name, and will get back the question/label. To change when the label breaks, change the argument linesize.
#'
#'@param labels a df containing columns with the column names corresponding to columns names from the data, and the row(s) containing the label that goes with each column.
#'@param question_name the column name (aka question name) we finding a name for
#'@param linesize When should we wrap our label to a new line?
#'
#'@return A data frame
#'@export
getLabelByName <- function(labels, question_name, linesize = 100, ...)
{
    # find the column in df with the column name question_name from above
    label_index <- match(question_name,colnames(labels), nomatch = -1)
    if (label_index != -1)
    {
        lbl <- labels[,label_index]
    }
    else
    {
        lbl <- "LABEL NOT FOUND"
        warning(paste('Label for name ', question_name, ' could not be found.',sep=""))
    }
    #TODO - issue
    #return(paste(strwrap(labels[1,label_index], width = linesize), collapse = "\n")) # we use strwrap to add in newlines, otherwise the question prints weird in the graphs.
    return(lbl)

}
