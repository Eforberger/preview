
#' T
#'
#' Q
#'
#'@param group_name TODO
#'@param subgroup_name TODO
#'@param linesize TODO
#'@param org TODO
#'@param raw_df TODO
#'@param label_df TODO
#'
#'
#'@return A data frame
#'@export
getLabel <- function(group_name, subgroup_name, linesize = 100, org = NULL, raw_df = NULL, label_df = NULL)
{
    if (!is.null(label_df))
    {
        q <- getLabelByName(label_df, group_name, subgroup_name, linesize)
    }
    else
    {
        if (!is.null(org) & !is.null(raw_df))
        {
            # pull labels from first row of raw qualtrics data
            labels <- dplyr::top_n(raw_df,1) #TODO - may be error
            colnames(labels) <- tolower(colnames(labels))
            # get question name
            qname <- getQuestionName(org, group_name, subgroup_name)
            q <- getLabelByName(labels, qname, linesize)
        }
        else
        {
            warning("Please supply either a raw_df and org or df_label. Returning NOT FOUND for label.")
            q <- "NOT FOUND"
        }
    }
    return(q)
}


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
#' getLabelByOrg returns a multi line label containing all questions for all columns with the matching group and subgroup. This is done by first getting the names of all rows with the specified group and subgroup. We then search the data frame labels for columns with the same names. The contents of each column are placed into a list. Once all columns have been found, the list is turned into one big string and returned as the final label.
#' The idea is that while questions might have the same group and subgroup, they might be phrased differently, and we want to display that difference. For example, "I hate bannanas." and "I hate apples" might both be in the same group and subgroup because we are testing how people feel about fruit - however, the two questions themselves are NOT the same, and we wish to show that.
#' This function is meant to work with qualtrics data. Qualtrics data contains two extra rows at the top, the first of which contains the question asked to get the data for that column. The idea is that you peel that row off the data, and pass it into getLabelByOrg along with a column name, and will get back the question/label. To change when the label breaks, change the argument linesize.
#'
#'#TODO!!!!!!!

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
