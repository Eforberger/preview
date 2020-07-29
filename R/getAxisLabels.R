
#' T
#'
#' Q
#'
#'@param text_df TODO
#'@param question_name TODO
#'@param label_df TODO
#'@param default TODO
#'
#'@return A data frame
#'@export
getLabelsX <- function(text_df, question_name, label_df = NULL, default = c("-3","-2","-1","0","1","2","3"))
{
    if (is.null(text_df) | is.null(label_df))
    {
        warning('text_df is NULL. Returning default scale.')
        return(default)
    }
    # if textual data is given, use that
    index = which(colnames(text_df) == question_name)
    if (!is.numeric(index))
    {
        warning('Could not find a column with arg name question_name in argument text_df')
        return(label_df[,1])
    }
    # for column c in scales
    for (c in 1:ncol(label_df))
    {
        ret = TRUE
        for (j in 1:nrow(text_df))
        {
            if( is.null(text_df[j,index]) || length(text_df[j,index]) == 0)
            {
                next
            }
            if (text_df[j,index] == "")
            {
                next
            }
            if( is.na(text_df[j,index]))
            {
                next
            }

            if (!(tolower(text_df[j,index]) %in% tolower(label_df[,c])))
            {
                ret = FALSE
                #cat("Failed to find scale for",colnames(text_data)[index],":[",tolower(text_data[j,index]),"] in col [",scales[1,c],"].\n")
                break
            }
        }
        if (ret)
        {
            l <- list()
            for (f in (label_df[,c]))
            {
                l[length(l)+1] <- paste(strwrap(f, width = 50), collapse = "\n")
            }
            return(label_df[,c])
        }

    }

    return(default) # default scale
}
