
#' Collects the data from the specified group
#'
#' TODO
#'
#'@param df A data frame from qualtrics.
#'@param org The organizational data frame.
#'@param group_name A string containing the name of the group to gather data in.
#'@param ignore_case A boolean signifying if we should ignore case with group names, column names, and subgroup names. Default is TRUE.
#'
#'@importFrom rlang .data
#'
#'@return A data frame
#'@export
groupData <- function(df, org, group_name, ignore_case = TRUE)
{
    if (ignore_case)
    {
        # convert everything to lower case
        group_name <- tolower(group_name)
        colnames(df) <- tolower(colnames(df))
        org$group <- tolower(org$group)
        org$name <- tolower(org$name)
    }
    # sort df to get only columns from the group
    sorted_org <- dplyr::filter(org, .data$group == group_name)
    if (nrow(sorted_org) == 0)
    {
        warning(paste("Could not find any data for group ",group_name,sep=""))
        return(NULL)
    }
    sorted_df <- dplyr::select(df, dplyr::one_of(sorted_org$name)) # get only columns from the data frame in the group group_name
    # get a list of all the categories
    category_names <- unique(sorted_org$category)
    # then do the lapply thingy!
    x <- lapply(category_names, groupDataHelper, df = sorted_df, grouped_org = sorted_org)
    #print(x)
    # TODO if the rows don't match up, error
    #print(lapply(x[[1]],class))
    #print(lapply(x[[2]],class))
    c <- dplyr::bind_rows(x)
    if (is.null(c))
    {
        warning(paste("groupData: No data for group ", group_name, "found. Returning Null.", sep = ""))
    }
    return(c)
}



#' T
#'
#' Q
#'
#'@param category_name
#'@param df
#'@param grouped_org
#'
#'@importFrom rlang .data
#'@importFrom dplyr %>%
#'
#'@return
#'@export
groupDataHelper <- function(category_name, df, grouped_org)
{
    if (ncol(grouped_org) == 0 || nrow(grouped_org) == 0)
    {
        warning("Grouped org has 0 columns or rows. Returning NULL")
        return(NULL)
    }
    categorized_grouped_org <- dplyr::filter(grouped_org, .data$category == category_name)
    categorized_df <- dplyr::select(df, dplyr::one_of(categorized_grouped_org$name))
    # rename the cols of categorized_df so that they are the name of the supbgroup
    for (colname_index in 1:length(colnames(categorized_df)))
    {
        # get the index of the row containing the name
        index = which(categorized_grouped_org$name == colnames(categorized_df)[colname_index])
        # check that the new name obeys R's variable laws
        if (grepl("^[0-9]",categorized_grouped_org$subgroup[index]))
        {
            #cat("A subgroup name starts with an integer, which creates an invalid column name later. Please change it so that it starts with a non-integer.\n")
            #cat("This issue occurs in name",categorized_grouped_org$name[index],"\n")
            #cat("During this code run, we will place 'X_' infront of it.\n")
            categorized_grouped_org$subgroup[index] <- paste("X_",categorized_grouped_org$subgroup[index], sep="")
        }
        # set the name of the column
        #cat('changing',colnames(categorized_df)[colname_index],'to',categorized_grouped_org$subgroup[index],'\n')
        colnames(categorized_df)[colname_index] <- categorized_grouped_org$subgroup[index]
    }

    b <- dplyr::select(categorized_df, sort(names(categorized_df)))
    if (ncol(b) != 0)
    {
        # get rid of rows with just empty values
        b[b ==""] <- NA # possible future error with this setting b to dim(0,0)
        b <- Filter(function(x)!all(is.na(x)),b)
        # make a column named category and give it the value of the category name from the org data frame
        b <- dplyr::mutate(b, .data$category == category_name)

    }
    else
    {
        warning(paste('No data for category ',category_name, 'from group ',grouped_org$group[1], ' could be found. Returning NULL', sep = ""))
        #cat("WARNING: no versions for group", marker, "found in prefix", prefix, ".\n")
        NULL
    }
}
