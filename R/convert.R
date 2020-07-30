
#' Converts data based on the type markers in the column name.
#'
#' Q
#'
#'@param df A data frame from qualtrics.
#'@param markers TODO
#'
#'@return A data frame
#'@export
convertDataByName <- function(df, markers = list("numeric" = "_num", "logical" = "_log", "integer" = "_int", "complex" = '_com', "character" = "_char", "factor" = "_fac"))
{
    df <- cleanup(df)
    for (marker_index in 1:length(markers))
    {
        pnum <- dplyr::select(df,dplyr::contains(markers[[marker_index]]))
        # convert them
        if (names(markers)[marker_index] == "numeric")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.numeric)
        }
        if (names(markers)[marker_index] == "logical")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.logical)
        }
        if (names(markers)[marker_index] == "integer")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.integer)
        }
        if (names(markers)[marker_index] == "complex")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.complex)
        }
        if (names(markers)[marker_index] == "character")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.character)
        }
        if (names(markers)[marker_index] == "factor")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.factor)
        }
        col = 1
        while (col < ncol(df))
        {
            ni = match(colnames(df)[col],colnames(pnum), nomatch = -1)
            if (ni != -1)
            {
                df[,col] <- pnum[,ni]
            }
            col = col +1
        }
    }
    return(df)
}

#TODO - if type_name is not recognized

#' Converts data based on the types in the org data frame
#'
#' Q
#'
#'@param df A data frame from qualtrics.
#'@param org The organizational data frame.
#'
#'@importFrom rlang .data
#'
#'@return A data frame
#'@export
convertDataByOrg <- function(df, org)
{
    # todo if df is null
    # todo if org doesn't have the right columns

    # Types are: logical, numeric, integer, complex, character, factor
    type_list <- unique(org$type)
    for (type_name in type_list)
    {
        # group the corresponding columns in df
        sorted_org <- dplyr::filter(org, .data$type == type_name)
        converted_sub_df <- dplyr::select(df, dplyr::one_of(sorted_org$name))
        # convert them
        if (tolower(type_name) == "numeric")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.numeric)
        }
        if (tolower(type_name) == "logical")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.logical)
        }
        if (tolower(type_name) == "integer")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.integer)
        }
        if (tolower(type_name) == "complex")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.complex)
        }
        if (tolower(type_name) == "character")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.character)
        }
        if (tolower(type_name) == "factor")
        {
            converted_sub_df <- dplyr::mutate_all(converted_sub_df, as.factor)
        }
        # replace the cols in df with the converted versions
        col = 1
        while (col < ncol(df))
        {
            ni = match(colnames(df)[col],colnames(converted_sub_df), nomatch = -1)
            if (ni != -1)
            {
                df[,col] <- converted_sub_df[,ni]
            }
            col = col +1
        }
    }
    return(df)
}
