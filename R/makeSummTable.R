#TODO - check if null
# TODO get scenarios
# TODO allow custom theme
#' T
#'
#' Q
#'
#'@param df A data frame from qualtrics.
#'@param org TODO
#'@param group_name TODO
#'@param width TODO
#'@param height TODO
#'@param unit TODO
#'
#'@importFrom rlang .data
#'
#'@return A data frame
#'@export
getSummTables<- function(df, org, group_name, width = 9, height = 6, unit = "in")
{
    # Table theme
    my_tbl_theme <- gridExtra::ttheme_default(
        core = list(fg_params=list(cex = .50)),
        colhead = list(fg_params=list(cex = .6)),
        rowhead = list(fg_params=list(cex = .6)))

    group_data <- groupData(df, org, group_name)# 1 - filtering out all non-numerical data
    table_list <- list()
    t_index = 1

    summ_mean <- dplyr::summarise_all(dplyr::select_if(dplyr::group_by(group_data, .data$category), is.numeric), list(~mean(., na.rm = TRUE)))
    summ_sd <- dplyr::summarise_all(dplyr::select_if(dplyr::group_by(group_data, .data$category), is.numeric), list(~sd(., na.rm = TRUE)))

    mean_tbl <- gridExtra::tableGrob(summ_mean, rows = rownames(summ_mean), cols = colnames(summ_mean), theme=my_tbl_theme)
    sd_tbl <- gridExtra::tableGrob(summ_sd, rows = rownames(summ_sd), cols = colnames(summ_sd), theme=my_tbl_theme)

    title_m <- grid::textGrob("Means",gp=grid::gpar(fontsize=9))
    title_s <- grid::textGrob("Standard Deviations",gp=grid::gpar(fontsize=9))
    padding <- unit(5,"mm")

    mean_tbl <- gtable::gtable_add_rows(mean_tbl, heights = grid::grobHeight(title_m) + padding, pos = 0)
    mean_tbl <- gtable::gtable_add_grob(mean_tbl, title_m, 1, 1, 1, ncol(mean_tbl))

    sd_tbl <- gtable::gtable_add_rows(sd_tbl, heights = grid::grobHeight(title_s) + padding, pos = 0)
    sd_tbl <- gtable::gtable_add_grob(sd_tbl, title_s, 1, 1, 1, ncol(sd_tbl))

    summ_tbl <- gridExtra::gtable_combine(mean_tbl, sd_tbl, along = 2, join = "outer")


    return(summ_tbl)
}
