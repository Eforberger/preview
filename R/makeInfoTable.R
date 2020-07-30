
makeInfoTable <- function(info_src, group_name, use_first_as_row_names = TRUE, colwidth = 100)
{

    my_tbl_theme <- gridExtra::ttheme_default(
        core = list(fg_params=list(cex = 1.0)),
        colhead = list(fg_params=list(cex = 1.0)),
        rowhead = list(fg_params=list(cex = 1.0)))

    info_col_index <- match(group_name,colnames(info_src), nomatch = -1)
    if (info_col_index != -1)
    {
        info_col <- info_src[,info_col_index]
        #rownames(info_col) <-info_src[,1]
        A <- function(x) paste(strwrap(x, width = colwidth), collapse = "\n")
        info_col <- lapply(info_col, A)
    }
    else
    {
        warning(paste("makeInfoTable: No info found for",group_name,sep=" "))
        return(gridExtra::tableGrob(c("NULL","NULL"),  theme=gridExtra::ttheme_default(base_size = 9)) )
    }

    title <- grid::textGrob(group_name,gp=grid::gpar(fontsize=12))
    padding <- grid::unit(5,"mm")
    tbl <- gridExtra::tableGrob(info_col,  cols = colnames(info_col), rows = info_src[,1],  theme=my_tbl_theme)
    tbl <- gtable::gtable_add_rows(tbl, heights = grid::grobHeight(title) + padding, pos = 0)
}
