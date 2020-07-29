
# TODO make number of columns changeable
# TODO make filename specificalyb;le
#' Creates and saves a file of graphs, tables, and text
#'
#' Q
#'
#'@param plot_list A list of plots.
#'@param page_title A string containing the title of page. This will be used in generating the file name.
#'@param table_grobs An optional list of table grobs to place at the top of the page. We suggest tables containing the mean/standard deviation.
#'@param tables_height The height of the tables part of the page in total
#'@param width The width of the page
#'@param plot_height The height of each plot
#'@param unit The units the preceding measurements are in
#'@param columns How many columns the plots should be arranged in.
#'
#'@return A data frame
#'@export
createPage <- function(plot_list, page_title, table_grobs = NULL, tables_height = 6, width = 9, plot_height = 4, unit = "in", columns = 1)
{
    if (is.null(plot_list))
    {
        warning("createPage: plot_list argument is NULL. Returning without creating page. Please supply a non-null argument.")
        return(NULL)
    }
    plot_grobs <- gridExtra::arrangeGrob(grobs = plot_list, ncol=columns, width = width) #ERROR: aesthetics length


    if (is.null(table_grobs))
    {
        height = plot_height*ceiling(length(plot_grobs)/columns)
        g <- gridExtra::arrangeGrob(grobs = list(plot_grobs), ncol=1, heights=unit(c(height),c(unit)), width=unit(c(8),c(unit)), top=page_title)
    }
    else
    {
        height = tables_height + plot_height*ceiling(length(plot_grobs)/columns)
        g <- gridExtra::arrangeGrob(grobs = list(table_grobs, plot_grobs), ncol=1, heights=unit(c(height),c(unit)), width=unit(c(8),c(unit)), top=page_title)
    }
    fname = paste(page_title,date(),".png",sep="_")
    ggplot2::ggsave(file=fname, g, dpi = 600, width = width, height = height, units = unit, device = "png")
    cat("Scenario",fname, "sucessfully graphed and saved!\n")
}
