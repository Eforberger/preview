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
makePage <- function(plot_list, page_title, table_list = NULL, table_height = 6, width = 9, plot_height = 4, unit = "in", columns = 1, padding = .75)
{
    if (is.null(plot_list))
    {
        warning("createPage: plot_list argument is NULL. Returning without creating page. Please supply a non-null argument.")
        return(NULL)
    }

    # create plot grobs now that we know we have a list to create them from
    plot_grobs <- gridExtra::arrangeGrob(grobs = plot_list, ncol=columns, width = width) #ERROR: aesthetics length

    # set up heights
    p_height = plot_height*ceiling(length(plot_list)/columns)

    table_grobs = NULL
    t_height = 0

    if (!is.null(table_list))#else
    {
        # create table grobs, since we know we have a list to create them from.
        t_height = table_height*length(table_list)
        table_grobs <- gridExtra::arrangeGrob(grobs = table_list, ncol = 1, width = width, height = t_height, unit = unit)

        g <- gridExtra::arrangeGrob(grobs = list(table_grobs, plot_grobs), ncol=1, heights=unit(c(t_height, p_height),c(unit, unit)), width=unit(c(width),c(unit)), top=page_title, limitsize = FALSE)
    }  else
    {
        g <- gridExtra::arrangeGrob(grobs = list(plot_grobs), ncol=1, heights=unit(c(p_height),c(unit)), width=unit(c(width),c(unit)), top=page_title, limitsize = FALSE)
    }

    fname = paste(page_title,date(),".png",sep="_")
    ggplot2::ggsave(file=fname, g, dpi = 300, width = width+padding, height = p_height+t_height+padding, units = unit, device = "png", limitsize = FALSE)
    cat("Scenario",fname, "sucessfully graphed and saved!\n")
}
