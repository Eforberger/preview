
# returns a list of plots, one for each subgroup within the group
# TODO - are we still using the textual and numerical data - assume so for now.1
# TODO don't cleanup the data, remove it
# TODO figure out the question stuff
# Pass in the raw numerical data frame. No altering, don't get rid of the top rows
#' T
#'
#' Assumes df has been cleaned and converted.
#'
#'@param df
#'@param org
#'@param group_name
#'@param usePercent
#'@param getAxisLimits
#'@param getAxisLabels
#'@param getGraphLabels
#'@param text_df
#'@param label_df
#'
#'@importFrom rlang .data
#'
#'@return
#'@export
makeGraphs <- function(df, org, group_name, usePercent = TRUE, getAxisLimits = getLimitsX, getAxisLabels = getLabelsX, getGraphLabels = getLabelByName, text_df = NULL, label_df = NULL)
{
    group_data <- groupData(df, org, .data$group_name)# 1 - filtering out all non-numerical data
    if (is.null(group_data))
    {
        warning("makeGraphs: Call to groupData returned NULL. makeGraphs will return with NULL as well. ")
        return(NULL)
    }
    scenario_plots <- list()
    p_index = 1

    #print(group_data)
    # get the means and standard deviations for that scenario and store them in a new data frame
    summ_mean <- dplyr::summarise_all(dplyr::select_if(dplyr::group_by(group_data, .data$category), is.numeric), list(~mean(., na.rm = TRUE)))
    summ <- dplyr::summarise_all(dplyr::select_if(dplyr::group_by(group_data, .data$category), is.numeric), list(~sd(., na.rm = TRUE), ~mean(., na.rm = TRUE)))

    num_data <- dplyr::select_if(group_data, is.numeric)
    for (name in colnames(num_data))
    {
        #print(paste(name,"mean", sep="_"))
        # TODO : check if the column named name is numerical data 1 - don't need to do this since i do it when creating group_Data, but if i change the org structure, then i may need to at a later date

        # create labels and limits for the x axis
        labels <- getAxisLabels(text_df, name)
        limits <- getAxisLimits(labels)

        # Remove "X_" from subgroup name if previously added so we can have the plain subgroup name to put in the graph title
        lbl_name <- stringr::str_replace(name,"_X","")# remove X_ from subgroup_name if we added it previously (We would do so if it started with a number)
        # we want to drop all rows in the column name that are NA:
        dropped_group_data <- dplyr::drop_na(group_data, dplyr::any_of(name))

        # same thing with summ and summ_mean - TODO
        #dropped_summ_mean <- summ_mean %>% filter(category %in% unique(dropped_group_data$category))
        #summ <- summ %>% filter(category %in% unique(dropped_group_data$category))
        #cat("summ unique:",unique(dropped_summ_mean$category),"\n")
        #cat("length:",length(unique(dropped_summ_mean$category)),"\n")
        #print(dropped_summ_mean)

        # create mean label y positions vector
        vec <- seq(0.7, 0.69+0.08*length(unique(summ_mean$category)), by = +0.08)

        if (usePercent)
        {
            # make the graph
            p <- ggplot2::ggplot(data = dropped_group_data, ggplot2::aes_string(x=name), position = ggplot2::position_dodge(1.0)) +
                # draw standard deviation
                ggplot2::geom_tile(data=dplyr::mutate_at(summ, dplyr::vars(dplyr::contains("sd")), ~.*2), ggplot2::aes_string(y = .5, x =paste(name,"mean", sep="_"),  height = Inf, width = paste(name,"sd", sep="_"), fill = "category", group = "category"), alpha = 0.3, show.legend = FALSE) + #x
                ggplot2::geom_bar(data =dropped_group_data, mapping = ggplot2::aes_string(x = name, y = "..prop..", group = "category", fill = "category", color = "category"), stat = "count", position = ggplot2::position_dodge(1)) +
                # set axis scales
                ggplot2::scale_x_discrete(limits = limits, labels = labels) +#, labels = labels)  +
                ggplot2::expand_limits(x = 1) +
                ggplot2::ylab("Percentage") +
                ggplot2::ylim(c(0, 1)) +
                # write question
                ggplot2::ggtitle(label = paste(group_name, " - ",lbl_name, sep=""), subtitle = getGraphLabels(labels = label_df, question_name = getQuestionName(org, group_name, name), group_name = group_name, subgroup_name = name, org = org)) +
                # create and label mean line
                ggplot2::geom_vline(data=summ_mean, ggplot2::aes_string(xintercept=name, color = "category"), linetype =2, show.legend = FALSE) +
                ggplot2::geom_label(data=summ_mean, y = vec, ggplot2::aes_string(x = name, label = name, group = "category", fill = "category"  ), show.legend = FALSE, nudge_y =.5) +
                # label each bar with count
                ggplot2::geom_text(stat = "count", color = "black", ggplot2::aes_string(x = name, y = "..prop.. - 0.05", label= "..count..", group = "category"), position = ggplot2::position_dodge(1), show.legend = FALSE)
        }
        else # use count instead
        {

        }
        # place the graph in the list
        scenario_plots[[p_index]] = p
        p_index = p_index + 1
    }
    return(scenario_plots)
}
