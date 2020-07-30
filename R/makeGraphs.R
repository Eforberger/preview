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
makeGraphs <- function(df, org, group_name, usePercent = TRUE, getXAxisLimits = getAxisLimits, getXAxisLabels = getAxisLabels, getGraphLabels = getLabelByGrouping, text_df = NULL, graph_label_df = NULL)
{
    print("makeGraphs")
    # get data for the group
    group_data <- groupData(df, org, group_name)# 1 - filtering out all non-numerical data
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
    summ_all <- dplyr::summarise_all(dplyr::select_if(dplyr::group_by(group_data, .data$category), is.numeric), list(~sd(., na.rm = TRUE), ~mean(., na.rm = TRUE)))

    print("Made it to here!")
    # round the means and standard deviations
    #summ_all <- dplyr::mutate_at(summ_all, dplyr::vars(dplyr::contains("sd")), ~round(., digits = 3))
    #summ_mean <- dplyr::mutate_at(summ_mean, dplyr::vars(-dplyr::contains("category")), ~round(., digits = 3))

    # TODO - future feature.
    # get text versions for labels - https://stackoverflow.com/questions/34189531/subset-or-grep-for-selectively-pasting-using-geom-text-in-r
    #summ_mean_text <- dplyr::mutate_all(summ_mean, funs(paste("mean: ", ., sep="")))
    #summ_std_text <- dplyr::mutate_all(dplyr::select(summ_all, dplyr::contains("sd")), funs(paste("std: ", ., sep="")))

    # graph the numerical columns only and iterate through them, creating a graph for each column
    num_data <- dplyr::select_if(group_data, is.numeric)
    for (name in colnames(num_data))
    {
        # create labels and limits for the x axis
        labels <- getXAxisLabels(text_df, name)
        limits <- getXAxisLimits(labels)

        # Remove "X_" from subgroup name if previously added so we can have the plain subgroup name to put in the graph title
        lbl_name <- stringr::str_replace(name,"X_","")# remove X_ from subgroup_name if we added it previously (We would do so if it started with a number)
        # we want to drop all rows in the column name that are NA:
        dropped_group_data <- tidyr::drop_na(group_data, dplyr::any_of(name))#dplyr::drop_na(group_data, dplyr::any_of(name))


        # create mean label y positions vector
        vec <- seq(0.7, 0.69+0.08*length(unique(summ_mean$category)), by = +0.08)

        # short function to deal with NA's in mean when labeling
        mean_handle <- function(name) {
            if (is.na(name))
            {
                return("NA")
            }
            return(name)
        }

        if (usePercent)
        {
            # make the graph
            p <- ggplot2::ggplot(data = dropped_group_data, ggplot2::aes_string(x=name), position = ggplot2::position_dodge(1.0)) +
                # draw and label standard deviation
                #ggplot2::geom_tile(data=dplyr::mutate_at(summ_all, dplyr::vars(dplyr::contains("sd")), ~.*2), ggplot2::aes_string(y = .5, x =paste(name,"mean", sep="_"),  height = Inf, width = paste(name,"sd", sep="_"), fill = "category", group = "category"), alpha = 0.3, show.legend = FALSE) +
                #ggplot2::geom_label(data=summ_all, y = vec, ggplot2::aes_string(x = name, label = paste("mean:",name,sep=" "), group = "category", fill = "category"  ), show.legend = FALSE, nudge_y =.5) +
                # draw the bars of the graph
                ggplot2::geom_bar(data =dropped_group_data, mapping = ggplot2::aes_string(x = name, y = "..prop..", group = "category", fill = "category", color = "category"), stat = "count", position = ggplot2::position_dodge(1)) +
                # set axis scales
                ggplot2::scale_x_discrete(limits = limits, labels = labels) +#, labels = labels)  +
                ggplot2::expand_limits(x = 1) +
                ggplot2::ylab("Percentage") +
                ggplot2::xlab(getGraphLabels(labels = graph_label_df, question_name = getNameByOrg(org, group_name, name), group_name = group_name, subgroup_name = name, org = org)) +
                ggplot2::ylim(c(0, 1)) +
                # write question
                ggplot2::ggtitle(label = paste("Group: ",group_name, " - Subgroup: ",lbl_name, sep="")) +#, subtitle = getGraphLabels(labels = graph_label_df, question_name = getNameByOrg(org, group_name, name), group_name = group_name, subgroup_name = name, org = org)) +
                # create and label mean line
                ggplot2::geom_vline(data=summ_mean, ggplot2::aes_string(xintercept=name, color = "category"), linetype =2, show.legend = FALSE) +
                ggplot2::geom_label(data=summ_mean, y = vec,  ggplot2::aes_string(x = name, label = name, group = "category", fill = "category"  ), show.legend = FALSE, nudge_y =.5) +
                # label each bar with count
                ggplot2::geom_text(stat = "count", color = "black", ggplot2::aes_string(x = name, y = "..prop.. - 0.05", label= "..count..", group = "category"), position = ggplot2::position_dodge(1), show.legend = FALSE)
        }
        else # use count instead
        {
            # TODO
        }
        # add mean line labels
        #  --- add in the t- test ---
        # make a list of unique categories
        unique_cat <- unique(dropped_group_data$category)
        # if there are only two unique categories:
        if (length(unique_cat) == 2)
        {
            # group by the categories
            grp1 <- stats::na.omit(dplyr::filter(dropped_group_data, .data$category == unique_cat[[1]]))
            grp2 <- stats::na.omit(dplyr::filter(dropped_group_data, .data$category == unique_cat[[2]]))

            ttest = tryCatch({
                ttest <- stats::t.test(grp1[[name]], grp2[[name]], conf.level = 0.95)
                n <- ttest$p.value
                #TODO - not showing up on graph
                p + ggplot2::geom_label(ggplot2::aes_string(label =ttest$p.value, x = -2.5, y = .9))
                #cat("group:",group_name,"subgroup:",lbl_name,unique_cat[[1]],"vs",unique_cat[[2]],"p-value:",ttest$p.value,"\n" )
            }, error = function(e) {
                warning(paste("Running a ttest FAILED with error: ",e,sep=""))

            })
        }


        # place the graph in the list
        scenario_plots[[p_index]] = p
        p_index = p_index + 1
    }
    return(scenario_plots)
}
