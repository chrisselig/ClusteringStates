# Script is used for plotting functions ----

# Histogram Plots: To create histograms ----
histogram_function <- function(data = arrests_tbl,
                               variable,
                               buckets = 8){

    variable_expr <- enquo(variable)
    
    data_stats <- arrests_stats_tbl %>% 
        filter(variable == !! variable_expr)
    
    data %>% 
        filter(variable == !! variable_expr) %>% 
        ggplot(aes(x = value)) +
        
        # Geoms
        geom_histogram(bins = buckets, fill = '#b4b4b4', color = '#a7a7a7') +
        # add mean vertical line
        geom_vline(data = data_stats,aes(xintercept = mean, color = mean), color = '#336600') +
        # add label for mean
        geom_label_repel(data = data_stats,
                         mapping = aes(label = mean_label,
                                       x = mean, y = 10),
                         color = '#336600',
                         size = 5,
                         hjust = -0.25,
                         segment.color = 'transparent'
        ) +
        # add vertical median line
        geom_vline(data = data_stats,aes(xintercept = median, color = median), color = 'darkblue') +
        # add labels for median line
        geom_label_repel(data = data_stats,
                         mapping = aes(label = median_label,
                                       x = median, y = 5),
                         color = 'darkblue',
                         size = 5,
                         hjust = 0.25,
                         segment.color = 'transparent'
        ) +
        
        # Formatting
        theme_tufte() +
        labs(
            x = '',
            y = 'Count')
    
    
}

# Boxplot function ----
boxplot_function <- function(data = arrests_tbl, variable){
    
    variable_expr <- enquo(variable)
    
    data %>% 
        filter(variable == !! variable_expr) %>% 
        ggplot(aes(factor(variable),value)) + 
        theme_tufte(ticks = FALSE) +
        geom_tufteboxplot(outlier.colour="transparent") + 
        coord_flip()+
        theme(axis.title=element_blank()) +
        labs(
            title = ''
        )
        # annotate("text", x = 8, y = 120, adj=1,  family="serif",
        #          label = c("Number of stations \nreporting Richter Magnitude\nof Fiji earthquakes (n=1000)"))
}

# Cooks Distance Scatterplot ----

cooks_distance_plot_function <- function(data = cooksd){
    
    data <- data %>% 
        enframe() %>% 
        cbind(arrests_raw_tbl)
    
    cooksPlot <- data %>% 
        ggplot(aes(name, value)) +
        geom_point(
            aes(color = cut(value,c(-Inf,0.08,Inf))),size = 2, show.legend = FALSE
        ) +
        scale_color_manual(values = c('black',  'red'))+
        geom_hline(yintercept = cooks_distance_measure, color = 'red') +
        
        # Label and color only the dots above the line
        geom_label_repel(data = data %>% filter(value >= cooks_distance_measure),
                         mapping = aes(label = str_glue('{state}'),
                                       x = name, y = value),
                         color = 'red',
                         size = 5,
                         hjust = 0.25,
                         vjust = 1.5,
                         segment.color = 'transparent'
        ) +
        theme_tufte() +
        labs(
            title = '',
            y = '',
            x = ''
        ) +
        theme(
            #remove x labels and ticks
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()
        )
    return(cooksPlot)
}
