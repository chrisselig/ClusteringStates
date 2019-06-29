# Script is used to create functions for data wrangling ----

# Create the arrests_tbl ----
arrests_tbl_function <- function(data = arrests_raw_tbl){
    arrests_tbl <- arrests_raw_tbl %>% 
        select(state,Murder:Assault,Rape,UrbanPop) %>% 
        rename(
            murder = Murder,
            assault = Assault,
            rape = Rape,
            urbanpop = UrbanPop
        ) %>% 
        gather(key = 'variable', value = 'value',murder:urbanpop)
    return(arrests_tbl)
}

# Create the arrests_stats_function ----
arrests_stats_function <- function(data = arrests_tbl){
    arrests_stats_tbl <- data %>% 
        #gather(key = 'variable', value = 'value',murder:urbanpop) %>% 
        group_by(variable) %>% 
        summarize(
            num = n(),
            mean = mean(value),
            median = median(value),
            variance = var(value),
            std = sd(value),
            min = min(value),
            max = max(value),
            iqr = IQR(value),
            skewness = skew(value),
            kurtosis = kurtosi(value)
        ) %>% 
        mutate(
            mean_label = str_glue("Mean: {mean}"),
            median_label = str_glue("Median: {median}")
        ) 
    
    return(arrests_stats_tbl)
}


# Exploratory Data Analysis ----





# Boxplot for outlier detection ----
    
# Hierarchical Cluster Analysis ----


