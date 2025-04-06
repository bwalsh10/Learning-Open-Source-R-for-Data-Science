CTT_eval_function
================

``` r
#libraries specified under "imports" for CTT_eval Package#
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.2.2

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.4      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ ggplot2::%+%()   masks psych::%+%()
    ## ✖ ggplot2::alpha() masks psych::alpha()
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ dplyr::lag()     masks stats::lag()

``` r
library(knitr)
```

``` r
#ANNOTATED PRIMARY FUNCTION FOR "CTT_eval" PACKAGE#

ctt_eval <- function(data) {
  if (!is.data.frame(data)) {
    stop("The input data should be a data frame.")
  }
  
  # Compute Cronbach's Alpha with psych::alpha
  alpha_result <- psych::alpha(data)
  
  # Extract Cronbach's Alpha value
  alpha_value <- alpha_result$total$raw_alpha
  
  # Extract item-total correlations (item discrimination)
  item_total_corr <- alpha_result$item.stats$r.drop
  
  # Extract alpha if item deleted
  alpha_if_deleted <- round(alpha_result$alpha.drop[, "raw_alpha"], 2)
  
  # Compute item-level descriptive statistics: mean, SD, skewness, kurtosis
  desc_stats <- psych::describe(data)[, c("mean", "sd", "skew", "kurtosis")]
  
  # Round all descriptive statistics to two decimal points
  desc_stats <- desc_stats %>%
    mutate(
      mean = round(mean, 2),
      sd = round(sd, 2),
      skew = round(skew, 2),
      kurtosis = round(kurtosis, 2)
    )
  
  # Compute item-level range of scores
  item_ranges <- apply(data, 2, function(x) paste0("[", min(x), ", ", max(x), "]"))
  
  # Add item discrimination and alpha-if-item-deleted to descriptive stats
  desc_stats$item_discrimination <- round(item_total_corr, 2)
  desc_stats$alpha_if_deleted <- alpha_if_deleted
  
  # Add descriptions for item discrimination quality
  desc_stats$item_quality <- sapply(item_total_corr, function(corr) {
    if (corr > 0.80) {
      return("Strong correlation")
    } else if (corr > 0.60) {
      return("Moderate correlation")
    } else if (corr > 0.40) {
      return("Weak correlation")
    } else {
      return("Item may need revision")
    }
  })
  
  # Add range of scores to descriptive stats
  desc_stats$item_range <- item_ranges
  
  # Create a clean output table combining all item-level statistics and descriptions
  clean_table <- desc_stats %>%
    mutate(Item = rownames(desc_stats)) %>%
    select(Item, mean, sd, skew, kurtosis, item_range, item_discrimination, alpha_if_deleted, item_quality)
  
  # Compute total scale statistics
  total_scores <- rowSums(data)
  total_mean <- round(mean(total_scores), 2)
  total_sd <- round(sd(total_scores), 2)
  total_range <- paste0("[", min(total_scores), ", ", max(total_scores), "]")
  
  reliability_quality <- cut(alpha_value,
                             breaks = c(-Inf, 0.7, 0.8, 0.9, Inf),
                             labels = c("Poor reliability", "Acceptable reliability", "Good reliability", "Excellent reliability"),
                             right = FALSE)
  
  # Create a separate table for total scale statistics
  scale_table <- tibble(
    Statistic = c("Mean", "SD", "Range", "Alpha", "Descriptor"),
    Value = c(total_mean, total_sd, total_range, round(alpha_value, 2), as.character(reliability_quality))
  )
  
  # Print both the tables using knitr::kable for clean output
  print(knitr::kable(clean_table,
                     format = "markdown",
                     caption = "Descriptive Statistics Table with Alpha-if-Item-Deleted and Range of Scores"))
  
  print(knitr::kable(scale_table,
                     format = "markdown",
                     caption = "Total Scale Statistics Table"))
  
  return(list(
    alpha = round(alpha_value, 2),
    alpha_quality = reliability_quality,
    descriptive_stats_table = clean_table,
    scale_statistics_table = scale_table
  ))
}
```

``` r
#TESTING ctt_eval function using simulated data (100 participants completing a 10-item self-report scale)

#Step 1: Read in data. Data must be formatted as a dataframe. Each column should correspond to each item of the scale. Each row should represent one participants data. Scores for each item should be listed accordingly. 
data <-read_csv("sim_data.csv")
```

    ## Rows: 100 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (10): Item1, Item2, Item3, Item4, Item5, Item6, Item7, Item8, Item9, Item10
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Step 2: apply the "ctt_eval" function to your data. The results will give you two seperate tables. One table for item-level statistics and one table for scale-level statistics. All indices can be used to evaluate the quality of your scale from a Classical Test Theory Perspective. Descriptive labels are applied to certain statistics to aid interpretation. 
ctt_results <- ctt_eval(data)
```

    ## Warning in psych::alpha(data): Some items were negatively correlated with the total scale and probably 
    ## should be reversed.  
    ## To do this, run the function again with the 'check.keys=TRUE' option

    ## Some items ( Item9 Item10 ) were negatively correlated with the total scale and 
    ## probably should be reversed.  
    ## To do this, run the function again with the 'check.keys=TRUE' option
    ## 
    ## Table: Descriptive Statistics Table with Alpha-if-Item-Deleted and Range of Scores
    ## 
    ## |       |Item   | mean|   sd|  skew| kurtosis|item_range | item_discrimination| alpha_if_deleted|item_quality           |
    ## |:------|:------|----:|----:|-----:|--------:|:----------|-------------------:|----------------:|:----------------------|
    ## |Item1  |Item1  | 3.10| 1.13|  0.01|    -0.74|[1, 5]     |                0.83|             0.84|Strong correlation     |
    ## |Item2  |Item2  | 2.97| 1.04| -0.05|    -0.65|[1, 5]     |                0.83|             0.84|Strong correlation     |
    ## |Item3  |Item3  | 3.05| 1.18| -0.02|    -0.78|[1, 5]     |                0.78|             0.84|Moderate correlation   |
    ## |Item4  |Item4  | 3.05| 1.08| -0.19|    -0.51|[1, 5]     |                0.85|             0.84|Strong correlation     |
    ## |Item5  |Item5  | 3.11| 1.15| -0.10|    -0.71|[1, 5]     |                0.78|             0.85|Moderate correlation   |
    ## |Item6  |Item6  | 3.00| 1.17|  0.07|    -0.88|[1, 5]     |                0.84|             0.84|Strong correlation     |
    ## |Item7  |Item7  | 2.99| 1.18| -0.02|    -0.90|[1, 5]     |                0.84|             0.84|Strong correlation     |
    ## |Item8  |Item8  | 3.01| 1.12|  0.15|    -0.72|[1, 5]     |                0.79|             0.84|Moderate correlation   |
    ## |Item9  |Item9  | 3.10| 1.38|  0.05|    -1.27|[1, 5]     |               -0.05|             0.91|Item may need revision |
    ## |Item10 |Item10 | 3.23| 1.35| -0.27|    -1.22|[1, 5]     |               -0.09|             0.91|Item may need revision |
    ## 
    ## 
    ## Table: Total Scale Statistics Table
    ## 
    ## |Statistic  |Value            |
    ## |:----------|:----------------|
    ## |Mean       |30.61            |
    ## |SD         |8.07             |
    ## |Range      |[12, 49]         |
    ## |Alpha      |0.87             |
    ## |Descriptor |Good reliability |
