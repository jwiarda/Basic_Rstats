Basic data plotting and statistical analysis
================

## Overview

Herein I show some basic code for statistically analyzing some data in R. This is the same data as presented in Figure 1B of my publication, 'Intraepithelial T cells diverge by intestinal location as pigs age' by Wiarda et al. 2020 (<https://doi.org/10.3389/fimmu.2020.01139>) but has now been re-visualized and re-analyzed using R instead of GraphPad/Prism.

The data contains measurements of the percentages of surface area positive for CD3 (a T cell marker) staining by immunohistochemistry. These surface area measurements were taken from each of eight pigs at 4, 6, and 8 weeks of age. Within each pig, measurements were taken from four different intestinal tissues, including jejunum, ileum, cecum, and colon.

I chose this dataset as an example since it displays characteristics of (1) paired data, such as comparing the percentage of positively-stained surface area across tissues within a timepoint, where we can pair data by pig of origin, and (2) unpaired data, such as comparing the percentage of postively-stained surface area across ages within a single tissue, where data can no longer be paired by pig of origin.

The data I use is organized in an Excel sheet, where I have four rows corresponding to animal age, tissue, animal ID, and the percentage of positively-stained surface area.

### Load required packages

To start, we need to make sure required packages are installed and then loaded. There are different ways to install different packages in R, but a simple way to install a package is to simply Google it and follow installation instructions provided by the package source/manual. Once installed, the following packages need to be loaded into your R environment by running this snippet of code:

``` r
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(rstatix)
```

R packages are often updated, with new versions coming out frequently. Sometimes, these updates can affect the results you obtain from running your code. Consequently, it's always good practice to include your package versions and R environment details for later reference. I do this at the end of the script by recording the session information. I encourage others to always do so as well.

## Import data

The first thing I want to do is import the Excel sheet containing my data. Data can be stored in other formats such as .txt, .csv, and .tsv but requires a different command to import these different formats. Additionally, if data does not follow a similar format as that found within my Excel sheet, then this may also need to be rearranged to make the subsequent script work. You need to adjust the following line of code to specify the directory for your file of interest:

``` r
data <- as.data.frame(read_excel('/Users/jayne.wiarda/Downloads/CD3_SA_data.xlsx'))
```

Now let's look at how this data is arranged by looking at the first 10 rows:

``` r
head(data, n = 10)
```

    ##    age_weeks  tissue pigID percent_CD3_area
    ## 1          4 jejunum     1         4.155033
    ## 2          4 jejunum     2         2.561854
    ## 3          4 jejunum     3         6.130792
    ## 4          4 jejunum     4         2.577399
    ## 5          4 jejunum     5         3.577736
    ## 6          4 jejunum     6         1.521371
    ## 7          4 jejunum     7         5.678728
    ## 8          4 jejunum     8         1.579533
    ## 9          6 jejunum     9         4.183209
    ## 10         6 jejunum    10         2.573144

We can also quickly find out the column names with the following one-liner:

``` r
colnames(data)
```

    ## [1] "age_weeks"        "tissue"           "pigID"            "percent_CD3_area"

Or we can find out the data dimensions (rows x columns) using this line:

``` r
dim(data)
```

    ## [1] 96  4

We also should define which columns contain numerical vs categorical data. In this case, only the percent\_CD3\_area data should be treated as quantitative, while the other columns all contain experimental variables that we can consider categorical variables. Consequently, make the percent\_CD3\_area column numeric and all other columns characters:

``` r
data$age_weeks <- as.character(data$age_weeks)
data$tissue <- as.character(data$tissue)
data$pigID <- as.character(data$pigID)
data$percent_CD3_area <- as.numeric(data$percent_CD3_area)
```

## Kruskal-Wallis rank sum test

#### For multi-group, non-parametric, unpaired alternative to the ANOVA

The example we will use: Does the percentage of CD3-positive surface area differ significantly between ileal tissue collected from pigs at different ages?

Start by subsetting down to only the ileum data we are concerned with:

``` r
ildata <- subset(data, tissue == 'ileum')
dim(ildata)
```

    ## [1] 24  4

``` r
colnames(ildata)
```

    ## [1] "age_weeks"        "tissue"           "pigID"            "percent_CD3_area"

Specify all experimental/categorical variables should be treated as factors:

``` r
ildata <- ildata %>% 
  convert_as_factor(age_weeks, tissue, pigID)
```

Perform the Kruskal-Wallis test:

``` r
il_age_kruskal <- ildata %>% kruskal_test(percent_CD3_area ~ age_weeks)
il_age_kruskal
```

    ## # A tibble: 1 x 6
    ##   .y.                  n statistic    df        p method        
    ## * <chr>            <int>     <dbl> <int>    <dbl> <chr>         
    ## 1 percent_CD3_area    24      16.1     2 0.000322 Kruskal-Wallis

We can see that we have a significant p-value, so let's move on to perform post-hoc testing....

## Post-hoc Mann-Whitney U test

#### For multi-group, non-parametric, unpaired multiple comparisons

We will use the data that we already arranged for the Kruskal-Wallis test above. We will perform every possible comparison between our three ages, resulting in a total of three comparisons. Note that the function we use is wilcox\_test(), indicating we perform a Wilcoxon test; however, since we specify paired = FALSE, this is actually now just a default Mann-Whitney test for unpaired data.

``` r
il_age_wilcox <- ildata %>%
  wilcox_test(percent_CD3_area ~ age_weeks, 
              paired = FALSE, 
              p.adjust.method = 'fdr')
il_age_wilcox
```

    ## # A tibble: 3 x 9
    ##   .y.          group1 group2    n1    n2 statistic        p   p.adj p.adj.signif
    ## * <chr>        <chr>  <chr>  <int> <int>     <dbl>    <dbl>   <dbl> <chr>       
    ## 1 percent_CD3… 4      6          8     8         1 0.000311 4.66e-4 ***         
    ## 2 percent_CD3… 4      8          8     8         0 0.000155 4.65e-4 ***         
    ## 3 percent_CD3… 6      8          8     8        17 0.13     1.3 e-1 ns

We see that ileum of 4-week-old pigs have significantly different percentages of positively-stained surface area compared to either 6- or 8-week-old pigs. However, 6- and 8-week-old pigs did not have significantly different percentages of positively stained surface area in ileum.

## Friedman rank sum test

#### For multi-group, non-parametric, paired alternative to the ANOVA

The example we will use: Does the percentage of CD3-positive surface area differ significantly between intestinal tissues collected from pigs at 8 weeks of age?

Start by subsetting down to only the data from 8-week-old pigs that we are concerned with:

``` r
data8 <- subset(data, age_weeks == '8')
dim(data8)
```

    ## [1] 32  4

``` r
colnames(data8)
```

    ## [1] "age_weeks"        "tissue"           "pigID"            "percent_CD3_area"

Specify all experimental/categorical variables should be treated as factors:

``` r
data8 <- data8 %>% 
  convert_as_factor(age_weeks, tissue, pigID)
```

Perform the Friedman test, specifying to pair by pig ID:

``` r
tissue_8_friedman <- data8 %>% friedman_test(percent_CD3_area ~ tissue | pigID)
tissue_8_friedman
```

    ## # A tibble: 1 x 6
    ##   .y.                  n statistic    df         p method       
    ## * <chr>            <int>     <dbl> <dbl>     <dbl> <chr>        
    ## 1 percent_CD3_area     8      21.2     3 0.0000980 Friedman test

We can see that we have a significant p-value, so let's move on to perform post-hoc testing....

## Post-hoc Wilcoxon rank sum test

#### For multi-group, non-parametric, paired multiple comparisons

We will use the data that we already arranged for the Friedman test above. We will perform every possible comparison between our four tissues, resulting in a total of six comparisons. We now specify paired = TRUE for wilcox\_test, so we now perform the Wilcoxon test (paired data) rather than Mann-Whitney test (unpaired data).

``` r
tissue_8_wilcox <- data8 %>%
  wilcox_test(percent_CD3_area ~ tissue, 
              paired = TRUE, 
              p.adjust.method = 'fdr')
tissue_8_wilcox 
```

    ## # A tibble: 6 x 9
    ##   .y.              group1 group2     n1    n2 statistic     p p.adj p.adj.signif
    ## * <chr>            <chr>  <chr>   <int> <int>     <dbl> <dbl> <dbl> <chr>       
    ## 1 percent_CD3_area cecum  colon       8     8        34 0.023 0.028 *           
    ## 2 percent_CD3_area cecum  ileum       8     8         0 0.008 0.012 *           
    ## 3 percent_CD3_area cecum  jejunum     8     8         0 0.008 0.012 *           
    ## 4 percent_CD3_area colon  ileum       8     8         0 0.008 0.012 *           
    ## 5 percent_CD3_area colon  jejunum     8     8         0 0.008 0.012 *           
    ## 6 percent_CD3_area ileum  jejunum     8     8        30 0.109 0.109 ns

We see that at 8 weeks of age, there is a statistically significant difference between the percentage of positively stained surface area between all pairwise tissue comparisons, except for between ileum and jejunum.

## Post-hoc testing back to a single 'control' sample

For some data analyses, we may not care to compare every treatment to each other. Perhaps we only want to compare each treatment back to a single control sample. In this instance, let's pretend that we have a single control sample we want to compare all other groups back to with post-hoc testing, using the Mann-Whitney (unpaired data) or Wilcoxon (paired data) test.

#### For unpaired comparison back to a single control sample

Let's run an unpaired Mann-Whitney test on the ildata dataframe we've already constructed and prepped. In this case, let's treat the 4-week-old timepoint as our control, so we want to compare every other treatment variable of age (for which we have two more in this case) back to this 'control' but not to each other. This will leave us with two comparisons we want to make in this case.

``` r
il_age_wilcox_4ctrl <- ildata %>%
  wilcox_test(percent_CD3_area ~ age_weeks, 
              paired = FALSE, 
              p.adjust.method = 'fdr',
              ref.group = '4')
il_age_wilcox_4ctrl
```

    ## # A tibble: 2 x 9
    ##   .y.          group1 group2    n1    n2 statistic        p   p.adj p.adj.signif
    ## * <chr>        <chr>  <chr>  <int> <int>     <dbl>    <dbl>   <dbl> <chr>       
    ## 1 percent_CD3… 4      6          8     8         1 0.000311 3.11e-4 ***         
    ## 2 percent_CD3… 4      8          8     8         0 0.000155 3.1 e-4 ***

#### For paired comparison back to a single control sample

Let's run a paired Wilcoxon test on the data8 dataframe we've already constructed and prepped. In this case, let's treat the ileum as our control, so we want to compare every other treatment variable of tissue (for which we have three more in this case) back to this 'control' but not to each other. This will leave us with three comparisons we want to make in this case.

``` r
tissue_8_wilcox_ilctrl <- data8 %>%
  wilcox_test(percent_CD3_area ~ tissue, 
              paired = TRUE, 
              p.adjust.method = 'fdr',
              ref.group = 'ileum')
tissue_8_wilcox_ilctrl
```

    ## # A tibble: 3 x 9
    ##   .y.              group1 group2     n1    n2 statistic     p p.adj p.adj.signif
    ## * <chr>            <chr>  <chr>   <int> <int>     <dbl> <dbl> <dbl> <chr>       
    ## 1 percent_CD3_area ileum  cecum       8     8        36 0.008 0.012 *           
    ## 2 percent_CD3_area ileum  colon       8     8        36 0.008 0.012 *           
    ## 3 percent_CD3_area ileum  jejunum     8     8        30 0.109 0.109 ns

## Two sample tests

#### For two-sample, non-parametric comparisons

Let's pretend now that we only have two data groups; we will go through an example of this with both paired and unpaired data.

For unpaired data, let's say we want to compare only between positively stained area of ileum at 4 and 6 weeks of age. To do this, we simply run the Mann-Whitney U test again, but for only this single comparison.

Start by isolating down to only the data we care about:

``` r
il46data <- subset(data, tissue == 'ileum' & (age_weeks == '4' | age_weeks == '6'))
dim(il46data)
```

    ## [1] 16  4

``` r
colnames(il46data)
```

    ## [1] "age_weeks"        "tissue"           "pigID"            "percent_CD3_area"

Now prep our data and run Mann-Whitney test for only a single comparison:

``` r
il46data <- il46data %>% 
  convert_as_factor(age_weeks, tissue, pigID)
il_age46_wilcox <- il46data %>%
  wilcox_test(percent_CD3_area ~ age_weeks, 
              paired = FALSE)
il_age46_wilcox
```

    ## # A tibble: 1 x 7
    ##   .y.              group1 group2    n1    n2 statistic        p
    ## * <chr>            <chr>  <chr>  <int> <int>     <dbl>    <dbl>
    ## 1 percent_CD3_area 4      6          8     8         1 0.000311

Note how here we don't have an output column for an adjusted p-value, since only one comparison was performed.

For paired data, let's say we want to compare only between positively stained area of ileum and colon at 8 weeks of age. To do this, we simply run the Wilcoxon rank sum test again, but for only this single comparison.

Start by isolating down to only the data we care about:

``` r
ilco8data <- subset(data, age_weeks == '8' & (tissue == 'ileum' | tissue == 'colon'))
dim(ilco8data)
```

    ## [1] 16  4

``` r
colnames(ilco8data)
```

    ## [1] "age_weeks"        "tissue"           "pigID"            "percent_CD3_area"

Now prep our data and run Mann-Whitney test for only a single comparison:

``` r
ilco8data <- ilco8data %>% 
  convert_as_factor(age_weeks, tissue, pigID)
ilco_8_wilcox <- ilco8data %>%
  wilcox_test(percent_CD3_area ~ tissue, 
              paired = TRUE)
ilco_8_wilcox 
```

    ## # A tibble: 1 x 7
    ##   .y.              group1 group2    n1    n2 statistic       p
    ## * <chr>            <chr>  <chr>  <int> <int>     <dbl>   <dbl>
    ## 1 percent_CD3_area colon  ileum      8     8         0 0.00781

Note how here we don't have an output column for an adjusted p-value, since only one comparison was performed.

An alternative format for the single comparison is to run a function similar to as follows. For unpaired data, simply change to paired = FALSE. In this case, we are no longer under the pretense that we don't have to perform a p value adjustment for multiple comparison, because we are acknowledging other data variable groupings do still exist, but we just don't care to know statistics for those comparisons.

``` r
ilco_8_wilcox <- data8 %>%
  wilcox_test(percent_CD3_area ~ tissue, 
              paired = TRUE,
              comparisons = list(c('ileum', 'colon')))
ilco_8_wilcox 
```

    ## # A tibble: 1 x 9
    ##   .y.              group1 group2    n1    n2 statistic     p p.adj p.adj.signif
    ## * <chr>            <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>       
    ## 1 percent_CD3_area colon  ileum      8     8         0 0.008 0.008 **

## An example of exporting results

Last, here is a brief example of how you can obtain and collate results. In this case, I'm collating results for all post-hoc comparisons across tissues within each age using the paired Wilcoxon test.

Start by testing across tissues in 4-week-old pigs:

``` r
data4 <- subset(data, age_weeks == '4')
dim(data4)
```

    ## [1] 32  4

``` r
colnames(data4)
```

    ## [1] "age_weeks"        "tissue"           "pigID"            "percent_CD3_area"

``` r
data4 <- data4 %>% 
  convert_as_factor(age_weeks, tissue, pigID)
tissue_4_wilcox <- data4 %>%
  wilcox_test(percent_CD3_area ~ tissue, 
              paired = TRUE, 
              p.adjust.method = 'fdr')
tissue_4_wilcox 
```

    ## # A tibble: 6 x 9
    ##   .y.              group1 group2     n1    n2 statistic     p p.adj p.adj.signif
    ## * <chr>            <chr>  <chr>   <int> <int>     <dbl> <dbl> <dbl> <chr>       
    ## 1 percent_CD3_area cecum  colon       8     8        31 0.078 0.094 ns          
    ## 2 percent_CD3_area cecum  ileum       8     8         2 0.023 0.035 *           
    ## 3 percent_CD3_area cecum  jejunum     8     8         0 0.008 0.016 *           
    ## 4 percent_CD3_area colon  ileum       8     8         0 0.008 0.016 *           
    ## 5 percent_CD3_area colon  jejunum     8     8         0 0.008 0.016 *           
    ## 6 percent_CD3_area ileum  jejunum     8     8         8 0.195 0.195 ns

Now let's add an additional column specifying which age we were using here:

``` r
tissue_4_wilcox$age_weeks <- '4'
```

Do the same for 6-week-old pigs:

``` r
data6 <- subset(data, age_weeks == '6')
dim(data6)
```

    ## [1] 32  4

``` r
colnames(data6)
```

    ## [1] "age_weeks"        "tissue"           "pigID"            "percent_CD3_area"

``` r
data6 <- data6 %>% 
  convert_as_factor(age_weeks, tissue, pigID)
tissue_6_wilcox <- data6 %>%
  wilcox_test(percent_CD3_area ~ tissue, 
              paired = TRUE, 
              p.adjust.method = 'fdr')
tissue_6_wilcox 
```

    ## # A tibble: 6 x 9
    ##   .y.              group1 group2     n1    n2 statistic     p p.adj p.adj.signif
    ## * <chr>            <chr>  <chr>   <int> <int>     <dbl> <dbl> <dbl> <chr>       
    ## 1 percent_CD3_area cecum  colon       8     8        36 0.008 0.012 *           
    ## 2 percent_CD3_area cecum  ileum       8     8         0 0.008 0.012 *           
    ## 3 percent_CD3_area cecum  jejunum     8     8         5 0.078 0.094 ns          
    ## 4 percent_CD3_area colon  ileum       8     8         0 0.008 0.012 *           
    ## 5 percent_CD3_area colon  jejunum     8     8         0 0.008 0.012 *           
    ## 6 percent_CD3_area ileum  jejunum     8     8        28 0.195 0.195 ns

``` r
tissue_6_wilcox$age_weeks <- '6'
```

And we already ran this comparison on 8-week-old pigs and just need to add the new column information:

``` r
tissue_8_wilcox$age_weeks <- '8'
```

Now we can collate these results together into a single object:

``` r
tissue_wilcox <- rbind(tissue_4_wilcox, tissue_6_wilcox, tissue_8_wilcox)
```

To see the full data frame run:

``` r
View(tissue_wilcox)
```

And now let's finally export these results as an Excel file that we can easily look through later:

``` r
write_xlsx(tissue_wilcox, '/Users/jayne.wiarda/Downloads/wilcoxon_multicomp_CD3SA_tissue.xlsx')
```

### And finally, don't forget to record your session information:

``` r
sessionInfo()
```

    ## R version 3.5.1 (2018-07-02)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS  10.14.6
    ## 
    ## Matrix products: default
    ## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] rstatix_0.7.0 tidyr_1.1.2   dplyr_1.0.2   writexl_1.3.1 readxl_1.3.1 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] zip_2.1.1         Rcpp_1.0.6        cellranger_1.1.0  pillar_1.6.0     
    ##  [5] compiler_3.5.1    forcats_0.5.0     tools_3.5.1       digest_0.6.27    
    ##  [9] evaluate_0.14     lifecycle_1.0.0   tibble_3.1.1      pkgconfig_2.0.3  
    ## [13] rlang_0.4.11      openxlsx_4.2.3    rstudioapi_0.11   cli_2.5.0        
    ## [17] curl_4.3.1        yaml_2.2.1        haven_2.3.1       xfun_0.18        
    ## [21] rio_0.5.16        stringr_1.4.0     knitr_1.30        hms_0.5.3        
    ## [25] generics_0.1.0    vctrs_0.3.8       tidyselect_1.1.1  glue_1.4.2       
    ## [29] data.table_1.14.0 R6_2.5.0          fansi_0.4.2       foreign_0.8-70   
    ## [33] rmarkdown_2.5     carData_3.0-4     purrr_0.3.4       car_3.0-11       
    ## [37] magrittr_2.0.1    backports_1.1.10  ellipsis_0.3.2    htmltools_0.5.1.1
    ## [41] abind_1.4-5       utf8_1.2.1        stringi_1.6.1     broom_0.7.8      
    ## [45] crayon_1.4.1

Ta-dah!
