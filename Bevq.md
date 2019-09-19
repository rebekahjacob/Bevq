Bevq Measures: Cleaning and Scoring
================

## Introduction

This is a working example of how we pull in, score and visualize the
bevq measures.

#### Data

We start with a dataframe (df) called bevq that has one childid per row.
This example is in “wide” form, which means we have 2 variables that
represent two different time frames (e.g.bev\_1 is baseline and
bev\_1\_t2 is post).

``` r
#libraries
library(haven) #read and write sav files (spss)
library(tidyverse) #data management
```

    ## -- Attaching packages ----------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts -------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(sjlabelled) #add variable labels
```

    ## 
    ## Attaching package: 'sjlabelled'

    ## The following object is masked from 'package:forcats':
    ## 
    ##     as_factor

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     as_label

    ## The following objects are masked from 'package:haven':
    ## 
    ##     as_factor, read_sas, read_spss, read_stata, write_sas,
    ##     zap_labels

``` r
library(openxlsx) #pull in variable labels
```

``` r
datDir<- datDir<- 'C:\\Users\\rebekahjacob\\Box\\Envolve study\\Qualtrics data downloads\\Rebekah 2018-2019\\REDCap Exports\\'

bevq_all<- read_sav(paste0(datDir, "p2_cs_2019-09-17.sav"))

bevq.df<- bevq_all %>%
  select(hhid, childid, 
         par_bev_1:par_bev_16, 
         par_bevamt_1:par_bevamt_16,
         par_bev_1_t2:par_bev_16_t2, 
         par_bevamt_1_t2:par_bevamt_16_t2, 
         child_bev_1:child_bev_16, 
         child_bevamt_1:child_bevamt_16,
         child_bev_1_t2:child_bev_16_t2, 
         child_bevamt_1_t2:child_bevamt_16_t2)
```

``` r
head(bevq.df)
```

    ## # A tibble: 6 x 130
    ##    hhid childid par_bev_1 par_bev_2 par_bev_3 par_bev_4 par_bev_5 par_bev_6
    ##   <dbl>   <dbl> <dbl+lbl> <dbl+lbl> <dbl+lbl> <dbl+lbl> <dbl+lbl> <dbl+lbl>
    ## 1 12001    2001 NA        NA        NA        NA        NA        NA       
    ## 2 12002    2002 NA        NA        NA        NA        NA        NA       
    ## 3 12003    2003 NA        NA        NA        NA        NA        NA       
    ## 4 30041   20041  7 [3+ t~  1 [Neve~  1 [Neve~  1 [Neve~  1 [Neve~  6 [2 ti~
    ## 5 30041   20042  7 [3+ t~  1 [Neve~  1 [Neve~  1 [Neve~  1 [Neve~  6 [2 ti~
    ## 6 30041   20043  7 [3+ t~  1 [Neve~  1 [Neve~  1 [Neve~  1 [Neve~  6 [2 ti~
    ## # ... with 122 more variables: par_bev_7 <dbl+lbl>, par_bev_8 <dbl+lbl>,
    ## #   par_bev_9 <dbl+lbl>, par_bev_10 <dbl+lbl>, par_bev_11 <dbl+lbl>,
    ## #   par_bev_12 <dbl+lbl>, par_bev_13 <dbl+lbl>, par_bev_14 <dbl+lbl>,
    ## #   par_bev_15 <dbl+lbl>, par_bev_16 <dbl+lbl>, par_bevamt_1 <dbl+lbl>,
    ## #   par_bevamt_2 <dbl+lbl>, par_bevamt_3 <dbl+lbl>,
    ## #   par_bevamt_4 <dbl+lbl>, par_bevamt_5 <dbl+lbl>,
    ## #   par_bevamt_6 <dbl+lbl>, par_bevamt_7 <dbl+lbl>,
    ## #   par_bevamt_8 <dbl+lbl>, par_bevamt_9 <dbl+lbl>,
    ## #   par_bevamt_10 <dbl+lbl>, par_bevamt_11 <dbl+lbl>,
    ## #   par_bevamt_12 <dbl+lbl>, par_bevamt_13 <dbl+lbl>,
    ## #   par_bevamt_14 <dbl+lbl>, par_bevamt_15 <dbl+lbl>,
    ## #   par_bevamt_16 <dbl+lbl>, par_bev_1_t2 <dbl+lbl>,
    ## #   par_bev_2_t2 <dbl+lbl>, par_bev_3_t2 <dbl+lbl>,
    ## #   par_bev_4_t2 <dbl+lbl>, par_bev_5_t2 <dbl+lbl>,
    ## #   par_bev_6_t2 <dbl+lbl>, par_bev_7_t2 <dbl+lbl>,
    ## #   par_bev_8_t2 <dbl+lbl>, par_bev_9_t2 <dbl+lbl>,
    ## #   par_bev_10_t2 <dbl+lbl>, par_bev_11_t2 <dbl+lbl>,
    ## #   par_bev_12_t2 <dbl+lbl>, par_bev_13_t2 <dbl+lbl>,
    ## #   par_bev_14_t2 <dbl+lbl>, par_bev_15_t2 <dbl+lbl>,
    ## #   par_bev_16_t2 <dbl+lbl>, par_bevamt_1_t2 <dbl+lbl>,
    ## #   par_bevamt_2_t2 <dbl+lbl>, par_bevamt_3_t2 <dbl+lbl>,
    ## #   par_bevamt_4_t2 <dbl+lbl>, par_bevamt_5_t2 <dbl+lbl>,
    ## #   par_bevamt_6_t2 <dbl+lbl>, par_bevamt_7_t2 <dbl+lbl>,
    ## #   par_bevamt_8_t2 <dbl+lbl>, par_bevamt_9_t2 <dbl+lbl>,
    ## #   par_bevamt_10_t2 <dbl+lbl>, par_bevamt_11_t2 <dbl+lbl>,
    ## #   par_bevamt_12_t2 <dbl+lbl>, par_bevamt_13_t2 <dbl+lbl>,
    ## #   par_bevamt_14_t2 <dbl+lbl>, par_bevamt_15_t2 <dbl+lbl>,
    ## #   par_bevamt_16_t2 <dbl+lbl>, child_bev_1 <dbl+lbl>,
    ## #   child_bev_2 <dbl+lbl>, child_bev_3 <dbl+lbl>, child_bev_4 <dbl+lbl>,
    ## #   child_bev_5 <dbl+lbl>, child_bev_6 <dbl+lbl>, child_bev_7 <dbl+lbl>,
    ## #   child_bev_8 <dbl+lbl>, child_bev_9 <dbl+lbl>, child_bev_10 <dbl+lbl>,
    ## #   child_bev_11 <dbl+lbl>, child_bev_12 <dbl+lbl>,
    ## #   child_bev_13 <dbl+lbl>, child_bev_14 <dbl+lbl>,
    ## #   child_bev_15 <dbl+lbl>, child_bev_16 <dbl+lbl>,
    ## #   child_bevamt_1 <dbl+lbl>, child_bevamt_2 <dbl+lbl>,
    ## #   child_bevamt_3 <dbl+lbl>, child_bevamt_4 <dbl+lbl>,
    ## #   child_bevamt_5 <dbl+lbl>, child_bevamt_6 <dbl+lbl>,
    ## #   child_bevamt_7 <dbl+lbl>, child_bevamt_8 <dbl+lbl>,
    ## #   child_bevamt_9 <dbl+lbl>, child_bevamt_10 <dbl+lbl>,
    ## #   child_bevamt_11 <dbl+lbl>, child_bevamt_12 <dbl+lbl>,
    ## #   child_bevamt_13 <dbl+lbl>, child_bevamt_14 <dbl+lbl>,
    ## #   child_bevamt_15 <dbl+lbl>, child_bevamt_16 <dbl+lbl>,
    ## #   child_bev_1_t2 <dbl+lbl>, child_bev_2_t2 <dbl+lbl>,
    ## #   child_bev_3_t2 <dbl+lbl>, child_bev_4_t2 <dbl+lbl>,
    ## #   child_bev_5_t2 <dbl+lbl>, child_bev_6_t2 <dbl+lbl>,
    ## #   child_bev_7_t2 <dbl+lbl>, child_bev_8_t2 <dbl+lbl>,
    ## #   child_bev_9_t2 <dbl+lbl>, child_bev_10_t2 <dbl+lbl>, ...

#### Variables in the df

``` r
names(bevq.df)
```

    ##   [1] "hhid"               "childid"            "par_bev_1"         
    ##   [4] "par_bev_2"          "par_bev_3"          "par_bev_4"         
    ##   [7] "par_bev_5"          "par_bev_6"          "par_bev_7"         
    ##  [10] "par_bev_8"          "par_bev_9"          "par_bev_10"        
    ##  [13] "par_bev_11"         "par_bev_12"         "par_bev_13"        
    ##  [16] "par_bev_14"         "par_bev_15"         "par_bev_16"        
    ##  [19] "par_bevamt_1"       "par_bevamt_2"       "par_bevamt_3"      
    ##  [22] "par_bevamt_4"       "par_bevamt_5"       "par_bevamt_6"      
    ##  [25] "par_bevamt_7"       "par_bevamt_8"       "par_bevamt_9"      
    ##  [28] "par_bevamt_10"      "par_bevamt_11"      "par_bevamt_12"     
    ##  [31] "par_bevamt_13"      "par_bevamt_14"      "par_bevamt_15"     
    ##  [34] "par_bevamt_16"      "par_bev_1_t2"       "par_bev_2_t2"      
    ##  [37] "par_bev_3_t2"       "par_bev_4_t2"       "par_bev_5_t2"      
    ##  [40] "par_bev_6_t2"       "par_bev_7_t2"       "par_bev_8_t2"      
    ##  [43] "par_bev_9_t2"       "par_bev_10_t2"      "par_bev_11_t2"     
    ##  [46] "par_bev_12_t2"      "par_bev_13_t2"      "par_bev_14_t2"     
    ##  [49] "par_bev_15_t2"      "par_bev_16_t2"      "par_bevamt_1_t2"   
    ##  [52] "par_bevamt_2_t2"    "par_bevamt_3_t2"    "par_bevamt_4_t2"   
    ##  [55] "par_bevamt_5_t2"    "par_bevamt_6_t2"    "par_bevamt_7_t2"   
    ##  [58] "par_bevamt_8_t2"    "par_bevamt_9_t2"    "par_bevamt_10_t2"  
    ##  [61] "par_bevamt_11_t2"   "par_bevamt_12_t2"   "par_bevamt_13_t2"  
    ##  [64] "par_bevamt_14_t2"   "par_bevamt_15_t2"   "par_bevamt_16_t2"  
    ##  [67] "child_bev_1"        "child_bev_2"        "child_bev_3"       
    ##  [70] "child_bev_4"        "child_bev_5"        "child_bev_6"       
    ##  [73] "child_bev_7"        "child_bev_8"        "child_bev_9"       
    ##  [76] "child_bev_10"       "child_bev_11"       "child_bev_12"      
    ##  [79] "child_bev_13"       "child_bev_14"       "child_bev_15"      
    ##  [82] "child_bev_16"       "child_bevamt_1"     "child_bevamt_2"    
    ##  [85] "child_bevamt_3"     "child_bevamt_4"     "child_bevamt_5"    
    ##  [88] "child_bevamt_6"     "child_bevamt_7"     "child_bevamt_8"    
    ##  [91] "child_bevamt_9"     "child_bevamt_10"    "child_bevamt_11"   
    ##  [94] "child_bevamt_12"    "child_bevamt_13"    "child_bevamt_14"   
    ##  [97] "child_bevamt_15"    "child_bevamt_16"    "child_bev_1_t2"    
    ## [100] "child_bev_2_t2"     "child_bev_3_t2"     "child_bev_4_t2"    
    ## [103] "child_bev_5_t2"     "child_bev_6_t2"     "child_bev_7_t2"    
    ## [106] "child_bev_8_t2"     "child_bev_9_t2"     "child_bev_10_t2"   
    ## [109] "child_bev_11_t2"    "child_bev_12_t2"    "child_bev_13_t2"   
    ## [112] "child_bev_14_t2"    "child_bev_15_t2"    "child_bev_16_t2"   
    ## [115] "child_bevamt_1_t2"  "child_bevamt_2_t2"  "child_bevamt_3_t2" 
    ## [118] "child_bevamt_4_t2"  "child_bevamt_5_t2"  "child_bevamt_6_t2" 
    ## [121] "child_bevamt_7_t2"  "child_bevamt_8_t2"  "child_bevamt_9_t2" 
    ## [124] "child_bevamt_10_t2" "child_bevamt_11_t2" "child_bevamt_12_t2"
    ## [127] "child_bevamt_13_t2" "child_bevamt_14_t2" "child_bevamt_15_t2"
    ## [130] "child_bevamt_16_t2"

## Parent Beverages questionnaire

### Unit of times per day (“pd”)

Here, we have several transformations that need to take place.

First we convert frequency (“How often in one week”) to the unit of
times per day (“pd”).

Original: par\_bev\_1 through par\_bev\_16

1= Never or less than 1 tme per week\_\_\_\_**new = 0**

2= 1 time per week\_\_\_\_**new = 1/7 or .0143**

3= 2-3 times per week\_\_\_\_**new = 2.5/7 or .357**

4= 4-6 times per week\_\_\_\_**new = 5/7 or .714**

5= 1 time per day\_\_\_\_**new = 1**

6= 2 times per day\_\_\_\_**new = 2**

7= 3 or more times per day\_\_\_\_**new = 3**

**New: par\_bev\_1\_pd through par\_bev\_16\_pd**

``` r
#this function takes x(variable) and recodes it to y, then returns the recoded y
bevperday_fun<- function(x){
  y=case_when(x==1~0,
              x==2~1/7,
              x==3~2.5/7,
              x==4~5/7,
              x==5~1,
              x==6~2,
              x==7~3)
  y
}

#here we name the vars so we can use it quickly in our recoding of all of them
par_bev_vars<- c("par_bev_1", "par_bev_2", "par_bev_3", "par_bev_4", "par_bev_5", "par_bev_6", "par_bev_7", "par_bev_8", "par_bev_9", "par_bev_10", "par_bev_11", "par_bev_12", "par_bev_13", "par_bev_14", "par_bev_15", "par_bev_16")

bevq.df<- bevq.df %>%
  mutate_at(par_bev_vars, .funs = list(pd= bevperday_fun)) #all new recodes will have "pd" (perday) at the end of var name


#time 2
par_bev_vars_t2<- c("par_bev_1_t2", "par_bev_2_t2", "par_bev_3_t2", "par_bev_4_t2", "par_bev_5_t2", "par_bev_6_t2", "par_bev_7_t2", "par_bev_8_t2", "par_bev_9_t2", "par_bev_10_t2", "par_bev_11_t2", "par_bev_12_t2", "par_bev_13_t2", "par_bev_14_t2", "par_bev_15_t2", "par_bev_16_t2")

bevq.df<- bevq.df %>%
  mutate_at(par_bev_vars_t2, .funs = list(pd= bevperday_fun)) #all new recodes will have "pd" at end of var name
```

### Unit ounces (“oz”)

Next, we convert the amount consumed (originally a likert scale) into
actual ounces so that it can then be used in an equation (per day x
ounces= oz/pd)

Original: par\_bevamt\_1 through par\_bevamt\_16

1= Less than 6 fl oz (3/4 cup)\_\_\_\_**new = 6 fl 0z**

2= 8 fl oz (1 cup)\_\_\_\_**new = 8 fl 0z**

3= 12 fl oz (1 1/2 cups)\_\_\_\_**new = 12 fl 0z**

4= 16 fl oz (2 cups)\_\_\_\_**new = 16 fl 0z**

5= More than 20 fl oz (2 1/2 cups)\_\_\_\_**new = 20 fl 0z**

**New: par\_bevamt\_1\_oz through par\_bevamt\_16\_oz**

``` r
#this function takes x(variable) and recodes it to y, then returns the recoded y
amtoz_fun<- function(x){
  y=case_when(x==1~6,
              x==2~8,
              x==3~12,
              x==4~16,
              x==5~20)
  y
}


#here we name the vars so we can use it quickly in our recoding of all of them
par_bevamt_vars<- c("par_bevamt_1", "par_bevamt_2", "par_bevamt_3", "par_bevamt_4", "par_bevamt_5", "par_bevamt_6", "par_bevamt_7", "par_bevamt_8", "par_bevamt_9", "par_bevamt_10", "par_bevamt_11", "par_bevamt_12", "par_bevamt_13", "par_bevamt_14", "par_bevamt_15", "par_bevamt_16")

bevq.df<- bevq.df %>%
  mutate_at(par_bevamt_vars, .funs = list(oz= amtoz_fun)) #all new recodes will have "oz" (ounces) at the end


#time 2
#here we name the vars so we can use it quickly in our recoding of all of them
par_bevamt_vars_t2<- c("par_bevamt_1_t2", "par_bevamt_2_t2", "par_bevamt_3_t2", "par_bevamt_4_t2", "par_bevamt_5_t2", "par_bevamt_6_t2", "par_bevamt_7_t2", "par_bevamt_8_t2", "par_bevamt_9_t2", "par_bevamt_10_t2", "par_bevamt_11_t2", "par_bevamt_12_t2", "par_bevamt_13_t2", "par_bevamt_14_t2", "par_bevamt_15_t2", "par_bevamt_16_t2")

bevq.df<- bevq.df %>%
  mutate_at(par_bevamt_vars_t2, .funs = list(oz= amtoz_fun)) #all new recodes will have "oz" (ounces) at the end
```

### Unit ounces per day (“bevtotal”)

Now we get the total ounces per day for each beverage by taking the
times per day and multiplying by ounces. But we keep zero as is since NA
X 0 would be NA (see function written).

Example:

par\_bevamt\_1\_pd X par\_bevamt\_1oz= par\_bevtotal\_1 (total ounces
per day consumed for beverage 1)

**New: par\_bevtotal\_1 through par\_bevtotal\_16**

``` r
pdoz_fun<- function(x, y) {
  z=ifelse(x==0, 0, x*y)
  z
}

bevq.df<- bevq.df %>%
  mutate(par_bevtotal_1=pdoz_fun(par_bev_1_pd, par_bevamt_1_oz),
         par_bevtotal_2=pdoz_fun(par_bev_2_pd, par_bevamt_2_oz),
         par_bevtotal_3=pdoz_fun(par_bev_3_pd, par_bevamt_3_oz),
         par_bevtotal_4=pdoz_fun(par_bev_4_pd, par_bevamt_4_oz),
         par_bevtotal_5=pdoz_fun(par_bev_5_pd, par_bevamt_5_oz),
         par_bevtotal_6=pdoz_fun(par_bev_6_pd, par_bevamt_6_oz),
         par_bevtotal_7=pdoz_fun(par_bev_7_pd, par_bevamt_7_oz),
         par_bevtotal_8=pdoz_fun(par_bev_8_pd, par_bevamt_8_oz),
         par_bevtotal_9=pdoz_fun(par_bev_9_pd, par_bevamt_9_oz),
         par_bevtotal_10=pdoz_fun(par_bev_10_pd, par_bevamt_10_oz),
         par_bevtotal_11=pdoz_fun(par_bev_11_pd, par_bevamt_11_oz),
         par_bevtotal_12=pdoz_fun(par_bev_12_pd, par_bevamt_12_oz),
         par_bevtotal_13=pdoz_fun(par_bev_13_pd, par_bevamt_13_oz),
         par_bevtotal_14=pdoz_fun(par_bev_14_pd, par_bevamt_14_oz),
         par_bevtotal_15=pdoz_fun(par_bev_15_pd, par_bevamt_15_oz),
         par_bevtotal_16=pdoz_fun(par_bev_16_pd, par_bevamt_16_oz))


#time 2
bevq.df<- bevq.df %>%
  mutate(par_bevtotal_1_t2=pdoz_fun(par_bev_1_t2_pd, par_bevamt_1_t2_oz),
         par_bevtotal_2_t2=pdoz_fun(par_bev_2_t2_pd, par_bevamt_2_t2_oz),
         par_bevtotal_3_t2=pdoz_fun(par_bev_3_t2_pd, par_bevamt_3_t2_oz),
         par_bevtotal_4_t2=pdoz_fun(par_bev_4_t2_pd, par_bevamt_4_t2_oz),
         par_bevtotal_5_t2=pdoz_fun(par_bev_5_t2_pd, par_bevamt_5_t2_oz),
         par_bevtotal_6_t2=pdoz_fun(par_bev_6_t2_pd, par_bevamt_6_t2_oz),
         par_bevtotal_7_t2=pdoz_fun(par_bev_7_t2_pd, par_bevamt_7_t2_oz),
         par_bevtotal_8_t2=pdoz_fun(par_bev_8_t2_pd, par_bevamt_8_t2_oz),
         par_bevtotal_9_t2=pdoz_fun(par_bev_9_t2_pd, par_bevamt_9_t2_oz),
         par_bevtotal_10_t2=pdoz_fun(par_bev_10_t2_pd, par_bevamt_10_t2_oz),
         par_bevtotal_11_t2=pdoz_fun(par_bev_11_t2_pd, par_bevamt_11_t2_oz),
         par_bevtotal_12_t2=pdoz_fun(par_bev_12_t2_pd, par_bevamt_12_t2_oz),
         par_bevtotal_13_t2=pdoz_fun(par_bev_13_t2_pd, par_bevamt_13_t2_oz),
         par_bevtotal_14_t2=pdoz_fun(par_bev_14_t2_pd, par_bevamt_14_t2_oz),
         par_bevtotal_15_t2=pdoz_fun(par_bev_15_t2_pd, par_bevamt_15_t2_oz),
         par_bevtotal_16_t2=pdoz_fun(par_bev_16_t2_pd, par_bevamt_16_t2_oz))

bevq.df %>%
  summarise_at(vars(par_bevtotal_1:par_bevtotal_16), list(mean), na.rm=TRUE)
```

    ## # A tibble: 1 x 16
    ##   par_bevtotal_1 par_bevtotal_2 par_bevtotal_3 par_bevtotal_4
    ##            <dbl>          <dbl>          <dbl>          <dbl>
    ## 1           41.9           4.32           4.48           1.53
    ## # ... with 12 more variables: par_bevtotal_5 <dbl>, par_bevtotal_6 <dbl>,
    ## #   par_bevtotal_7 <dbl>, par_bevtotal_8 <dbl>, par_bevtotal_9 <dbl>,
    ## #   par_bevtotal_10 <dbl>, par_bevtotal_11 <dbl>, par_bevtotal_12 <dbl>,
    ## #   par_bevtotal_13 <dbl>, par_bevtotal_14 <dbl>, par_bevtotal_15 <dbl>,
    ## #   par_bevtotal_16 <dbl>

``` r
bevq.df %>%
  summarise_at(vars(par_bevtotal_1_t2:par_bevtotal_16_t2), list(mean), na.rm=TRUE)
```

    ## # A tibble: 1 x 16
    ##   par_bevtotal_1_~ par_bevtotal_2_~ par_bevtotal_3_~ par_bevtotal_4_~
    ##              <dbl>            <dbl>            <dbl>            <dbl>
    ## 1             38.7             1.38             2.80            0.617
    ## # ... with 12 more variables: par_bevtotal_5_t2 <dbl>,
    ## #   par_bevtotal_6_t2 <dbl>, par_bevtotal_7_t2 <dbl>,
    ## #   par_bevtotal_8_t2 <dbl>, par_bevtotal_9_t2 <dbl>,
    ## #   par_bevtotal_10_t2 <dbl>, par_bevtotal_11_t2 <dbl>,
    ## #   par_bevtotal_12_t2 <dbl>, par_bevtotal_13_t2 <dbl>,
    ## #   par_bevtotal_14_t2 <dbl>, par_bevtotal_15_t2 <dbl>,
    ## #   par_bevtotal_16_t2 <dbl>

### Unit ounces per day of sugar-sweetened beverages (ssb)

To quantify average daily SSB consumption during the past month,
beverage categories containing added sugars were summed (sweetened juice
beverages and drinks (par\_bev\_3), regular sugar-sweetened carbonated
beverages (par\_bev\_8), sweet tea (par\_bev\_10), sweetened coffee
(par\_bev\_11), energy drinks (par\_bev\_13), and mixed alcoholic
drinks).

Note that since this is a sum, we keep cases where some beverages are
NA, unless all beverages are NA, then the total will also be NA.

For example, if bev\_3 has a total, but NA for other ssb variables, the
ssb\_total for that case will be the sum of bev\_3.

**New: par\_bevtotal\_ssb**

``` r
bevq.df<- bevq.df %>%
  mutate(par_bevtotal_ssb=ifelse(is.na(par_bevtotal_3) & is.na(par_bevtotal_8) & is.na(par_bevtotal_10) & is.na(par_bevtotal_11) & is.na(par_bevtotal_13), NA, rowSums(select(., par_bevtotal_3,par_bevtotal_8,par_bevtotal_10,par_bevtotal_11,par_bevtotal_13), na.rm=TRUE)))


#time 2
bevq.df<- bevq.df %>%
  mutate(par_bevtotal_ssb_t2=ifelse(is.na(par_bevtotal_3_t2) & is.na(par_bevtotal_8_t2) & is.na(par_bevtotal_10_t2) & is.na(par_bevtotal_11_t2) & is.na(par_bevtotal_13_t2), NA, rowSums(select(., par_bevtotal_3_t2,par_bevtotal_8_t2,par_bevtotal_10_t2,par_bevtotal_11_t2,par_bevtotal_13_t2), na.rm=TRUE)))
```

### Unit binary (y/n)

Now we can make binary per day drink “any” vs. none for each beverage.

Original: par\_bev\_1\_pd through par\_bev\_pd

**New: par\_bev\_1\_pd\_bi through par\_bev\_16\_pd\_bi**

``` r
#function to create binary, but so we can pass on multiple
pdbi_fun<- function(x) {
  y=ifelse(x>0, 1, 0)
  y
}

par_bev_pd_vars<- c("par_bev_1_pd", "par_bev_2_pd", "par_bev_3_pd", "par_bev_4_pd", "par_bev_5_pd", "par_bev_6_pd", "par_bev_7_pd", "par_bev_8_pd", "par_bev_9_pd", "par_bev_10_pd", "par_bev_11_pd", "par_bev_12_pd", "par_bev_13_pd", "par_bev_14_pd", "par_bev_15_pd", "par_bev_16_pd")

bevq.df<- bevq.df %>%
  mutate_at(par_bev_pd_vars, .funs = list(bi= pdbi_fun)) #makes "bi" add to each var name

#Time 2
par_bev_pd_vars_t2<- c("par_bev_1_t2_pd", "par_bev_2_t2_pd", "par_bev_3_t2_pd", "par_bev_4_t2_pd", "par_bev_5_t2_pd", "par_bev_6_t2_pd", "par_bev_7_t2_pd", "par_bev_8_t2_pd", "par_bev_9_t2_pd", "par_bev_10_t2_pd", "par_bev_11_t2_pd", "par_bev_12_t2_pd", "par_bev_13_t2_pd", "par_bev_14_t2_pd", "par_bev_15_t2_pd", "par_bev_16_t2_pd")

bevq.df<- bevq.df %>%
  mutate_at(par_bev_pd_vars_t2, .funs = list(bi= pdbi_fun))
```

## Child Beverages

Now we repeat all the beverage steps from above with the **child\_
data**.

All functions are already in the global environment, so no need to
respecify them.

For brevity, I will not annotate the children’s section like I did
above.

``` r
#here we name the vars so we can use it quickly in our recoding of all of them
child_bev_vars<- c("child_bev_1", "child_bev_2", "child_bev_3", "child_bev_4", "child_bev_5", "child_bev_6", "child_bev_7", "child_bev_8", "child_bev_9", "child_bev_10", "child_bev_11", "child_bev_12", "child_bev_13", "child_bev_14", "child_bev_15", "child_bev_16")

bevq.df<- bevq.df %>%
  mutate_at(child_bev_vars, .funs = list(pd= bevperday_fun)) #all new recodes will have "pd" (perday) at the end

#here we name the vars so we can use it quickly in our recoding of all of them
child_bevamt_vars<- c("child_bevamt_1", "child_bevamt_2", "child_bevamt_3", "child_bevamt_4", "child_bevamt_5", "child_bevamt_6", "child_bevamt_7", "child_bevamt_8", "child_bevamt_9", "child_bevamt_10", "child_bevamt_11", "child_bevamt_12", "child_bevamt_13", "child_bevamt_14", "child_bevamt_15", "child_bevamt_16")

bevq.df<- bevq.df %>%
  mutate_at(child_bevamt_vars, .funs = list(oz= amtoz_fun)) #all new recodes will have "oz" (ounces) at the end

bevq.df<- bevq.df %>%
  mutate(child_bevtotal_1=pdoz_fun(child_bev_1_pd, child_bevamt_1_oz),
         child_bevtotal_2=pdoz_fun(child_bev_2_pd, child_bevamt_2_oz),
         child_bevtotal_3=pdoz_fun(child_bev_3_pd, child_bevamt_3_oz),
         child_bevtotal_4=pdoz_fun(child_bev_4_pd, child_bevamt_4_oz),
         child_bevtotal_5=pdoz_fun(child_bev_5_pd, child_bevamt_5_oz),
         child_bevtotal_6=pdoz_fun(child_bev_6_pd, child_bevamt_6_oz),
         child_bevtotal_7=pdoz_fun(child_bev_7_pd, child_bevamt_7_oz),
         child_bevtotal_8=pdoz_fun(child_bev_8_pd, child_bevamt_8_oz),
         child_bevtotal_9=pdoz_fun(child_bev_9_pd, child_bevamt_9_oz),
         child_bevtotal_10=pdoz_fun(child_bev_10_pd, child_bevamt_10_oz),
         child_bevtotal_11=pdoz_fun(child_bev_11_pd, child_bevamt_11_oz),
         child_bevtotal_12=pdoz_fun(child_bev_12_pd, child_bevamt_12_oz),
         child_bevtotal_13=pdoz_fun(child_bev_13_pd, child_bevamt_13_oz),
         child_bevtotal_14=pdoz_fun(child_bev_14_pd, child_bevamt_14_oz),
         child_bevtotal_15=pdoz_fun(child_bev_15_pd, child_bevamt_15_oz),
         child_bevtotal_16=pdoz_fun(child_bev_16_pd, child_bevamt_16_oz))

bevq.df<- bevq.df %>%
mutate(child_bevtotal_ssb=ifelse(is.na(child_bevtotal_3) & is.na(child_bevtotal_8) & is.na(child_bevtotal_10) & is.na(child_bevtotal_11) & is.na(child_bevtotal_13), NA, rowSums(select(., child_bevtotal_3,child_bevtotal_8,child_bevtotal_10,child_bevtotal_11,child_bevtotal_13), na.rm=TRUE)))


child_bev_pd_vars<- c("child_bev_1_pd", "child_bev_2_pd", "child_bev_3_pd", "child_bev_4_pd", "child_bev_5_pd", "child_bev_6_pd", "child_bev_7_pd", "child_bev_8_pd", "child_bev_9_pd", "child_bev_10_pd", "child_bev_11_pd", "child_bev_12_pd", "child_bev_13_pd", "child_bev_14_pd", "child_bev_15_pd", "child_bev_16_pd")

bevq.df<- bevq.df %>%
  mutate_at(child_bev_pd_vars, .funs = list(bi= pdbi_fun))


#Time 2

#here we name the vars so we can use it quickly in our recoding of all of them
child_bev_vars_t2<- c("child_bev_1_t2", "child_bev_2_t2", "child_bev_3_t2", "child_bev_4_t2", "child_bev_5_t2", "child_bev_6_t2", "child_bev_7_t2", "child_bev_8_t2", "child_bev_9_t2", "child_bev_10_t2", "child_bev_11_t2", "child_bev_12_t2", "child_bev_13_t2", "child_bev_14_t2", "child_bev_15_t2", "child_bev_16_t2")

bevq.df<- bevq.df %>%
  mutate_at(child_bev_vars_t2, .funs = list(pd= bevperday_fun)) #all new recodes will have "pd" (perday) at the end

#here we name the vars so we can use it quickly in our recoding of all of them
child_bevamt_vars_t2<- c("child_bevamt_1_t2", "child_bevamt_2_t2", "child_bevamt_3_t2", "child_bevamt_4_t2", "child_bevamt_5_t2", "child_bevamt_6_t2", "child_bevamt_7_t2", "child_bevamt_8_t2", "child_bevamt_9_t2", "child_bevamt_10_t2", "child_bevamt_11_t2", "child_bevamt_12_t2", "child_bevamt_13_t2", "child_bevamt_14_t2", "child_bevamt_15_t2", "child_bevamt_16_t2")

bevq.df<- bevq.df %>%
  mutate_at(child_bevamt_vars_t2, .funs = list(oz= amtoz_fun)) #all new recodes will have "oz" (ounces) at the end

bevq.df<- bevq.df %>%
  mutate(child_bevtotal_1_t2=pdoz_fun(child_bev_1_t2_pd, child_bevamt_1_t2_oz),
         child_bevtotal_2_t2=pdoz_fun(child_bev_2_t2_pd, child_bevamt_2_t2_oz),
         child_bevtotal_3_t2=pdoz_fun(child_bev_3_t2_pd, child_bevamt_3_t2_oz),
         child_bevtotal_4_t2=pdoz_fun(child_bev_4_t2_pd, child_bevamt_4_t2_oz),
         child_bevtotal_5_t2=pdoz_fun(child_bev_5_t2_pd, child_bevamt_5_t2_oz),
         child_bevtotal_6_t2=pdoz_fun(child_bev_6_t2_pd, child_bevamt_6_t2_oz),
         child_bevtotal_7_t2=pdoz_fun(child_bev_7_t2_pd, child_bevamt_7_t2_oz),
         child_bevtotal_8_t2=pdoz_fun(child_bev_8_t2_pd, child_bevamt_8_t2_oz),
         child_bevtotal_9_t2=pdoz_fun(child_bev_9_t2_pd, child_bevamt_9_t2_oz),
         child_bevtotal_10_t2=pdoz_fun(child_bev_10_t2_pd, child_bevamt_10_t2_oz),
         child_bevtotal_11_t2=pdoz_fun(child_bev_11_t2_pd, child_bevamt_11_t2_oz),
         child_bevtotal_12_t2=pdoz_fun(child_bev_12_t2_pd, child_bevamt_12_t2_oz),
         child_bevtotal_13_t2=pdoz_fun(child_bev_13_t2_pd, child_bevamt_13_t2_oz),
         child_bevtotal_14_t2=pdoz_fun(child_bev_14_t2_pd, child_bevamt_14_t2_oz),
         child_bevtotal_15_t2=pdoz_fun(child_bev_15_t2_pd, child_bevamt_15_t2_oz),
         child_bevtotal_16_t2=pdoz_fun(child_bev_16_t2_pd, child_bevamt_16_t2_oz))

bevq.df<- bevq.df %>%
mutate(child_bevtotal_ssb_t2=ifelse(is.na(child_bevtotal_3_t2) & is.na(child_bevtotal_8_t2) & is.na(child_bevtotal_10_t2) & is.na(child_bevtotal_11_t2) & is.na(child_bevtotal_13_t2), NA, rowSums(select(., child_bevtotal_3_t2,child_bevtotal_8_t2,child_bevtotal_10_t2,child_bevtotal_11_t2,child_bevtotal_13_t2), na.rm=TRUE)))


child_bev_pd_vars_t2<- c("child_bev_1_t2_pd", "child_bev_2_t2_pd", "child_bev_3_t2_pd", "child_bev_4_t2_pd", "child_bev_5_t2_pd", "child_bev_6_t2_pd", "child_bev_7_t2_pd", "child_bev_8_t2_pd", "child_bev_9_t2_pd", "child_bev_10_t2_pd", "child_bev_11_t2_pd", "child_bev_12_t2_pd", "child_bev_13_t2_pd", "child_bev_14_t2_pd", "child_bev_15_t2_pd", "child_bev_16_t2_pd")

bevq.df<- bevq.df %>%
  mutate_at(child_bev_pd_vars_t2, .funs = list(bi= pdbi_fun))
```

## Cleaning

Wide datasets that go through a bunch of mutations often end with
variables with \_t2 at the end of the variable name or in the middle
somewhere along the mutation path. This bugs me, so I clean it up with
this code.

Here, I am just swapping the location of the “t2” in a few of the
variables that I created by passing functions. I want the dataset to be
consistent with \_t2 always at the very end of the variable name.

``` r
bevq.df<-bevq.df %>%
  rename_at(vars(ends_with("t2_pd")), funs(sub("t2_pd", "pd_t2", .)))
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once per session.

``` r
bevq.df<-bevq.df %>%
  rename_at(vars(ends_with("t2_oz")), funs(sub("t2_oz", "oz_t2", .)))
            
            
bevq.df<-bevq.df %>%
  rename_at(vars(ends_with("t2_pd_bi")), funs(sub("t2_pd_bi", "pd_bi_t2", .)))
```

## Results

Lets take a look at what we returned.

``` r
bevq.df %>%
  select(hhid, childid, child_bev_1, child_bev_1_pd, child_bevamt_1, child_bevamt_1_oz, child_bevtotal_1) %>%
  arrange(hhid)
```

    ## # A tibble: 114 x 7
    ##     hhid childid child_bev_1 child_bev_1_pd child_bevamt_1 child_bevamt_1_~
    ##    <dbl>   <dbl>   <dbl+lbl>          <dbl>      <dbl+lbl>            <dbl>
    ##  1 12001    2001 NA                      NA NA                           NA
    ##  2 12002    2002 NA                      NA NA                           NA
    ##  3 12003    2003 NA                      NA NA                           NA
    ##  4 12006    2006  7 [3+ tim~              3  4 [16 fl oz ~               16
    ##  5 12007    2007  1 [Never ~              0 NA                           NA
    ##  6 12008    2008  7 [3+ tim~              3  3 [12 fl oz ~               12
    ##  7 12009    2009  7 [3+ tim~              3  2 [8 fl oz (~                8
    ##  8 12010    2010  7 [3+ tim~              3  3 [12 fl oz ~               12
    ##  9 12011    2011  7 [3+ tim~              3  3 [12 fl oz ~               12
    ## 10 12012    2012  7 [3+ tim~              3  5 [More than~               20
    ## # ... with 104 more rows, and 1 more variable: child_bevtotal_1 <dbl>

``` r
bevq.df %>%
  select(contains("_pd")) %>%
  skimr::skim()%>%
  dplyr::filter(stat == "mean") %>%
  select(variable, value)
```

    ## # A tibble: 128 x 2
    ##    variable      value
    ##    <chr>         <dbl>
    ##  1 par_bev_1_pd  2.49 
    ##  2 par_bev_2_pd  0.367
    ##  3 par_bev_3_pd  0.364
    ##  4 par_bev_4_pd  0.161
    ##  5 par_bev_5_pd  0.119
    ##  6 par_bev_6_pd  0.140
    ##  7 par_bev_7_pd  0.217
    ##  8 par_bev_8_pd  0.469
    ##  9 par_bev_9_pd  0.306
    ## 10 par_bev_10_pd 0.362
    ## # ... with 118 more rows

``` r
bevq.df %>%
  select(contains("_oz")) %>%
  skimr::skim()%>%
  dplyr::filter(stat == "mean") %>%
  select(variable, value)
```

    ## # A tibble: 64 x 2
    ##    variable         value
    ##    <chr>            <dbl>
    ##  1 par_bevamt_1_oz  16.0 
    ##  2 par_bevamt_2_oz  12.2 
    ##  3 par_bevamt_3_oz  11.3 
    ##  4 par_bevamt_4_oz  11.2 
    ##  5 par_bevamt_5_oz   8.77
    ##  6 par_bevamt_6_oz  10   
    ##  7 par_bevamt_7_oz   9.18
    ##  8 par_bevamt_8_oz  13.6 
    ##  9 par_bevamt_9_oz  13.9 
    ## 10 par_bevamt_10_oz 12.2 
    ## # ... with 54 more rows

``` r
bevq.df %>%
  select(contains("_bevtotal")) %>%
  skimr::skim() %>%
  dplyr::filter(stat == "mean") %>%
  select(variable, value)
```

    ## # A tibble: 68 x 2
    ##    variable        value
    ##    <chr>           <dbl>
    ##  1 par_bevtotal_1  41.9 
    ##  2 par_bevtotal_2   4.32
    ##  3 par_bevtotal_3   4.48
    ##  4 par_bevtotal_4   1.53
    ##  5 par_bevtotal_5   1.06
    ##  6 par_bevtotal_6   1.54
    ##  7 par_bevtotal_7   2.27
    ##  8 par_bevtotal_8   6.62
    ##  9 par_bevtotal_9   4.65
    ## 10 par_bevtotal_10  4.22
    ## # ... with 58 more rows

## Labels

If you need to add variable labels, it’s best to do this by pulling in
an excel file and applying that way.

``` r
var_label.df<- read.xlsx(paste0(datDir, "Beverage labels.xlsx"))


#we have to arrange the df with the labels by the column "variable" to be in alphabetical order
var_label.df<- var_label.df %>%
  arrange(variable)
var_label.df
```

    ##                  variable
    ## 1             child_bev_1
    ## 2          child_bev_1_pd
    ## 3       child_bev_1_pd_bi
    ## 4    child_bev_1_pd_bi_t2
    ## 5       child_bev_1_pd_t2
    ## 6          child_bev_1_t2
    ## 7            child_bev_10
    ## 8         child_bev_10_pd
    ## 9      child_bev_10_pd_bi
    ## 10  child_bev_10_pd_bi_t2
    ## 11     child_bev_10_pd_t2
    ## 12        child_bev_10_t2
    ## 13           child_bev_11
    ## 14        child_bev_11_pd
    ## 15     child_bev_11_pd_bi
    ## 16  child_bev_11_pd_bi_t2
    ## 17     child_bev_11_pd_t2
    ## 18        child_bev_11_t2
    ## 19           child_bev_12
    ## 20        child_bev_12_pd
    ## 21     child_bev_12_pd_bi
    ## 22  child_bev_12_pd_bi_t2
    ## 23     child_bev_12_pd_t2
    ## 24        child_bev_12_t2
    ## 25           child_bev_13
    ## 26        child_bev_13_pd
    ## 27     child_bev_13_pd_bi
    ## 28  child_bev_13_pd_bi_t2
    ## 29     child_bev_13_pd_t2
    ## 30        child_bev_13_t2
    ## 31           child_bev_14
    ## 32        child_bev_14_pd
    ## 33     child_bev_14_pd_bi
    ## 34  child_bev_14_pd_bi_t2
    ## 35     child_bev_14_pd_t2
    ## 36        child_bev_14_t2
    ## 37           child_bev_15
    ## 38        child_bev_15_pd
    ## 39     child_bev_15_pd_bi
    ## 40  child_bev_15_pd_bi_t2
    ## 41     child_bev_15_pd_t2
    ## 42        child_bev_15_t2
    ## 43           child_bev_16
    ## 44        child_bev_16_pd
    ## 45     child_bev_16_pd_bi
    ## 46  child_bev_16_pd_bi_t2
    ## 47     child_bev_16_pd_t2
    ## 48        child_bev_16_t2
    ## 49            child_bev_2
    ## 50         child_bev_2_pd
    ## 51      child_bev_2_pd_bi
    ## 52   child_bev_2_pd_bi_t2
    ## 53      child_bev_2_pd_t2
    ## 54         child_bev_2_t2
    ## 55            child_bev_3
    ## 56         child_bev_3_pd
    ## 57      child_bev_3_pd_bi
    ## 58   child_bev_3_pd_bi_t2
    ## 59      child_bev_3_pd_t2
    ## 60         child_bev_3_t2
    ## 61            child_bev_4
    ## 62         child_bev_4_pd
    ## 63      child_bev_4_pd_bi
    ## 64   child_bev_4_pd_bi_t2
    ## 65      child_bev_4_pd_t2
    ## 66         child_bev_4_t2
    ## 67            child_bev_5
    ## 68         child_bev_5_pd
    ## 69      child_bev_5_pd_bi
    ## 70   child_bev_5_pd_bi_t2
    ## 71      child_bev_5_pd_t2
    ## 72         child_bev_5_t2
    ## 73            child_bev_6
    ## 74         child_bev_6_pd
    ## 75      child_bev_6_pd_bi
    ## 76   child_bev_6_pd_bi_t2
    ## 77      child_bev_6_pd_t2
    ## 78         child_bev_6_t2
    ## 79            child_bev_7
    ## 80         child_bev_7_pd
    ## 81      child_bev_7_pd_bi
    ## 82   child_bev_7_pd_bi_t2
    ## 83      child_bev_7_pd_t2
    ## 84         child_bev_7_t2
    ## 85            child_bev_8
    ## 86         child_bev_8_pd
    ## 87      child_bev_8_pd_bi
    ## 88   child_bev_8_pd_bi_t2
    ## 89      child_bev_8_pd_t2
    ## 90         child_bev_8_t2
    ## 91            child_bev_9
    ## 92         child_bev_9_pd
    ## 93      child_bev_9_pd_bi
    ## 94   child_bev_9_pd_bi_t2
    ## 95      child_bev_9_pd_t2
    ## 96         child_bev_9_t2
    ## 97         child_bevamt_1
    ## 98      child_bevamt_1_oz
    ## 99   child_bevamt_1_oz_t2
    ## 100     child_bevamt_1_t2
    ## 101       child_bevamt_10
    ## 102    child_bevamt_10_oz
    ## 103 child_bevamt_10_oz_t2
    ## 104    child_bevamt_10_t2
    ## 105       child_bevamt_11
    ## 106    child_bevamt_11_oz
    ## 107 child_bevamt_11_oz_t2
    ## 108    child_bevamt_11_t2
    ## 109       child_bevamt_12
    ## 110    child_bevamt_12_oz
    ## 111 child_bevamt_12_oz_t2
    ## 112    child_bevamt_12_t2
    ## 113       child_bevamt_13
    ## 114    child_bevamt_13_oz
    ## 115 child_bevamt_13_oz_t2
    ## 116    child_bevamt_13_t2
    ## 117       child_bevamt_14
    ## 118    child_bevamt_14_oz
    ## 119 child_bevamt_14_oz_t2
    ## 120    child_bevamt_14_t2
    ## 121       child_bevamt_15
    ## 122    child_bevamt_15_oz
    ## 123 child_bevamt_15_oz_t2
    ## 124    child_bevamt_15_t2
    ## 125       child_bevamt_16
    ## 126    child_bevamt_16_oz
    ## 127 child_bevamt_16_oz_t2
    ## 128    child_bevamt_16_t2
    ## 129        child_bevamt_2
    ## 130     child_bevamt_2_oz
    ## 131  child_bevamt_2_oz_t2
    ## 132     child_bevamt_2_t2
    ## 133        child_bevamt_3
    ## 134     child_bevamt_3_oz
    ## 135  child_bevamt_3_oz_t2
    ## 136     child_bevamt_3_t2
    ## 137        child_bevamt_4
    ## 138     child_bevamt_4_oz
    ## 139  child_bevamt_4_oz_t2
    ## 140     child_bevamt_4_t2
    ## 141        child_bevamt_5
    ## 142     child_bevamt_5_oz
    ## 143  child_bevamt_5_oz_t2
    ## 144     child_bevamt_5_t2
    ## 145        child_bevamt_6
    ## 146     child_bevamt_6_oz
    ## 147  child_bevamt_6_oz_t2
    ## 148     child_bevamt_6_t2
    ## 149        child_bevamt_7
    ## 150     child_bevamt_7_oz
    ## 151  child_bevamt_7_oz_t2
    ## 152     child_bevamt_7_t2
    ## 153        child_bevamt_8
    ## 154     child_bevamt_8_oz
    ## 155  child_bevamt_8_oz_t2
    ## 156     child_bevamt_8_t2
    ## 157        child_bevamt_9
    ## 158     child_bevamt_9_oz
    ## 159  child_bevamt_9_oz_t2
    ## 160     child_bevamt_9_t2
    ## 161      child_bevtotal_1
    ## 162   child_bevtotal_1_t2
    ## 163     child_bevtotal_10
    ## 164  child_bevtotal_10_t2
    ## 165     child_bevtotal_11
    ## 166  child_bevtotal_11_t2
    ## 167     child_bevtotal_12
    ## 168  child_bevtotal_12_t2
    ## 169     child_bevtotal_13
    ## 170  child_bevtotal_13_t2
    ## 171     child_bevtotal_14
    ## 172  child_bevtotal_14_t2
    ## 173     child_bevtotal_15
    ## 174  child_bevtotal_15_t2
    ## 175     child_bevtotal_16
    ## 176  child_bevtotal_16_t2
    ## 177      child_bevtotal_2
    ## 178   child_bevtotal_2_t2
    ## 179      child_bevtotal_3
    ## 180   child_bevtotal_3_t2
    ## 181      child_bevtotal_4
    ## 182   child_bevtotal_4_t2
    ## 183      child_bevtotal_5
    ## 184   child_bevtotal_5_t2
    ## 185      child_bevtotal_6
    ## 186   child_bevtotal_6_t2
    ## 187      child_bevtotal_7
    ## 188   child_bevtotal_7_t2
    ## 189      child_bevtotal_8
    ## 190   child_bevtotal_8_t2
    ## 191      child_bevtotal_9
    ## 192   child_bevtotal_9_t2
    ## 193    child_bevtotal_ssb
    ## 194 child_bevtotal_ssb_t2
    ## 195               childid
    ## 196                  hhid
    ## 197             par_bev_1
    ## 198          par_bev_1_pd
    ## 199       par_bev_1_pd_bi
    ## 200    par_bev_1_pd_bi_t2
    ## 201       par_bev_1_pd_t2
    ## 202          par_bev_1_t2
    ## 203            par_bev_10
    ## 204         par_bev_10_pd
    ## 205      par_bev_10_pd_bi
    ## 206   par_bev_10_pd_bi_t2
    ## 207      par_bev_10_pd_t2
    ## 208         par_bev_10_t2
    ## 209            par_bev_11
    ## 210         par_bev_11_pd
    ## 211      par_bev_11_pd_bi
    ## 212   par_bev_11_pd_bi_t2
    ## 213      par_bev_11_pd_t2
    ## 214         par_bev_11_t2
    ## 215            par_bev_12
    ## 216         par_bev_12_pd
    ## 217      par_bev_12_pd_bi
    ## 218   par_bev_12_pd_bi_t2
    ## 219      par_bev_12_pd_t2
    ## 220         par_bev_12_t2
    ## 221            par_bev_13
    ## 222         par_bev_13_pd
    ## 223      par_bev_13_pd_bi
    ## 224   par_bev_13_pd_bi_t2
    ## 225      par_bev_13_pd_t2
    ## 226         par_bev_13_t2
    ## 227            par_bev_14
    ## 228         par_bev_14_pd
    ## 229      par_bev_14_pd_bi
    ## 230   par_bev_14_pd_bi_t2
    ## 231      par_bev_14_pd_t2
    ## 232         par_bev_14_t2
    ## 233            par_bev_15
    ## 234         par_bev_15_pd
    ## 235      par_bev_15_pd_bi
    ## 236   par_bev_15_pd_bi_t2
    ## 237      par_bev_15_pd_t2
    ## 238         par_bev_15_t2
    ## 239            par_bev_16
    ## 240         par_bev_16_pd
    ## 241      par_bev_16_pd_bi
    ## 242   par_bev_16_pd_bi_t2
    ## 243      par_bev_16_pd_t2
    ## 244         par_bev_16_t2
    ## 245             par_bev_2
    ## 246          par_bev_2_pd
    ## 247       par_bev_2_pd_bi
    ## 248    par_bev_2_pd_bi_t2
    ## 249       par_bev_2_pd_t2
    ## 250          par_bev_2_t2
    ## 251             par_bev_3
    ## 252          par_bev_3_pd
    ## 253       par_bev_3_pd_bi
    ## 254    par_bev_3_pd_bi_t2
    ## 255       par_bev_3_pd_t2
    ## 256          par_bev_3_t2
    ## 257             par_bev_4
    ## 258          par_bev_4_pd
    ## 259       par_bev_4_pd_bi
    ## 260    par_bev_4_pd_bi_t2
    ## 261       par_bev_4_pd_t2
    ## 262          par_bev_4_t2
    ## 263             par_bev_5
    ## 264          par_bev_5_pd
    ## 265       par_bev_5_pd_bi
    ## 266    par_bev_5_pd_bi_t2
    ## 267       par_bev_5_pd_t2
    ## 268          par_bev_5_t2
    ## 269             par_bev_6
    ## 270          par_bev_6_pd
    ## 271       par_bev_6_pd_bi
    ## 272    par_bev_6_pd_bi_t2
    ## 273       par_bev_6_pd_t2
    ## 274          par_bev_6_t2
    ## 275             par_bev_7
    ## 276          par_bev_7_pd
    ## 277       par_bev_7_pd_bi
    ## 278    par_bev_7_pd_bi_t2
    ## 279       par_bev_7_pd_t2
    ## 280          par_bev_7_t2
    ## 281             par_bev_8
    ## 282          par_bev_8_pd
    ## 283       par_bev_8_pd_bi
    ## 284    par_bev_8_pd_bi_t2
    ## 285       par_bev_8_pd_t2
    ## 286          par_bev_8_t2
    ## 287             par_bev_9
    ## 288          par_bev_9_pd
    ## 289       par_bev_9_pd_bi
    ## 290    par_bev_9_pd_bi_t2
    ## 291       par_bev_9_pd_t2
    ## 292          par_bev_9_t2
    ## 293          par_bevamt_1
    ## 294       par_bevamt_1_oz
    ## 295    par_bevamt_1_oz_t2
    ## 296       par_bevamt_1_t2
    ## 297         par_bevamt_10
    ## 298      par_bevamt_10_oz
    ## 299   par_bevamt_10_oz_t2
    ## 300      par_bevamt_10_t2
    ## 301         par_bevamt_11
    ## 302      par_bevamt_11_oz
    ## 303   par_bevamt_11_oz_t2
    ## 304      par_bevamt_11_t2
    ## 305         par_bevamt_12
    ## 306      par_bevamt_12_oz
    ## 307   par_bevamt_12_oz_t2
    ## 308      par_bevamt_12_t2
    ## 309         par_bevamt_13
    ## 310      par_bevamt_13_oz
    ## 311   par_bevamt_13_oz_t2
    ## 312      par_bevamt_13_t2
    ## 313         par_bevamt_14
    ## 314      par_bevamt_14_oz
    ## 315   par_bevamt_14_oz_t2
    ## 316      par_bevamt_14_t2
    ## 317         par_bevamt_15
    ## 318      par_bevamt_15_oz
    ## 319   par_bevamt_15_oz_t2
    ## 320      par_bevamt_15_t2
    ## 321         par_bevamt_16
    ## 322      par_bevamt_16_oz
    ## 323   par_bevamt_16_oz_t2
    ## 324      par_bevamt_16_t2
    ## 325          par_bevamt_2
    ## 326       par_bevamt_2_oz
    ## 327    par_bevamt_2_oz_t2
    ## 328       par_bevamt_2_t2
    ## 329          par_bevamt_3
    ## 330       par_bevamt_3_oz
    ## 331    par_bevamt_3_oz_t2
    ## 332       par_bevamt_3_t2
    ## 333          par_bevamt_4
    ## 334       par_bevamt_4_oz
    ## 335    par_bevamt_4_oz_t2
    ## 336       par_bevamt_4_t2
    ## 337          par_bevamt_5
    ## 338       par_bevamt_5_oz
    ## 339    par_bevamt_5_oz_t2
    ## 340       par_bevamt_5_t2
    ## 341          par_bevamt_6
    ## 342       par_bevamt_6_oz
    ## 343    par_bevamt_6_oz_t2
    ## 344       par_bevamt_6_t2
    ## 345          par_bevamt_7
    ## 346       par_bevamt_7_oz
    ## 347    par_bevamt_7_oz_t2
    ## 348       par_bevamt_7_t2
    ## 349          par_bevamt_8
    ## 350       par_bevamt_8_oz
    ## 351    par_bevamt_8_oz_t2
    ## 352       par_bevamt_8_t2
    ## 353          par_bevamt_9
    ## 354       par_bevamt_9_oz
    ## 355    par_bevamt_9_oz_t2
    ## 356       par_bevamt_9_t2
    ## 357        par_bevtotal_1
    ## 358     par_bevtotal_1_t2
    ## 359       par_bevtotal_10
    ## 360    par_bevtotal_10_t2
    ## 361       par_bevtotal_11
    ## 362    par_bevtotal_11_t2
    ## 363       par_bevtotal_12
    ## 364    par_bevtotal_12_t2
    ## 365       par_bevtotal_13
    ## 366    par_bevtotal_13_t2
    ## 367       par_bevtotal_14
    ## 368    par_bevtotal_14_t2
    ## 369       par_bevtotal_15
    ## 370    par_bevtotal_15_t2
    ## 371       par_bevtotal_16
    ## 372    par_bevtotal_16_t2
    ## 373        par_bevtotal_2
    ## 374     par_bevtotal_2_t2
    ## 375        par_bevtotal_3
    ## 376     par_bevtotal_3_t2
    ## 377        par_bevtotal_4
    ## 378     par_bevtotal_4_t2
    ## 379        par_bevtotal_5
    ## 380     par_bevtotal_5_t2
    ## 381        par_bevtotal_6
    ## 382     par_bevtotal_6_t2
    ## 383        par_bevtotal_7
    ## 384     par_bevtotal_7_t2
    ## 385        par_bevtotal_8
    ## 386     par_bevtotal_8_t2
    ## 387        par_bevtotal_9
    ## 388     par_bevtotal_9_t2
    ## 389      par_bevtotal_ssb
    ## 390   par_bevtotal_ssb_t2
    ##                                                                                                             var_label
    ## 1                                                                                               Times per week: Water
    ## 2                                                                                                Times per day: Water
    ## 3                                                                                   Dichotomous: Times per day: Water
    ## 4                                                                                   Dichotomous: Times per day: Water
    ## 5                                                                                                Times per day: Water
    ## 6                                                                                               Times per week: Water
    ## 7                                                                                       Times per week: Sweetened tea
    ## 8                                                                                        Times per day: Sweetened tea
    ## 9                                                                           Dichotomous: Times per day: Sweetened tea
    ## 10                                                                          Dichotomous: Times per day: Sweetened tea
    ## 11                                                                                       Times per day: Sweetened tea
    ## 12                                                                                      Times per week: Sweetened tea
    ## 13                                       Times per week: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 14                                        Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 15                           Dichotomous: Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 16                           Dichotomous: Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 17                                        Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 18                                       Times per week: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 19                        Times per week: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 20                         Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 21            Dichotomous: Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 22            Dichotomous: Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 23                         Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 24                        Times per week: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 25                                            Times per week: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 26                                             Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 27                                Dichotomous: Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 28                                Dichotomous: Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 29                                             Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 30                                            Times per week: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 31                                                 Times per week: Other beverage 1 (list in box below if applicable)
    ## 32                                                  Times per day: Other beverage 1 (list in box below if applicable)
    ## 33                                     Dichotomous: Times per day: Other beverage 1 (list in box below if applicable)
    ## 34                                     Dichotomous: Times per day: Other beverage 1 (list in box below if applicable)
    ## 35                                                  Times per day: Other beverage 1 (list in box below if applicable)
    ## 36                                                 Times per week: Other beverage 1 (list in box below if applicable)
    ## 37                                                 Times per week: Other beverage 2 (list in box below if applicable)
    ## 38                                                  Times per day: Other beverage 2 (list in box below if applicable)
    ## 39                                     Dichotomous: Times per day: Other beverage 2 (list in box below if applicable)
    ## 40                                     Dichotomous: Times per day: Other beverage 2 (list in box below if applicable)
    ## 41                                                  Times per day: Other beverage 2 (list in box below if applicable)
    ## 42                                                 Times per week: Other beverage 2 (list in box below if applicable)
    ## 43                                                 Times per week: Other beverage 3 (list in box below if applicable)
    ## 44                                                  Times per day: Other beverage 3 (list in box below if applicable)
    ## 45                                     Dichotomous: Times per day: Other beverage 3 (list in box below if applicable)
    ## 46                                     Dichotomous: Times per day: Other beverage 3 (list in box below if applicable)
    ## 47                                                  Times per day: Other beverage 3 (list in box below if applicable)
    ## 48                                                 Times per week: Other beverage 3 (list in box below if applicable)
    ## 49                                                                                   Times per week: 100% fruit juice
    ## 50                                                                                    Times per day: 100% fruit juice
    ## 51                                                                       Dichotomous: Times per day: 100% fruit juice
    ## 52                                                                       Dichotomous: Times per day: 100% fruit juice
    ## 53                                                                                    Times per day: 100% fruit juice
    ## 54                                                                                   Times per week: 100% fruit juice
    ## 55              Times per week: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 56               Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 57  Dichotomous: Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 58  Dichotomous: Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 59               Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 60              Times per week: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 61                                                                     Times per week: 100% Vegetable Juice (V8, etc)
    ## 62                                                                      Times per day: 100% Vegetable Juice (V8, etc)
    ## 63                                                         Dichotomous: Times per day: 100% Vegetable Juice (V8, etc)
    ## 64                                                         Dichotomous: Times per day: 100% Vegetable Juice (V8, etc)
    ## 65                                                                      Times per day: 100% Vegetable Juice (V8, etc)
    ## 66                                                                     Times per week: 100% Vegetable Juice (V8, etc)
    ## 67                                                                                         Times per week: Whole milk
    ## 68                                                                                          Times per day: Whole milk
    ## 69                                                                             Dichotomous: Times per day: Whole milk
    ## 70                                                                             Dichotomous: Times per day: Whole milk
    ## 71                                                                                          Times per day: Whole milk
    ## 72                                                                                         Times per week: Whole milk
    ## 73                                                                              Times per week: Reduced fat milk (2%)
    ## 74                                                                               Times per day: Reduced fat milk (2%)
    ## 75                                                                  Dichotomous: Times per day: Reduced fat milk (2%)
    ## 76                                                                  Dichotomous: Times per day: Reduced fat milk (2%)
    ## 77                                                                               Times per day: Reduced fat milk (2%)
    ## 78                                                                              Times per week: Reduced fat milk (2%)
    ## 79                                              Times per week: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 80                                               Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 81                                  Dichotomous: Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 82                                  Dichotomous: Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 83                                               Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 84                                              Times per week: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 85                                                                               Times per week: Soft drinks, regular
    ## 86                                                                                Times per day: Soft drinks, regular
    ## 87                                                                   Dichotomous: Times per day: Soft drinks, regular
    ## 88                                                                   Dichotomous: Times per day: Soft drinks, regular
    ## 89                                                                                Times per day: Soft drinks, regular
    ## 90                                                                               Times per week: Soft drinks, regular
    ## 91                                Times per week: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 92                                 Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 93                    Dichotomous: Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 94                    Dichotomous: Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 95                                 Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 96                                Times per week: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 97                                                                                     Category of amount (oz): Water
    ## 98                                                                                                Amount of oz: Water
    ## 99                                                                                                Amount of oz: Water
    ## 100                                                                                    Category of amount (oz): Water
    ## 101                                                                            Category of amount (oz): Sweetened tea
    ## 102                                                                                       Amount of oz: Sweetened tea
    ## 103                                                                                       Amount of oz: Sweetened tea
    ## 104                                                                            Category of amount (oz): Sweetened tea
    ## 105                             Category of amount (oz): Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 106                                        Amount of oz: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 107                                        Amount of oz: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 108                             Category of amount (oz): Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 109              Category of amount (oz): Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 110                         Amount of oz: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 111                         Amount of oz: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 112              Category of amount (oz): Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 113                                  Category of amount (oz): Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 114                                             Amount of oz: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 115                                             Amount of oz: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 116                                  Category of amount (oz): Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 117                                       Category of amount (oz): Other beverage 1 (list in box below if applicable)
    ## 118                                                  Amount of oz: Other beverage 1 (list in box below if applicable)
    ## 119                                                  Amount of oz: Other beverage 1 (list in box below if applicable)
    ## 120                                       Category of amount (oz): Other beverage 1 (list in box below if applicable)
    ## 121                                       Category of amount (oz): Other beverage 2 (list in box below if applicable)
    ## 122                                                  Amount of oz: Other beverage 2 (list in box below if applicable)
    ## 123                                                  Amount of oz: Other beverage 2 (list in box below if applicable)
    ## 124                                       Category of amount (oz): Other beverage 2 (list in box below if applicable)
    ## 125                                       Category of amount (oz): Other beverage 3 (list in box below if applicable)
    ## 126                                                  Amount of oz: Other beverage 3 (list in box below if applicable)
    ## 127                                                  Amount of oz: Other beverage 3 (list in box below if applicable)
    ## 128                                       Category of amount (oz): Other beverage 3 (list in box below if applicable)
    ## 129                                                                         Category of amount (oz): 100% fruit juice
    ## 130                                                                                    Amount of oz: 100% fruit juice
    ## 131                                                                                    Amount of oz: 100% fruit juice
    ## 132                                                                         Category of amount (oz): 100% fruit juice
    ## 133    Category of amount (oz): Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 134               Amount of oz: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 135               Amount of oz: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 136    Category of amount (oz): Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 137                                                           Category of amount (oz): 100% Vegetable Juice (V8, etc)
    ## 138                                                                      Amount of oz: 100% Vegetable Juice (V8, etc)
    ## 139                                                                      Amount of oz: 100% Vegetable Juice (V8, etc)
    ## 140                                                           Category of amount (oz): 100% Vegetable Juice (V8, etc)
    ## 141                                                                               Category of amount (oz): Whole milk
    ## 142                                                                                          Amount of oz: Whole milk
    ## 143                                                                                          Amount of oz: Whole milk
    ## 144                                                                               Category of amount (oz): Whole milk
    ## 145                                                                    Category of amount (oz): Reduced fat milk (2%)
    ## 146                                                                               Amount of oz: Reduced fat milk (2%)
    ## 147                                                                               Amount of oz: Reduced fat milk (2%)
    ## 148                                                                    Category of amount (oz): Reduced fat milk (2%)
    ## 149                                    Category of amount (oz): Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 150                                               Amount of oz: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 151                                               Amount of oz: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 152                                    Category of amount (oz): Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 153                                                                     Category of amount (oz): Soft drinks, regular
    ## 154                                                                                Amount of oz: Soft drinks, regular
    ## 155                                                                                Amount of oz: Soft drinks, regular
    ## 156                                                                     Category of amount (oz): Soft drinks, regular
    ## 157                      Category of amount (oz): Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 158                                 Amount of oz: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 159                                 Amount of oz: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 160                      Category of amount (oz): Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 161                                                                                           Average daily oz: Water
    ## 162                                                                                           Average daily oz: Water
    ## 163                                                                                   Average daily oz: Sweetened tea
    ## 164                                                                                   Average daily oz: Sweetened tea
    ## 165                                    Average daily oz: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 166                                    Average daily oz: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 167                     Average daily oz: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 168                     Average daily oz: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 169                                         Average daily oz: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 170                                         Average daily oz: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 171                                              Average daily oz: Other beverage 1 (list in box below if applicable)
    ## 172                                              Average daily oz: Other beverage 1 (list in box below if applicable)
    ## 173                                              Average daily oz: Other beverage 2 (list in box below if applicable)
    ## 174                                              Average daily oz: Other beverage 2 (list in box below if applicable)
    ## 175                                              Average daily oz: Other beverage 3 (list in box below if applicable)
    ## 176                                              Average daily oz: Other beverage 3 (list in box below if applicable)
    ## 177                                                                                Average daily oz: 100% fruit juice
    ## 178                                                                                Average daily oz: 100% fruit juice
    ## 179           Average daily oz: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 180           Average daily oz: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 181                                                                  Average daily oz: 100% Vegetable Juice (V8, etc)
    ## 182                                                                  Average daily oz: 100% Vegetable Juice (V8, etc)
    ## 183                                                                                      Average daily oz: Whole milk
    ## 184                                                                                      Average daily oz: Whole milk
    ## 185                                                                           Average daily oz: Reduced fat milk (2%)
    ## 186                                                                           Average daily oz: Reduced fat milk (2%)
    ## 187                                           Average daily oz: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 188                                           Average daily oz: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 189                                                                            Average daily oz: Soft drinks, regular
    ## 190                                                                            Average daily oz: Soft drinks, regular
    ## 191                             Average daily oz: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 192                             Average daily oz: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 193                                                                                     CHILD Average daily oz of SSB
    ## 194                                                                                     CHILD Average daily oz of SSB
    ## 195                                                                                                          Child ID
    ## 196                                                                                                      Household ID
    ## 197                                                                                             Times per week: Water
    ## 198                                                                                              Times per day: Water
    ## 199                                                                                 Dichotomous: Times per day: Water
    ## 200                                                                                 Dichotomous: Times per day: Water
    ## 201                                                                                              Times per day: Water
    ## 202                                                                                             Times per week: Water
    ## 203                                                                                     Times per week: Sweetened tea
    ## 204                                                                                      Times per day: Sweetened tea
    ## 205                                                                         Dichotomous: Times per day: Sweetened tea
    ## 206                                                                         Dichotomous: Times per day: Sweetened tea
    ## 207                                                                                      Times per day: Sweetened tea
    ## 208                                                                                     Times per week: Sweetened tea
    ## 209                                      Times per week: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 210                                       Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 211                          Dichotomous: Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 212                          Dichotomous: Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 213                                       Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 214                                      Times per week: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 215                       Times per week: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 216                        Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 217           Dichotomous: Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 218           Dichotomous: Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 219                        Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 220                       Times per week: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 221                                           Times per week: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 222                                            Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 223                               Dichotomous: Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 224                               Dichotomous: Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 225                                            Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 226                                           Times per week: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 227                                                Times per week: Other beverage 1 (list in box below if applicable)
    ## 228                                                 Times per day: Other beverage 1 (list in box below if applicable)
    ## 229                                    Dichotomous: Times per day: Other beverage 1 (list in box below if applicable)
    ## 230                                    Dichotomous: Times per day: Other beverage 1 (list in box below if applicable)
    ## 231                                                 Times per day: Other beverage 1 (list in box below if applicable)
    ## 232                                                Times per week: Other beverage 1 (list in box below if applicable)
    ## 233                                                Times per week: Other beverage 2 (list in box below if applicable)
    ## 234                                                 Times per day: Other beverage 2 (list in box below if applicable)
    ## 235                                    Dichotomous: Times per day: Other beverage 2 (list in box below if applicable)
    ## 236                                    Dichotomous: Times per day: Other beverage 2 (list in box below if applicable)
    ## 237                                                 Times per day: Other beverage 2 (list in box below if applicable)
    ## 238                                                Times per week: Other beverage 2 (list in box below if applicable)
    ## 239                                                Times per week: Other beverage 3 (list in box below if applicable)
    ## 240                                                 Times per day: Other beverage 3 (list in box below if applicable)
    ## 241                                    Dichotomous: Times per day: Other beverage 3 (list in box below if applicable)
    ## 242                                    Dichotomous: Times per day: Other beverage 3 (list in box below if applicable)
    ## 243                                                 Times per day: Other beverage 3 (list in box below if applicable)
    ## 244                                                Times per week: Other beverage 3 (list in box below if applicable)
    ## 245                                                                                  Times per week: 100% fruit juice
    ## 246                                                                                   Times per day: 100% fruit juice
    ## 247                                                                      Dichotomous: Times per day: 100% fruit juice
    ## 248                                                                      Dichotomous: Times per day: 100% fruit juice
    ## 249                                                                                   Times per day: 100% fruit juice
    ## 250                                                                                  Times per week: 100% fruit juice
    ## 251             Times per week: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 252              Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 253 Dichotomous: Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 254 Dichotomous: Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 255              Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 256             Times per week: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 257                                                                    Times per week: 100% Vegetable Juice (V8, etc)
    ## 258                                                                     Times per day: 100% Vegetable Juice (V8, etc)
    ## 259                                                        Dichotomous: Times per day: 100% Vegetable Juice (V8, etc)
    ## 260                                                        Dichotomous: Times per day: 100% Vegetable Juice (V8, etc)
    ## 261                                                                     Times per day: 100% Vegetable Juice (V8, etc)
    ## 262                                                                    Times per week: 100% Vegetable Juice (V8, etc)
    ## 263                                                                                        Times per week: Whole milk
    ## 264                                                                                         Times per day: Whole milk
    ## 265                                                                            Dichotomous: Times per day: Whole milk
    ## 266                                                                            Dichotomous: Times per day: Whole milk
    ## 267                                                                                         Times per day: Whole milk
    ## 268                                                                                        Times per week: Whole milk
    ## 269                                                                             Times per week: Reduced fat milk (2%)
    ## 270                                                                              Times per day: Reduced fat milk (2%)
    ## 271                                                                 Dichotomous: Times per day: Reduced fat milk (2%)
    ## 272                                                                 Dichotomous: Times per day: Reduced fat milk (2%)
    ## 273                                                                              Times per day: Reduced fat milk (2%)
    ## 274                                                                             Times per week: Reduced fat milk (2%)
    ## 275                                             Times per week: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 276                                              Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 277                                 Dichotomous: Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 278                                 Dichotomous: Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 279                                              Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 280                                             Times per week: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 281                                                                              Times per week: Soft drinks, regular
    ## 282                                                                               Times per day: Soft drinks, regular
    ## 283                                                                  Dichotomous: Times per day: Soft drinks, regular
    ## 284                                                                  Dichotomous: Times per day: Soft drinks, regular
    ## 285                                                                               Times per day: Soft drinks, regular
    ## 286                                                                              Times per week: Soft drinks, regular
    ## 287                               Times per week: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 288                                Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 289                   Dichotomous: Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 290                   Dichotomous: Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 291                                Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 292                               Times per week: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 293                                                                                    Category of amount (oz): Water
    ## 294                                                                                               Amount of oz: Water
    ## 295                                                                                               Amount of oz: Water
    ## 296                                                                                    Category of amount (oz): Water
    ## 297                                                                            Category of amount (oz): Sweetened tea
    ## 298                                                                                       Amount of oz: Sweetened tea
    ## 299                                                                                       Amount of oz: Sweetened tea
    ## 300                                                                            Category of amount (oz): Sweetened tea
    ## 301                             Category of amount (oz): Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 302                                        Amount of oz: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 303                                        Amount of oz: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 304                             Category of amount (oz): Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 305              Category of amount (oz): Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 306                         Amount of oz: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 307                         Amount of oz: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 308              Category of amount (oz): Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 309                                  Category of amount (oz): Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 310                                             Amount of oz: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 311                                             Amount of oz: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 312                                  Category of amount (oz): Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 313                                       Category of amount (oz): Other beverage 1 (list in box below if applicable)
    ## 314                                                  Amount of oz: Other beverage 1 (list in box below if applicable)
    ## 315                                                  Amount of oz: Other beverage 1 (list in box below if applicable)
    ## 316                                       Category of amount (oz): Other beverage 1 (list in box below if applicable)
    ## 317                                       Category of amount (oz): Other beverage 2 (list in box below if applicable)
    ## 318                                                  Amount of oz: Other beverage 2 (list in box below if applicable)
    ## 319                                                  Amount of oz: Other beverage 2 (list in box below if applicable)
    ## 320                                       Category of amount (oz): Other beverage 2 (list in box below if applicable)
    ## 321                                       Category of amount (oz): Other beverage 3 (list in box below if applicable)
    ## 322                                                  Amount of oz: Other beverage 3 (list in box below if applicable)
    ## 323                                                  Amount of oz: Other beverage 3 (list in box below if applicable)
    ## 324                                       Category of amount (oz): Other beverage 3 (list in box below if applicable)
    ## 325                                                                         Category of amount (oz): 100% fruit juice
    ## 326                                                                                    Amount of oz: 100% fruit juice
    ## 327                                                                                    Amount of oz: 100% fruit juice
    ## 328                                                                         Category of amount (oz): 100% fruit juice
    ## 329    Category of amount (oz): Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 330               Amount of oz: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 331               Amount of oz: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 332    Category of amount (oz): Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 333                                                           Category of amount (oz): 100% Vegetable Juice (V8, etc)
    ## 334                                                                      Amount of oz: 100% Vegetable Juice (V8, etc)
    ## 335                                                                      Amount of oz: 100% Vegetable Juice (V8, etc)
    ## 336                                                           Category of amount (oz): 100% Vegetable Juice (V8, etc)
    ## 337                                                                               Category of amount (oz): Whole milk
    ## 338                                                                                          Amount of oz: Whole milk
    ## 339                                                                                          Amount of oz: Whole milk
    ## 340                                                                               Category of amount (oz): Whole milk
    ## 341                                                                    Category of amount (oz): Reduced fat milk (2%)
    ## 342                                                                               Amount of oz: Reduced fat milk (2%)
    ## 343                                                                               Amount of oz: Reduced fat milk (2%)
    ## 344                                                                    Category of amount (oz): Reduced fat milk (2%)
    ## 345                                    Category of amount (oz): Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 346                                               Amount of oz: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 347                                               Amount of oz: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 348                                    Category of amount (oz): Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 349                                                                     Category of amount (oz): Soft drinks, regular
    ## 350                                                                                Amount of oz: Soft drinks, regular
    ## 351                                                                                Amount of oz: Soft drinks, regular
    ## 352                                                                     Category of amount (oz): Soft drinks, regular
    ## 353                      Category of amount (oz): Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 354                                 Amount of oz: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 355                                 Amount of oz: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 356                      Category of amount (oz): Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 357                                                                                           Average daily oz: Water
    ## 358                                                                                           Average daily oz: Water
    ## 359                                                                                   Average daily oz: Sweetened tea
    ## 360                                                                                   Average daily oz: Sweetened tea
    ## 361                                    Average daily oz: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 362                                    Average daily oz: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 363                     Average daily oz: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 364                     Average daily oz: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 365                                         Average daily oz: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 366                                         Average daily oz: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 367                                              Average daily oz: Other beverage 1 (list in box below if applicable)
    ## 368                                              Average daily oz: Other beverage 1 (list in box below if applicable)
    ## 369                                              Average daily oz: Other beverage 2 (list in box below if applicable)
    ## 370                                              Average daily oz: Other beverage 2 (list in box below if applicable)
    ## 371                                              Average daily oz: Other beverage 3 (list in box below if applicable)
    ## 372                                              Average daily oz: Other beverage 3 (list in box below if applicable)
    ## 373                                                                                Average daily oz: 100% fruit juice
    ## 374                                                                                Average daily oz: 100% fruit juice
    ## 375           Average daily oz: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 376           Average daily oz: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 377                                                                  Average daily oz: 100% Vegetable Juice (V8, etc)
    ## 378                                                                  Average daily oz: 100% Vegetable Juice (V8, etc)
    ## 379                                                                                      Average daily oz: Whole milk
    ## 380                                                                                      Average daily oz: Whole milk
    ## 381                                                                           Average daily oz: Reduced fat milk (2%)
    ## 382                                                                           Average daily oz: Reduced fat milk (2%)
    ## 383                                           Average daily oz: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 384                                           Average daily oz: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 385                                                                            Average daily oz: Soft drinks, regular
    ## 386                                                                            Average daily oz: Soft drinks, regular
    ## 387                             Average daily oz: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 388                             Average daily oz: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 389                                                                                    PARENT Average daily oz of SSB
    ## 390                                                                                    PARENT Average daily oz of SSB

``` r
#we have to arrange the full df by the name of each varable, ordering the dataframe in alphabetical order
#but we want to keep the order of the variables in tact from our original, so we'll save that as a vector
bevq.df_var_order<- names(bevq.df)


bevq.df<- bevq.df %>%
  select(order(colnames(.)))
names(bevq.df)
```

    ##   [1] "child_bev_1"           "child_bev_1_pd"       
    ##   [3] "child_bev_1_pd_bi"     "child_bev_1_pd_bi_t2" 
    ##   [5] "child_bev_1_pd_t2"     "child_bev_1_t2"       
    ##   [7] "child_bev_10"          "child_bev_10_pd"      
    ##   [9] "child_bev_10_pd_bi"    "child_bev_10_pd_bi_t2"
    ##  [11] "child_bev_10_pd_t2"    "child_bev_10_t2"      
    ##  [13] "child_bev_11"          "child_bev_11_pd"      
    ##  [15] "child_bev_11_pd_bi"    "child_bev_11_pd_bi_t2"
    ##  [17] "child_bev_11_pd_t2"    "child_bev_11_t2"      
    ##  [19] "child_bev_12"          "child_bev_12_pd"      
    ##  [21] "child_bev_12_pd_bi"    "child_bev_12_pd_bi_t2"
    ##  [23] "child_bev_12_pd_t2"    "child_bev_12_t2"      
    ##  [25] "child_bev_13"          "child_bev_13_pd"      
    ##  [27] "child_bev_13_pd_bi"    "child_bev_13_pd_bi_t2"
    ##  [29] "child_bev_13_pd_t2"    "child_bev_13_t2"      
    ##  [31] "child_bev_14"          "child_bev_14_pd"      
    ##  [33] "child_bev_14_pd_bi"    "child_bev_14_pd_bi_t2"
    ##  [35] "child_bev_14_pd_t2"    "child_bev_14_t2"      
    ##  [37] "child_bev_15"          "child_bev_15_pd"      
    ##  [39] "child_bev_15_pd_bi"    "child_bev_15_pd_bi_t2"
    ##  [41] "child_bev_15_pd_t2"    "child_bev_15_t2"      
    ##  [43] "child_bev_16"          "child_bev_16_pd"      
    ##  [45] "child_bev_16_pd_bi"    "child_bev_16_pd_bi_t2"
    ##  [47] "child_bev_16_pd_t2"    "child_bev_16_t2"      
    ##  [49] "child_bev_2"           "child_bev_2_pd"       
    ##  [51] "child_bev_2_pd_bi"     "child_bev_2_pd_bi_t2" 
    ##  [53] "child_bev_2_pd_t2"     "child_bev_2_t2"       
    ##  [55] "child_bev_3"           "child_bev_3_pd"       
    ##  [57] "child_bev_3_pd_bi"     "child_bev_3_pd_bi_t2" 
    ##  [59] "child_bev_3_pd_t2"     "child_bev_3_t2"       
    ##  [61] "child_bev_4"           "child_bev_4_pd"       
    ##  [63] "child_bev_4_pd_bi"     "child_bev_4_pd_bi_t2" 
    ##  [65] "child_bev_4_pd_t2"     "child_bev_4_t2"       
    ##  [67] "child_bev_5"           "child_bev_5_pd"       
    ##  [69] "child_bev_5_pd_bi"     "child_bev_5_pd_bi_t2" 
    ##  [71] "child_bev_5_pd_t2"     "child_bev_5_t2"       
    ##  [73] "child_bev_6"           "child_bev_6_pd"       
    ##  [75] "child_bev_6_pd_bi"     "child_bev_6_pd_bi_t2" 
    ##  [77] "child_bev_6_pd_t2"     "child_bev_6_t2"       
    ##  [79] "child_bev_7"           "child_bev_7_pd"       
    ##  [81] "child_bev_7_pd_bi"     "child_bev_7_pd_bi_t2" 
    ##  [83] "child_bev_7_pd_t2"     "child_bev_7_t2"       
    ##  [85] "child_bev_8"           "child_bev_8_pd"       
    ##  [87] "child_bev_8_pd_bi"     "child_bev_8_pd_bi_t2" 
    ##  [89] "child_bev_8_pd_t2"     "child_bev_8_t2"       
    ##  [91] "child_bev_9"           "child_bev_9_pd"       
    ##  [93] "child_bev_9_pd_bi"     "child_bev_9_pd_bi_t2" 
    ##  [95] "child_bev_9_pd_t2"     "child_bev_9_t2"       
    ##  [97] "child_bevamt_1"        "child_bevamt_1_oz"    
    ##  [99] "child_bevamt_1_oz_t2"  "child_bevamt_1_t2"    
    ## [101] "child_bevamt_10"       "child_bevamt_10_oz"   
    ## [103] "child_bevamt_10_oz_t2" "child_bevamt_10_t2"   
    ## [105] "child_bevamt_11"       "child_bevamt_11_oz"   
    ## [107] "child_bevamt_11_oz_t2" "child_bevamt_11_t2"   
    ## [109] "child_bevamt_12"       "child_bevamt_12_oz"   
    ## [111] "child_bevamt_12_oz_t2" "child_bevamt_12_t2"   
    ## [113] "child_bevamt_13"       "child_bevamt_13_oz"   
    ## [115] "child_bevamt_13_oz_t2" "child_bevamt_13_t2"   
    ## [117] "child_bevamt_14"       "child_bevamt_14_oz"   
    ## [119] "child_bevamt_14_oz_t2" "child_bevamt_14_t2"   
    ## [121] "child_bevamt_15"       "child_bevamt_15_oz"   
    ## [123] "child_bevamt_15_oz_t2" "child_bevamt_15_t2"   
    ## [125] "child_bevamt_16"       "child_bevamt_16_oz"   
    ## [127] "child_bevamt_16_oz_t2" "child_bevamt_16_t2"   
    ## [129] "child_bevamt_2"        "child_bevamt_2_oz"    
    ## [131] "child_bevamt_2_oz_t2"  "child_bevamt_2_t2"    
    ## [133] "child_bevamt_3"        "child_bevamt_3_oz"    
    ## [135] "child_bevamt_3_oz_t2"  "child_bevamt_3_t2"    
    ## [137] "child_bevamt_4"        "child_bevamt_4_oz"    
    ## [139] "child_bevamt_4_oz_t2"  "child_bevamt_4_t2"    
    ## [141] "child_bevamt_5"        "child_bevamt_5_oz"    
    ## [143] "child_bevamt_5_oz_t2"  "child_bevamt_5_t2"    
    ## [145] "child_bevamt_6"        "child_bevamt_6_oz"    
    ## [147] "child_bevamt_6_oz_t2"  "child_bevamt_6_t2"    
    ## [149] "child_bevamt_7"        "child_bevamt_7_oz"    
    ## [151] "child_bevamt_7_oz_t2"  "child_bevamt_7_t2"    
    ## [153] "child_bevamt_8"        "child_bevamt_8_oz"    
    ## [155] "child_bevamt_8_oz_t2"  "child_bevamt_8_t2"    
    ## [157] "child_bevamt_9"        "child_bevamt_9_oz"    
    ## [159] "child_bevamt_9_oz_t2"  "child_bevamt_9_t2"    
    ## [161] "child_bevtotal_1"      "child_bevtotal_1_t2"  
    ## [163] "child_bevtotal_10"     "child_bevtotal_10_t2" 
    ## [165] "child_bevtotal_11"     "child_bevtotal_11_t2" 
    ## [167] "child_bevtotal_12"     "child_bevtotal_12_t2" 
    ## [169] "child_bevtotal_13"     "child_bevtotal_13_t2" 
    ## [171] "child_bevtotal_14"     "child_bevtotal_14_t2" 
    ## [173] "child_bevtotal_15"     "child_bevtotal_15_t2" 
    ## [175] "child_bevtotal_16"     "child_bevtotal_16_t2" 
    ## [177] "child_bevtotal_2"      "child_bevtotal_2_t2"  
    ## [179] "child_bevtotal_3"      "child_bevtotal_3_t2"  
    ## [181] "child_bevtotal_4"      "child_bevtotal_4_t2"  
    ## [183] "child_bevtotal_5"      "child_bevtotal_5_t2"  
    ## [185] "child_bevtotal_6"      "child_bevtotal_6_t2"  
    ## [187] "child_bevtotal_7"      "child_bevtotal_7_t2"  
    ## [189] "child_bevtotal_8"      "child_bevtotal_8_t2"  
    ## [191] "child_bevtotal_9"      "child_bevtotal_9_t2"  
    ## [193] "child_bevtotal_ssb"    "child_bevtotal_ssb_t2"
    ## [195] "childid"               "hhid"                 
    ## [197] "par_bev_1"             "par_bev_1_pd"         
    ## [199] "par_bev_1_pd_bi"       "par_bev_1_pd_bi_t2"   
    ## [201] "par_bev_1_pd_t2"       "par_bev_1_t2"         
    ## [203] "par_bev_10"            "par_bev_10_pd"        
    ## [205] "par_bev_10_pd_bi"      "par_bev_10_pd_bi_t2"  
    ## [207] "par_bev_10_pd_t2"      "par_bev_10_t2"        
    ## [209] "par_bev_11"            "par_bev_11_pd"        
    ## [211] "par_bev_11_pd_bi"      "par_bev_11_pd_bi_t2"  
    ## [213] "par_bev_11_pd_t2"      "par_bev_11_t2"        
    ## [215] "par_bev_12"            "par_bev_12_pd"        
    ## [217] "par_bev_12_pd_bi"      "par_bev_12_pd_bi_t2"  
    ## [219] "par_bev_12_pd_t2"      "par_bev_12_t2"        
    ## [221] "par_bev_13"            "par_bev_13_pd"        
    ## [223] "par_bev_13_pd_bi"      "par_bev_13_pd_bi_t2"  
    ## [225] "par_bev_13_pd_t2"      "par_bev_13_t2"        
    ## [227] "par_bev_14"            "par_bev_14_pd"        
    ## [229] "par_bev_14_pd_bi"      "par_bev_14_pd_bi_t2"  
    ## [231] "par_bev_14_pd_t2"      "par_bev_14_t2"        
    ## [233] "par_bev_15"            "par_bev_15_pd"        
    ## [235] "par_bev_15_pd_bi"      "par_bev_15_pd_bi_t2"  
    ## [237] "par_bev_15_pd_t2"      "par_bev_15_t2"        
    ## [239] "par_bev_16"            "par_bev_16_pd"        
    ## [241] "par_bev_16_pd_bi"      "par_bev_16_pd_bi_t2"  
    ## [243] "par_bev_16_pd_t2"      "par_bev_16_t2"        
    ## [245] "par_bev_2"             "par_bev_2_pd"         
    ## [247] "par_bev_2_pd_bi"       "par_bev_2_pd_bi_t2"   
    ## [249] "par_bev_2_pd_t2"       "par_bev_2_t2"         
    ## [251] "par_bev_3"             "par_bev_3_pd"         
    ## [253] "par_bev_3_pd_bi"       "par_bev_3_pd_bi_t2"   
    ## [255] "par_bev_3_pd_t2"       "par_bev_3_t2"         
    ## [257] "par_bev_4"             "par_bev_4_pd"         
    ## [259] "par_bev_4_pd_bi"       "par_bev_4_pd_bi_t2"   
    ## [261] "par_bev_4_pd_t2"       "par_bev_4_t2"         
    ## [263] "par_bev_5"             "par_bev_5_pd"         
    ## [265] "par_bev_5_pd_bi"       "par_bev_5_pd_bi_t2"   
    ## [267] "par_bev_5_pd_t2"       "par_bev_5_t2"         
    ## [269] "par_bev_6"             "par_bev_6_pd"         
    ## [271] "par_bev_6_pd_bi"       "par_bev_6_pd_bi_t2"   
    ## [273] "par_bev_6_pd_t2"       "par_bev_6_t2"         
    ## [275] "par_bev_7"             "par_bev_7_pd"         
    ## [277] "par_bev_7_pd_bi"       "par_bev_7_pd_bi_t2"   
    ## [279] "par_bev_7_pd_t2"       "par_bev_7_t2"         
    ## [281] "par_bev_8"             "par_bev_8_pd"         
    ## [283] "par_bev_8_pd_bi"       "par_bev_8_pd_bi_t2"   
    ## [285] "par_bev_8_pd_t2"       "par_bev_8_t2"         
    ## [287] "par_bev_9"             "par_bev_9_pd"         
    ## [289] "par_bev_9_pd_bi"       "par_bev_9_pd_bi_t2"   
    ## [291] "par_bev_9_pd_t2"       "par_bev_9_t2"         
    ## [293] "par_bevamt_1"          "par_bevamt_1_oz"      
    ## [295] "par_bevamt_1_oz_t2"    "par_bevamt_1_t2"      
    ## [297] "par_bevamt_10"         "par_bevamt_10_oz"     
    ## [299] "par_bevamt_10_oz_t2"   "par_bevamt_10_t2"     
    ## [301] "par_bevamt_11"         "par_bevamt_11_oz"     
    ## [303] "par_bevamt_11_oz_t2"   "par_bevamt_11_t2"     
    ## [305] "par_bevamt_12"         "par_bevamt_12_oz"     
    ## [307] "par_bevamt_12_oz_t2"   "par_bevamt_12_t2"     
    ## [309] "par_bevamt_13"         "par_bevamt_13_oz"     
    ## [311] "par_bevamt_13_oz_t2"   "par_bevamt_13_t2"     
    ## [313] "par_bevamt_14"         "par_bevamt_14_oz"     
    ## [315] "par_bevamt_14_oz_t2"   "par_bevamt_14_t2"     
    ## [317] "par_bevamt_15"         "par_bevamt_15_oz"     
    ## [319] "par_bevamt_15_oz_t2"   "par_bevamt_15_t2"     
    ## [321] "par_bevamt_16"         "par_bevamt_16_oz"     
    ## [323] "par_bevamt_16_oz_t2"   "par_bevamt_16_t2"     
    ## [325] "par_bevamt_2"          "par_bevamt_2_oz"      
    ## [327] "par_bevamt_2_oz_t2"    "par_bevamt_2_t2"      
    ## [329] "par_bevamt_3"          "par_bevamt_3_oz"      
    ## [331] "par_bevamt_3_oz_t2"    "par_bevamt_3_t2"      
    ## [333] "par_bevamt_4"          "par_bevamt_4_oz"      
    ## [335] "par_bevamt_4_oz_t2"    "par_bevamt_4_t2"      
    ## [337] "par_bevamt_5"          "par_bevamt_5_oz"      
    ## [339] "par_bevamt_5_oz_t2"    "par_bevamt_5_t2"      
    ## [341] "par_bevamt_6"          "par_bevamt_6_oz"      
    ## [343] "par_bevamt_6_oz_t2"    "par_bevamt_6_t2"      
    ## [345] "par_bevamt_7"          "par_bevamt_7_oz"      
    ## [347] "par_bevamt_7_oz_t2"    "par_bevamt_7_t2"      
    ## [349] "par_bevamt_8"          "par_bevamt_8_oz"      
    ## [351] "par_bevamt_8_oz_t2"    "par_bevamt_8_t2"      
    ## [353] "par_bevamt_9"          "par_bevamt_9_oz"      
    ## [355] "par_bevamt_9_oz_t2"    "par_bevamt_9_t2"      
    ## [357] "par_bevtotal_1"        "par_bevtotal_1_t2"    
    ## [359] "par_bevtotal_10"       "par_bevtotal_10_t2"   
    ## [361] "par_bevtotal_11"       "par_bevtotal_11_t2"   
    ## [363] "par_bevtotal_12"       "par_bevtotal_12_t2"   
    ## [365] "par_bevtotal_13"       "par_bevtotal_13_t2"   
    ## [367] "par_bevtotal_14"       "par_bevtotal_14_t2"   
    ## [369] "par_bevtotal_15"       "par_bevtotal_15_t2"   
    ## [371] "par_bevtotal_16"       "par_bevtotal_16_t2"   
    ## [373] "par_bevtotal_2"        "par_bevtotal_2_t2"    
    ## [375] "par_bevtotal_3"        "par_bevtotal_3_t2"    
    ## [377] "par_bevtotal_4"        "par_bevtotal_4_t2"    
    ## [379] "par_bevtotal_5"        "par_bevtotal_5_t2"    
    ## [381] "par_bevtotal_6"        "par_bevtotal_6_t2"    
    ## [383] "par_bevtotal_7"        "par_bevtotal_7_t2"    
    ## [385] "par_bevtotal_8"        "par_bevtotal_8_t2"    
    ## [387] "par_bevtotal_9"        "par_bevtotal_9_t2"    
    ## [389] "par_bevtotal_ssb"      "par_bevtotal_ssb_t2"

``` r
#here we assign the values from one column in the label dataset to each of the variables in our large set
#note, that in doing this, our variable are a bit out of sorts, so we can rearrange them, or at least, for me, I like my childid and hhid to be in the front of the data, and not worry about the rest
bevq.df <- set_label(bevq.df, label = var_label.df$var_label) %>%
  select(bevq.df_var_order) #here is where we use that vector we created to reorder back to original variable ordering

#Now you can write out your file for others to use if you need to
#here is to write out spss file and add date
write_sav(bevq.df, paste0(datDir, "Bevq ", Sys.Date(), ".sav"))
```
