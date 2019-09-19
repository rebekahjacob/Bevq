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

First, pull in the file that have all the labels (should have 2 columns,
one with var name and one with var label)

Then make sure your working with the same number of variables.

Here I use the ‘sjlabelled’ package which plays very nice with SPSS
files. But there are other ways to go about labeling your variables and
values.

``` r
var_label.df<- read.xlsx(paste0(datDir, "Beverage labels.xlsx"))

#number of variable names we pulled in
length(var_label.df$variable)
```

    ## [1] 390

``` r
#number of variable names in our current dataframe
length(bevq.df)
```

    ## [1] 390

Now we need to do a little sorting/arranging before we copy over the
labels to our current dataframe.

``` r
#we have to arrange the df with the labels by the column "variable" to be in alphabetical order
var_label.df<- var_label.df %>%
  arrange(variable)
head(var_label.df, 10) #view the first several
```

    ##                 variable                                 var_label
    ## 1            child_bev_1                     Times per week: Water
    ## 2         child_bev_1_pd                      Times per day: Water
    ## 3      child_bev_1_pd_bi         Dichotomous: Times per day: Water
    ## 4   child_bev_1_pd_bi_t2         Dichotomous: Times per day: Water
    ## 5      child_bev_1_pd_t2                      Times per day: Water
    ## 6         child_bev_1_t2                     Times per week: Water
    ## 7           child_bev_10             Times per week: Sweetened tea
    ## 8        child_bev_10_pd              Times per day: Sweetened tea
    ## 9     child_bev_10_pd_bi Dichotomous: Times per day: Sweetened tea
    ## 10 child_bev_10_pd_bi_t2 Dichotomous: Times per day: Sweetened tea

``` r
#we have to arrange the full df by the name of each varable, ordering the dataframe in alphabetical order
#but we want to keep the order of the variables in tact from our original, so we'll save that as a vector
bevq.df_var_order<- names(bevq.df)

#now we arrange alphabetical order- see results
bevq.df<- bevq.df %>%
  select(order(colnames(.)))
head(names(bevq.df), 10) #check the order matches the other df's order
```

    ##  [1] "child_bev_1"           "child_bev_1_pd"       
    ##  [3] "child_bev_1_pd_bi"     "child_bev_1_pd_bi_t2" 
    ##  [5] "child_bev_1_pd_t2"     "child_bev_1_t2"       
    ##  [7] "child_bev_10"          "child_bev_10_pd"      
    ##  [9] "child_bev_10_pd_bi"    "child_bev_10_pd_bi_t2"

Now that we’ve made sure they match, we go ahead and copy the labels to
our current dataframe from the vector of variable labels we pulled in
from the excel file.

``` r
#here we assign the values from one column in the label dataset to each of the variables in our large set
#note, that in doing this, our variable are a bit out of sorts, so we can rearrange them, or at least, for me, I like my childid and hhid to be in the front of the data, and not worry about the rest
bevq.df <- set_label(bevq.df, label = var_label.df$var_label) %>%
  select(bevq.df_var_order) #here is where we use the vector we created to reorder back to original variable ordering
```

## Write out file

Depending on how you need to use the data, you may need to write out the
file to stats program that others can use (spss, sas, csv, xlsx, etc).

I use the `haven` package to write SPSS files so that I can keep any
variable or value labels.

``` r
#Now you can write out your file for others to use if you need to
#here is to write out spss file and add date
write_sav(bevq.df, paste0(datDir, "Bevq ", Sys.Date(), ".sav"))
```
