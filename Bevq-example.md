Bevq Measures: Cleaning and Scoring
================

## Introduction

This is a working example of how we pull in, score and visualize the
bevq measures.

#### Data

We start with a dataframe (df) called bevq that has one childid per row.

``` r
#libraries
library(sjlabelled) #add variable labels
library(haven) #read and write sav files (spss)
```

    ## 
    ## Attaching package: 'haven'

    ## The following objects are masked from 'package:sjlabelled':
    ## 
    ##     as_factor, read_sas, read_spss, read_stata, write_sas,
    ##     zap_labels

``` r
library(tidyverse) #data management
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x forcats::as_factor() masks haven::as_factor(), sjlabelled::as_factor()
    ## x dplyr::as_label()    masks sjlabelled::as_label()
    ## x dplyr::filter()      masks stats::filter()
    ## x dplyr::lag()         masks stats::lag()
    ## x haven::read_sas()    masks sjlabelled::read_sas()
    ## x haven::read_spss()   masks sjlabelled::read_spss()
    ## x haven::read_stata()  masks sjlabelled::read_stata()
    ## x haven::write_sas()   masks sjlabelled::write_sas()
    ## x haven::zap_labels()  masks sjlabelled::zap_labels()

``` r
library(openxlsx) #read write Excel files
```

Read in data.

``` r
rj_url <- "https://raw.github.com/rebekahjacob/Bevq/master/bevq_data.xlsx"
data<- read.xlsx(rj_url)[-1, ] #getting rid of label row, we'll add labels later...

bevq.df<- data %>%
  select(hhid, childid, 
         par_bev_1:par_bev_18, 
         par_bevamt_1:par_bevamt_18,
         child_bev_1:child_bev_18, 
         child_bevamt_1:child_bevamt_18, -ends_with("TEXT"))
```

Quick view

``` r
bevq.df
```

    ##    hhid childid par_bev_1 par_bev_2 par_bev_3 par_bev_4 par_bev_5
    ## 2  1101     101         7         1         1         1         2
    ## 3  1102     102         7         1         1         1         2
    ## 4  1103     103         7         3         1         1         4
    ## 5  1104     104         7         1         1         1         1
    ## 6  1105     105         7         1         1         1         3
    ## 7  1106     106         7         1         1         1         1
    ## 8  1107     107         7         1         1         1         1
    ## 9  1108     108         7         1         1         1         1
    ## 10 1109     109         7         1         1         1         1
    ## 11 1110     110         7         1         1         1         1
    ## 12 1111     111         7         1         1         1         1
    ## 13 1112     112         7         1         1         1         1
    ## 14 1113     113         7         1         1         1         4
    ## 15 1114     114         7         1         1         1         1
    ## 16 1115     115         7         1         2         1         1
    ## 17 1116     116         6         3         1         1         3
    ## 18 1117     117         7         3         1         1         1
    ##    par_bev_6 par_bev_7 par_bev_8 par_bev_9 par_bev_10 par_bev_11
    ## 2          1         1         2         1          5          1
    ## 3          3         1         3         1          5          6
    ## 4          1         1         1         1          1          6
    ## 5          1         1         1         1          5          1
    ## 6          1         1         4         1          1          5
    ## 7          1         1         7         1          1          5
    ## 8          1         1         1         1          6          1
    ## 9          1         1         1         1          2          5
    ## 10         5         1         2         1          6          1
    ## 11         5         2         1         1          6          1
    ## 12         1         1         6         1          5          1
    ## 13         2         2         1         1          3          1
    ## 14         1         1         1         4          5          2
    ## 15         1         1         1         1          1          6
    ## 16         1         1         1         1          1          7
    ## 17         1         5         1         1          1          5
    ## 18         4         1         2         1          2          6
    ##    par_bev_12 par_bev_13 par_bev_14 par_bev_15 par_bev_16 par_bev_17
    ## 2           2          1          3          1          1          1
    ## 3           2          1          3          1          1          1
    ## 4           1          1          3          1          1          1
    ## 5           2          1          2          1          1          1
    ## 6           3          2          3          1          4          4
    ## 7           1          1          2          1          1          1
    ## 8           2          2          2          1          1          1
    ## 9           2          1          1          1          4          1
    ## 10          2          1          1          1          2          1
    ## 11          1          1          2          1          1          1
    ## 12          1          2          2          1          1          1
    ## 13          1          1          1          1          1          1
    ## 14          3          2          3          1          1          1
    ## 15          2          1          2          1          2          1
    ## 16          5          6          4          1          1          1
    ## 17          4          2          2          2          1          1
    ## 18          2          1          3          1          1          1
    ##    par_bev_18 par_bevamt_1 par_bevamt_2 par_bevamt_3 par_bevamt_4
    ## 2           1            3         <NA>         <NA>         <NA>
    ## 3           1            4         <NA>         <NA>         <NA>
    ## 4           1            2            2         <NA>         <NA>
    ## 5           1            5         <NA>         <NA>         <NA>
    ## 6           1            4         <NA>         <NA>         <NA>
    ## 7           1            4         <NA>         <NA>         <NA>
    ## 8           1            3         <NA>         <NA>         <NA>
    ## 9           1            3         <NA>         <NA>         <NA>
    ## 10          1         <NA>         <NA>         <NA>         <NA>
    ## 11          1            5         <NA>         <NA>         <NA>
    ## 12          1            3         <NA>         <NA>         <NA>
    ## 13          1            4         <NA>         <NA>         <NA>
    ## 14          1            4         <NA>         <NA>         <NA>
    ## 15          1            5         <NA>         <NA>         <NA>
    ## 16          1            5         <NA>            1         <NA>
    ## 17          1         <NA>         <NA>         <NA>         <NA>
    ## 18          1            4            1         <NA>         <NA>
    ##    par_bevamt_5 par_bevamt_6 par_bevamt_7 par_bevamt_8 par_bevamt_9
    ## 2             2         <NA>         <NA>            3         <NA>
    ## 3             2            2         <NA>            2         <NA>
    ## 4             1         <NA>         <NA>         <NA>         <NA>
    ## 5          <NA>         <NA>         <NA>         <NA>         <NA>
    ## 6             1         <NA>         <NA>            2         <NA>
    ## 7          <NA>         <NA>         <NA>            4         <NA>
    ## 8          <NA>         <NA>         <NA>         <NA>         <NA>
    ## 9          <NA>         <NA>         <NA>         <NA>         <NA>
    ## 10         <NA>            1         <NA>            2         <NA>
    ## 11         <NA>            2            3         <NA>         <NA>
    ## 12         <NA>         <NA>         <NA>            3         <NA>
    ## 13         <NA>            1            3         <NA>         <NA>
    ## 14            2         <NA>         <NA>         <NA>            3
    ## 15         <NA>         <NA>         <NA>         <NA>         <NA>
    ## 16         <NA>         <NA>         <NA>         <NA>         <NA>
    ## 17         <NA>         <NA>         <NA>         <NA>         <NA>
    ## 18         <NA>            3         <NA>            2         <NA>
    ##    par_bevamt_10 par_bevamt_11 par_bevamt_12 par_bevamt_13 par_bevamt_14
    ## 2              3          <NA>             3          <NA>             2
    ## 3              4             4             4          <NA>             1
    ## 4           <NA>             1          <NA>          <NA>             2
    ## 5              3          <NA>             3          <NA>             1
    ## 6           <NA>             3             3             1             1
    ## 7           <NA>             4          <NA>          <NA>             2
    ## 8              2          <NA>             1             1             1
    ## 9              2             2             3          <NA>          <NA>
    ## 10             2          <NA>             3          <NA>          <NA>
    ## 11             3          <NA>          <NA>          <NA>             1
    ## 12             3          <NA>          <NA>             1             1
    ## 13             2          <NA>          <NA>          <NA>          <NA>
    ## 14             3             3             3             1             1
    ## 15          <NA>             3             2          <NA>             2
    ## 16          <NA>             4             2             1             1
    ## 17          <NA>          <NA>          <NA>          <NA>          <NA>
    ## 18             2             4             3          <NA>             2
    ##    par_bevamt_15 par_bevamt_16 par_bevamt_17 par_bevamt_18 child_bev_1
    ## 2           <NA>          <NA>          <NA>          <NA>           7
    ## 3           <NA>          <NA>          <NA>          <NA>        <NA>
    ## 4           <NA>          <NA>          <NA>          <NA>        <NA>
    ## 5           <NA>          <NA>          <NA>          <NA>        <NA>
    ## 6           <NA>             3             3             1        <NA>
    ## 7           <NA>          <NA>          <NA>          <NA>           7
    ## 8           <NA>             1             1             1        <NA>
    ## 9           <NA>             1          <NA>          <NA>           7
    ## 10          <NA>             2          <NA>          <NA>           7
    ## 11          <NA>          <NA>          <NA>          <NA>        <NA>
    ## 12          <NA>          <NA>          <NA>          <NA>        <NA>
    ## 13          <NA>             1          <NA>          <NA>        <NA>
    ## 14          <NA>             1             1             1        <NA>
    ## 15          <NA>             2          <NA>          <NA>        <NA>
    ## 16          <NA>          <NA>          <NA>          <NA>        <NA>
    ## 17          <NA>          <NA>          <NA>          <NA>        <NA>
    ## 18          <NA>          <NA>          <NA>          <NA>        <NA>
    ##    child_bev_2 child_bev_3 child_bev_4 child_bev_5 child_bev_6 child_bev_7
    ## 2            1           2           1           5           1           1
    ## 3         <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 4         <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 5         <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 6         <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 7            1           1           5           5           1           1
    ## 8         <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 9            2           1           1           5           1           1
    ## 10           1           1           1           1           5           2
    ## 11        <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 12        <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 13        <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 14        <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 15        <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 16        <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 17        <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ## 18        <NA>        <NA>        <NA>        <NA>        <NA>        <NA>
    ##    child_bev_8 child_bev_9 child_bev_10 child_bev_11 child_bev_12
    ## 2            1           1            1            1            1
    ## 3         <NA>        <NA>         <NA>         <NA>         <NA>
    ## 4         <NA>        <NA>         <NA>         <NA>         <NA>
    ## 5         <NA>        <NA>         <NA>         <NA>         <NA>
    ## 6         <NA>        <NA>         <NA>         <NA>         <NA>
    ## 7            1           1            1            1            1
    ## 8         <NA>        <NA>         <NA>         <NA>         <NA>
    ## 9            1           1            1            1            1
    ## 10           1           1            1            1            1
    ## 11        <NA>        <NA>         <NA>         <NA>         <NA>
    ## 12        <NA>        <NA>         <NA>         <NA>         <NA>
    ## 13        <NA>        <NA>         <NA>         <NA>         <NA>
    ## 14        <NA>        <NA>         <NA>         <NA>         <NA>
    ## 15        <NA>        <NA>         <NA>         <NA>         <NA>
    ## 16        <NA>        <NA>         <NA>         <NA>         <NA>
    ## 17        <NA>        <NA>         <NA>         <NA>         <NA>
    ## 18        <NA>        <NA>         <NA>         <NA>         <NA>
    ##    child_bev_13 child_bev_14 child_bev_15 child_bev_16 child_bev_17
    ## 2             1            1            2            1            1
    ## 3          <NA>         <NA>         <NA>         <NA>         <NA>
    ## 4          <NA>         <NA>         <NA>         <NA>         <NA>
    ## 5          <NA>         <NA>         <NA>         <NA>         <NA>
    ## 6          <NA>         <NA>         <NA>         <NA>         <NA>
    ## 7             1            1            1            1            1
    ## 8          <NA>         <NA>         <NA>         <NA>         <NA>
    ## 9             1            1            1            1            1
    ## 10            1            1            1            2            1
    ## 11         <NA>         <NA>         <NA>         <NA>         <NA>
    ## 12         <NA>         <NA>         <NA>         <NA>         <NA>
    ## 13         <NA>         <NA>         <NA>         <NA>         <NA>
    ## 14         <NA>         <NA>         <NA>         <NA>         <NA>
    ## 15         <NA>         <NA>         <NA>         <NA>         <NA>
    ## 16         <NA>         <NA>         <NA>         <NA>         <NA>
    ## 17         <NA>         <NA>         <NA>         <NA>         <NA>
    ## 18         <NA>         <NA>         <NA>         <NA>         <NA>
    ##    child_bev_18 child_bevamt_1 child_bevamt_2 child_bevamt_3
    ## 2             1              3           <NA>              4
    ## 3          <NA>           <NA>           <NA>           <NA>
    ## 4          <NA>           <NA>           <NA>           <NA>
    ## 5          <NA>           <NA>           <NA>           <NA>
    ## 6          <NA>           <NA>           <NA>           <NA>
    ## 7             1              2           <NA>           <NA>
    ## 8          <NA>           <NA>           <NA>           <NA>
    ## 9             1              1              1           <NA>
    ## 10            1              2           <NA>           <NA>
    ## 11         <NA>           <NA>           <NA>           <NA>
    ## 12         <NA>           <NA>           <NA>           <NA>
    ## 13         <NA>           <NA>           <NA>           <NA>
    ## 14         <NA>           <NA>           <NA>           <NA>
    ## 15         <NA>           <NA>           <NA>           <NA>
    ## 16         <NA>           <NA>           <NA>           <NA>
    ## 17         <NA>           <NA>           <NA>           <NA>
    ## 18         <NA>           <NA>           <NA>           <NA>
    ##    child_bevamt_4 child_bevamt_5 child_bevamt_6 child_bevamt_7
    ## 2            <NA>              2           <NA>           <NA>
    ## 3            <NA>           <NA>           <NA>           <NA>
    ## 4            <NA>           <NA>           <NA>           <NA>
    ## 5            <NA>           <NA>           <NA>           <NA>
    ## 6            <NA>           <NA>           <NA>           <NA>
    ## 7               2              2           <NA>           <NA>
    ## 8            <NA>           <NA>           <NA>           <NA>
    ## 9            <NA>              1           <NA>           <NA>
    ## 10           <NA>           <NA>              2              2
    ## 11           <NA>           <NA>           <NA>           <NA>
    ## 12           <NA>           <NA>           <NA>           <NA>
    ## 13           <NA>           <NA>           <NA>           <NA>
    ## 14           <NA>           <NA>           <NA>           <NA>
    ## 15           <NA>           <NA>           <NA>           <NA>
    ## 16           <NA>           <NA>           <NA>           <NA>
    ## 17           <NA>           <NA>           <NA>           <NA>
    ## 18           <NA>           <NA>           <NA>           <NA>
    ##    child_bevamt_8 child_bevamt_9 child_bevamt_10 child_bevamt_11
    ## 2            <NA>           <NA>            <NA>            <NA>
    ## 3            <NA>           <NA>            <NA>            <NA>
    ## 4            <NA>           <NA>            <NA>            <NA>
    ## 5            <NA>           <NA>            <NA>            <NA>
    ## 6            <NA>           <NA>            <NA>            <NA>
    ## 7            <NA>           <NA>            <NA>            <NA>
    ## 8            <NA>           <NA>            <NA>            <NA>
    ## 9            <NA>           <NA>            <NA>            <NA>
    ## 10           <NA>           <NA>            <NA>            <NA>
    ## 11           <NA>           <NA>            <NA>            <NA>
    ## 12           <NA>           <NA>            <NA>            <NA>
    ## 13           <NA>           <NA>            <NA>            <NA>
    ## 14           <NA>           <NA>            <NA>            <NA>
    ## 15           <NA>           <NA>            <NA>            <NA>
    ## 16           <NA>           <NA>            <NA>            <NA>
    ## 17           <NA>           <NA>            <NA>            <NA>
    ## 18           <NA>           <NA>            <NA>            <NA>
    ##    child_bevamt_12 child_bevamt_13 child_bevamt_14 child_bevamt_15
    ## 2             <NA>            <NA>            <NA>               4
    ## 3             <NA>            <NA>            <NA>            <NA>
    ## 4             <NA>            <NA>            <NA>            <NA>
    ## 5             <NA>            <NA>            <NA>            <NA>
    ## 6             <NA>            <NA>            <NA>            <NA>
    ## 7             <NA>            <NA>            <NA>            <NA>
    ## 8             <NA>            <NA>            <NA>            <NA>
    ## 9             <NA>            <NA>            <NA>            <NA>
    ## 10            <NA>            <NA>            <NA>            <NA>
    ## 11            <NA>            <NA>            <NA>            <NA>
    ## 12            <NA>            <NA>            <NA>            <NA>
    ## 13            <NA>            <NA>            <NA>            <NA>
    ## 14            <NA>            <NA>            <NA>            <NA>
    ## 15            <NA>            <NA>            <NA>            <NA>
    ## 16            <NA>            <NA>            <NA>            <NA>
    ## 17            <NA>            <NA>            <NA>            <NA>
    ## 18            <NA>            <NA>            <NA>            <NA>
    ##    child_bevamt_16 child_bevamt_17 child_bevamt_18
    ## 2             <NA>            <NA>            <NA>
    ## 3             <NA>            <NA>            <NA>
    ## 4             <NA>            <NA>            <NA>
    ## 5             <NA>            <NA>            <NA>
    ## 6             <NA>            <NA>            <NA>
    ## 7             <NA>            <NA>            <NA>
    ## 8             <NA>            <NA>            <NA>
    ## 9             <NA>            <NA>            <NA>
    ## 10               2            <NA>            <NA>
    ## 11            <NA>            <NA>            <NA>
    ## 12            <NA>            <NA>            <NA>
    ## 13            <NA>            <NA>            <NA>
    ## 14            <NA>            <NA>            <NA>
    ## 15            <NA>            <NA>            <NA>
    ## 16            <NA>            <NA>            <NA>
    ## 17            <NA>            <NA>            <NA>
    ## 18            <NA>            <NA>            <NA>

#### Variables in the df

``` r
names(bevq.df)
```

    ##  [1] "hhid"            "childid"         "par_bev_1"      
    ##  [4] "par_bev_2"       "par_bev_3"       "par_bev_4"      
    ##  [7] "par_bev_5"       "par_bev_6"       "par_bev_7"      
    ## [10] "par_bev_8"       "par_bev_9"       "par_bev_10"     
    ## [13] "par_bev_11"      "par_bev_12"      "par_bev_13"     
    ## [16] "par_bev_14"      "par_bev_15"      "par_bev_16"     
    ## [19] "par_bev_17"      "par_bev_18"      "par_bevamt_1"   
    ## [22] "par_bevamt_2"    "par_bevamt_3"    "par_bevamt_4"   
    ## [25] "par_bevamt_5"    "par_bevamt_6"    "par_bevamt_7"   
    ## [28] "par_bevamt_8"    "par_bevamt_9"    "par_bevamt_10"  
    ## [31] "par_bevamt_11"   "par_bevamt_12"   "par_bevamt_13"  
    ## [34] "par_bevamt_14"   "par_bevamt_15"   "par_bevamt_16"  
    ## [37] "par_bevamt_17"   "par_bevamt_18"   "child_bev_1"    
    ## [40] "child_bev_2"     "child_bev_3"     "child_bev_4"    
    ## [43] "child_bev_5"     "child_bev_6"     "child_bev_7"    
    ## [46] "child_bev_8"     "child_bev_9"     "child_bev_10"   
    ## [49] "child_bev_11"    "child_bev_12"    "child_bev_13"   
    ## [52] "child_bev_14"    "child_bev_15"    "child_bev_16"   
    ## [55] "child_bev_17"    "child_bev_18"    "child_bevamt_1" 
    ## [58] "child_bevamt_2"  "child_bevamt_3"  "child_bevamt_4" 
    ## [61] "child_bevamt_5"  "child_bevamt_6"  "child_bevamt_7" 
    ## [64] "child_bevamt_8"  "child_bevamt_9"  "child_bevamt_10"
    ## [67] "child_bevamt_11" "child_bevamt_12" "child_bevamt_13"
    ## [70] "child_bevamt_14" "child_bevamt_15" "child_bevamt_16"
    ## [73] "child_bevamt_17" "child_bevamt_18"

## Calculate new vars

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

**New: par\_bev\_1\_pd through par\_bev\_18\_pd** **New:
child\_bev\_1\_pd through child\_bev\_18\_pd**

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
bev_vars<- names(bevq.df %>% #create list of variable names
  select(matches("bev_\\d"))) #vars that match regrex

bevq.df<- bevq.df %>%
  mutate_at(bev_vars, .funs = list(pd= bevperday_fun)) #all new recodes will have "pd" (perday) at the end of var name
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

**New: par\_bevamt\_1\_oz through par\_bevamt\_18\_oz**

**New: child\_bevamt\_1\_oz through child\_bevamt\_18\_oz**

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
bevamt_vars<- names(bevq.df %>% #create list of variable names
  select(matches("bevamt_\\d"))) #vars that match regrex

bevq.df<- bevq.df %>%
  mutate_at(bevamt_vars, .funs = list(oz= amtoz_fun)) #all new recodes will have "oz" (ounces) at the end
```

### Unit ounces per day (“bevtotal”)

Now we get the total ounces per day for each beverage by taking the
times per day and multiplying by ounces. But we keep zero as is since NA
X 0 would be NA (see function written).

Example:

par\_bevamt\_1\_pd X par\_bevamt\_1oz= par\_bevtotal\_1 (total ounces
per day consumed for beverage 1)

**New: par\_bevtotal\_1 through par\_bevtotal\_18**

**New: child\_bevtotal\_1 through child\_bevtotal\_18**

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
         par_bevtotal_16=pdoz_fun(par_bev_16_pd, par_bevamt_16_oz),
         par_bevtotal_17=pdoz_fun(par_bev_17_pd, par_bevamt_17_oz),
         par_bevtotal_18=pdoz_fun(par_bev_18_pd, par_bevamt_18_oz),
         child_bevtotal_1=pdoz_fun(child_bev_1_pd, child_bevamt_1_oz),
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
         child_bevtotal_16=pdoz_fun(child_bev_16_pd, child_bevamt_16_oz),
         child_bevtotal_17=pdoz_fun(child_bev_17_pd, child_bevamt_17_oz),
         child_bevtotal_18=pdoz_fun(child_bev_18_pd, child_bevamt_18_oz))
```

### Unit ounces per day of sugar-sweetened beverages (ssb)

To quantify average daily SSB consumption during the past month,
beverage categories containing added sugars were summed (sweetened juice
beverages and drinks (par\_bev\_3), regular sugar-sweetened carbonated
beverages (par\_bev\_7), sweet tea (par\_bev\_9), sweetened coffee
(par\_bev\_10), energy drinks (par\_bev\_15), and mixed alcoholic
drinks).

Note that since this is a sum, we keep cases where some beverages are
NA, unless all beverages are NA, then the total will also be NA.

For example, if bev\_3 has a total, but NA for other ssb variables, the
ssb\_total for that case will be the sum of bev\_3.

**New: par\_bevtotal\_ssb**

**New: child\_bevtotal\_ssb**

``` r
bevq.df<- bevq.df %>%
  mutate(par_bevtotal_ssb=ifelse(is.na(par_bevtotal_3) & is.na(par_bevtotal_7) & is.na(par_bevtotal_9) & is.na(par_bevtotal_10) & is.na(par_bevtotal_15), NA, rowSums(select(., par_bevtotal_3,par_bevtotal_7,par_bevtotal_9,par_bevtotal_11,par_bevtotal_15), na.rm=TRUE)),
         child_bevtotal_ssb=ifelse(is.na(child_bevtotal_3) & is.na(child_bevtotal_7) & is.na(child_bevtotal_9) & is.na(child_bevtotal_10) & is.na(child_bevtotal_15), NA, rowSums(select(., child_bevtotal_3,child_bevtotal_7,child_bevtotal_9,child_bevtotal_11,child_bevtotal_15), na.rm=TRUE)))
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

bev_pd_vars<- names(bevq.df %>%
                      select(matches("_pd")))

bevq.df<- bevq.df %>%
  mutate_at(bev_pd_vars, .funs = list(bi= pdbi_fun)) #makes "bi" add to each new var name
```

## Results

Lets take a look at what we returned.

``` r
bevq.df %>%
  select(matches("_pd")) %>%
  skimr::skim()%>%
  dplyr::filter(stat == "mean") %>%
  select(variable, value)
```

    ## # A tibble: 72 x 2
    ##    variable        value
    ##    <chr>           <dbl>
    ##  1 par_bev_1_pd  2.94   
    ##  2 par_bev_2_pd  0.0630 
    ##  3 par_bev_3_pd  0.00840
    ##  4 par_bev_4_pd  0      
    ##  5 par_bev_5_pd  0.143  
    ##  6 par_bev_6_pd  0.189  
    ##  7 par_bev_7_pd  0.0756 
    ##  8 par_bev_8_pd  0.382  
    ##  9 par_bev_9_pd  0.0420 
    ## 10 par_bev_10_pd 0.685  
    ## # ... with 62 more rows

``` r
bevq.df %>%
  select(contains("_oz")) %>%
  skimr::skim()%>%
  dplyr::filter(stat == "mean") %>%
  select(variable, value)
```

    ## # A tibble: 36 x 2
    ##    variable         value
    ##    <chr>            <dbl>
    ##  1 par_bevamt_1_oz   15.5
    ##  2 par_bevamt_2_oz    7  
    ##  3 par_bevamt_3_oz    6  
    ##  4 par_bevamt_4_oz  NaN  
    ##  5 par_bevamt_5_oz    7.2
    ##  6 par_bevamt_6_oz    8  
    ##  7 par_bevamt_7_oz   12  
    ##  8 par_bevamt_8_oz   10.3
    ##  9 par_bevamt_9_oz   12  
    ## 10 par_bevamt_10_oz  10.5
    ## # ... with 26 more rows

``` r
bevq.df %>%
  select(contains("_bevtotal")) %>%
  skimr::skim() %>%
  dplyr::filter(stat == "mean") %>%
  select(variable, value)
```

    ## # A tibble: 38 x 2
    ##    variable          value
    ##    <chr>             <dbl>
    ##  1 par_bevtotal_1  46.4   
    ##  2 par_bevtotal_2   0.312 
    ##  3 par_bevtotal_3   0.0504
    ##  4 par_bevtotal_4   0     
    ##  5 par_bevtotal_5   0.902 
    ##  6 par_bevtotal_6   1.55  
    ##  7 par_bevtotal_7   0.214 
    ##  8 par_bevtotal_8   4.97  
    ##  9 par_bevtotal_9   0.504 
    ## 10 par_bevtotal_10  7.36  
    ## # ... with 28 more rows

``` r
bevq.df %>%
  select(contains("_ssb")) %>%
  skimr::skim() %>%
  dplyr::filter(stat == "mean") %>%
  select(variable, value)
```

    ## # A tibble: 2 x 2
    ##   variable           value
    ##   <chr>              <dbl>
    ## 1 par_bevtotal_ssb   11.7 
    ## 2 child_bevtotal_ssb  1.43

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
rj_url_2 <- "https://raw.github.com/rebekahjacob/Bevq/master/bevq labels.xlsx"
labels.df<- read.xlsx(rj_url_2)
labels.df
```

    ##               variable
    ## 1              childid
    ## 2                 hhid
    ## 3            par_bev_1
    ## 4            par_bev_2
    ## 5            par_bev_3
    ## 6            par_bev_4
    ## 7            par_bev_5
    ## 8            par_bev_6
    ## 9            par_bev_7
    ## 10           par_bev_8
    ## 11           par_bev_9
    ## 12          par_bev_10
    ## 13          par_bev_11
    ## 14          par_bev_12
    ## 15          par_bev_13
    ## 16          par_bev_14
    ## 17          par_bev_15
    ## 18          par_bev_16
    ## 19          par_bev_17
    ## 20          par_bev_18
    ## 21        par_bevamt_1
    ## 22        par_bevamt_2
    ## 23        par_bevamt_3
    ## 24        par_bevamt_4
    ## 25        par_bevamt_5
    ## 26        par_bevamt_6
    ## 27        par_bevamt_7
    ## 28        par_bevamt_8
    ## 29        par_bevamt_9
    ## 30       par_bevamt_10
    ## 31       par_bevamt_11
    ## 32       par_bevamt_12
    ## 33       par_bevamt_13
    ## 34       par_bevamt_14
    ## 35       par_bevamt_15
    ## 36       par_bevamt_16
    ## 37       par_bevamt_17
    ## 38       par_bevamt_18
    ## 39        par_bev_1_pd
    ## 40        par_bev_2_pd
    ## 41        par_bev_3_pd
    ## 42        par_bev_4_pd
    ## 43        par_bev_5_pd
    ## 44        par_bev_6_pd
    ## 45        par_bev_7_pd
    ## 46        par_bev_8_pd
    ## 47        par_bev_9_pd
    ## 48       par_bev_10_pd
    ## 49       par_bev_11_pd
    ## 50       par_bev_12_pd
    ## 51       par_bev_13_pd
    ## 52       par_bev_14_pd
    ## 53       par_bev_15_pd
    ## 54       par_bev_16_pd
    ## 55       par_bev_17_pd
    ## 56       par_bev_18_pd
    ## 57     par_bevamt_1_oz
    ## 58     par_bevamt_2_oz
    ## 59     par_bevamt_3_oz
    ## 60     par_bevamt_4_oz
    ## 61     par_bevamt_5_oz
    ## 62     par_bevamt_6_oz
    ## 63     par_bevamt_7_oz
    ## 64     par_bevamt_8_oz
    ## 65     par_bevamt_9_oz
    ## 66    par_bevamt_10_oz
    ## 67    par_bevamt_11_oz
    ## 68    par_bevamt_12_oz
    ## 69    par_bevamt_13_oz
    ## 70    par_bevamt_14_oz
    ## 71    par_bevamt_15_oz
    ## 72    par_bevamt_16_oz
    ## 73    par_bevamt_17_oz
    ## 74    par_bevamt_18_oz
    ## 75      par_bevtotal_1
    ## 76      par_bevtotal_2
    ## 77      par_bevtotal_3
    ## 78      par_bevtotal_4
    ## 79      par_bevtotal_5
    ## 80      par_bevtotal_6
    ## 81      par_bevtotal_7
    ## 82      par_bevtotal_8
    ## 83      par_bevtotal_9
    ## 84     par_bevtotal_10
    ## 85     par_bevtotal_11
    ## 86     par_bevtotal_12
    ## 87     par_bevtotal_13
    ## 88     par_bevtotal_14
    ## 89     par_bevtotal_15
    ## 90     par_bevtotal_16
    ## 91     par_bevtotal_17
    ## 92     par_bevtotal_18
    ## 93     par_bev_1_pd_bi
    ## 94     par_bev_2_pd_bi
    ## 95     par_bev_3_pd_bi
    ## 96     par_bev_4_pd_bi
    ## 97     par_bev_5_pd_bi
    ## 98     par_bev_6_pd_bi
    ## 99     par_bev_7_pd_bi
    ## 100    par_bev_8_pd_bi
    ## 101    par_bev_9_pd_bi
    ## 102   par_bev_10_pd_bi
    ## 103   par_bev_11_pd_bi
    ## 104   par_bev_12_pd_bi
    ## 105   par_bev_13_pd_bi
    ## 106   par_bev_14_pd_bi
    ## 107   par_bev_15_pd_bi
    ## 108   par_bev_16_pd_bi
    ## 109   par_bev_17_pd_bi
    ## 110   par_bev_18_pd_bi
    ## 111        child_bev_1
    ## 112        child_bev_2
    ## 113        child_bev_3
    ## 114        child_bev_4
    ## 115        child_bev_5
    ## 116        child_bev_6
    ## 117        child_bev_7
    ## 118        child_bev_8
    ## 119        child_bev_9
    ## 120       child_bev_10
    ## 121       child_bev_11
    ## 122       child_bev_12
    ## 123       child_bev_13
    ## 124       child_bev_14
    ## 125       child_bev_15
    ## 126       child_bev_16
    ## 127       child_bev_17
    ## 128       child_bev_18
    ## 129     child_bevamt_1
    ## 130     child_bevamt_2
    ## 131     child_bevamt_3
    ## 132     child_bevamt_4
    ## 133     child_bevamt_5
    ## 134     child_bevamt_6
    ## 135     child_bevamt_7
    ## 136     child_bevamt_8
    ## 137     child_bevamt_9
    ## 138    child_bevamt_10
    ## 139    child_bevamt_11
    ## 140    child_bevamt_12
    ## 141    child_bevamt_13
    ## 142    child_bevamt_14
    ## 143    child_bevamt_15
    ## 144    child_bevamt_16
    ## 145    child_bevamt_17
    ## 146    child_bevamt_18
    ## 147     child_bev_1_pd
    ## 148     child_bev_2_pd
    ## 149     child_bev_3_pd
    ## 150     child_bev_4_pd
    ## 151     child_bev_5_pd
    ## 152     child_bev_6_pd
    ## 153     child_bev_7_pd
    ## 154     child_bev_8_pd
    ## 155     child_bev_9_pd
    ## 156    child_bev_10_pd
    ## 157    child_bev_11_pd
    ## 158    child_bev_12_pd
    ## 159    child_bev_13_pd
    ## 160    child_bev_14_pd
    ## 161    child_bev_15_pd
    ## 162    child_bev_16_pd
    ## 163    child_bev_17_pd
    ## 164    child_bev_18_pd
    ## 165  child_bevamt_1_oz
    ## 166  child_bevamt_2_oz
    ## 167  child_bevamt_3_oz
    ## 168  child_bevamt_4_oz
    ## 169  child_bevamt_5_oz
    ## 170  child_bevamt_6_oz
    ## 171  child_bevamt_7_oz
    ## 172  child_bevamt_8_oz
    ## 173  child_bevamt_9_oz
    ## 174 child_bevamt_10_oz
    ## 175 child_bevamt_11_oz
    ## 176 child_bevamt_12_oz
    ## 177 child_bevamt_13_oz
    ## 178 child_bevamt_14_oz
    ## 179 child_bevamt_15_oz
    ## 180 child_bevamt_16_oz
    ## 181 child_bevamt_17_oz
    ## 182 child_bevamt_18_oz
    ## 183   child_bevtotal_1
    ## 184   child_bevtotal_2
    ## 185   child_bevtotal_3
    ## 186   child_bevtotal_4
    ## 187   child_bevtotal_5
    ## 188   child_bevtotal_6
    ## 189   child_bevtotal_7
    ## 190   child_bevtotal_8
    ## 191   child_bevtotal_9
    ## 192  child_bevtotal_10
    ## 193  child_bevtotal_11
    ## 194  child_bevtotal_12
    ## 195  child_bevtotal_13
    ## 196  child_bevtotal_14
    ## 197  child_bevtotal_15
    ## 198  child_bevtotal_16
    ## 199  child_bevtotal_17
    ## 200  child_bevtotal_18
    ## 201  child_bev_1_pd_bi
    ## 202  child_bev_2_pd_bi
    ## 203  child_bev_3_pd_bi
    ## 204  child_bev_4_pd_bi
    ## 205  child_bev_5_pd_bi
    ## 206  child_bev_6_pd_bi
    ## 207  child_bev_7_pd_bi
    ## 208  child_bev_8_pd_bi
    ## 209  child_bev_9_pd_bi
    ## 210 child_bev_10_pd_bi
    ## 211 child_bev_11_pd_bi
    ## 212 child_bev_12_pd_bi
    ## 213 child_bev_13_pd_bi
    ## 214 child_bev_14_pd_bi
    ## 215 child_bev_15_pd_bi
    ## 216 child_bev_16_pd_bi
    ## 217 child_bev_17_pd_bi
    ## 218 child_bev_18_pd_bi
    ## 219   par_bevtotal_ssb
    ## 220 child_bevtotal_ssb
    ##                                                                                                             var_label
    ## 1                                                                                                            Child ID
    ## 2                                                                                                        Household ID
    ## 3                                                                                               Times per week: Water
    ## 4                                                                                    Times per week: 100% fruit juice
    ## 5               Times per week: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 6                                                                                          Times per week: Whole milk
    ## 7                                                                               Times per week: Reduced fat milk (2%)
    ## 8                                               Times per week: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 9                                                                                Times per week: Soft drinks, regular
    ## 10                                Times per week: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 11                                                                                      Times per week: Sweetened tea
    ## 12                                       Times per week: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 13                        Times per week: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 14                                              Times per week: Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 15                                                            Times per week: Hard liquor (shots, rum, tequila, etc.)
    ## 16                                                                                Times per week: Wine (red or white)
    ## 17                                            Times per week: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 18                                                 Times per week: Other beverage 1 (list in box below if applicable)
    ## 19                                                 Times per week: Other beverage 2 (list in box below if applicable)
    ## 20                                                 Times per week: Other beverage 3 (list in box below if applicable)
    ## 21                                                                                   Categories of amount (oz): Water
    ## 22                                                                        Categories of amount (oz): 100% fruit juice
    ## 23   Categories of amount (oz): Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 24                                                                              Categories of amount (oz): Whole milk
    ## 25                                                                   Categories of amount (oz): Reduced fat milk (2%)
    ## 26                                   Categories of amount (oz): Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 27                                                                    Categories of amount (oz): Soft drinks, regular
    ## 28                     Categories of amount (oz): Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 29                                                                           Categories of amount (oz): Sweetened tea
    ## 30                            Categories of amount (oz): Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 31             Categories of amount (oz): Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 32                                   Categories of amount (oz): Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 33                                                 Categories of amount (oz): Hard liquor (shots, rum, tequila, etc.)
    ## 34                                                                     Categories of amount (oz): Wine (red or white)
    ## 35                                 Categories of amount (oz): Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 36                                      Categories of amount (oz): Other beverage 1 (list in box below if applicable)
    ## 37                                      Categories of amount (oz): Other beverage 2 (list in box below if applicable)
    ## 38                                      Categories of amount (oz): Other beverage 3 (list in box below if applicable)
    ## 39                                                                                               Times per day: Water
    ## 40                                                                                    Times per day: 100% fruit juice
    ## 41               Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 42                                                                                          Times per day: Whole milk
    ## 43                                                                               Times per day: Reduced fat milk (2%)
    ## 44                                               Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 45                                                                                Times per day: Soft drinks, regular
    ## 46                                 Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 47                                                                                       Times per day: Sweetened tea
    ## 48                                        Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 49                         Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 50                                               Times per day: Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 51                                                             Times per day: Hard liquor (shots, rum, tequila, etc.)
    ## 52                                                                                 Times per day: Wine (red or white)
    ## 53                                             Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 54                                                  Times per day: Other beverage 1 (list in box below if applicable)
    ## 55                                                  Times per day: Other beverage 2 (list in box below if applicable)
    ## 56                                                  Times per day: Other beverage 3 (list in box below if applicable)
    ## 57                                                                                                 Amount (oz): Water
    ## 58                                                                                      Amount (oz): 100% fruit juice
    ## 59                 Amount (oz): Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 60                                                                                            Amount (oz): Whole milk
    ## 61                                                                                 Amount (oz): Reduced fat milk (2%)
    ## 62                                                 Amount (oz): Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 63                                                                                  Amount (oz): Soft drinks, regular
    ## 64                                   Amount (oz): Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 65                                                                                         Amount (oz): Sweetened tea
    ## 66                                          Amount (oz): Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 67                           Amount (oz): Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 68                                                 Amount (oz): Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 69                                                               Amount (oz): Hard liquor (shots, rum, tequila, etc.)
    ## 70                                                                                   Amount (oz): Wine (red or white)
    ## 71                                               Amount (oz): Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 72                                                    Amount (oz): Other beverage 1 (list in box below if applicable)
    ## 73                                                    Amount (oz): Other beverage 2 (list in box below if applicable)
    ## 74                                                    Amount (oz): Other beverage 3 (list in box below if applicable)
    ## 75                                                                                   Average daily amount (oz): Water
    ## 76                                                                        Average daily amount (oz): 100% fruit juice
    ## 77   Average daily amount (oz): Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 78                                                                              Average daily amount (oz): Whole milk
    ## 79                                                                   Average daily amount (oz): Reduced fat milk (2%)
    ## 80                                   Average daily amount (oz): Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 81                                                                    Average daily amount (oz): Soft drinks, regular
    ## 82                     Average daily amount (oz): Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 83                                                                           Average daily amount (oz): Sweetened tea
    ## 84                            Average daily amount (oz): Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 85             Average daily amount (oz): Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 86                                   Average daily amount (oz): Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 87                                                 Average daily amount (oz): Hard liquor (shots, rum, tequila, etc.)
    ## 88                                                                     Average daily amount (oz): Wine (red or white)
    ## 89                                 Average daily amount (oz): Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 90                                      Average daily amount (oz): Other beverage 1 (list in box below if applicable)
    ## 91                                      Average daily amount (oz): Other beverage 2 (list in box below if applicable)
    ## 92                                      Average daily amount (oz): Other beverage 3 (list in box below if applicable)
    ## 93                                                                                  Dichotomous: Times per day: Water
    ## 94                                                                       Dichotomous: Times per day: 100% fruit juice
    ## 95  Dichotomous: Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 96                                                                             Dichotomous: Times per day: Whole milk
    ## 97                                                                  Dichotomous: Times per day: Reduced fat milk (2%)
    ## 98                                  Dichotomous: Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 99                                                                   Dichotomous: Times per day: Soft drinks, regular
    ## 100                   Dichotomous: Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 101                                                                         Dichotomous: Times per day: Sweetened tea
    ## 102                          Dichotomous: Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 103           Dichotomous: Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 104                                 Dichotomous: Times per day: Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 105                                               Dichotomous: Times per day: Hard liquor (shots, rum, tequila, etc.)
    ## 106                                                                   Dichotomous: Times per day: Wine (red or white)
    ## 107                               Dichotomous: Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 108                                    Dichotomous: Times per day: Other beverage 1 (list in box below if applicable)
    ## 109                                    Dichotomous: Times per day: Other beverage 2 (list in box below if applicable)
    ## 110                                    Dichotomous: Times per day: Other beverage 3 (list in box below if applicable)
    ## 111                                                                                             Times per week: Water
    ## 112                                                                                  Times per week: 100% fruit juice
    ## 113             Times per week: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 114                                                                                        Times per week: Whole milk
    ## 115                                                                             Times per week: Reduced fat milk (2%)
    ## 116                                             Times per week: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 117                                                                              Times per week: Soft drinks, regular
    ## 118                               Times per week: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 119                                                                                     Times per week: Sweetened tea
    ## 120                                      Times per week: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 121                       Times per week: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 122                                             Times per week: Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 123                                                           Times per week: Hard liquor (shots, rum, tequila, etc.)
    ## 124                                                                               Times per week: Wine (red or white)
    ## 125                                           Times per week: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 126                                                Times per week: Other beverage 1 (list in box below if applicable)
    ## 127                                                Times per week: Other beverage 2 (list in box below if applicable)
    ## 128                                                Times per week: Other beverage 3 (list in box below if applicable)
    ## 129                                                                                  Categories of amount (oz): Water
    ## 130                                                                       Categories of amount (oz): 100% fruit juice
    ## 131  Categories of amount (oz): Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 132                                                                             Categories of amount (oz): Whole milk
    ## 133                                                                  Categories of amount (oz): Reduced fat milk (2%)
    ## 134                                  Categories of amount (oz): Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 135                                                                   Categories of amount (oz): Soft drinks, regular
    ## 136                    Categories of amount (oz): Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 137                                                                          Categories of amount (oz): Sweetened tea
    ## 138                           Categories of amount (oz): Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 139            Categories of amount (oz): Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 140                                  Categories of amount (oz): Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 141                                                Categories of amount (oz): Hard liquor (shots, rum, tequila, etc.)
    ## 142                                                                    Categories of amount (oz): Wine (red or white)
    ## 143                                Categories of amount (oz): Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 144                                     Categories of amount (oz): Other beverage 1 (list in box below if applicable)
    ## 145                                     Categories of amount (oz): Other beverage 2 (list in box below if applicable)
    ## 146                                     Categories of amount (oz): Other beverage 3 (list in box below if applicable)
    ## 147                                                                                              Times per day: Water
    ## 148                                                                                   Times per day: 100% fruit juice
    ## 149              Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 150                                                                                         Times per day: Whole milk
    ## 151                                                                              Times per day: Reduced fat milk (2%)
    ## 152                                              Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 153                                                                               Times per day: Soft drinks, regular
    ## 154                                Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 155                                                                                      Times per day: Sweetened tea
    ## 156                                       Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 157                        Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 158                                              Times per day: Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 159                                                            Times per day: Hard liquor (shots, rum, tequila, etc.)
    ## 160                                                                                Times per day: Wine (red or white)
    ## 161                                            Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 162                                                 Times per day: Other beverage 1 (list in box below if applicable)
    ## 163                                                 Times per day: Other beverage 2 (list in box below if applicable)
    ## 164                                                 Times per day: Other beverage 3 (list in box below if applicable)
    ## 165                                                                                                Amount (oz): Water
    ## 166                                                                                     Amount (oz): 100% fruit juice
    ## 167                Amount (oz): Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 168                                                                                           Amount (oz): Whole milk
    ## 169                                                                                Amount (oz): Reduced fat milk (2%)
    ## 170                                                Amount (oz): Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 171                                                                                 Amount (oz): Soft drinks, regular
    ## 172                                  Amount (oz): Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 173                                                                                        Amount (oz): Sweetened tea
    ## 174                                         Amount (oz): Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 175                          Amount (oz): Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 176                                                Amount (oz): Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 177                                                              Amount (oz): Hard liquor (shots, rum, tequila, etc.)
    ## 178                                                                                  Amount (oz): Wine (red or white)
    ## 179                                              Amount (oz): Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 180                                                   Amount (oz): Other beverage 1 (list in box below if applicable)
    ## 181                                                   Amount (oz): Other beverage 2 (list in box below if applicable)
    ## 182                                                   Amount (oz): Other beverage 3 (list in box below if applicable)
    ## 183                                                                                  Average daily amount (oz): Water
    ## 184                                                                       Average daily amount (oz): 100% fruit juice
    ## 185  Average daily amount (oz): Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 186                                                                             Average daily amount (oz): Whole milk
    ## 187                                                                  Average daily amount (oz): Reduced fat milk (2%)
    ## 188                                  Average daily amount (oz): Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 189                                                                   Average daily amount (oz): Soft drinks, regular
    ## 190                    Average daily amount (oz): Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 191                                                                          Average daily amount (oz): Sweetened tea
    ## 192                           Average daily amount (oz): Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 193            Average daily amount (oz): Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 194                                  Average daily amount (oz): Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 195                                                Average daily amount (oz): Hard liquor (shots, rum, tequila, etc.)
    ## 196                                                                    Average daily amount (oz): Wine (red or white)
    ## 197                                Average daily amount (oz): Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 198                                     Average daily amount (oz): Other beverage 1 (list in box below if applicable)
    ## 199                                     Average daily amount (oz): Other beverage 2 (list in box below if applicable)
    ## 200                                     Average daily amount (oz): Other beverage 3 (list in box below if applicable)
    ## 201                                                                                 Dichotomous: Times per day: Water
    ## 202                                                                      Dichotomous: Times per day: 100% fruit juice
    ## 203 Dichotomous: Times per day: Sweetened juice beverage/drink ((fruitades, lemonade, Kool aid, punch, Sunny Delight)
    ## 204                                                                            Dichotomous: Times per day: Whole milk
    ## 205                                                                 Dichotomous: Times per day: Reduced fat milk (2%)
    ## 206                                 Dichotomous: Times per day: Low fat/fat free milk (skim, 1% buttermilk, soy milk)
    ## 207                                                                  Dichotomous: Times per day: Soft drinks, regular
    ## 208                   Dichotomous: Times per day: Diet Soft Drinks/Artificially Sweetened Drinks (i.e. Crystal Light)
    ## 209                                                                         Dichotomous: Times per day: Sweetened tea
    ## 210                          Dichotomous: Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 211           Dichotomous: Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 212                                 Dichotomous: Times per day: Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer
    ## 213                                               Dichotomous: Times per day: Hard liquor (shots, rum, tequila, etc.)
    ## 214                                                                   Dichotomous: Times per day: Wine (red or white)
    ## 215                               Dichotomous: Times per day: Energy Drinks (Red Bull, Rockstar, Full Throttle, etc.)
    ## 216                                    Dichotomous: Times per day: Other beverage 1 (list in box below if applicable)
    ## 217                                    Dichotomous: Times per day: Other beverage 2 (list in box below if applicable)
    ## 218                                    Dichotomous: Times per day: Other beverage 3 (list in box below if applicable)
    ## 219                                                                                    PARENT Average daily oz of SSB
    ## 220                                                                                     CHILD Average daily oz of SSB

``` r
#number of variable names we pulled in
length(labels.df$variable)
```

    ## [1] 220

``` r
#number of variable names in our current dataframe
length(bevq.df)
```

    ## [1] 220

Now we need to do a little sorting/arranging before we copy over the
labels to our current dataframe.

``` r
#we have to arrange the df with the labels by the column "variable" to be in alphabetical order
labels.df<- labels.df %>%
  arrange(variable)

head(labels.df, 10) #view the first several
```

    ##              variable
    ## 1         child_bev_1
    ## 2      child_bev_1_pd
    ## 3   child_bev_1_pd_bi
    ## 4        child_bev_10
    ## 5     child_bev_10_pd
    ## 6  child_bev_10_pd_bi
    ## 7        child_bev_11
    ## 8     child_bev_11_pd
    ## 9  child_bev_11_pd_bi
    ## 10       child_bev_12
    ##                                                                                                  var_label
    ## 1                                                                                    Times per week: Water
    ## 2                                                                                     Times per day: Water
    ## 3                                                                        Dichotomous: Times per day: Water
    ## 4                             Times per week: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 5                              Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 6                 Dichotomous: Times per day: Coffee, with cream and/or sugar (includes non-dairy creamer)
    ## 7              Times per week: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 8               Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 9  Dichotomous: Times per day: Tea or coffee, black, with/without artificial sweetener (no cream or sugar)
    ## 10                                   Times per week: Beer, Ales, Wine Coolers, Non-alcoholic or Light Beer

``` r
#we have to arrange the full df by the name of each varable, ordering the dataframe in alphabetical order
#but we want to keep the order of the variables in tact from our original, so we'll save that as a vector
bevq.df_var_order<- names(bevq.df)

#now we arrange alphabetical order- see results
bevq.df<- bevq.df %>%
  select(order(colnames(.)))

head(names(bevq.df), 10) #check the order matches the other df's order
```

    ##  [1] "child_bev_1"        "child_bev_1_pd"     "child_bev_1_pd_bi" 
    ##  [4] "child_bev_10"       "child_bev_10_pd"    "child_bev_10_pd_bi"
    ##  [7] "child_bev_11"       "child_bev_11_pd"    "child_bev_11_pd_bi"
    ## [10] "child_bev_12"

Now that we’ve made sure they match, we go ahead and copy the labels to
our current dataframe from the vector of variable labels we pulled in
from the excel file.

``` r
#here we assign the values from one column in the label dataset to each of the variables in our large set
#note, that in doing this, our variable are a bit out of sorts, so we can rearrange them, or at least, for me, I like my childid and hhid to be in the front of the data, and not worry about the rest
bevq.df <- set_label(bevq.df, label = labels.df$var_label) %>%
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
write_sav(bevq.df, paste0( "Bevq ", Sys.Date(), ".sav"))
```
