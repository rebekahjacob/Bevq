---
title: "Bevq Measures: Cleaning and Scoring"
Author: "Rebekah R. Jacob, Prevention Research Center"
output: github_document
---

## Introduction
This is a working example of how we pull in, score and visualize the bevq measures.

#### Data
We start with a dataframe (df) called bevq that has one childid per row.
```{r message=FALSE, warning=FALSE}
#libraries
library(sjlabelled) #add variable labels
library(haven) #read and write sav files (spss)
library(tidyverse) #data management
library(openxlsx) #read write Excel files
```

Read in data.
```{r}
rj_url <- "https://raw.github.com/rebekahjacob/Bevq/master/bevq_data.xlsx"
data<- read.xlsx(rj_url)[-1, ] #getting rid of label row, we'll add labels later...

bevq.df<- data %>%
  select(hhid, childid, 
         par_bev_1:par_bev_18, 
         par_bevamt_1:par_bevamt_18,
         child_bev_1:child_bev_18, 
         child_bevamt_1:child_bevamt_18, -ends_with("TEXT"))
```

#### Variables in the df
```{r}
names(bevq.df)
```

## Calculate new vars

### Unit of times per day ("pd")

Here, we have several transformations that need to take place.

First we convert frequency (“How often in one week”) to the unit of times per day ("pd").

Original: par_bev_1 through par_bev_16

1= Never or less than 1 tme per week____**new = 0**

2= 1 time per week____**new = 1/7 or .0143**

3= 2-3 times per week____**new = 2.5/7 or .357**

4= 4-6 times per week____**new = 5/7 or .714**

5= 1 time per day____**new = 1**

6= 2 times per day____**new = 2**

7= 3 or more times per day____**new = 3**

**New: par_bev_1_pd through par_bev_18_pd**
**New: child_bev_1_pd through child_bev_18_pd**

```{r}

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

### Unit ounces ("oz")
Next, we convert the amount consumed (originally a likert scale) into actual ounces so that it can then be used in an equation (per day x ounces= oz/pd)

Original: par_bevamt_1 through par_bevamt_16

1= Less than 6 fl oz (3/4 cup)____**new = 6 fl 0z**

2= 8 fl oz (1 cup)____**new = 8 fl 0z**

3= 12 fl oz (1 1/2 cups)____**new = 12 fl 0z**

4= 16 fl oz (2 cups)____**new = 16 fl 0z**

5= More than 20 fl oz (2 1/2 cups)____**new = 20 fl 0z**

**New: par_bevamt_1_oz through par_bevamt_18_oz**

**New: child_bevamt_1_oz through child_bevamt_18_oz**

```{r message=FALSE, warning=FALSE}
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

### Unit ounces per day ("bevtotal")
Now we get the total ounces per day for each beverage by taking the times per day and multiplying by ounces. But we keep zero as is since NA X 0 would be NA (see function written).

Example:

par_bevamt_1_pd X par_bevamt_1oz= par_bevtotal_1 (total ounces per day  consumed for beverage 1)

**New: par_bevtotal_1 through par_bevtotal_18**

**New: child_bevtotal_1 through child_bevtotal_18**

```{r message=FALSE, warning=FALSE}

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

To quantify average daily SSB consumption during the past month, beverage categories containing added sugars were summed 
(sweetened juice beverages and drinks (par_bev_3), regular sugar-sweetened carbonated beverages (par_bev_7), 
sweet tea (par_bev_9), sweetened coffee (par_bev_10), energy drinks (par_bev_15), 
and mixed alcoholic drinks).

Note that since this is a sum, we keep cases where some beverages are NA, unless all beverages are NA, then the total will also be NA.

For example, if bev_3 has a total, but NA for other ssb variables, the ssb_total for that case will be the sum of bev_3.

**New: par_bevtotal_ssb**

**New: child_bevtotal_ssb**

```{r message=FALSE, warning=FALSE}
bevq.df<- bevq.df %>%
  mutate(par_bevtotal_ssb=ifelse(is.na(par_bevtotal_3) & is.na(par_bevtotal_7) & is.na(par_bevtotal_9) & is.na(par_bevtotal_10) & is.na(par_bevtotal_15), NA, rowSums(select(., par_bevtotal_3,par_bevtotal_7,par_bevtotal_9,par_bevtotal_11,par_bevtotal_15), na.rm=TRUE)),
         child_bevtotal_ssb=ifelse(is.na(child_bevtotal_3) & is.na(child_bevtotal_7) & is.na(child_bevtotal_9) & is.na(child_bevtotal_10) & is.na(child_bevtotal_15), NA, rowSums(select(., child_bevtotal_3,child_bevtotal_7,child_bevtotal_9,child_bevtotal_11,child_bevtotal_15), na.rm=TRUE)))

```

### Unit binary (y/n)

Now we can make binary per day drink "any" vs. none for each beverage.

Original: par_bev_1_pd through par_bev_pd

**New: par_bev_1_pd_bi through par_bev_16_pd_bi**

```{r message=FALSE, warning=FALSE}

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
```{r warning=FALSE}

bevq.df %>%
  select(matches("_pd")) %>%
  skimr::skim()%>%
  dplyr::filter(stat == "mean") %>%
  select(variable, value)


bevq.df %>%
  select(contains("_oz")) %>%
  skimr::skim()%>%
  dplyr::filter(stat == "mean") %>%
  select(variable, value)

bevq.df %>%
  select(contains("_bevtotal")) %>%
  skimr::skim() %>%
  dplyr::filter(stat == "mean") %>%
  select(variable, value)


bevq.df %>%
  select(contains("_ssb")) %>%
  skimr::skim() %>%
  dplyr::filter(stat == "mean") %>%
  select(variable, value)
```

## Labels
If you need to add variable labels, it's best to do this by pulling in an excel file and applying that way.

First, pull in the file that have all the labels (should have 2 columns, one with var name and one with var label)

Then make sure your working with the same number of variables.

Here I use the 'sjlabelled' package which plays very nice with SPSS files. But there are other ways to go about labeling your variables and values.
```{r}
rj_url_2 <- "https://raw.github.com/rebekahjacob/Bevq/master/bevq labels.xlsx"
labels.df<- read.xlsx(rj_url_2)
head(labels.df)

#number of variable names we pulled in
length(labels.df$variable)

#number of variable names in our current dataframe
length(bevq.df)

```

Now we need to do a little sorting/arranging before we copy over the labels to our current dataframe.
```{r}
#we have to arrange the df with the labels by the column "variable" to be in alphabetical order
labels.df<- labels.df %>%
  arrange(variable)

head(labels.df, 10) #view the first several

#we have to arrange the full df by the name of each varable, ordering the dataframe in alphabetical order
#but we want to keep the order of the variables in tact from our original, so we'll save that as a vector
bevq.df_var_order<- names(bevq.df)

#now we arrange alphabetical order- see results
bevq.df<- bevq.df %>%
  select(order(colnames(.)))

head(names(bevq.df), 10) #check the order matches the other df's order
```

Now that we've made sure they match, we go ahead and copy the labels to our current dataframe from the vector of variable labels we pulled in from the excel file.
```{r}
#here we assign the values from one column in the label dataset to each of the variables in our large set
#note, that in doing this, our variable are a bit out of sorts, so we can rearrange them, or at least, for me, I like my childid and hhid to be in the front of the data, and not worry about the rest
bevq.df <- set_label(bevq.df, label = labels.df$var_label) %>%
  select(bevq.df_var_order) #here is where we use the vector we created to reorder back to original variable ordering
```

## Write out file
Depending on how you need to use the data, you may need to write out the file to stats program that others can use (spss, sas, csv, xlsx, etc).

Here I use the `haven` package to write SPSS file, but you can write whatever file you need for analysis. See .sav example of full calulated set.
```{r}
#Now you can write out your file for others to use if you need to
#here is to write out spss file and add date
write_sav(bevq.df, paste0( "Bevq ", Sys.Date(), ".sav"))

```
