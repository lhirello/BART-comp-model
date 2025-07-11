---
title: "BART comp model data prep"
author: "Laura Hirello"
date: "2025-07-10"
output: 
  html_document: 
    keep_md: true
---


```r
#Data cleaning
library(dplyr) 
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(data.table)
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
library("readxl")
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ forcats   1.0.0     ✔ readr     2.1.5
## ✔ ggplot2   3.5.1     ✔ stringr   1.5.1
## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ data.table::between() masks dplyr::between()
## ✖ dplyr::filter()       masks stats::filter()
## ✖ data.table::first()   masks dplyr::first()
## ✖ lubridate::hour()     masks data.table::hour()
## ✖ lubridate::isoweek()  masks data.table::isoweek()
## ✖ dplyr::lag()          masks stats::lag()
## ✖ data.table::last()    masks dplyr::last()
## ✖ lubridate::mday()     masks data.table::mday()
## ✖ lubridate::minute()   masks data.table::minute()
## ✖ lubridate::month()    masks data.table::month()
## ✖ lubridate::quarter()  masks data.table::quarter()
## ✖ lubridate::second()   masks data.table::second()
## ✖ purrr::transpose()    masks data.table::transpose()
## ✖ lubridate::wday()     masks data.table::wday()
## ✖ lubridate::week()     masks data.table::week()
## ✖ lubridate::yday()     masks data.table::yday()
## ✖ lubridate::year()     masks data.table::year()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(foreach)
```

```
## 
## Attaching package: 'foreach'
## 
## The following objects are masked from 'package:purrr':
## 
##     accumulate, when
```

```r
library(readr)
library("writexl")
library(tibble)

#visualization
library(ggplot2)
library(ggpubr)
library(visreg)

#modelling
library(lme4)
```

```
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
```

```r
library(lmerTest)
```

```
## 
## Attaching package: 'lmerTest'
## 
## The following object is masked from 'package:lme4':
## 
##     lmer
## 
## The following object is masked from 'package:stats':
## 
##     step
```

```r
#read data
bart_dt <- as.data.table(read_excel("bart_df.xlsx"))
bart_dt[, c("date", "time", "blockcode", "trialnum", "pumpresult", "pumpcount", "timebefore1stpump", "rotation", "d_n", "totalearningsBART", "explosionpoint") := NULL] #remove extra columns
bart_dt$trial_ID <- paste0(bart_dt$participant, bart_dt$shift, bart_dt$activity, collapse = NULL, recycle0 = FALSE) #add in unique trial identifier
setcolorder(bart_dt, c("trial_ID", "participant", "shift", "activity", "ballooncount", "wantedpumps", "explosion")) #reorder columns
setnames(bart_dt, "wantedpumps", "pumps") #rename to match existing models
setnames(bart_dt, "trial_ID", "subjID") #rename to match existing models
```
