---
Pavadinimas: "Exploratory Analysis"
Studentai: "Matas ir Ieva DVDA-3V"
Data: "27/12/2023"
output:
  html_document:
    keep_md: true
---

Užkrauname reikalingas bibliotekas

``` r
library(h2o)
library(tidyverse)
```

Nuskaitome duomenis.

``` r
df <- h2o.importFile("1-data/train_data.csv")
test_data <- h2o.importFile("1-data/test_data.csv")
```

Duomenų failo dimensijos:

``` r
dim(df)
```

```         
## [1] 1000000      17
```

# Kintamųjų apžvalga

``` r
summary(df[7:13]) %>%
  kable()
```

|     | yearly_income    | home_ownership   | bankruptcies   | years_current_job | monthly_debt   | years_credit_history | months_since_last_delinquent |
|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|
|     | Min. : 76627     | Length:1000000   | Min. :0.0000   | Min. : 0.00       | Min. : 0       | Min. : 4.0           | Min. : 0.0                   |
|     | 1st Qu.: 825797  | Class :character | 1st Qu.:0.0000 | 1st Qu.: 3.00     | 1st Qu.: 10324 | 1st Qu.:13.0         | 1st Qu.: 16.0                |
|     | Median : 1148550 | Mode :character  | Median :0.0000 | Median : 6.00     | Median : 16319 | Median :17.0         | Median : 32.0                |
|     | Mean : 1344805   | NA               | Mean :0.1192   | Mean : 5.88       | Mean : 18550   | Mean :18.1           | Mean : 34.9                  |
|     | 3rd Qu.: 1605899 | NA               | 3rd Qu.:0.0000 | 3rd Qu.:10.00     | 3rd Qu.: 24059 | 3rd Qu.:22.0         | 3rd Qu.: 51.0                |
|     | Max. :165557393  | NA               | Max. :7.0000   | Max. :10.00       | Max. :435843   | Max. :70.0           | Max. :176.0                  |
|     | NA's :219439     | NA               | NA's :1805     | NA's :45949       | NA             | NA                   | NA's :529539                 |
