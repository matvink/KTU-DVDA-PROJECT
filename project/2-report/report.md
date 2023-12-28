---
Pavadinimas: "Exploratory Analysis"
Studentai: "Matas ir Ieva DVDA-3V"
Data: "27/12/2023"
output:
  html_document:
    keep_md: true
---

## Žvalgomoji analizė

Duomenis apjungėme iš trijų skirtingų failų:

*1.* 1-sample_data.csv

*2.* 2-additional_data.csv

*3.* 3-additional_features.csv

Užkrovėme reikalingas bibliotekas:

```{r, eval=FALSE}
library(tidyverse)
library(visdat)
library(dplyr)
```

Nuskaitėme reikalingus failus:

```{r, eval=FALSE}
file_path <- ("1-data/1-sample_data.csv")
data <- read_csv(file_path)
data_additional <- read_csv("1-data/2-additional_data.csv")
data_feat <- read_csv("1-data/3-additional_features.csv")
```

Apjungėme duomenis į test duomenų imtį:

```{r, eval=FALSE}
combined_df <- dplyr::bind_rows(data_additional, data)
df <- inner_join(combined_df, data_feat, by = "id")
write_csv(df, "1-data/train_data.csv")
```

## Duomenų importavimas

```{r, eval=FALSE}
df <- h2o.importFile("../../project/1-data/train_data.csv")
y <- "y"
x <- setdiff(names(df), c(y, "id"))
df$y <- as.factor(df$y)
```

Failuose buvo laikoma informacija apie banko klientus, norinčius pasiimti paskolą. Jas visas apjungus duomenų failas buvo sudaryta iš 10mln. skirtingų įrašų su 17 kintamųjų:

Duomenų failo dimensijos:

```{r, eval=FALSE}
dim(df)
```

[1] 1000000 17

iš kurių:

*1.* id;

*2.* y - prognozuojamas kintamasis;

*3-15.* likę požymiai apie klientus, tokie kaip: metinės pajamos, dabartinis darbas, kredito reitingas, atidarytų sąskaitų kiekis ir t.t.

## Kintamųjų apžvalga

```{r, eval=FALSE}
summary(df[7:13]) %>%
  kable()
```

|     | yearly_income    | home_ownership   | bankruptcies   | years_current_job | monthly_debt   | years_credit_history | months_since_last_delinquent |
|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|
|     | Min. : 76627     | Length:1000000   | Min. :0.0000   | Min. : 0.00       | Min. : 0       | Min. : 4.0           | Min. : 0.0                   |
|     | 1st Qu.: 848844  | Class :character | 1st Qu.:0.0000 | 1st Qu.: 3.00     | 1st Qu.: 10200 | 1st Qu.:14.0         | 1st Qu.: 16.0                |
|     | Median : 1174371 | Mode :character  | Median :0.0000 | Median : 6.00     | Median : 16221 | Median :17.0         | Median : 32.0                |
|     | Mean : 1378367   | NA               | Mean :0.117    | Mean : 5.9        | Mean : 18471   | Mean :18.22          | Mean : 35.0                  |
|     | 3rd Qu.: 1651499 | NA               | 3rd Qu.:0.0000 | 3rd Qu.:10.00     | 3rd Qu.: 24012 | 3rd Qu.:22.0         | 3rd Qu.: 51.0                |
|     | Max. :165557393  | NA               | Max. :7.0000   | Max. :10.00       | Max. :435843   | Max. :70.0           | Max. :176.0                  |
|     | NA's :1919747    | NA               | NA's :18354    | NA's :422327      | NA             | NA                   | NA's :5317819                |

Iš summary komandos reikšmių galime pastebėti, kad turime nemažai N/A reikšmių (pvz., months_since_last_delinquent turime 5317819 tuščias reikšmes), bet modeliavimą atlikome su duomenimis, kuriuose yra N/A reikšmių, nes pakeitus jas pvz., vidurkiu modeliavimo rezultatai ženkliai nepagerėja, o išmetus tokį stulpelį - rezultatų kokybė suprastėja.

## Klasių tipo kintamųjų apžvalga

```{r, eval=FALSE}
h2o.nacnt(df)

summary(df$y, exact_quantiles=TRUE)
summary(df$term, exact_quantiles=TRUE)
summary(df$credit_score, exact_quantiles=TRUE)
summary(df$loan_purpose, exact_quantiles=TRUE)
summary(df$home_ownership, exact_quantiles=TRUE)
```

```{r, eval=FALSE}
df_ %>%
  group_by(y, loan_purpose) %>%
  summarise(n = n()) %>%
  ggplot(aes(fill=y, y=n, x=loan_purpose)) + 
  geom_bar(position="dodge", stat="identity") + 
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  theme_dark()
```

<img src="Grafikas_2.png" width="500"/>

```{r, eval=FALSE}
df_ %>%
  filter(y == 1) %>%
  group_by(loan_purpose) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
```

<img src="C:/Users/MatasV/OneDrive - SOCMIN/Darbalaukis/KTU/Projektas/KTU-DVDA-PROJECT/3_g.png" width="300"/>

```{r, eval=FALSE}
library(plotly)
df_ %>%
  group_by(y, credit_score) %>%
  summarise(n = n()) %>%
  plot_ly(x = ~credit_score, y = ~n, name = ~y, type = "bar")
```

Iš grafiko galime pamatyti, kad trūksta nemažai duomenų, tačiau didesnė dalis žmonių turi gerą arba labai gerą kredito įvertinimą.

<img src="C:/Users/MatasV/OneDrive - SOCMIN/Darbalaukis/KTU/Projektas/KTU-DVDA-PROJECT/4g.png" width="500"/>

```{r, eval=FALSE}
library(plotly)
df_ %>%
  group_by(y, term) %>%
  summarise(n = n()) %>%
  plot_ly(x = ~term, y = ~n, name = ~y, type = "bar")
```

Iš grafiko galime pamatyti, kad daugiausiai prašoma trumpalaikių paskolų ir jų negaunama.

<img src="C:/Users/MatasV/OneDrive - SOCMIN/Darbalaukis/KTU/Projektas/KTU-DVDA-PROJECT/5g.png" width="500"/>

```{r, eval=FALSE}
library(plotly)
df_ %>%
  group_by(y, home_ownership) %>%
  summarise(n = n()) %>%
  plot_ly(x = ~home_ownership, y = ~n, name = ~y, type = "bar")
```

Iš grafiko galime pamatyti, didžioji dalis prašančiųjų paskolos neturi nuosavo būsto ir jį nuomojasi arba moka už jį paskolą.

<img src="C:/Users/MatasV/OneDrive - SOCMIN/Darbalaukis/KTU/Projektas/KTU-DVDA-PROJECT/6g.png" width="500"/>

```{r, eval=FALSE}
library(plotly)
df_ %>%
  roup_by(y, loan_purpose) %>%
  summarise(n = n()) %>%
  plot_ly(x = ~loan_purpose, y = ~n, name = ~y, type = "bar")
```

<img src="C:/Users/MatasV/OneDrive - SOCMIN/Darbalaukis/KTU/Projektas/KTU-DVDA-PROJECT/7g.png" width="500"/>

## Modeliavimas

Užkrauname reikalingas bibliotekas ir inicijuojame h2o:

```{r, eval=FALSE}
library(h2o)
library(tidyverse)
h2o.init(max_mem_size = "8g")
```

Nuskaitome train ir test duomenis:

```{r, eval=FALSE}
df <- h2o.importFile("1-data/train_data.csv")
test_data <- h2o.importFile("1-data/test_data.csv")

Train data duomenų failą suskirstytėme į treniravimosi, testavimo ir validavimo imtis atitinkamomis proporcijomis (60,20,20):

splits <- h2o.splitFrame(df, c(0.6,0.2), seed=123)
train  <- h2o.assign(splits[[1]], "train") # 60%
valid  <- h2o.assign(splits[[2]], "valid") # 20%
test   <- h2o.assign(splits[[3]], "test")  # 20%
```

Ir pritaikėme Auto_ML modelį, su parametru `{r, eval=FALSE} max_runtime_secs = 1200`

```{r, eval=FALSE}
aml <- h2o.automl(x = x,
                  y = y,
                  training_frame = train,
                  validation_frame = valid,
                  max_runtime_secs = 1200)

aml@leaderboard
```

Gavome, kad modelio AUC rodiklis testiniams duomenims pasiekė **\~0.82.**

<img src="C:/Users/MatasV/OneDrive - SOCMIN/Darbalaukis/KTU/Projektas/KTU-DVDA-PROJECT/Rplot.png" width="500"/>

Išsaugojome rezultatus į **predictions_last**.**csv** failą, o modelį - **my_best_automlmode.\
**

```{r, eval=FALSE}
predictions %>%
  as_tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select(id, y) %>%
  write_csv("../5-predictions/predictions_last.csv")


### ID, Y

h2o.saveModel(model, "../4-model/", filename = "my_best_automlmode")
model <- h2o.loadModel("../4-model/my_best_automlmode")
```

Taip pat atlikome eksperimentą su random forest modeliu, kurio parametrus parinkome tokius `{r, eval=FALSE} ntrees = 50, max_depth = 20, stopping_metric = "AUC", seed = 123`

```{r, eval=FALSE}
rf_model <- h2o.randomForest(x,
                             y,
                             training_frame = train,
                             validation_frame = valid,
                             ntrees = 50,
                             max_depth = 20,
                             stopping_metric = "AUC",
                             seed = 123)
rf_model
h2o.auc(rf_model)
h2o.auc(h2o.performance(rf_model, valid = TRUE))
h2o.auc(h2o.performance(rf_model, newdata = test))


h2o.saveModel(rf_model, "../4-model/", filename = "rf_model")
```

Patikriname, kurie parametrai turi didžiausią svarbą apmokant šį modelį:

```{r, eval=FALSE}
var_imp <- h2o.varimp_plot(rf_model)
```

Pastebime, didžiausią įtaką modelio apmokymui daro parametrai: "credit_score", "yearly_income", "amount_current_loan" bei "term".

<img src="C:/Users/MatasV/OneDrive - SOCMIN/Darbalaukis/KTU/Projektas/KTU-DVDA-PROJECT/8g.png" width="500"/>
