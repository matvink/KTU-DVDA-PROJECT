# Motivation

Help business to classify bad customers

# Dependencies

-   R kernel
-   R packages:

```         

    install.packages("h2o")
    install.packages("shiny")
    install.packages("tidyverse")
```

# Directory structure

```         
    ├───1-data
    ├───2-report
    ├───3-R
    ├───4-model
    ├───5-predictions
    └───app
```

# How to execute code

-   Download data from ...
-   Run data preparation script:

```         
        git clone https://github.com/kestutisd/KTU-DVDA-PROJECT.git
        cd KTU-DVDA-PROJECT/3-R
        RScript data_transformation.R
```

# Results

-   Random Forest proved to be the best model and reached AUC = 0.78 on training dataset
