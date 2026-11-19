# R-Studio-Tutorial

[![Run tutorial scripts](https://github.com/Syukrondzeko/R-Studio-Tutorial/actions/workflows/run-scripts.yml/badge.svg)](https://github.com/Syukrondzeko/R-Studio-Tutorial/actions/workflows/run-scripts.yml)

R code for the statistics and machine-learning tutorials on my YouTube channel
**[Manusia Setengah Chi Kuadrat](https://www.youtube.com/c/ManusiaSetengahChiKuadrat)**.
Hope you enjoy it, and don't forget to like, comment, and subscribe :D

Code comments are written in Bahasa Indonesia, following the videos.

## Quick start

1. Clone or download this repository.
2. Open **`R-Studio-Tutorial.Rproj`** in RStudio. This sets the working
   directory to the repo root, which every script relies on to find `data/`.
3. Install the required packages once:
   ```r
   source("install_packages.R")
   ```
4. Open any script below and run it.

Requires R 4.1 or newer.

## Topics

| Topic | Folder | Scripts | Main packages |
|---|---|---|---|
| ANOVA & Kruskal-Wallis | [`Anova_Kruskal_Wallis`](Anova_Kruskal_Wallis) | ANOVA from scratch, ANOVA with `aov`, assumption & Tukey post-hoc tests, Kruskal-Wallis | base R, `dplyr`, `car` |
| Correlation | [`Korelasi`](Korelasi) | Pearson, Kendall, Spearman | base R, `ggplot2` |
| Linear regression | [`Regresi_Linier`](Regresi_Linier) | Modelling, plots, classical assumption tests | `lmtest`, `car`, `ggplot2` |
| Panel data regression | [`Regresi_Data_Panel`](Regresi_Data_Panel) | Common / fixed / random effect, Chow, Hausman, LM tests | `plm` |
| Logistic regression, Naive Bayes, SVM | [`Klasifikasi`](Klasifikasi) | Binary logistic regression, Naive Bayes, SVM (linear, polynomial, RBF) | `e1071`, `caret`, `generalhoslem` |
| Decision tree | [`Decision_Tree`](Decision_Tree) | CART, CHAID | `rpart`, `CHAID` |
| Fuzzy K-NN | [`Nearest_Neighbours`](Nearest_Neighbours) | FKNN, FKNN in every class (FKNNC) | base R |
| Clustering | [`Clustering`](Clustering) | K-means, elbow method, hierarchical clustering | base R, `ggplot2` |
| Density-based clustering | [`DBSCAN`](DBSCAN) | DBSCAN | `dbscan`, `fpc`, `factoextra` |
| Fuzzy clustering | [`Fuzzy_Clustering`](Fuzzy_Clustering) | Fuzzy C-Means, Gustafson-Kessel | `ppclust`, `factoextra` |
| Market basket analysis | [`Market_Basket_Analysis`](Market_Basket_Analysis) | Apriori association rules | `arules`, `arulesViz` |
| Time series | [`ARIMA_SARIMA`](ARIMA_SARIMA) | ARIMA, SARIMA, auto ARIMA | `tseries`, `astsa`, `forecast` |
| Nonparametric regression | [`Regresi_Nonparametrik`](Regresi_Nonparametrik) | Kernel (Nadaraya-Watson), truncated spline, local polynomial, B-spline, P-spline | base R, `splines2` |
| Geographically Weighted Regression | [`Geographically_Weighted_Regression`](Geographically_Weighted_Regression) | GWR with fixed and adaptive bandwidth | `spgwr`, `spData`, `sf` |
| Mixed GWR | [`Mixed_GWR`](Mixed_GWR) | MGWR estimation and hypothesis tests | `spgwr` |
| Value at Risk | [`Value_at_Risk`](Value_at_Risk) | VaR single stock: normal, t-Student, historical simulation | base R |
| Value at Risk (portfolio) | [`Value_at_Risk_Portofolio`](Value_at_Risk_Portofolio) | Minimum-variance portfolio VaR: normal, t-Student, historical simulation | base R |
| Shiny GUI | [`Shiny_GUI`](Shiny_GUI) | K-means app, CSV upload + regression app, dashboard | `shiny`, `shinythemes`, `shinydashboard` |

Run a Shiny app with, for example:

```r
shiny::runApp("Shiny_GUI/Gui_Regresi")
```

## Data

All datasets live in [`data/`](data).

- Most of the original datasets used in the videos were not published, so
  **the CSV files in `data/` are synthetic (dummy) data** with the same column
  structure. They are created by
  [`data/generate_dummy_data.R`](data/generate_dummy_data.R) with fixed seeds,
  so you can regenerate them at any time. Your numbers will therefore differ
  from the ones shown in the videos.
- `data/reglog_admisi.csv` is the original logistic regression dataset.
- Several tutorials use datasets that ship with R or with a package
  (`iris`, `USArrests`, `AirPassengers`, `women`, `Grunfeld`, `ptitanic`,
  `Groceries`, `columbus`).

## Repository structure

```
.
├── R-Studio-Tutorial.Rproj   # open this first
├── install_packages.R        # installs every package used
├── data/                     # datasets + dummy data generator
├── tools/run_all_scripts.R   # smoke test: runs every script
├── <Topic folders>/          # one folder per tutorial topic
└── Regresi_Nonparametrik/helpers/  # shared functions (kernel, matrix inverse, truncated)
```

## Differences from the videos

The code was updated in 2026 so it runs on current R versions:

- Data is read with relative paths from `data/` instead of local `C:/...` and
  `F:/...` paths, and from CSV instead of Excel or the Windows clipboard.
- Retired packages were replaced: `rgdal` → `sf`, `FitAR::LjungBoxTest` →
  `Box.test`, `advclust::fuzzy.GK` → `ppclust::gk`, `matlib::inv` → `solve`,
  `psych::tr` → `sum(diag())`. The unused `timeSeries`, `sarima` and
  `markdown` packages were dropped.
- Windows-only `win.graph()` calls and interactive `readline()` prompts were
  removed.
- Bugs were fixed, including undefined objects (`new`, `hp`, `np.gcv`,
  `pred1` in SVM), hard-coded degrees of freedom in the ANOVA from scratch, the
  MGWR kernel weights and one-sided p-values, and the local polynomial
  estimator.
- `set.seed()` was added wherever results depend on random numbers.
- Folder names were cleaned up (no spaces, typos fixed). Links from older video
  descriptions to the old folder names will no longer work.

`arules` (market basket analysis) needs R 4.5 or newer for its latest version.
On older R, `install_packages.R` installs the last compatible version
automatically. `CHAID` is installed from R-Forge because it is not on CRAN.

## Checking that everything runs

```bash
Rscript tools/run_all_scripts.R
```

This runs every tutorial script in a fresh R session and tests the Shiny
servers. The same check runs on GitHub Actions for every push and pull request.

## License

[MIT](LICENSE)
