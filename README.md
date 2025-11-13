## Time Series Analysis and Machine Learning - Assignment 1

This assignment explores parameter selection and dimensionality reduction techniques in regression, including ridge regularization and principal-component-analysis with reproducible R implementations.

The repository contains our group’s submission for Assignment 1 of the *Time Series Analysis and Machine Learning* course at WHU – Otto Beisheim School of Management.

## Overview

The assignment is based on Chapter 6 of *An Introduction to Statistical Learning (2nd ed.)* by James et al. (2021).  
Our work focuses on:

1. **Empirical Exercise** – Replicating the two panels of **Figure 6.4**.  
2. **Theoretical Exercise** – Explaining in detail how the two lines in **Figure 6.14** are derived, their interpretation, and why they are orthogonal.

The analysis and discussion are presented in an RMarkdown document (`assignment_1.Rmd`), which can be compiled into PDF format directly on the WHU RStudio server.

## Project Structure
```
tsa-group-project/
│
├── assignment_1.Rmd      # Main RMarkdown file
├── credit_data.csv       # Credit data set for figure 6.4
├── figures/              # Output plots generated in R
└── .gitignore            # Ignored RStudio and system files
```

## How to Run

1. Clone this repository  
   ```bash
   git clone https://github.com/ennioeberwein/course-stats-project.git

2. Open `assignment_1.Rmd` in RStudio.
3. Knit the file to PDF.

## Team

- *Ennio Eberwein*  
- *Patrick Trost*  
- *Daria Göbel*  

## References

- James, G. et al. (2021). *An Introduction to Statistical Learning, with Applications in R (2nd ed.).* Springer.  
- Stock, J. H., & Watson, M. W. (2020). *Introduction to Econometrics (4th ed.).* Pearson.