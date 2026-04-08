# Seminar Climate and Statistics II

Checkout the actual version of the [book](https://henrifnk.github.io/Seminar_ClimateNStatistics2425/).

As the world faces the reality of climate change, natural hazards and extreme weather events have become a major concern, with devastating consequences for nature and humans. The quantification and definition of climate change, extreme events and its implications for nature, life and health on our planet is one of the major concerns in climate science. 

This book explains current statistical methods in climate science and their application.
The methods include compound events, low flow events and return periods, natural variability, teleconnections and causal discovery.
All of those methods are used to quantify and anticipate the changing climate.

This book is the outcome of the seminar "Climate and Statistics" which took place in winter 24/25 at the Department of Statistics, LMU Munich.

## Style guide

### Structuring of files

There are four subdirectories:

work/

- code
- data
- figures
- results


### Conventions

The proposed convention here does not have to be adopted. It's just a suggestion for a possible naming scheme. __But__: Think about useful naming schemes, otherwise the repo might get very messy!

- Chapters are saved in the home directory of the repository and have a prefix `xx-chapter-name.Rmd` (e.g. `01-about-linear-models.Rmd`)
- Code, data, figures, and results are stored in the directories mentioned above in subdirectories that are named as the chapter itself. E.g. the R code `fit-linear-model.R` for the chapter `01-about-linear-models.Rmd` is saved in `code/01-about-linaer-models/fit-linear-model.R`. The same holds for data, figures, and results.

## How this book came about

This book is the result of a student seminar for Bachelor and Master in Statistics and Data Science at the LMU in the summer semester 2024.
Each student in the seminar wrote about a specific chapter of the book to pass the seminar.

## How to build the book

Step 0: Prerequisites

Make sure you have git and R up and running on your computer.

Step 1: Clone the repository to your machine

With RStudio: https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN

With command-line:
```
git clone git@github.com:https://github.com/henrifnk/Seminar_ClimateNStatistics2425.git
```

Step 2: Install dependencies

Start R in the project folder:

```
install.packages("devtools")
devtools::install_dev_deps()
```

Step 3: Render the book (R commands)

```{r}
# HTML
bookdown::render_book('./', 'bookdown::gitbook')
# PDF
bookdown::render_book('./', 'bookdown::pdf_book')
```


