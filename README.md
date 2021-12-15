# bcrtools: Bjoern's little R-function collection for ecological statistics

## Overview
This package contains several functions I developed aside from my analyses of ecological data. I started in December 2021, and I hope it will grow through time. This R-Package is my first, and it will also be my first open repository on GitHub. If you find issues and bugs, please report them to me. I'll try to fix them as fast as I can. I'd also appreciate it if you had comments on improving the code in general.

I'll try to keep a Version Log below also up-to-date.

## ToDo's and Issues
- I want to add the AICc to `select_random()`.

## Version Log
### 2021-12-14 - 0.1.0
#### Added function `select_random()`
I build this function to automatically select the random structure for an analysis using `lme()` from the `nlme` package. The function selects the whole random structure using either AIC or BIC. Especially random intercept and slopes models often crash with an error. I decided to catch this behaviour using `try()` and assign `Inf` to the AIC and BIC of the crashed analyses to keep the function running. Moreover, the function uses the parallelized `foreach()` loop to increase the speed of this time-consuming selection process. I added some very simple if-clauses to exclude the multi-core looping if it is not needed.

#### Added function `read_data()`
This function is simply a wrapper around `read.csv()` and allows to convert character strings to factors directly after reading the CSV file. And it automatically shows the data structure via `str()`.
