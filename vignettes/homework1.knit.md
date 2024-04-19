---
title: "BIOST 561: Homework 1"
date: "Due date: 7:00pm on Monday, April 8th, 2024"
output: 
  bookdown::html_document2:
    number_sections: false
    toc: true
    toc_float:
      toc_collapsed: true
    theme: united
vignette: >
  %\VignetteIndexEntry{BIOST 561: Homework 1}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Q0: Survey

**(A)** My preferred name is Alejandro Hernandez.

**(B)** I have read and understood the entire syllabus.

**(C)** Biostatistics I and II (BIOST 514/515) had assignments completed in R.

**(D)** I want my GitHub portfolio to be clean and show off my work and coding expertise.

# Q1: Setting up an R package and GitHub

Completed.

# Q2: Simulating the Central Limit Theorem


```r
# load relevant libraries
library(ggplot2)    # plotting
library(dplyr)      # dataframe manipulation
library(gridExtra)  # plotting
library(knitr)      # table formatting

# load generate_data()
source("https://raw.githubusercontent.com/linnykos/561_s2024_public/main/homework1/generate_data.R")
```

<u>**Question 2A**</u>: 
Read the function (either at the URL provided or by typing `generate_data` into your console), 
and in one to three sentences, describe the input and output of the provided
function and what the function is doing. You may use equations if needed.

The function `generate_data(n)` returns a vector of randomly generated numbers that are randomly selected from three distributions: a normal (mean=10, sd=1), a gamma (shape = scale = 2), and a chi-squared (df=3). The function's parameter is `n`, the size of the random vector. 

<u>**Question 2B**</u>:
Compute a (random, empirical) mean from 10,000 samples of size `n` = 10, and plot a histogram of the 10,000 different empirical means with 100 breaks.
Repeat this process for `n` with values of 1, 2, 3, 5, and 100, and plot these six histograms in order of increasing `n`. 


```r
sizes <- c(1,2,3,5,10,100)
K <- 10000
plots <- list() # empty list of plots

# iterate sample sizes
for (j in 1:length(sizes)) {
  # generate 10,000 sample means
  sample_means <- rep(NA, K)
  for(i in 1:K) {
    sample_means[i] = mean(generate_data(n=sizes[j]))
  }
  # plot frequency of sample means
  plot <- data.frame(mean=sample_means) %>% 
    ggplot2::ggplot(aes(x=mean, y=after_stat(density))) + 
      geom_histogram(bins=100, alpha=0.5) +
      geom_density() +
      labs(subtitle=paste("Sample size:", sizes[j]), x="Value", y="Frequency") +
      theme_minimal() + theme(plot.subtitle = element_text(hjust = 0.5))
  # append plot to list and repeat for next sample size
  plots[[j]] <- plot
}

grid.arrange(grobs=plots, nrow=2)
```

<div class="figure">
<img src="C:/Users/alega/AppData/Local/Temp/Rtmp6jkNXh/preview-26c86b0c604f.dir/homework1_files/figure-html/unnamed-chunk-2-1.png" alt="Histograms of 10,000 different empirical means" width="672" />
<p class="caption">(\#fig:unnamed-chunk-2)Histograms of 10,000 different empirical means</p>
</div>

<u>**Question 2C**</u>: 
In one to three sentences, write how the plot you reproduced helps to verify the Central Limit Theorem.

What is the underlying distribution of a sample mean? If we count the frequencies of many, many means from many, many samples, do we notice a particular shape? The figure above illustrates a theory in Statistics- the Central Limit Theorem- which states that any sample's mean has an underlying Normal distribution, meaning its measured value is frequently near some central value. This truth is easily observed when we plot many sample means from large samples.

# Q3: Basic data analysis

In this question, we'll load a simple dataset related to Alzheimer's Disease (AD) research.
The data originates from a single-cell database from [this study](https://portal.brain-map.org/explore/seattle-alzheimers-disease).
However, since this is the first homework, we'll mainly work with donor-level
data instead of the cell-level data.


```r
df <- read.csv("https://raw.githubusercontent.com/linnykos/561_s2024_public/main/homework1/sea-ad.csv")

head(df)
#>     Donor.ID Age.at.Death    Sex APOE4.Status Cognitive.Status Last.CASI.Score
#> 1 H19.33.004           80 Female            N      No dementia              85
#> 2 H20.33.001           82   Male            N      No dementia              97
#> 3 H20.33.002          90+ Female            N      No dementia              93
#> 4 H20.33.004           86   Male            Y         Dementia              80
#> 5 H20.33.005          90+ Female            N      No dementia              94
#> 6 H20.33.008          90+ Female            Y      No dementia              92
#>      Braak
#> 1 Braak IV
#> 2 Braak IV
#> 3 Braak IV
#> 4  Braak V
#> 5 Braak IV
#> 6  Braak V

summary(df)
#>    Donor.ID         Age.at.Death           Sex            APOE4.Status      
#>  Length:84          Length:84          Length:84          Length:84         
#>  Class :character   Class :character   Class :character   Class :character  
#>  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
#>                                                                             
#>                                                                             
#>                                                                             
#>                                                                             
#>  Cognitive.Status   Last.CASI.Score    Braak          
#>  Length:84          Min.   :66.00   Length:84         
#>  Class :character   1st Qu.:80.00   Class :character  
#>  Mode  :character   Median :89.00   Mode  :character  
#>                     Mean   :87.32                     
#>                     3rd Qu.:95.00                     
#>                     Max.   :99.00                     
#>                     NA's   :15
```

Here are brief descriptions of each variable:

- `Donor.ID`: The anonymized ID of the donor who donated their brain to AD research
- `Age.at.Death`: The age of the donor when they passed away
- `Sex`: The biological sex of the donor. (Kevin's comment: In this study, it seems like this is strictly only `Male` or `Female`...?)
- `APOE4.Status`: `Y` (for yes) or `N` (for no) if the donor has a particular genetic variant that is known to be a risk factor for AD
- `Cognitive.Status`: Whether the donor has dementia or not based on a wide array of cognitive assessments before death
- `Last.CASI.Score`: The last score the donor had, measured via CASI (a specific set of cognitive questions), before death
- `Braak`: Different severities of neuropathology based on the donor's brain tissue, extracted after the donor's consent and death 

Hopefully, this is a reminder to carefully describe your study variables in any dataset you publish or collaborate on!

<u>**Question 3A**</u>: Describe, in two to four sentences, what the `head(df)` and `summary(df)` results display. 

Executing `head(df)` returns the column values of the first 6 rows in the DataFrame named `df`. Calling `summary(df)` returns a list of summaries for each variable in `df`; numeric variables are summarized by their minimum, quartiles, mean, and number NA, and character variables are summarized by their length, type, and mode type.

<u>**Question 3B**</u>: Use a simple function to print out the dimension of `df` (i.e., how many rows and columns there are). What is the class of `df`?


```r
dim(df) # 84 rows, 7 columns
#> [1] 84  7
class(df) # df is a DataFrame
#> [1] "data.frame"
```

<u>**Question 3C**</u>: You'll note that in `summary(df)`, the variable `Age.at.Death`
is a `character`, even though it should be a numeric. This is because
there are values of `90+`. As a simple diagnostic, 1) replace all the `90+` values
in `df$Age.at.Death` with `90`, 2) convert `df$Age.at.Death` from a character
to a numeric, and 3) plot a histogram of `df$Age.at.Death`.


```r
# replace "90+" values with (numeric) 90
df$Age.at.Death[df$Age.at.Death=="90+"] <- 90
# make variable numeric
df$Age.at.Death <- as.numeric(df$Age.at.Death)

# plot histogram
hist(x=df$Age.at.Death, breaks=5, main=paste("Histogram of donor age"), xlab="Donor age")
```

<img src="C:/Users/alega/AppData/Local/Temp/Rtmp6jkNXh/preview-26c86b0c604f.dir/homework1_files/figure-html/question-3c-1.png" width="672" />

```r
# alternatively, with ggplot2
ggplot(df, aes(x=Age.at.Death)) + 
  geom_histogram(bins = 5) +
  xlab("Donor age") + ylab("Frequency") + 
  labs(title="Histogram of donor age") +
  scale_x_continuous(breaks=seq(65,90,5)) + 
  theme_bw()
```

<img src="C:/Users/alega/AppData/Local/Temp/Rtmp6jkNXh/preview-26c86b0c604f.dir/homework1_files/figure-html/question-3c-2.png" width="672" />

<u>**Question 3D**</u>: The `summary()` function is much more useful when the columns
of `df` are factors, not characters. Write some lines of code to convert the variables
`Sex`, `APOE4.Status`, `Cognitive.Status`, and `Braak` to be factors. (This should take
less than 10 lines of code.)


```r
# convert specified variables to factors
df <- df %>% 
  mutate_at(c("Sex", "APOE4.Status", "Cognitive.Status", "Braak"), as.factor)
```

<u>**Question 3E**</u>: With this new version of `df`, after finishing the previous
questions, show the result of `summary(df)`.


```r
# print summary
summary(df)
#>    Donor.ID          Age.at.Death       Sex     APOE4.Status
#>  Length:84          Min.   :65.00   Female:51   N:59        
#>  Class :character   1st Qu.:83.75   Male  :33   Y:25        
#>  Mode  :character   Median :90.00                           
#>                     Mean   :86.26                           
#>                     3rd Qu.:90.00                           
#>                     Max.   :90.00                           
#>                                                             
#>     Cognitive.Status Last.CASI.Score       Braak   
#>  Dementia   :42      Min.   :66.00   Braak 0  : 2  
#>  No dementia:42      1st Qu.:80.00   Braak II : 4  
#>                      Median :89.00   Braak III: 6  
#>                      Mean   :87.32   Braak IV :23  
#>                      3rd Qu.:95.00   Braak V  :34  
#>                      Max.   :99.00   Braak VI :15  
#>                      NA's   :15
```

In one sentence, describe how the summary here (in Question 3E) is more
informative than the summary shown at the start of Question 3.

The levels of factor variables are listed alongside their frequencies, which is more useful information than the variable length and type.

<u>**Question 3F**</u>: Using the `table()` function, display the relation
between `Braak` and `Cognitive.Status`. Please look at `?table` (for the documentation
of `table()`) if you are unfamiliar with this function.


```r
# contingency table of Braak and Congnitive Status
df %>%
  select(Braak, Cognitive.Status) %>%
  table
#>            Cognitive.Status
#> Braak       Dementia No dementia
#>   Braak 0          0           2
#>   Braak II         2           2
#>   Braak III        2           4
#>   Braak IV         4          19
#>   Braak V         20          14
#>   Braak VI        14           1
```

<u>**Question 3G**</u>: This question will be slightly challenging. The `table()`
function is not as useful when there are many unique values (such as in `Last.CASI.Score`).
To overcome this, look up the documentation for the `cut` and `quantile` functions.
The goal is to use the `table()`, `cut()`, and `quantile()` to
show the relation of the quantiles of `Last.CASI.Score` with `Cognitive.Status`.
You will want to use `na.rm=TRUE` when using the `quantile()` function.


```r
# contingency table of Cognitive Status and quantiles of Last CASI Score
df %>% 
  # define the quantile each CASI Score falls into
  mutate(Last.CASI.Score.quant = cut(df$Last.CASI.Score, 
                                     breaks=quantile(df$Last.CASI.Score, 
                                                     na.rm=T))) %>%
  select(Last.CASI.Score.quant, Cognitive.Status) %>%
  table
#>                      Cognitive.Status
#> Last.CASI.Score.quant Dementia No dementia
#>               (66,80]       16           1
#>               (80,89]       10           8
#>               (89,95]        2          15
#>               (95,99]        3          13
```

In one to two sentences, describe what you can learn from the relation between
`Last.CASI.Score` and `Cognitive.Status` based on this result.

Cases of dementia are overwhelmingly present among CASI scores below the median,
and non-dementia cases are overwhelmingly prevalent among CASI scores above the median.
Last CASI score and dementia status appear to be negatively associated. 

# Q4: Pushing the homework onto GitHub

<mark>**Intent**: This section is not a question but a guide 
on submitting your homework. This procedure will 
be the expectation for all future homeworks.</mark>

Now that you have completed this homework, all that's remaining is to
push this homework onto your `UWBiost561` GitHub repository.
To do this, please ensure:

1. Your file is named `homework1.Rmd`. (This current homework file you are
writing in will replace the initial `homework1.Rmd` you made when
you completed the walkthrough in Question 1.)
2. Your file is in the `vignettes` folder in your `UWBiost561` R package.
3. You push your two files, `homework1.Rmd` and `homework1.html` (the latter is the knitted HTML file) onto your GitHub repository.
4. Double-check that you can see both files online on github.com when you look at your repository.

Again, ensure the username `linnykos` is added to your private GitHub repository. And with that, you're done! (For future homeworks, you will be "submitting" your homeworks like this.)

(You do not need to write anything for this question.)

# Q5: Feedback (Optional)

This "question" is an additional way for students to communicate with instructors. 
You could include positive feedback about topics you enjoyed learning this module, critiques about 
the course difficulty/pacing, 
or some questions/confusions you had about course material.
Your feedback can help us shape the course for the rest of this quarter and 
future years. Please be mindful and polite when providing feedback.
You may leave this question blank.

**This is an excellent first assignment. Great job getting us going with GitHub. I feel productive :-)!**
