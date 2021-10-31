## Online News Popularity

## Purpose

In this repo, we analyze the rate at which certain news articles are shared, attempting to learn more about why some articles are shared more than others. We were especially interested in how this differed across six news categories: Lifestyle, Entertainment, Business, Social Media, Techonology, and World. 

To answer these questions, we first explore the data through plots and summary tables. Then, we build linear regression, random forest, and boosted tree models that predict how many times an article got shared based on 16 different inputs. Through these models, we learn how well the number of shares can be predicted, as well as which inputs lead to more shares.

## R Packages

In this project, we use several open-source packages:

- `{rmarkdown}`: Render new .Rmd files
- `{tidyverse}`: Collection of data wrangling & visualization packages
- `{caret}`: For model building
- `{knitr}`: Knit R Markdown files
- `{corrplot}`: Correlation plot
- `{here}`: Access files from current working directory with `here::here()`
- `{kableExtra}`: Tables
- `{scales}`: Number formatting for tables and plots
- `{ggpubr}`: Correlation coefficient on plots
- `{ggeasy}`: `{ggplot2}` helpers
- `{gbm}`: Boosted tree model

## Links

Links to the six analyses (one for each news/channel category) are below:

- [Lifestyle](analysis-lifestyle.md)
- [Entertainment](analysis-entertainment.md)
- [Business](analysis-bus.md)
- [Social Media](analysis-socmed.md)
- [Technology](analysis-tech.md)
- [World](analysis-world.md)

## Code to Create Analyses

To output the six analyses, we run a loop that works off of the base .Rmd file, `project2.Rmd`, and outputs an analysis for a specific channel using an R Markdown parameter.

```
# define the 6 channel types, then loop through the base .Rmd script (one time for each channel)

chans <- c("lifestyle", "entertainment", "bus", "socmed", "tech", "world")

for (chan in chans) {
  rmarkdown::render(input = "project2.Rmd",
                    output_file = paste0("analysis-", chan, ".md"),
                    params = list(chan = chan))
}
```


This code can also be found in [this R script](render-the-six-analyses.R).
