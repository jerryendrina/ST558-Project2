### News Popularity Predictive Modeling

### Purpose

In this repo, we analyze the rate at which certain news articles are shared, attempting to learn more about why some articles are shared more than others. We were especially interested in how this differed across six news categories: Lifestyle, Entertainment, Business, Social Media, Techonology, and World. 

To answer these questions, we first explore the data through plots and summary tables. Then, we build linear regression, random forest, and boosted tree models that predict how many times an article got shared based on 16 different inputs. Through these models, we learn how well the number of shares can be predicted, as well as which inputs lead to more shares.

### R Packages

In this project, we use several open-source packages:

- `{rmarkdown}`: Render new .Rmd files
- `{tidyverse}`: Collection of data wrangling & visualization packages
- `{caret}`: For model building
- `{knitr}`:
- `{corrplot}`: Correlation plot
- `{leaps}`:
- `{here}`: Access files from current working directory with `here::here()`
- `{kableExtra}`: Tables
- `{scales}`: Number formatting for tables and plots
- `{ggpubr}`: Correlation coefficient on plots
- `{ggeasy}`: `{ggplot2}` helpers

### Links

Links to the six analyses (one for each news/channel category) are below:

- [Lifestyle](analysis-lifestyle.html)
- [Entertainment](analysis-entertainment.html)
- [Business](analysis-bus.html)
- [Social Media](analysis-socmed.html)
- [Technology](analysis-tech.html)
- [World](analysis-world.html)

### Code to Create Analyses

To output the six analyses, we run a loop that works off of the base .Rmd file, `project2.Rmd`, and outputs an analysis for a specific channel using an R Markdown parameter.

```
# define the 6 channel types -- will then loop through the base .Rmd script,
# one time for each channel (define as param in yaml)

chans <- c("lifestyle", "entertainment", "bus", "socmed", "tech", "world")

# NOTE: in main file, will need to add paste0("data_channel_is_", chan) in
# order to reference the actual column

for (chan in chans) {
  rmarkdown::render(input = "project2.Rmd",
                    output_file = paste0("analysis-", chan, ".html"),
                    params = list(chan = chan))
}
```


This code can also be found in [this R script](render-the-six-analyses.R).
