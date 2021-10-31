
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
