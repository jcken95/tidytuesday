# processing script
# expects chapters to be chapters/1.txt, chapters/2.txt etc
# expects only numbered chapters in chapters/
# I used detexify chapter1.tex > 1.txt to remove the
# LaTeX commands from the thesis

set.seed(03012023)
library(tidyverse)
library(tidytext)

n_chapters <- length(list.files("chapters"))

for (i in seq_len(n_chapters)) {
  fname <- paste0("chapters/", i, ".txt")
  x <- unlist(readLines(fname))
  if (i == 1) {
    thesis <- tibble(words = x,
                     chapter = i)
  } else {
    thesis <- thesis %>%
      add_row(words = x,
              chapter = rep(i, length(x)))
  }
}
# remove blanks
thesis <- thesis %>%
  filter(words != "")
# unnest tokens and remove comma-words
thesis <- thesis %>%
  unnest_tokens(input = words, output = word) %>%
  filter(word != ",")

# permute the thesisordering for privacy - my thesis hasn't been examined yet!
n <- nrow(thesis)
permute <- sample(seq_len(n), n)
thesis <- thesis[permute, ]
readr::write_csv(thesis, "thesis.csv")
