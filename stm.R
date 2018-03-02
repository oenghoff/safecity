# Environment -------------------------------------------------------------


library(tidyverse)
library(magrittr)
library(stm)

setwd("~/SafeCity")


# Load data ---------------------------------------------------------------


safecity_raw <- read.csv("https://www.dropbox.com/s/eesua704mi9wg62/clean_reports.csv?dl=1", stringsAsFactors = F)


# Process and filter raw data ---------------------------------------------


safecity_raw$description %<>%
  iconv("UTF-8", "ASCII")                 # Remove weird characters

safecity <-
  safecity_raw %>%
  filter(nchar(description) > 60) %>%     # Min. 60 characters (= ~10 words)
  group_by(categories) %>%
  filter(n() > 50) %>%                    # Min. 50 in category
  ungroup %>%
  mutate(categories = factor(categories)) # Factorise



# Pre-process text --------------------------------------------------------


processed <- textProcessor(documents = safecity$description,
                           metadata = safecity %>% select(categories),
                           lowercase = T,
                           removestopwords = T,
                           removenumbers = T,
                           removepunctuation = T,
                           stem = T,
                           wordLengths = c(3, Inf),
                           sparselevel = 1,
                           language = "en",
                           onlycharacter = T)

prepped <- with(processed,
                prepDocuments(documents, vocab, meta))

documents <- prepped$documents
vocab <- prepped$vocab
meta <- prepped$meta

# Estimate topic models with various K ------------------------------------


model.search <- searchK(documents = documents,
                        vocab = vocab,
                        data = meta,
                        K = c(5, 7, 10, 15, 20), # Number of topics
                        init.type = "Spectral",
                        LDAbeta = T)

plot(model.search) # K = 10 seems good


# Estimate topic model (K = 10) without covariates ------------------------


model.basic <- stm(documents = documents,
                   vocab = vocab,
                   data = meta,
                   K = 10, # Number of topics
                   init.type = "Spectral",
                   LDAbeta = T)


# Or: Estimate topic model (K = 10) with covariates -----------------------


model.covariates <- stm(documents = documents,
                        vocab = vocab,
                        data = meta,
                        K = 10, # Number of topics
                        init.type = "Spectral",
                        LDAbeta = T,
                        prevalence = ~categories)



# Inspect model output ----------------------------------------------------

# Which model?
model <- model.basic
# model <- model.covariates

# Topic words
labelTopics(model)

model.labels <-
  labelTopics(model, n = 5) %>%
  use_series(prob) %>%
  apply(1, paste0, collapse = " ")

# Topic prevalence plot
plot(model, n = 5)

# Inter-topic correlations
correlations <- 
  topicCorr(model, method = "simple") %T>%
  plot(vlabels = model.labels)

# Copy correlation matrix to clipboard for inspection in e.g. Excel
correlations %>% 
  use_series(cor) %>%
  write.table("clipboard", sep="\t", row.names = F, col.names = F)

# Calculate topic means per. category (and copy to clipboard)
catprops <-
  data_frame(category = meta$categories) %>%
  cbind(model$theta) %>%
  group_by(category) %>%
  summarise_all(mean) %T>%
  write.table("clipboard", sep="\t", row.names = F, col.names = F)

View(catprops)

# Most prevalent topic pr. category - seems legit
catprops %>%
  select(-category) %>%
  apply(1, which.max) %>%
  data_frame(category = catprops$category,
             top_topic = .,
             top_topic_label = model.labels[.])
