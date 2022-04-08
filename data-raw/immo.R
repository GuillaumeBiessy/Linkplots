## code to prepare `immo` dataset goes here

library(tidyverse)

immo_train <- read_csv("data-raw/immo_train.csv")
immo_test <- read_csv("data-raw/immo_test.csv")
immo_sample_submission <- read_csv("data-raw/immo_sample_submission.csv")

immo <- list(train = immo_train,
             test = immo_test,
             sample_submission = immo_sample_submission)

usethis::use_data(immo, overwrite = TRUE)
