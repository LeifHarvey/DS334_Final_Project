

## install.packages("neuralnet")
library(neuralnet)

library(tidyverse)
library(palmerpenguins)

## classify species by bill length, depth, and flipper length
penguins_clean <- penguins |> filter(!is.na(bill_length_mm) &
                     !is.na(bill_depth_mm) &
                     !is.na(flipper_length_mm)) |>
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm) |>
  mutate(across(where(is.numeric), ~ (.x - min(.x))/(max(.x) - min(.x))))

train <- penguins_clean |> slice_sample(n = 250)
no_train <- anti_join(penguins_clean, train)

test <- no_train |> slice_sample(n = 50)
## going to ignore validation set for this example
val <- anti_join(no_train, test)

## fit neural networ with 3 predictors and 2 hidden layers
nn <- neuralnet(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
                data = train,
                hidden = c(4, 3),
                linear.output = FALSE) ## indicates classification
## hidden = c(4, 3) indicates that there will be 2 hidden layers
## the first will have 4 vertices, the second will have 3

plot(nn, rep = "best")

preds <- predict(nn, test) |> data.frame()
colnames(preds) <- c("Adelie", "Chinstrap", "Gentoo")

## figure out which species has the highest value for each row
class_preds <- preds |> mutate(id = row_number()) |>
  pivot_longer(1:3) |>
  group_by(id) |>
  filter(value == max(value)) |>
  pull(name)

## make classification table: perfect classification because
## species of penguins are very easy to classify
table(class_preds, test |> pull(species))
