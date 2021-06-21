test_that("Theme works", {

  library(ggplot2)

  theme_LinkPact() |> theme_set()

  mtcars2 <- within(mtcars, {
    vs <- factor(vs, labels = c("V-shaped", "Straight"))
    am <- factor(am, labels = c("Automatic", "Manual"))
    cyl  <- factor(cyl)
    gear <- factor(gear)
  })

  p1 <- ggplot(mtcars2) +
    geom_point(aes(x = wt, y = mpg, colour = gear)) +
    labs(title = "Fuel economy declines as weight increases",
         subtitle = "(1973-74)",
         caption = "Data from the 1974 Motor Trend US magazine.",
         x = "Weight (1000 lbs)",
         y = "Fuel economy (mpg)",
         colour = "Gears")

  expect_silent(p1)
  expect_silent(p1 + facet_grid(vs ~ am))
})

