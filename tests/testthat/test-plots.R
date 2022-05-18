test_that("Plots work", {

  data(immo)

  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(purrr)

  theme_LinkPact() |> theme_set()

  summary(immo$train)

  immo$train |>
    select(LotArea, SalePrice, MSSubClass) |>
    filter(LotArea < 1.5e4) |>
    mutate(SqPrice = SalePrice / LotArea) |>
    ggplot(aes(x = LotArea, y = log(SalePrice))) +
    geom_point() +
    geom_smooth()

  data <- immo$train |>
    filter(HouseStyle %in% c("1Story", "1.5Fin", "2Story")) |>
    mutate(HouseStyle = factor(HouseStyle, levels = c("1Story", "1.5Fin", "2Story"))) |>
    select(LotArea, SalePrice, YrSold, YearBuilt, HouseStyle, Neighborhood)

  data |>
    count(Neighborhood, YrSold) |>
    mutate(YrSold = factor(YrSold)) |>
    rename(x = Neighborhood, y = n) |>
    ll_bar("YrSold") + coord_flip()

  data |>
    count(YrSold) |>
    rename(x = YrSold, y = n) |>
    ll_ridgeline()

  data |>
    mutate(YrSold = factor(YrSold)) |>
    rename(x = LotArea) |>
    ll_density("YrSold")

  data |>
    group_by(YrSold, HouseStyle) |>
    summarize(n = n(),
              mu = mean(SalePrice),
              sigma = sd(SalePrice),
              .groups = "drop") |>
    rename(x = YrSold) |>
    mutate(y = mu,
           ymin = mu - 2 * sigma / n,
           ymax = mu + 2 * sigma / n) |>
    ll_lines(fill = "HouseStyle")

  data |>
    group_by(YrSold) |>
    summarize(n = n(),
              mu = mean(SalePrice),
              sigma = sd(SalePrice),
              .groups = "drop") |>
    rename(x = YrSold) |>
    mutate(y = mu,
           ymin = mu - 2 * sigma / n,
           ymax = mu + 2 * sigma / n) |>
    ll_linerange()

  data |>
    filter(YrSold == 2010) |>
    group_by(HouseStyle) |>
    summarize(n = n(),
              mu = mean(SalePrice),
              sigma = sd(SalePrice),
              .groups = "drop") |>
    rename(x = HouseStyle) |>
    mutate(y = mu,
           ymin = mu - 2 * sigma / n,
           ymax = mu + 2 * sigma / n) |>
    ll_linerange()

  quantiles <- seq(0, 1, 1 / 100)

  suppressWarnings(data |>
    mutate(x = SalePrice,
           group = if_else(YrSold == 2010, "new", "old") |> factor(levels = c("old", "new"))) |>
    select(x, group) |>
    pivot_wider(values_from = x, names_from = group, values_fn = list) |>
    mutate(q1 = map(old, quantile, quantiles),
           q2 = map(new, quantile, quantiles),
           pvalue = map2(old, new, ks.test) |> map_dbl("p.value")) |>
    select(- old, - new) |>
    unnest(cols = c(q1, q2)) |>
    ll_qqplot())

  # Histogram
  (p1 <- lkp_hist(data, LotArea, bw = 1e3))
  (p2 <- lkp_hist(data, HouseStyle, what = "freq"))
  (p3 <- lkp_hist(data |> mutate(YrSold = factor(YrSold)), YearBuilt, f2 = HouseStyle, fill = YrSold, nrow = 2))
  (p4 <- lkp_hist(data |> mutate(LotArea = LotArea / 1e3), LotArea, HouseStyle, bw = 1, what = "subfreq"))
  (p5 <- lkp_hist(data, YrSold, fill = HouseStyle, bw = 1, what = "freq", accuracy_text = 0.1))

  # Ridgeline
  (p1 <- count(data, YrSold, HouseStyle) |>
      lkp_ridgeline(YrSold, n, HouseStyle, nrow = 2, scales = "free_y", default_color = lkp_comp_blue))

  # Density
  (p1 <- lkp_density(data |> filter(LotArea < 3e4), LotArea, from = 0))
  (p2 <- lkp_density(data |> filter(LotArea < 3e4) |> mutate(YrSold = factor(YrSold)),
                     LotArea, fill = YrSold, from = 0))

  # Dots and lines
  (p1 <- count(data |> filter(HouseStyle %in% c("1.5Fin", "1Story", "2Story")), YrSold, HouseStyle) |>
      lkp_lines(YrSold, n, fill = HouseStyle, accuracy_text = 1))
  (p2 <- data |>
      filter(HouseStyle %in% c("1.5Fin", "1Story", "2Story")) |>
      group_by(YrSold, HouseStyle) |>
      summarize(n = ,
                mu = mean(SalePrice),
                sigma = sd(SalePrice) / sqrt(n()),
                .groups = "drop") |>
      mutate(SalePrice = mu,
             ymin = mu - 2 * sigma,
             ymax = mu + 2 * sigma) |>
      lkp_lines(YrSold, SalePrice, fill = HouseStyle))
  (p3 <- data |>
      filter(HouseStyle %in% c("1.5Fin", "1Story", "2Story")) |>
      group_by(YrSold, HouseStyle, Neighborhood) |>
      lkp_meanplot(YrSold, SalePrice, fill = HouseStyle))

  ## QQplot
  (p1 <- data |>
    mutate(g = if_else(YrSold == 2010, "new", "old") |> factor(levels = c("old", "new"))) |>
    lkp_qqplot(SalePrice, HouseStyle, group = g, nrow = 2))

  # 2D plot
  data |>
    lkp_2d_tile(YearBuilt, YrSold, fill = SalePrice)

  data |>
    group_by(YearBuilt, YrSold, HouseStyle) |>
    summarize(SalePrice = sum(SalePrice)) |>
    lkp_2d_contour(YrSold - YearBuilt, YrSold, SalePrice / 1e3, HouseStyle)
})



