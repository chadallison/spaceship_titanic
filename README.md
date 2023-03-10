spaceship titanic
================

*IN PROGRESS*

### contents

- [setup](#setup)
- [data import](#data-import)
- [finding missing data](#finding-missing-data)
- [visualizing where missing values
  occur](#visualizing-where-missing-values-occur)
- [exploring `home_planet`](#exploring-home_planet)
- [exploring `cryo_sleep`](#exploring-cryo_sleep)
- [separating `cabin` into `cabin_x` and
  `cabin_y`](#separating-cabin-into-cabin_x-and-cabin_y)
- [exploring `cabin_x`](#exploring-cabin_x)
- [moving `cabin_x` level *T* to level *other* (low
  frequency)](#moving-cabin_x-level-t-to-level-other-low-frequency)
- [exploring `cabin_y`](#exploring-cabin_y)
- [exploring `destination`](#exploring-destination)
- [exploring `age`](#exploring-age)
- [replacing missing `age` values & exploring
  `age`](#replacing-missing-age-values-exploring-age)
- [exploring `vip`](#exploring-vip)
- [exploring `room_service`](#exploring-room_service)
- [exploring `food_court`](#exploring-food_court)
- [exploring `shopping_mall`](#exploring-shopping_mall)
- [exploring `spa`](#exploring-spa)
- [exploring `vr_deck`](#exploring-vr_deck)
- [separating `name` into `first_name` and
  `last_name`](#separating-name-into-first_name-and-last_name)
- [exploring `first_name`](#exploring-first_name)
- [exploring `last_name`](#exploring-last_name)
- [changing `transported` labels for
  modeling](#changing-transported-labels-for-modeling)
- [data splitting](#data-splitting)
- [creating preprocessing recipe](#creating-preprocessing-recipe)
- [creating prepped data and cross-validation
  folds](#creating-prepped-data-and-cross-validation-folds)
- [building model specification](#building-model-specification)
- [creating tuning parameters, grid, and modeling
  workflow](#creating-tuning-parameters-grid-and-modeling-workflow)
- [tuning the model](#tuning-the-model)
- [finalizing model](#finalizing-model)
- [fitting tuned model on all training
  data](#fitting-tuned-model-on-all-training-data)

### setup

``` r
library(tidyverse)
library(tidymodels)
library(tvthemes)
library(janitor)
library(patchwork)

theme_custom = theme_avatar() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(linewidth = 0.5, colour = "#D6D0C4"),
        panel.grid.minor = element_line(linewidth = 0.5, colour = "#D6D0C4"))

theme_set(theme_custom)
```

### data import

``` r
train = clean_names(read_csv("train.csv", col_types = cols()))
test = clean_names(read_csv("test.csv", col_types = cols()))
paste0("training data: ", nrow(train), " rows, ", ncol(train), " columns")
```

    ## [1] "training data: 8693 rows, 14 columns"

``` r
paste0("testing data: ", nrow(test), " rows, ", ncol(test), " columns")
```

    ## [1] "testing data: 4277 rows, 13 columns"

### finding missing data

``` r
train_NA = data.frame(names(train), colSums(is.na(train))) |>
  magrittr::set_colnames(c("var", "n")) |>
  ggplot(aes(reorder(var, n), n)) +
  geom_col(aes(fill = var), show.legend = F) +
  geom_text(aes(label = n), hjust = -0.25, size = 3) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "missing data in training data")

test_NA = data.frame(names(test), colSums(is.na(test))) |>
  magrittr::set_colnames(c("var", "n")) |>
  ggplot(aes(reorder(var, n), n)) +
  geom_col(aes(fill = var), show.legend = F) +
  geom_text(aes(label = n), hjust = -0.25, size = 3) +
  coord_flip() +
  labs(x = NULL, y = "missing data", title = "missing data in testing data")

train_NA / test_NA
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### visualizing where missing values occur

``` r
train_NA = data.frame(is.na(train)) |>
  mutate(id = 1:nrow(train)) |>
  pivot_longer(!id) |>
  ggplot(aes(name, id)) +
  geom_tile(aes(fill = value)) +
  scale_fill_manual(values = c("#A8C6A5", "#DA9090")) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "missing training data", fill = "missing") +
  theme(axis.text.x = element_blank())

test_NA = data.frame(is.na(test)) |>
  mutate(id = 1:nrow(test)) |>
  pivot_longer(!id) |>
  ggplot(aes(name, id)) +
  geom_tile(aes(fill = value)) +
  scale_fill_manual(values = c("#A8C6A5", "#DA9090")) +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  labs(x = NULL, y = NULL, title = "missing testing data", fill = "missing") +
  theme(axis.text.x = element_blank())

train_NA + test_NA
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
rm(train_NA, test_NA)
```

### exploring `home_planet`

``` r
train = train |>
  mutate(home_planet = ifelse(is.na(home_planet), "Unknown", home_planet),
         transported = ifelse(transported == T, 1, 0))

hp_counts = train |>
  count(home_planet) |>
  ggplot(aes(home_planet, n)) +
  geom_col(aes(fill = home_planet), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "home planet counts") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 5000))

hp_rates = train |>
  group_by(home_planet) |>
  summarise(pct = round(mean(transported), 3)) |>
  mutate(pct_lab = paste0(pct * 100, "%")) |>
  ggplot(aes(home_planet, pct)) +
  geom_col(aes(fill = home_planet), show.legend = F) +
  geom_text(aes(label = pct_lab), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "transport rate", title = "home planet transport rates") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.75))

hp_counts / hp_rates
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
rm(hp_counts, hp_rates)
```

### exploring `cryo_sleep`

``` r
train = train |>
  mutate(cryo_sleep = case_when(cryo_sleep == T ~ "cryo sleep",
                                cryo_sleep == F ~ "no cryo sleep",
                                is.na(cryo_sleep) ~ "unknown"))

cryo_count = train |>
  count(cryo_sleep) |>
  ggplot(aes(cryo_sleep, n)) +
  geom_col(aes(fill = cryo_sleep), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "counts of cryo sleep") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 6000))

cryo_rate = train |>
  group_by(cryo_sleep) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(cryo_sleep, pct)) +
  geom_col(aes(fill = cryo_sleep), show.legend = F) +
  geom_text(aes(label = lab), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "transport rate", title = "transport rate by cryo sleep status") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1))

cryo_count / cryo_rate
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
rm(cryo_count, cryo_rate)
```

### separating `cabin` into `cabin_x` and `cabin_y`

``` r
train = train |>
  separate(cabin, into = c("cabin1", "cabin2", "cabin3"), sep = "/") |>
  select(-cabin2) |>
  rename(cabin_x = cabin1, cabin_y = cabin3) |>
  mutate(cabin_x = ifelse(is.na(cabin_x), "other", cabin_x),
         cabin_y = ifelse(is.na(cabin_y), "other", cabin_y))

train |>
  select(cabin_x, cabin_y) |>
  sample_n(5)
```

    ## # A tibble: 5 ?? 2
    ##   cabin_x cabin_y
    ##   <chr>   <chr>  
    ## 1 F       P      
    ## 2 A       S      
    ## 3 C       P      
    ## 4 G       P      
    ## 5 B       S

### exploring `cabin_x`

``` r
cx_count = train |>
  count(cabin_x) |>
  ggplot(aes(cabin_x, n)) +
  geom_col(aes(fill = cabin_x), show.legend = F) +
  geom_text(aes(label = n), size = 3, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "counts of cabin_x") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 3000))

cx_rate = train |>
  group_by(cabin_x) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(cabin_x, pct)) +
  geom_col(aes(fill = cabin_x), show.legend = F) +
  geom_text(aes(label = lab), size = 3, vjust = -0.5) +
  labs(y = "transport rate", title = "transport rates by cabin_x") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.8))

cx_count / cx_rate
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
rm(cx_count, cx_rate)
```

### moving `cabin_x` level *T* to level *other* (low frequency)

``` r
train = train |>
  mutate(cabin_x = ifelse(cabin_x == "T", "other", cabin_x))

cx_count = train |>
  count(cabin_x) |>
  ggplot(aes(cabin_x, n)) +
  geom_col(aes(fill = cabin_x), show.legend = F) +
  geom_text(aes(label = n), size = 3, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "counts of cabin_x") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 3000))

cx_rate = train |>
  group_by(cabin_x) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(cabin_x, pct)) +
  geom_col(aes(fill = cabin_x), show.legend = F) +
  geom_text(aes(label = lab), size = 3, vjust = -0.5) +
  labs(y = "transport rate", title = "transport rates by cabin_x") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.8))

cx_count / cx_rate
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
rm(cx_count, cx_rate)
```

### exploring `cabin_y`

``` r
cy_count = train |>
  count(cabin_y) |>
  ggplot(aes(cabin_y, n)) +
  geom_col(aes(fill = cabin_y), show.legend = F) +
  geom_text(aes(label = n), size = 3, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "counts of cabin_y") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 4500))

cy_rate = train |>
  group_by(cabin_y) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(cabin_y, pct)) +
  geom_col(aes(fill = cabin_y), show.legend = F) +
  geom_text(aes(label = lab), size = 3, vjust = -0.5) +
  labs(y = "transport rate", title = "transport rates by cabin_y") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.7))

cy_count / cy_rate
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
rm(cy_count, cy_rate)
```

### exploring `destination`

``` r
train = train |>
  mutate(destination = ifelse(is.na(destination), "unknown", destination))

dest_count = train |>
  count(destination) |>
  ggplot(aes(destination, n)) +
  geom_col(aes(fill = destination), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  labs(x = "destination", y = "count", title = "destination counts") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 6500))

dest_rate = train |>
  group_by(destination) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(destination, pct)) +
  geom_col(aes(fill = destination), show.legend = F) +
  geom_text(aes(label = lab), size = 3.5, vjust = -0.5) +
  labs(y = "transport rate", title = "transport rates by destination") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.75))

dest_count / dest_rate
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
rm(dest_count, dest_rate)
```

### exploring `age`

``` r
train |>
  filter(!is.na(age)) |>
  ggplot(aes(age)) +
  geom_histogram(bins = 25, fill = "#A3BBA0", col = "black") +
  labs(x = "age", y = "count", title = "histogram of age",
       subtitle = "slightly right-skewed; will impute missing data with median") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic", vjust = 2.75))
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### replacing missing `age` values & exploring `age`

``` r
train = train |>
  mutate(age = ifelse(is.na(age), median(train$age, na.rm = T), age))

age_scat = train |>
  group_by(age) |>
  summarise(pct = sum(transported) / n(),
            n = n()) |>
  ggplot(aes(age, pct)) +
  geom_point(aes(size = n), col = "#A191A6") +
  labs(y = "transport rate", size = "count", title = "transport rate by age")

train = train |>
  mutate(age = cut_number(age, n = 5))

age_cut = train |>
  group_by(age) |>
  summarise(pct = round(sum(transported) / n(), 3),
            n = n()) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(age, pct)) +
  geom_col(aes(fill = age), show.legend = F) +
  geom_text(aes(label = lab), size = 3.5, vjust = -0.5) +
  labs(x = "age group", y = NULL, title = "transport rate by age group") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.7))

age_scat + age_cut
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
rm(age_scat, age_cut)
```

### exploring `vip`

``` r
train = train |>
  mutate(vip = case_when(vip == T ~ "vip",
                         vip == F ~ "non-vip",
                         is.na(vip) ~ "unknown")) |>
  mutate(vip = factor(vip, levels = c("vip", "non-vip", "unknown")))

vip_count = train |>
  count(vip) |>
  ggplot(aes(vip, n)) +
  geom_col(aes(fill = vip), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "vip status counts; heavily imbalanced") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 9000))

vip_rate = train |>
  group_by(vip) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(vip = factor(vip, levels = c("vip", "non-vip", "unknown")),
         lab = paste0(pct * 100, "%")) |>
  ggplot(aes(vip, pct)) +
  geom_col(aes(fill = vip), show.legend = F) +
  geom_text(aes(label = lab), size = 3.5, vjust = -0.5) +
  labs(x = "vip status", y = "transport rate", title = "transport rate by vip status") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.6))

vip_count / vip_rate
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
rm(vip_count, vip_rate)
```

### exploring `room_service`

``` r
nonzero_median = pull(summarise(filter(train, room_service > 0), med = median(room_service)), med)

train = train |>
  mutate(room_service = ifelse(is.na(room_service), 0, room_service),
         room_service = case_when(room_service == 0 ~ "no rs",
                              room_service > 0 & room_service <= nonzero_median ~ "less rs",
                              room_service > nonzero_median ~ "most rs"),
         room_service = factor(room_service, levels = c("no rs", "less rs", "most rs")))

rs_count = train |>
  count(room_service) |>
  ggplot(aes(room_service, n)) +
  geom_col(aes(fill = room_service), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "room service counts") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 6100))

rs_rate = train |>
  group_by(room_service) |>
  summarise(n = n(),
            pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(room_service, pct)) +
  geom_col(aes(fill = room_service), show.legend = F) +
  geom_text(aes(label = lab), size = 3.5, vjust = -0.5) +
  labs(x = "room service status", y = "transport rate", title = "transport rate by room service status") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.7))

rs_count / rs_rate
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
rm(rs_count, rs_rate)
```

### exploring `food_court`

``` r
nonzero_median = pull(summarise(filter(train, food_court > 0), med = median(food_court)), med)

train = train |>
  mutate(food_court = case_when(is.na(food_court) | food_court == 0 ~ "no fc",
                                food_court > 0 & food_court <= nonzero_median ~ "some fc",
                                food_court > nonzero_median ~ "most fc"),
         food_court = factor(food_court, levels = c("no fc", "some fc", "most fc")))

fc_count = train |>
  count(food_court) |>
  ggplot(aes(food_court, n)) +
  geom_col(aes(fill = food_court), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "food court counts") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 6000))

fc_rate = train |>
  group_by(food_court) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(food_court, pct)) +
  geom_col(aes(fill = food_court), show.legend = F) +
  geom_text(aes(label = lab), size = 3.5, vjust = -0.5) +
  labs(x = "food court", y = "transport rate", title = "transport rate by food court status") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.65))

fc_count / fc_rate
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
rm(fc_count, fc_rate)
```

### exploring `shopping_mall`

``` r
nonzero_median = pull(summarise(filter(train, shopping_mall > 0), med = median(shopping_mall), med))

train = train |>
  mutate(shopping_mall = replace_na(shopping_mall, 0),
         shopping_mall = case_when(shopping_mall == 0 ~ "no shopping",
                                shopping_mall > 0 & shopping_mall <= nonzero_median ~ "some shopping",
                                shopping_mall > nonzero_median ~ "most shopping"),
         shopping_mall = factor(shopping_mall, levels = c("no shopping", "some shopping", "most shopping")))

shop_count = train |>
  count(shopping_mall) |>
  ggplot(aes(shopping_mall, n)) +
  geom_col(aes(fill = shopping_mall), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "shopping mall counts") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 6200))

shop_rate = train |>
  group_by(shopping_mall) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(shopping_mall, pct)) +
  geom_col(aes(fill = shopping_mall), show.legend = F) +
  geom_text(aes(label = lab), size = 3.5, vjust = -0.5) +
  labs(x = "shopping group", y = "transport rate", title = "transport rate by shopping group") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.65))

shop_count / shop_rate
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
rm(shop_count, shop_rate)
```

### exploring `spa`

``` r
nonzero_median = pull(summarise(filter(train, spa > 0), med = median(spa), med))

train = train |>
  mutate(spa = case_when(is.na(spa) | spa == 0 ~ "no spa",
                         spa > 0 & spa <= nonzero_median ~ "some spa",
                         spa > nonzero_median ~ "most spa"),
         spa = factor(spa, levels = c("no spa", "some spa", "most spa")))

spa_count = train |>
  count(spa) |>
  ggplot(aes(spa, n)) +
  geom_col(aes(fill = spa), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "spa spend counts") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 6000))

spa_rate = train |>
  group_by(spa) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(spa, pct)) +
  geom_col(aes(fill = spa), show.legend = F) +
  geom_text(aes(label = lab), size = 3.5, vjust = -0.5) +
  labs(x = "spa spend", y = "transport rate", title = "transport rate by spa spend") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.7))

spa_count / spa_rate
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
rm(spa_count, spa_rate)
```

### exploring `vr_deck`

``` r
nonzero_median = pull(summarise(filter(train, vr_deck > 0), med = median(vr_deck), med))

train = train |>
  mutate(vr_deck = case_when(is.na(vr_deck) | vr_deck == 0 ~ "no vr",
                         vr_deck > 0 & vr_deck <= nonzero_median ~ "some vr",
                         vr_deck > nonzero_median ~ "most vr"),
         vr_deck = factor(vr_deck, levels = c("no vr", "some vr", "most vr")))

vr_deck_count = train |>
  count(vr_deck) |>
  ggplot(aes(vr_deck, n)) +
  geom_col(aes(fill = vr_deck), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "vr deck spend counts") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 6100))

vr_deck_rate = train |>
  group_by(vr_deck) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(vr_deck, pct)) +
  geom_col(aes(fill = vr_deck), show.legend = F) +
  geom_text(aes(label = lab), size = 3.5, vjust = -0.5) +
  labs(x = "vr deck spend", y = "transport rate", title = "transport rate by vr deck spend") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.7))

vr_deck_count / vr_deck_rate
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
rm(vr_deck_count, vr_deck_rate)
```

### separating `name` into `first_name` and `last_name`

``` r
train = train |>
  mutate(name = ifelse(is.na(name), "unknown unknown", name)) |>
  separate(name, into = c("first_name", "last_name"), sep = " ")

train |>
  select(first_name, last_name) |>
  sample_n(5)
```

    ## # A tibble: 5 ?? 2
    ##   first_name last_name 
    ##   <chr>      <chr>     
    ## 1 Heald      Johnsby   
    ## 2 Estina     Holcompson
    ## 3 Lynney     Chanan    
    ## 4 Antitak    Scerodbox 
    ## 5 Emmax      Baketton

### exploring `first_name`

``` r
first_name_classes = train |>
  group_by(first_name) |>
  summarise(n = n(),
            pct = round(sum(transported) / n(), 3)) |>
  mutate(class = case_when(n >= 3 & pct >= 0.5 ~ "high freq, high rate",
                           n >= 3 & pct < 0.5 ~ "high freq, low rate",
                           first_name == "unknown" | n < 3 ~ "low freq / unknown"),
         class = factor(class, levels = c("high freq, high rate", "high freq, low rate", "low freq / unknown"))) |>
  select(first_name, first_name_class = class)

train = train |>
  left_join(first_name_classes, by = "first_name") |>
  select(-first_name)

first_count = train |>
  count(first_name_class) |>
  ggplot(aes(first_name_class, n)) +
  geom_col(aes(fill = first_name_class), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "first name class counts") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 4500))

first_rate = train |>
  group_by(first_name_class) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(first_name_class, pct)) +
  geom_col(aes(fill = first_name_class), show.legend = F) +
  geom_text(aes(label = lab), size = 3.5, vjust = -0.5) +
  labs(x = "first name class", y = "transport rate", title = "transport rate by first name class") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.75))

first_count / first_rate
```

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
rm(first_name_classes, first_count, first_rate)
```

### exploring `last_name`

``` r
last_name_classes = train |>
  group_by(last_name) |>
  summarise(n = n(),
            pct = round(sum(transported) / n(), 3)) |>
  mutate(class = case_when(n >= 3 & pct >= 0.5 ~ "high freq / rate",
                           n >= 3 & pct < 0.5 ~ "high freq, low rate",
                           n < 3 | last_name == "unknown" ~ "low freq / unknown"),
         class = factor(class, levels = c("high freq / rate", "high freq, low rate", "low freq / unknown"))) |>
  select(last_name, last_name_class = class)

train = train |>
  left_join(last_name_classes, by = "last_name") |>
  select(-last_name)

last_count = train |>
  count(last_name_class) |>
  ggplot(aes(last_name_class, n)) +
  geom_col(aes(fill = last_name_class), show.legend = F) +
  geom_text(aes(label = n), size = 3.5, vjust = -0.5) +
  labs(x = NULL, y = "count", title = "last name class counts") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 4500))

last_rate = train |>
  group_by(last_name_class) |>
  summarise(pct = round(sum(transported) / n(), 3)) |>
  mutate(lab = paste0(pct * 100, "%")) |>
  ggplot(aes(last_name_class, pct)) +
  geom_col(aes(fill = last_name_class), show.legend = F) +
  geom_text(aes(label = lab), size = 3.5, vjust = -0.5) +
  labs(x = "last name class", y = "transport rate", title = "transport rate by last name class") +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 0.75))

last_count / last_rate
```

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
rm(last_name_classes, last_count, last_rate)
```

### changing `transported` labels for modeling

``` r
train = train |>
  mutate(transported = ifelse(transported == 1, "transported", "not transported"))

train |>
  count(transported)
```

    ## # A tibble: 2 ?? 2
    ##   transported         n
    ##   <chr>           <int>
    ## 1 not transported  4315
    ## 2 transported      4378

### data splitting

``` r
split = initial_split(sample_n(train, 500), prop = 0.75, strata = transported)
train_data = training(split)
test_data = testing(split)

paste0("training data: ", nrow(train_data), " rows, ", ncol(train_data), " columns")
```

    ## [1] "training data: 375 rows, 16 columns"

``` r
paste0("testing data: ", nrow(test_data), " rows, ", ncol(test_data), " columns")
```

    ## [1] "testing data: 125 rows, 16 columns"

### creating preprocessing recipe

``` r
pre_recipe = recipe(transported ~ ., data = train_data) |>
  update_role(passenger_id, new_role = "ID") |>
  step_string2factor(all_nominal()) |>
  prep()

pre_recipe
```

    ## Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##         ID          1
    ##    outcome          1
    ##  predictor         14
    ## 
    ## Training data contained 375 data points and no missing data.
    ## 
    ## Operations:
    ## 
    ## Factor variables from passenger_id, home_planet, cryo_sleep, cabin_x,... [trained]

### creating prepped data and cross-validation folds

``` r
prepped_data = bake(pre_recipe, new_data = train_data)
cv_folds = vfold_cv(prepped_data, v = 3)
cv_folds
```

    ## #  3-fold cross-validation 
    ## # A tibble: 3 ?? 2
    ##   splits            id   
    ##   <list>            <chr>
    ## 1 <split [250/125]> Fold1
    ## 2 <split [250/125]> Fold2
    ## 3 <split [250/125]> Fold3

### building model specification

``` r
xgb_model = boost_tree(trees = 1000, min_n = tune(), tree_depth = tune(),
                       learn_rate = tune(), loss_reduction = tune()) |>
  set_engine("xgboost", eval_metric = "logloss") |>
  set_mode("classification")

xgb_model
```

    ## Boosted Tree Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   trees = 1000
    ##   min_n = tune()
    ##   tree_depth = tune()
    ##   learn_rate = tune()
    ##   loss_reduction = tune()
    ## 
    ## Engine-Specific Arguments:
    ##   eval_metric = logloss
    ## 
    ## Computational engine: xgboost

### creating tuning parameters, grid, and modeling workflow

``` r
xgb_params = parameters(min_n(), tree_depth(), learn_rate(), loss_reduction())
xgb_grid = grid_max_entropy(xgb_params, size = 10)

xgb_wf = workflow() |>
  add_model(xgb_model) |>
  add_formula(transported ~ .)

xgb_wf
```

    ## ?????? Workflow ????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
    ## Preprocessor: Formula
    ## Model: boost_tree()
    ## 
    ## ?????? Preprocessor ????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
    ## transported ~ .
    ## 
    ## ?????? Model ?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
    ## Boosted Tree Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   trees = 1000
    ##   min_n = tune()
    ##   tree_depth = tune()
    ##   learn_rate = tune()
    ##   loss_reduction = tune()
    ## 
    ## Engine-Specific Arguments:
    ##   eval_metric = logloss
    ## 
    ## Computational engine: xgboost

### tuning the model

``` r
# doParallel::registerDoParallel()

xgb_tuned = tune_grid(object = xgb_wf, resamples = cv_folds, grid = xgb_grid,
                      metrics = metric_set(accuracy), control = control_grid(verbose = F))

xgb_tuned |>
  show_best("accuracy")
```

    ## # A tibble: 5 ?? 10
    ##   min_n tree_depth learn_r????? loss_????? .metric .esti?????  mean     n std_err .config
    ##   <int>      <int>     <dbl>   <dbl> <chr>   <chr>   <dbl> <int>   <dbl> <chr>  
    ## 1    19         11   3.34e-3 5.07e-7 accura??? binary  0.795     3 0.00706 Prepro???
    ## 2     9         12   3.45e-4 9.82e-2 accura??? binary  0.795     3 0.00267 Prepro???
    ## 3    11          3   8.47e-4 1.43e+0 accura??? binary  0.765     3 0.00706 Prepro???
    ## 4     6          2   1.73e-8 3.87e-8 accura??? binary  0.76      3 0.0122  Prepro???
    ## 5     3         10   2.44e-8 2.18e-9 accura??? binary  0.757     3 0.0141  Prepro???
    ## # ??? with abbreviated variable names ?????learn_rate, ?????loss_reduction, ?????.estimator

### finalizing model

``` r
xgb_best_params = xgb_tuned |>
  select_best("accuracy")

xgb_model_final = xgb_model |>
  finalize_model(xgb_best_params)

xgb_model_final
```

    ## Boosted Tree Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   trees = 1000
    ##   min_n = 19
    ##   tree_depth = 11
    ##   learn_rate = 0.00333691313099339
    ##   loss_reduction = 5.06656141520727e-07
    ## 
    ## Engine-Specific Arguments:
    ##   eval_metric = logloss
    ## 
    ## Computational engine: xgboost

### fitting tuned model on all training data

``` r
train_prediction = xgb_model_final |>
  fit(formula = transported ~ ., data = prepped_data) |>
  predict(new_data = prepped_data) |>
  bind_cols(prepped_data)

res = train_prediction |>
  count(.pred_class == transported) |>
  pull(n)

paste0("accuracy: ", round(res[2] / sum(res), 4) * 100, "%")
```

    ## [1] "accuracy: 83.2%"
