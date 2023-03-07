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
- [replacing missing `age` values & creating
  `age_group`](#replacing-missing-age-values-creating-age_group)
- [exploring `vip`](#exploring-vip)
- \[\]

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

    ## # A tibble: 5 x 2
    ##   cabin_x cabin_y
    ##   <chr>   <chr>  
    ## 1 G       P      
    ## 2 F       P      
    ## 3 D       S      
    ## 4 E       S      
    ## 5 C       P

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

### replacing missing `age` values & creating `age_group`

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

### xxx

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

### xxx

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

    ## # A tibble: 5 x 2
    ##   first_name last_name  
    ##   <chr>      <chr>      
    ## 1 Suhelik    Opshosent  
    ## 2 Oalls      Flate      
    ## 3 Vanley     Woodgezalez
    ## 4 Gleney     Carpennels 
    ## 5 Nelley     Freevenson

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

### basic glm

``` r
glm_split = initial_split(train, prop = 0.8)
traintrain = training(glm_split)
traintest = testing(glm_split)

mod = glm(transported ~ home_planet + cryo_sleep + cabin_x + cabin_y + destination + age +
                        vip + room_service + food_court + shopping_mall + spa + vr_deck,
          data = traintrain, family = "binomial")

res = traintest |>
  mutate(pred = predict(mod, traintest, type = "response"),
         pred_trans = ifelse(pred >= 0.5, 1, 0)) |>
  count(transported == pred_trans) |>
  pull(n)

acc = round(res[2] / sum(res), 4)
paste0("BASIC GLM ACCURACY: ", acc * 100, "%")
```

    ## [1] "BASIC GLM ACCURACY: 75.5%"
