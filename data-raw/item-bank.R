library(tidyverse)
library(purrrlyr)
library(gtools)

statements <-
  read_csv("data-raw/text.csv", col_types = cols()) %>%
  filter(!is.na(statement_id)) %>%
  select(- EN)

clips <- read_csv("data-raw/clips.csv", col_types = cols()) %>%
  rename(clip_file_name = file_name)

params <- read_csv("data-raw/params.csv", col_types = cols()) %>%
  select_all(~ gsub(".", "_", ., fixed = TRUE))


n <- 4L
categories <- seq_len(n)

category_counts <- clips %>% count(clip_category)
category_orders <- permutations(n = n, r = n, v = category_counts$clip_category)

clip_ids_by_category_id <- map(categories,
                               function(x) clips %>% filter(clip_category == x) %>% pull(clip_id))

unordered_items <- do.call(expand.grid, clip_ids_by_category_id)

category_orders <- permutations(n = n, r = n) %>%
  as.tibble() %>%
  set_names(sprintf("clip_%i_category_id", categories))

stopifnot(identical(clips$clip_id, seq_len(nrow(clips))))

ordered_items <- by_row(unordered_items, function(x) {
  clip_ids <- as.integer(x)
  df1 <- category_orders
  df2 <- apply(df1, 1, function(y) clip_ids[y]) %>%
    t %>% as.tibble %>% set_names(sprintf("clip_%i_id", categories))
  df3 <- apply(df2, 1, function(y) clips$clip_file_name[y]) %>%
    t %>% as.tibble %>% set_names(sprintf("clip_%i_file_name", categories))
  bind_cols(df1, df2, df3)
}, .labels = FALSE, .collate = "list") %>% extract2(1) %>% bind_rows()

# To prevent the combinatorial space from exploding,
# we randomly assign one target to each ordered
set.seed(1)
ordered_items_plus_target <- ordered_items %>%
  mutate(., target_position = sample(x = n, size = nrow(.), replace = TRUE))



  by_row(function(x) {
    tibble(target = seq_len(n),
           target_clip_id = map_int(target, function(i) x[[sprintf("clip_%i_id", i)]]),
           target_category_id = map_int(target, function(i) x[[sprintf("clip_%i_category_id", i)]]))
  }, .collate = "rows")
ordered_items_plus_target$.row <- NULL

clip_num <- nrow(clips)
clip_combinations <- expand.grid(clip_1 = 1:5,
                                 clip_2 = 1:3)

# One from each category
all_unordered_clip_sets


generate_items <- function(row) {
  target_category <- row$statement_category
  distract_categories <- setdiff(categories, target_category)
  # Find all eligible unordered clip sets

  # Find all potential orderings
}


params %>% by_row(generate_items, .collate = "rows")



devtools::use_data(item_bank, overwrite = TRUE)

