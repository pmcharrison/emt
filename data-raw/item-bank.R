library(tidyverse)
library(purrrlyr)
library(gtools)

statements <-
  read_csv("data-raw/text.csv", col_types = cols()) %>%
  filter(!is.na(statement_id)) %>%
  select(- EN)

clips <- read_csv("data-raw/clips.csv", col_types = cols()) %>%
  rename(clip_file_name = file_name) %>%
  mutate(clip_file_name = paste(clip_file_name, "mp3", sep = "."))

params <- read_csv("data-raw/params.csv", col_types = cols()) %>%
  select_all(~ gsub(".", "_", ., fixed = TRUE))


n <- 4L
categories <- seq_len(n)

category_counts <- clips %>% count(clip_category)
category_orders <- permutations(n = n, r = n, v = category_counts$clip_category)

clip_ids_by_category_id <- map(
  categories,
  function(x) clips %>% filter(clip_category == x) %>% pull(clip_id))

statement_ids_by_category_id <- map(
  categories,
  function(x) statements %>% filter(statement_category == x) %>% pull(statement_id))

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
# we randomly assign one target to each clip ordering
set.seed(1)
items <- ordered_items %>%
  mutate(.,
         target_position = sample(x = n, size = nrow(.), replace = TRUE),
         target_clip_id = NA,
         target_category_id = NA,
         statement_id = NA,
         statement_dict_id = NA)
for (i in seq_len(nrow(items))) {
  pos <- items$target_position[i]

  target_category_id <- items[[sprintf("clip_%i_category_id", pos)]][i]
  target_clip_id <- items[[sprintf("clip_%i_id", pos)]][i]

  items$target_category_id[i] <- target_category_id
  items$target_clip_id[i] <- target_clip_id

  statement_id <- sample(x = statement_ids_by_category_id[[target_category_id]],
                         size = 1)
  items$statement_id[i] <- statement_id
  items$statement_dict_id[i] <- statements$dict_id[statements$statement_id == statement_id]
}

item_bank <-
  left_join(x = items,
            y = rename(params,
                       target_category_id = statement_category),
            by = c("target_category_id", "statement_id")) %>%
  na.omit() %>%
  add_column(., item_id = seq_len(nrow(.)), .before = 1) %>%
  mutate(answer = sprintf("clip_%i", target_position)) %>%
  as.data.frame()

devtools::use_data(item_bank, overwrite = TRUE)
