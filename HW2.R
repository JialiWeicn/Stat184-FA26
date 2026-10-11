# Homework 2

library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)

# Problem 1: Clean, validate, and summarize

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

# Problem 1A

sales_clean <- sales_raw %>%
  mutate(
    item_clean = item_raw %>%
      str_trim() %>%
      str_replace_all("\\s+", " ") %>%
      str_to_lower(),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )

# Display requested columns.
print(sales_clean %>%
        select(sale_id, item_clean, amount, discount_rate, net_amount, size))

# Problem 1B

sales_codes <- sales_clean %>%
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

invalid_codes <- sales_codes %>%
  filter(!valid_code) %>%
  select(sale_id, code_raw, code_clean)

print(invalid_codes)

valid_code_parts <- sales_codes %>%
  filter(valid_code) %>%
  select(sale_id, code_clean) %>%
  separate_wider_delim(
    code_clean, delim = "-", names = c("channel", "number")
  ) %>%
  select(sale_id, channel, number)

print(valid_code_parts)

sale_tags <- sales_clean %>%
  select(sale_id, tags) %>%
  separate_longer_delim(tags, delim = ";")

print(sale_tags)

# separate_wider_delim() splits each input row into multiple columns;
# separate_longer_delim() splits delimited values into multiple rows.

# Problem 1C

customer_summary <- sales_clean %>%
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if (sum(!is.na(net_amount)) == 0) {
      NA_real_
    } else {
      sum(net_amount >= 20, na.rm = TRUE) / sum(!is.na(net_amount))
    },
    .by = customer_id
  ) %>%
  arrange(customer_id)

print(customer_summary)

# C1: numerator = number of C1 sales with known net_amount >= 20 (0);
# denominator = number of C1 sales with a known net_amount (1).
# Therefore C1's large_share is 0 / 1 = 0.

sales_factors <- sales_clean %>%
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(factor(item_clean), n = 2,
                            other_level = "Other")
  )

# Display levels and category counts for item_factor.
print(levels(sales_factors$item_factor))
print(sales_factors %>% count(item_factor, .drop = FALSE))

# Display levels and category counts for item_group.
print(levels(sales_factors$item_group))
print(sales_factors %>% count(item_group, .drop = FALSE))

# fct_infreq() reorders factor levels by frequency; it does not merge categories.
# fct_lump_n() keeps the n most frequent categories and combines the rest as Other.

# Problem 2: Missing Values and Joins

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# Problem 2A

reports_labeled <- reports_raw %>%
  arrange(form, line) %>%
  group_by(form) %>%
  fill(customer_id, .direction = "down") %>%
  ungroup()

reports_clean <- reports_labeled %>%
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

print(reports_clean %>%
        select(form, line, customer_id, round,
               primary_amount, backup_amount, amount))

# coalesce() uses the first non-missing value. A recorded primary amount,
# including zero, is not missing, so the backup is used only if primary is NA.

# Mark original rows before complete() adds absent customer-round combinations.
reports_marked <- reports_clean %>%
  select(customer_id, round, amount) %>%
  mutate(recorded_row = TRUE)

reports_complete <- reports_marked %>%
  complete(customer_id, round = 1:3,
           fill = list(recorded_row = FALSE)) %>%
  mutate(recorded_row = coalesce(recorded_row, FALSE)) %>%
  arrange(customer_id, round)

# Counts before completion.
before_n_rows <- nrow(reports_marked)
before_added_rows <- 0L
before_missing_amounts <- sum(is.na(reports_marked$amount))

# Counts after completion.
after_n_rows <- nrow(reports_complete)
after_added_rows <- sum(!reports_complete$recorded_row)
after_missing_amounts <- sum(is.na(reports_complete$amount))

completion_counts <- tibble(
  stage = c("Before completion", "After completion"),
  rows = c(before_n_rows, after_n_rows),
  added_rows = c(before_added_rows, after_added_rows),
  missing_amounts = c(before_missing_amounts, after_missing_amounts)
)
print(completion_counts)

# Explicit missing amount before completion: customer C3, round 1 (amount NA).
# Implicit absent customer-round before completion: C1, round 2 (no row).

# Problem 2B

# Check customer metadata IDs.
customer_id_check <- tibble(
  has_missing_id = any(is.na(customers$customer_id) |
                         str_trim(customers$customer_id) == ""),
  has_duplicate_id = anyDuplicated(customers$customer_id) > 0
)
print(customer_id_check)

# Check duplicate customer_id-round combinations in reports_clean.
duplicate_report_keys <- reports_clean %>%
  count(customer_id, round, name = "n") %>%
  filter(n > 1)
print(duplicate_report_keys)

customers_marked <- customers %>%
  mutate(customer_found = TRUE)

reports_joined <- reports_clean %>%
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(customer_found = coalesce(customer_found, FALSE))

print(reports_joined %>%
        select(customer_id, round, amount, region, customer_found))

reports_only_ids <- reports_clean %>%
  distinct(customer_id) %>%
  anti_join(
    customers %>% distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )
customers_only_ids <- customers %>%
  distinct(customer_id) %>%
  anti_join(
    reports_clean %>% distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )

print(reports_only_ids)
print(customers_only_ids)

# C3 has customer_found = TRUE despite region being NA: its metadata record
# exists, but its region is unknown. C4 has customer_found = FALSE: no matching
# customer metadata record exists, so its region is also NA for a different reason.

# ----------------------------
# Problem 2C
# ----------------------------

# The supplied PDF references customers_bad but does not define it.
# This illustrative table includes a duplicated C2 record (which duplicates
# a known amount) and conflicting C3 metadata. Replace this table if your
# instructor supplied a specific customers_bad dataset.
customers_bad <- tibble(
  customer_id = c("C1", "C2", "C2", "C3", "C3", "C5"),
  region = c("North", "South", "East", NA, "Central", "West")
)

reports_duplicated <- reports_clean %>%
  left_join(
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-many",
    na_matches = "never"
  )

before_join_comparison <- tibble(
  stage = "reports_clean",
  rows = nrow(reports_clean),
  total_amount = sum(reports_clean$amount, na.rm = TRUE)
)
after_join_comparison <- tibble(
  stage = "reports_duplicated",
  rows = nrow(reports_duplicated),
  total_amount = sum(reports_duplicated$amount, na.rm = TRUE)
)
print(bind_rows(before_join_comparison, after_join_comparison))

# The repeated known-amount report is customer C2, round 1; its amount is
# counted twice because C2 has two matching metadata rows. The C3, round 1
# report is also duplicated, but its amount is NA and is ignored in the total.
# With relationship = "many-to-one", the join errors because C2 and C3 have
# multiple matches. Allowing many-to-many permits the join but does not resolve
# the conflicting metadata records.

reports_wide <- reports_complete %>%
  select(customer_id, round, amount) %>%
  mutate(round = paste0("round_", round)) %>%
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_expand = TRUE
  ) %>%
  arrange(customer_id)

print(reports_wide)

# If pivoted back to long form with values_drop_na = TRUE, only rows with
# non-missing amounts remain. The number is calculated explicitly below.
reports_long_nonmissing <- reports_wide %>%
  pivot_longer(
    cols = starts_with("round_"),
    names_to = "round",
    values_to = "amount",
    values_drop_na = TRUE
  )
cat("Rows after pivoting back with values_drop_na = TRUE:",
    nrow(reports_long_nonmissing), "\n")

# Dropping NA values removes unknown amounts; it cannot recover them.

# Problem 3: Two-sample t-tests and simulation

# Problem 3A

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  nx <- length(x)
  ny <- length(y)
  x_bar <- mean(x)
  y_bar <- mean(y)
  s2x <- var(x)
  s2y <- var(y)
  
  if (equal_var) {
    sp2 <- ((nx - 1) * s2x + (ny - 1) * s2y) / (nx + ny - 2)
    se <- sqrt(sp2 * (1 / nx + 1 / ny))
    df <- nx + ny - 2
    method <- "Pooled"
  } else {
    se <- sqrt(s2x / nx + s2y / ny)
    df <- (s2x / nx + s2y / ny)^2 /
      ((s2x / nx)^2 / (nx - 1) + (s2y / ny)^2 / (ny - 1))
    method <- "Welch"
  }
  
  t_stat <- (x_bar - y_bar) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  
  list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = p_value < alpha
  )
}

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

welch_result <- two_sample_t(x_check, y_check)
pooled_result <- two_sample_t(x_check, y_check, equal_var = TRUE)

print(welch_result)
print(pooled_result)

welch_reference <- t.test(x_check, y_check, var.equal = FALSE)
pooled_reference <- t.test(x_check, y_check, var.equal = TRUE)

verification <- tibble(
  method = c("Welch", "Pooled"),
  t_diff = c(
    abs(welch_result$t_stat - unname(welch_reference$statistic)),
    abs(pooled_result$t_stat - unname(pooled_reference$statistic))
  ),
  df_diff = c(
    abs(welch_result$df - unname(welch_reference$parameter)),
    abs(pooled_result$df - unname(pooled_reference$parameter))
  ),
  p_value_diff = c(
    abs(welch_result$p_value - welch_reference$p.value),
    abs(pooled_result$p_value - pooled_reference$p.value)
  )
) %>%
  mutate(all_differences_below_1e_8 =
           t_diff < 1e-8 & df_diff < 1e-8 & p_value_diff < 1e-8)

print(verification)

# t_stat, df, and p_value should agree with t.test() to within 1e-8.

# Problem 3B

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

# Set the seed once immediately before the simulation loops.
set.seed(2026)

# Two methods x B repetitions x four scenarios = 8000 rows.
simulation_results <- tibble(
  scenario = character(2 * B * nrow(scenarios)),
  replicate = integer(2 * B * nrow(scenarios)),
  method = character(2 * B * nrow(scenarios)),
  t_stat = numeric(2 * B * nrow(scenarios)),
  df = numeric(2 * B * nrow(scenarios)),
  p_value = numeric(2 * B * nrow(scenarios)),
  reject = logical(2 * B * nrow(scenarios))
)

row_i <- 1L
for (i in seq_len(nrow(scenarios))) {
  for (b in seq_len(B)) {
    x <- rnorm(n_x, mean = scenarios$mean_x[i], sd = scenarios$sd_x[i])
    y <- rnorm(n_y, mean = scenarios$mean_y[i], sd = scenarios$sd_y[i])
    
    result_welch <- two_sample_t(x, y, equal_var = FALSE, alpha = alpha)
    result_pooled <- two_sample_t(x, y, equal_var = TRUE, alpha = alpha)
    
    results_this_rep <- list(result_welch, result_pooled)
    for (result in results_this_rep) {
      simulation_results[row_i, ] <- tibble(
        scenario = scenarios$scenario[i],
        replicate = b,
        method = result$method,
        t_stat = result$t_stat,
        df = result$df,
        p_value = result$p_value,
        reject = result$reject
      )
      row_i <- row_i + 1L
    }
  }
}

print(head(simulation_results, 6))
cat("Total simulation rows:", nrow(simulation_results), "\n")

simulation_summary <- simulation_results %>%
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) %>%
  mutate(
    mean_t = round(mean_t, 3),
    sd_t = round(sd_t, 3),
    rejection_rate = round(rejection_rate, 3)
  ) %>%
  arrange(match(scenario, scenarios$scenario), method)

print(simulation_summary)

# Optional plot of rejection rates by scenario and method.
simulation_summary_plot <- simulation_results %>%
  summarise(
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  )

barplot(
  height = matrix(
    simulation_summary_plot$rejection_rate,
    nrow = length(unique(simulation_summary_plot$scenario)),
    byrow = TRUE,
    dimnames = list(
      unique(simulation_summary_plot$scenario),
      unique(simulation_summary_plot$method)
    )
  ),
  beside = TRUE,
  legend.text = TRUE,
  ylab = "Rejection rate",
  main = "Rejection rates by scenario and test",
  ylim = c(0, 1)
)

# Short report (simulation rates vary slightly from run to run):
# Under both null scenarios, rejection rates should be near 0.05; a substantial
# departure indicates possible Type I error inflation or conservatism, especially
# when population variances differ and sample sizes are unequal. When the means
# differ by one, rejection rates increase, reflecting greater power to detect
# the difference. Higher power alone is not enough to judge a test because a
# test that rejects too often under the null can appear powerful while having
# an inflated Type I error rate.
