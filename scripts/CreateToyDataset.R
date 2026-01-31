library(dlexDB)
library(dplyr)
library(duckdb)

# 1. Connect to your local full DB
con <- dlex_connect()

# 2. Extract Top 1000 words (ordered by frequency)
# We collect into R memory (it's tiny)
toy_df <- tbl(con, "dlex") %>%
  arrange(desc(typposlem_freq_nor)) %>%
  head(1000) %>%
  collect()

# 3. Disconnect
DBI::dbDisconnect(con)

# 4. Save as Parquet file locally
# We use arrow here because it's the standard for writing parquet in R
# If you don't have it: install.packages("arrow")
arrow::write_parquet(toy_df, "dlex_de_demo_v1.parquet")

message("Toy dataset created: dlex_de_demo_v1.parquet")
