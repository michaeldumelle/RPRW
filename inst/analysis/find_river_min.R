# find some summary statistics on rivers with pattern "C"
library(RPRW)

## find minimums
river_min <- river_stats(river, "C", min)

min_df <- data.frame(
  data = "river",
  pattern = "C",
  length_min = river_min[[1]],
  discharge_min = river_min[[2]]
)

write.csv(min_df, "inst/output/min_df", row.names = FALSE)
