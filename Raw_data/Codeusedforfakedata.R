
df <-
  readxl::read_excel("./Raw_data/Survey_result.xlsx")

fake_df <- as.data.frame(lapply(df, sample))

colnames(fake_df) <-
  colnames(df)

library(writexl)
writexl::write_xlsx(fake_df, "Fake_Survey_FullyShuffled.xlsx")
