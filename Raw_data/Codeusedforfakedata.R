
df <-
  readxl::read_excel("./Raw_data/TrueSurvey_result.xlsx")

fake_df <- as.data.frame(lapply(df, sample))


View(fake_df)
library(writexl)
write_xlsx(fake_df, "Fake_Survey_FullyShuffled.xlsx")
View(fake_df)
