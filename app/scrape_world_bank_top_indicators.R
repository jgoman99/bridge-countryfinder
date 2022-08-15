library(rvest)

page <- read_html("https://data.worldbank.org/indicator")

top_indicators <- data.frame(page %>% html_elements('#main li a') %>% html_text())
colnames(top_indicators) <- c("indicator")
write.csv(top_indicators,"assets/world_bank_top_indicators.csv",row.names=FALSE)