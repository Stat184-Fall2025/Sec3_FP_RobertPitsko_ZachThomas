


install.packages("readxl")
library(readxl)
#data
PA_2010 <- read_excel("C:/Users/Thoma/OneDrive - Lycoming College/Documents/GitHub/Sec3_FP_RobertPitsko_ZachThomas/data/FederalFunds_2010Data_PA.xls")
PA_2009 <- read_excel("C:/Users/Thoma/OneDrive - Lycoming College/Documents/GitHub/Sec3_FP_RobertPitsko_ZachThomas/data/FederalFunds_2009Data_PA.xls")
PA_2008 <- read_excel("C:/Users/Thoma/OneDrive - Lycoming College/Documents/GitHub/Sec3_FP_RobertPitsko_ZachThomas/data/FederalFunds_2008Data_PA.xls")
PA_2007 <- read_excel("C:/Users/Thoma/OneDrive - Lycoming College/Documents/GitHub/Sec3_FP_RobertPitsko_ZachThomas/data/FederalFunds_2007Data_PA.xls")
PA_2006 <- read_excel("C:/Users/Thoma/OneDrive - Lycoming College/Documents/GitHub/Sec3_FP_RobertPitsko_ZachThomas/data/FederalFunds_2006Data_PA.xls")
PA_2005 <- read_excel("C:/Users/Thoma/OneDrive - Lycoming College/Documents/GitHub/Sec3_FP_RobertPitsko_ZachThomas/data/FederalFunds_2005Data_PA.xls")
PA_2004 <- read_excel("C:/Users/Thoma/OneDrive - Lycoming College/Documents/GitHub/Sec3_FP_RobertPitsko_ZachThomas/data/FederalFunds_2004Data_PA.xls")

#drop rows
PA_2010_clean <- PA_2010[1:10]
PA_2009_clean <- PA_2009[1:10]
PA_2008_clean <- PA_2008[1:10]
PA_2007_clean <- PA_2007[1:10]
PA_2006_clean <- PA_2006[1:10]
PA_2005_clean <- PA_2005[1:10]
PA_2004_clean <- PA_2004[1:10]

#grab columns
PA_2010_short <- PA_2010_clean[, c("ObjectCode","StateTotal")]
PA_2009_short <- PA_2009_clean[, c("ObjectCode","StateTotal")]
PA_2008_short <- PA_2008_clean[, c("ObjectCode","StateTotal")]
PA_2007_short <- PA_2007_clean[, c("ObjectCode","StateTotal")]
PA_2006_short <- PA_2006_clean[, c("ObjectCode","StateTotal")]
PA_2005_short <- PA_2005_clean[, c("ObjectCode","StateTotal")]
PA_2004_short <- PA_2004_clean[, c("ObjectCode","StateTotal")]


#add year column
PA_2010_short$Year <- 2010
PA_2009_short$Year <- 2009
PA_2008_short$Year <- 2008
PA_2007_short$Year <- 2007
PA_2006_short$Year <- 2006
PA_2005_short$Year <- 2005
PA_2004_short$Year <- 2004

#combine
PA_all_raw <- rbind(PA_2010_short,PA_2009_short, PA_2008_short,PA_2007_short, PA_2006_short,PA_2005_short, PA_2004_short)
PA_all <- PA_all_raw[-1,]

#plot
library(dplyr)
library(ggplot2)

PA_all$StateTotal <- as.numeric(PA_all$StateTotal)

library(dplyr)
library(ggplot2)

# Make a millions column
spending_totals <- spending_totals %>%
  mutate(TotalSpending_M = TotalSpending / 1e6)

spending_totals_clean <- spending_totals %>%
  filter(!is.na(ObjectCode))

code_labels <- c(
  GG = "Grants",
  DL = "Direct Loans",
  GL = "Guaranteed Loans",
  DX = "Direct Payments",
  DO = "Other Direct Payments",
  DR = "Retirement/Disability",
  PC = "Procurement Contracts",
  SW = "Salaries/Wages"
)

library(ggplot2)
library(scales)

Spending_OT <- ggplot(spending_totals_clean, 
                      aes(x = Year, 
                          y = TotalSpending, 
                          color = ObjectCode,
                          group = ObjectCode)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B")) +
  scale_color_manual(
    name = "Spending Type",
    values = scales::hue_pal()(length(code_labels)),  # auto colors
    labels = code_labels
  ) +
  labs(title = "Total Funding Over Time by Program Type",
       x = "Year",
       y = "Total Spending (Billions of $)") +
  theme_minimal()


Spending_OT
  



