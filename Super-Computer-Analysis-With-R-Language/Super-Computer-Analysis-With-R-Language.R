library(tidyverse)

library(readxl)


# In this part, the data obtained from [1] is taken from the Excel file.
ham_veri <- read_excel("/Users/cenkatlig/Super_Bilgisayar.xlsx") %>% tbl_df()

# Then making the raw data smoother
# It is provided to write separately on each line (originally, systems would sell 2 series)
ara_veri <-
  ham_veri %>%
  slice(seq(1,nrow(.),by=2)) %>%
  cbind(.,ham_veri %>% slice(seq(2,nrow(.),by=2)) %>% select(Country=Site,Company=System)) %>% mutate_at(vars(Cores:`Power (kW)`),funs(as.numeric(gsub(",","",.)))) %>% tbl_df()

# ara_veri
# ggplot(ara_veri, aes(x = Site, y = Country)) + geom_point()
# country_list <- ara_veri %>% filter(Country == "United States" & Rank < 100 )
# ggplot(country_list, aes(x = Rank, y = Cores)) + geom_point()


ggplot(ara_veri,aes(x=Country)) + geom_bar() + theme(axis.text.x = element_text(angle=90,hjust = 1))

company_list <- ara_veri %>% filter(Company == "IBM" & Rank < 100 )

ggplot(company_list, aes(x=Company)) + geom_bar() # + theme(axis.text.x = element_text(angle=90,hjust = 1))

# Number of top 100 systems from the USA
country_list_100_US <- ara_veri %>% filter(Country == "United States" & Rank < 10 )

ggplot(country_list_100_US, aes(x = System)) + geom_bar(fill = "white", colour = "green") + theme(axis.text.x = element_text(angle=30, hjust = 1))

# theme_stata()
# Number of systems by non-Chinese, Japanese, US companies
Non_CN_JP_US <- ara_veri %>% filter(Country != "Japan" & Country != "United States" & Country != "China")

ggplot(Non_CN_JP_US, aes(x=Company)) + geom_bar(fill = "white", colour = "red") + theme(axis.text.x = element_text(angle=45,hjust = 1))  +
  ggtitle("Number of systems by non-Chinese, Japanese, US companies")


minimum_core = min(ara_veri$Cores)
max_core = max(ara_veri$Cores)

message("Minimum CORE (Core) NUMBER: ", minimum_core)
## Minimum CORE (Core) NUMBER: 9.792
message("Maximum CORE (Core) NUMBER: ", max_core)
##Maximum CORE (Core) NUMBER: 19860000
minimum_enerji = min(ara_veri$`Power (kW)`,na.rm=TRUE)
max_enerji = max(ara_veri$`Power (kW)`,na.rm=TRUE)

message("THE CONSUMPTION OF THE ENERGY CONSUMER SYSTEM (kW): ", minimum_enerji)
## THE CONSUMPTION OF THE ENERGY CONSUMER SYSTEM(kW): 1
message("En fazla ENERJi TUKETEN SiSTEMiN TUKETiM MiKTARI (kW): ", max_enerji)
## CONSUMPTION AMOUNT OF THE MOST ENERGY CONSUMING SYSTEM (kW): 997
