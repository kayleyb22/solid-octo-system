# solid-octo-system

My name is Kayley Bayne, and I am learning to use GitHub.


```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

OECD_IMF <- read.csv("OECD_IMF.csv")

OECD_IMFlong <- structure(list(Country = structure(c(7L, 2L, 5L, 1L, 6L, 3L, 4L, 7L, 2L, 5L, 1L, 6L, 3L, 4L, 7L, 2L, 5L, 1L, 6L, 3L, 4L), .Label = c("Canada", "France", "Germany", "Italy", "Japan", "UK", "USA"), class = "factor"), GDP_Nom = c(6.7063e-05, 4.4062e-05, 4.345e-05, 4.845e-05, 4.3118e-05, 4.9617e-05, 3.4575e-05, 6.7063e-05, 4.4062e-05, 4.345e-05, 4.845e-05, 4.3118e-05, 4.9617e-05, 3.4575e-05, 6.7063e-05, 4.4062e-05, 4.345e-05, 4.845e-05, 4.3118e-05, 4.9617e-05, 3.4575e-05), measure = c("GDP_Nom_T", "GDP_Nom_T", "GDP_Nom_T", "GDP_Nom_T", "GDP_Nom_T", "GDP_Nom_T", "GDP_Nom_T", "GDP_PPP", "GDP_PPP", "GDP_PPP", "GDP_PPP", "GDP_PPP", "GDP_PPP", "GDP_PPP", "Aid_Perc", "Aid_Perc", "Aid_Perc", "Aid_Perc", "Aid_Perc", "Aid_Perc", "Aid_Perc"), value = c("$67,063.00", "$44,062.00", "$43,450.00", "$48,553.00", "$43,118.00", "$49,617.00", "$34,575.00", "$20.54", "$2.78", "$4.97", "$1.71", "$2.86", "$3.95", "$2.08", "0.1717", "0.0467", "0.2108", "0.2843", "0.4963", "0.374", "0.1869")), row.names = c(NA, -21L), class = "data.frame")


variable_names <- list("GDP_Nom_T" = "GDP Per Capita 2018 (USD Thousands)" ,
                       "GDP_PPP" = "GDP Purchasing Power Parity 2018 (USD Trillions)",
                       "Aid_Perc" = "Average % of Gross National Income Given in Foreign Aid (2000-2016)")

variable_labeller <- function(variable,value){
  return(variable_names[value])}


OECD_IMFlong_tidy <- OECD_IMFlong %>% 
  mutate(value1 = str_replace_all(value, "(,|\\$)", "")) %>% 
  mutate(value1 = as.numeric(value1))


g1 <- ggplot(OECD_IMFlong_tidy, aes(x=Country, y=value1, fill=Country))+
  geom_bar(stat='identity')+ 
  geom_text(aes(label=value), vjust=1.6, color="white", size=1.4)+ 
  facet_wrap(~measure, scales="free_y", ncol=1, labeller= variable_labeller)+ 
  labs(caption="Source: Official Development Assistance Database & IMF World Economic Outlook", size=11) + 
 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())
        


```

```{r, message=FALSE,echo=FALSE,warning=FALSE}
print(g1)
```
