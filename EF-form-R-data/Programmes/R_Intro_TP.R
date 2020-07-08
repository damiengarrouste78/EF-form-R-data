# Annexe: TP de synthèse ==================
# segmentation RFM
setwd("C:/Users/dgr/OneDrive - Publicis Groupe/Documents/Formations/R/Intro_R/")

library(tidyverse)

# import de données
customers <- read_delim("customers.txt",delim = "|")
orders <- read_delim("orders.txt",delim = "|")

rfm_data <- left_join(orders,customers)
rm(customers, orders)

# aperçu des données
str(rfm_data)
glimpse(rfm_data)
summary(rfm_data)

# dataviz
#esquisse::esquisser(rfm_data)

# série des ventes par jour
days_data <- rfm_data %>% group_by(order_date) %>% summarise(daily = sum(revenue))
esquisse::esquisser(days_data)

end_date <- days_data %>%
    summarise(max_date = max(order_date)) %>%
    t() %>% as.character()
start_date <- days_data %>%
    summarise(min_date = min(order_date)) %>%
    t() %>% as.character()

boxplot(days_data$order_date)
min(days_data$order_date)
rfm_data <- rfm_data %>%
    filter(order_date > min(order_date))
min(rfm_data$order_date)
days_data <- rfm_data %>% group_by(order_date) %>% summarise(daily = sum(revenue))
esquisse::esquisser(days_data)


ggplot(days_data) +
 aes(x = order_date, y = daily) +
 geom_line(size = 1.54, colour = "#1f9e89") +
 labs(x = "date de commande", y = "montant des ventes", title = "Ventes journalières") +
    ggthemes::theme_fivethirtyeight()


# série des ventes par client
customers_sales <- rfm_data %>% group_by(customer_id) %>% summarise(sales = sum(revenue))
#esquisse::esquisser(customers_sales)


ggplot(customers_sales) +
 aes(x = sales) +
 geom_density(adjust = 1L, fill = "#0c4c8a") +
 labs(title = "Distribution des ventes (en valeur)") +
    ggthemes::theme_fivethirtyeight()


# indicateurs RFM

recency <- rfm_data %>%
    group_by(customer_id) %>%
    summarise(recency = as.numeric(as.Date(end_date)-max(order_date)))

frequency <- rfm_data %>%
    group_by(customer_id) %>%
    summarise(frequency = n())

monetary <- rfm_data %>%
    group_by(customer_id) %>%
    summarise(value = mean(revenue))

indicateurs <- left_join(recency,frequency) %>%
    left_join(.,monetary)

#questionr::icut(indicateurs)


## Recodage de indicateurs$recency en indicateurs$recency_rec
indicateurs$recency_rec <- cut(indicateurs$recency,
                               include.lowest = TRUE,
                               right = TRUE,
                               breaks = c(-Inf, 160, 380, Inf)
)
indicateurs$recency_rec <- as.numeric(indicateurs$recency_rec)

## Recodage de indicateurs$frequency en indicateurs$frequency_rec
indicateurs$frequency_rec <- cut(indicateurs$frequency,
                                 include.lowest = TRUE,
                                 right = TRUE,
                                 breaks = c(-Inf, 4, 6, Inf)
)
indicateurs$frequency_rec <- as.numeric(indicateurs$frequency_rec)


## Recodage de indicateurs$value en indicateurs$value_rec
indicateurs$value_rec <- cut(indicateurs$value,
                             include.lowest = TRUE,
                             right = TRUE,
                             breaks = c(-Inf, 83, 105, Inf)
)
indicateurs$value_rec <- as.numeric(indicateurs$value_rec)

# calcul indicateur RFM global
indicateurs <- indicateurs %>%
    mutate(rfm = ceiling((recency_rec + frequency_rec + value_rec) / 3))

# portrait robot des segments
(portrait <- indicateurs %>% group_by(rfm) %>%
    summarise(min_recency = min(recency),
              median_recency = median(recency),
              max_recency = max(recency),
              min_frequency = min(frequency),
              median_frequency = median(frequency),
              max_frequency = max(frequency),
              min_monetary = min(value),
              median_monetary = median(value),
              max_monetary = max(value)))

# concentration de la valeur par segment
indicateurs %>% group_by(rfm) %>%
    summarise(avg_value=sum(value),
              n=n()) %>%
    mutate(total_value = avg_value*n) %>%
    mutate(pct_total_value = 100*total_value / sum(total_value),
           pct_n = 100*n/sum(n))


# dataviz indicateurs

# boxplot recency
ggplot(indicateurs) +
 aes(x = "", y = recency, group = rfm) +
 geom_boxplot(fill = "#bd3786") +
 labs(title = "Recency par segment") +
 ggthemes::theme_fivethirtyeight()

# boxplot frequency
ggplot(indicateurs) +
    aes(x = "", y = frequency, group = rfm) +
    geom_boxplot(fill = "#bd3786") +
    labs(title = "Frequency par segment") +
    ggthemes::theme_fivethirtyeight()

# boxplot value
ggplot(indicateurs) +
    aes(x = "", y = value, group = rfm) +
    geom_boxplot(fill = "#bd3786") +
    labs(title = "Value par segment") +
    ggthemes::theme_fivethirtyeight()

# heatmap recency * frequency * value
ggplot(indicateurs) +
    aes(x = recency_rec, y = frequency_rec, fill = value) +
    geom_tile(size = 1L) +
    scale_fill_distiller(palette = "Greens") +
    theme_minimal()











