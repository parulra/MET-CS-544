library(quantmod)
library(gridExtra)
library(Hmisc)
library(plotly)
library(sampling)
library(ggplotlyExtra)
library(ggplot2)
library(tidyverse)
options(scipen=999)

cd = read.csv("C:/Users/adety/Desktop/Courses/544/Project/consolidated_coin_data.csv")

# Initial view of the data

head(cd)
tail(cd)
summary(cd)

# summary function revels that all features are saved as of character type. 
# converting each feature to its appropriate format.
# removing comma's and then converting to numeric as otherwise numeric introduces 
# NA's.
# Add new feature year

cd$Date = as.Date(cd$Date, '%b %d, %Y')
cd$Open = as.numeric(gsub(pattern = ',','',cd$Open))
cd$High = as.numeric(gsub(pattern = ',','',cd$High))
cd$Low = as.numeric(gsub(pattern = ',','',cd$Low))
cd$Close = as.numeric(gsub(pattern = ',','',cd$Close))
cd$Volume = as.numeric(gsub(pattern = ',','',cd$Volume))
cd$Market.Cap = as.numeric(gsub(pattern = ',','',cd$Market.Cap))
cd$Currency = capitalize(cd$Currency)
cd$year = as.numeric(format(cd$Date,"%Y"))

# Checking data again

summary(cd)

# checking which currencies are in the data set.

plot_ly(as.data.frame(cd$Currency),labels = cd$Currency,type = 'pie',hole = 0.6) %>%
  layout(title = "Currency Representation in Dataset",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
 
# Creating subsets of each currency for easier analyses
# Also dropping the name cols for individual currencies and Changing Names

Bitcoin_sv = subset(cd,cd$Currency == 'Bitcoin-sv')
Bitcoin_sv$Currency = NULL
colnames(Bitcoin_sv) = c('Bitcoin_svDate','Bitcoin_svOpen','Bitcoin_svHigh',
                         'Bitcoin_svLow','Bitcoin_svClose','Bitcoin_svVolume',
                         'Bitcoin_svMarket.Cap')
Bitcoin_sv = xts(Bitcoin_sv[,-1], order.by=Bitcoin_sv[,1])

Bitcoin_cash = subset(cd,cd$Currency == 'Bitcoin-cash')
Bitcoin_cash$Currency = NULL
colnames(Bitcoin_cash) = c('Bitcoin_cashDate','Bitcoin_cashOpen','Bitcoin_cashHigh',
                         'Bitcoin_cashLow','Bitcoin_cashClose','Bitcoin_cashVolume',
                         'Bitcoin_cashMarket.Cap')
Bitcoin_cash = xts(Bitcoin_cash[,-1], order.by=Bitcoin_cash[,1])

Bitcoin = subset(cd,cd$Currency == 'Bitcoin')
Bitcoin$Currency = NULL
colnames(Bitcoin) = c('BitcoinDate','BitcoinOpen','BitcoinHigh',
                           'BitcoinLow','BitcoinClose','BitcoinVolume',
                           'BitcoinMarket.Cap')
Bitcoin = xts(Bitcoin[,-1], order.by=Bitcoin[,1])

Cardano = subset(cd,cd$Currency == 'Cardano')
Cardano$Currency = NULL
colnames(Cardano) = c('CardanoDate','CardanoOpen','CardanoHigh',
                      'CardanoLow','CardanoClose','CardanoVolume',
                      'CardanoMarket.Cap')
Cardano = xts(Cardano[,-1], order.by=Cardano[,1])

Eos = subset(cd,cd$Currency == 'Eos')
Eos$Currency = NULL
colnames(Eos) = c('EosDate','EosOpen','EosHigh',
                      'EosLow','EosClose','EosVolume',
                      'EosMarket.Cap')
Eos = xts(Eos[,-1], order.by=Eos[,1])

Ethereum = subset(cd,cd$Currency == 'Ethereum')
Ethereum$Currency = NULL
colnames(Ethereum) = c('EthereumDate','EthereumOpen','EthereumHigh',
                  'EthereumLow','EthereumClose','EthereumVolume',
                  'EthereumMarket.Cap')
Ethereum = xts(Ethereum[,-1], order.by=Ethereum[,1])

Litecoin = subset(cd,cd$Currency == 'Litecoin')
Litecoin$Currency = NULL
colnames(Litecoin) = c('LitecoinDate','LitecoinOpen','LitecoinHigh',
                       'LitecoinLow','LitecoinClose','LitecoinVolume',
                       'LitecoinMarket.Cap')
Litecoin = xts(Litecoin[,-1], order.by=Litecoin[,1])

Stellar = subset(cd,cd$Currency == 'Stellar')
Stellar$Currency = NULL
colnames(Stellar) = c('StellarDate','StellarOpen','StellarHigh',
                       'StellarLow','StellarClose','StellarVolume',
                       'StellarMarket.Cap')
Stellar = xts(Stellar[,-1], order.by=Stellar[,1])

Tether = subset(cd,cd$Currency == 'Tether')
Tether$Currency = NULL
colnames(Tether) = c('TetherDate','TetherOpen','TetherHigh',
                      'TetherLow','TetherClose','TetherVolume',
                      'TetherMarket.Cap')
Tether = xts(Tether[,-1], order.by=Tether[,1])

Tezos = subset(cd,cd$Currency == 'Tezos')
Tezos$Currency = NULL
colnames(Tezos) = c('TezosDate','TezosOpen','TezosHigh',
                     'TezosLow','TezosClose','TezosVolume',
                     'TezosMarket.Cap')
Tezos = xts(Tezos[,-1], order.by=Tezos[,1])

Xrp = subset(cd,cd$Currency == 'Xrp')
Xrp$Currency = NULL
colnames(Xrp) = c('XrpDate','XrpOpen','XrpHigh',
                    'XrpLow','XrpClose','XrpVolume',
                    'XrpMarket.Cap')
Xrp = xts(Xrp[,-1], order.by=Xrp[,1])

Binance_coin = subset(cd,cd$Currency == 'Binance-coin')
Binance_coin$Currency = NULL
colnames(Binance_coin) = c('Binance_coinDate','Binance_coinOpen','Binance_coinHigh',
                  'Binance_coinLow','Binance_coinClose','Binance_coinVolume',
                  'Binance_coinMarket.Cap')
Binance_coin = xts(Binance_coin[,-1], order.by=Binance_coin[,1])

# Plot opening prices of all the currencies.

ggplot(Bitcoin, aes(x = index(Bitcoin),y = Bitcoin[,1]/100,color = 'Bitcoin')) + 
  geom_line() + 
  ggtitle("All Crypto Opening price series") + 
  theme(legend.position = "top") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(
    x = "Date",
    y = "Opening Prices",
    color = "Currencies",
    caption = "Adi") +
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.key.width = unit(2,"cm")) +
  geom_line(aes(y = Xrp[,1],color = 'Xrp')) +
  geom_line(aes(y = Cardano[,1],,color = 'Cardano')) + 
  geom_line(aes(y = Tezos[,1],color = 'Tezos')) +
  geom_line(aes(y = Binance_coin[,1],color = 'Binance_coin')) +
  geom_line(aes(y = Eos[,1],color = 'Eos')) +
  geom_line(aes(y = Tether[,1],color = 'Tether')) +
  geom_line(aes(y = Bitcoin_cash[,1],color = 'Bitcoin_cash')) +
  geom_line(aes(y = Stellar[,1],color = 'Stellar')) +
  geom_line(aes(y = Litecoin[,1],color = 'Litecoin')) + 
  geom_line(aes(y = Ethereum[,1],color = 'Ethereum')) +
  geom_line(aes(y = Bitcoin_sv[,1],color = 'Bitcoin_sv'))

# Boxplots all prices all currencies

cd %>%
  filter(year == '2018'| year == '2019' | year == '2019') %>%
  plot_ly(type = 'box',x = ~High,transforms = list(list(type = 'filter',
                                                        target = ~Currency,
                                                        operation = '=',
                                                        value = unique(cd$Currency)[1])),name="High") %>%
  add_boxplot(x = ~Low,name = 'Low') %>%
  add_boxplot(x = ~Open,name = 'Open') %>%
  add_boxplot(x = ~Close,name = 'Close') %>%
  
  layout(title = 'All Price Indicators of all Currencies',
         xaxis = list(title = 'Value'), 
         updatemenus = list(list(type = 'dropdown',buttons = list(
           list(method = "restyle",args = list("transforms[0].value", unique(cd$Currency)[1]),
                label = unique(cd$Currency)[1]),
           list(method = "restyle",args = list("transforms[0].value", unique(cd$Currency)[2]),
                label = unique(cd$Currency)[2]),
           list(method = "restyle",args = list("transforms[0].value", unique(cd$Currency)[3]),
                label = unique(cd$Currency)[3]),
           list(method = "restyle",args = list("transforms[0].value", unique(cd$Currency)[4]),
                label = unique(cd$Currency)[4]),
           list(method = "restyle",args = list("transforms[0].value", unique(cd$Currency)[5]),
                label = unique(cd$Currency)[5]),
           list(method = "restyle",args = list("transforms[0].value", unique(cd$Currency)[6]),
                label = unique(cd$Currency)[6]),
           list(method = "restyle",args = list("transforms[0].value", unique(cd$Currency)[7]),
                label = unique(cd$Currency)[7]),
           list(method = "restyle",args = list("transforms[0].value", unique(cd$Currency)[8]),
                label = unique(cd$Currency)[8]),
           list(method = "restyle",args = list("transforms[0].value", unique(cd$Currency)[9]),
                label = unique(cd$Currency)[9]),
           list(method = "restyle",args = list("transforms[0].value", unique(cd$Currency)[10]),
                label = unique(cd$Currency)[10]),
           list(method = "restyle",args = list("transforms[0].value", unique(cd$Currency)[11]),
                label = unique(cd$Currency)[11]),list(method = "restyle",
                                                      args = list("transforms[0].value", unique(cd$Currency)[12]),
                                                      label = unique(cd$Currency)[12]))))) %>%
  layout(annotations = list(list(text = "For the years 2017-19",  xref = "paper", yref = "paper",
                                 yanchor = "bottom",xanchor = "center", align = "center",
                                 x = 0.5, y = .97, showarrow = FALSE)))


# Plot market capitalization 

ggplot(Bitcoin, aes(x = index(Bitcoin),y = Bitcoin[,6]/100000000,color = 'Bitcoin')) + 
  geom_line() + 
  ggtitle("All Crypto Market Cap") + 
  theme(legend.position = "top") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(
    x = "Date",
    y = "Total Mean Capital",
    color = "Currencies",
    title = "All Currencies Market Capatalization",
    subtitle = "in 100 millions",
    caption = "Adi") +
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.key.width = unit(2,"cm")) +
  geom_line(aes(y = Xrp[,6]/100000000,color = 'Xrp')) +
  geom_line(aes(y = Cardano[,6]/100000000,,color = 'Cardano')) + 
  geom_line(aes(y = Tezos[,6]/100000000,color = 'Tezos')) +
  geom_line(aes(y = Binance_coin[,6]/100000000,color = 'Binance_coin')) +
  geom_line(aes(y = Eos[,6]/100000000,color = 'Eos')) +
  geom_line(aes(y = Tether[,6]/100000000,color = 'Tether')) +
  geom_line(aes(y = Bitcoin_cash[,6]/100000000,color = 'Bitcoin_cash')) +
  geom_line(aes(y = Stellar[,6]/100000000,color = 'Stellar')) +
  geom_line(aes(y = Litecoin[,6]/100000000,color = 'Litecoin')) + 
  geom_line(aes(y = Ethereum[,6]/100000000,color = 'Ethereum')) +
  geom_line(aes(y = Bitcoin_sv[,6]/100000000,color = 'Bitcoin_sv'))

# Plot market cap yearly all coins

cd %>%
  plot_ly(labels = ~Currency,values = ~Market.Cap, type = 'pie',
          transforms = list(list(type = 'filter',target = ~year,operation = '=',
                          value = unique(cd$year)[1])),name="2013") %>%
  layout(title = "Currency Yearly Market Capatalization Representation",
         xaxis = list(title = 'Value'), 
updatemenus = list(list(type = 'dropdown',buttons = list(
  list(method = "restyle",args = list("transforms[0].value", unique(cd$year)[1]),
       label = unique(cd$year)[1]),
  list(method = "restyle",args = list("transforms[0].value", unique(cd$year)[2]),
       label = unique(cd$year)[2]),
  list(method = "restyle",args = list("transforms[0].value", unique(cd$year)[3]),
       label = unique(cd$year)[3]),
  list(method = "restyle",args = list("transforms[0].value", unique(cd$year)[4]),
       label = unique(cd$year)[4]),
  list(method = "restyle",args = list("transforms[0].value", unique(cd$year)[5]),
       label = unique(cd$year)[5]),
  list(method = "restyle",args = list("transforms[0].value", unique(cd$year)[6]),
       label = unique(cd$year)[6]),
  list(method = "restyle",args = list("transforms[0].value", unique(cd$year)[7]),
       label = unique(cd$year)[7])))))

# All Currencies Volume Traded.

ggplot(Bitcoin['2017-08/'], aes(x = index(Bitcoin['2017-08/']),
                                y = Bitcoin['2017-08/'][,5]/100000000,
                                color = 'Bitcoin')) + 
  geom_line() + 
  ggtitle("All Crypto Volume Traded") + 
  theme(legend.position = "top") + 
  labs(
    x = "Date",
    y = "Total Mean Volume",
    color = "Currencies",
    subtitle = "in 100 millions",
    caption = "Adi") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
  theme(legend.key.width = unit(2,"cm")) +
  geom_line(aes(y = Xrp['2017-08/'][,5]/100000000,color = 'Xrp')) +
  geom_line(aes(y = Cardano['2017-08/'][,5]/100000000,color = 'Cardano')) + 
  geom_line(aes(y = Tezos['2017-08/'][,5]/100000000,color = 'Tezos')) +
  geom_line(aes(y = Binance_coin['2017-08/'][,5]/100000000,color = 'Binance_coin')) +
  geom_line(aes(y = Eos['2017-08/'][,5]/100000000,color = 'Eos')) +
  geom_line(aes(y = Tether['2017-08/'][,5]/100000000,color = 'Tether')) +
  geom_line(aes(y = Bitcoin_cash['2017-08/'][,5]/100000000,color = 'Bitcoin_cash')) +
  geom_line(aes(y = Stellar['2017-08/'][,5]/100000000,color = 'Stellar')) +
  geom_line(aes(y = Litecoin['2017-08/'][,5]/100000000,color = 'Litecoin')) + 
  geom_line(aes(y = Ethereum['2017-08/'][,5]/100000000,color = 'Ethereum')) +
  geom_line(aes(y = Bitcoin_sv['2017-08/'][,5]/100000000,color = 'Bitcoin_sv'))

# Tether & Bitcoin volume in the last two years.

Bitcoin$year = as.numeric(format(index(Bitcoin),"%Y"))
Tether$year = as.numeric(format(index(Tether),"%Y"))

Vol_yrly_BT = aggregate(BitcoinVolume ~ year, data=Bitcoin , FUN= mean)
Vol_yrly_BT$TetherVolume = aggregate(TetherVolume ~ year, data=Tether , FUN= mean)[2]

Vol_yrly_BT$TetherVolume = as.numeric(unlist(Vol_yrly_BT$TetherVolume))

plot_ly(Vol_yrly_BT,x =~year ,y = ~BitcoinVolume, type = 'bar',name = 'Bitcoin Volume',
        marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~TetherVolume, name = 'Tether Volume',
            marker = list(color = 'rgb(204,204,204)')) %>%
  layout(title = 'Yearly Volume Traded by Bitcoin & Tether',
         xaxis = list(title = 'Years',tickangle = -45),
         yaxis = list(title = 'Total Volume Traded'),
         margin = list(b=100),
         barmode = 'group')

# The distribution of the data.
# Volume of last two years of Bitcoin and Tether
# Correlation between volume of trading and market capitalization

Bitcoin1819 = subset(Bitcoin,year == '2018' | year == '2019')
Tether1819  = subset(Tether,year == '2018' | year == '2019')

# Bitcoin Volume Chart

ggplot(Bitcoin1819,aes(x = BitcoinVolume/100000000, fill = cut(BitcoinVolume,30))) +
  geom_histogram(show.legend = FALSE,bins = 50,color = 'black') + 
  scale_fill_discrete(h = c(240,10)) + 
  theme_minimal() +
  labs(x = 'Bitcoin Volume', y = 'Frequency',title = 'Bitcoin Volume for 2018-19',
  subtitle = 'in Hundred Millions',caption = "Adi" ) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x=303, y=92, label= paste("Mean =",round(mean(Bitcoin1819$BitcoinVolume/100000000),2))) +
  annotate("text", x=303, y=97, label= paste("SD =",round(sd(Bitcoin1819$BitcoinVolume/100000000),2))) +
  annotate("text", x=300, y=87, label= paste("Median =",round(median(Bitcoin1819$BitcoinVolume/100000000),2))) +
  scale_x_continuous(breaks = seq(0, 500, 50)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_vline(aes(xintercept = mean(Bitcoin1819$BitcoinVolume/100000000)), linetype = "dashed")

# Tether Volume Chart 

ggplot(Tether1819,aes(TetherVolume/100000000, fill = cut(TetherVolume,30))) + 
  geom_histogram(show.legend = FALSE,bins = 50,color = 'black',size = .001) + 
  scale_fill_discrete(h = c(240,10)) + 
  theme_minimal() +
  labs(x = 'Tether Volume', y = 'Frequency',title = 'Tether Volume for 2018-19',
  subtitle = 'in Hundred Millions' ) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 500, 50)) +
  annotate("text", x=452, y=95, label= paste("Mean =",round(mean(Tether1819$TetherVolume/100000000),2))) +
  annotate("text", x=454, y=100, label= paste("SD =",round(sd(Tether1819$TetherVolume/100000000),2))) +
  annotate("text", x=450, y=90, label= paste("Median =",round(median(Tether1819$TetherVolume/100000000),2))) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_vline(aes(xintercept = mean(Tether1819$TetherVolume/100000000)), linetype = "dashed") 

# Correlation Plots

ggplot(Tether1819, aes(x=scale(TetherMarket.Cap), y=scale(TetherVolume))) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE,color = 'red') + 
  geom_rug()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = 'Scaled Tether Market Capatalization ', y = 'Scaled Tether Volume',
       title = 'Tether Volume and Market Capatalization for 2018-19',
       subtitle = 'Scaled',caption = "Adi" ) +
  theme(panel.background = element_rect(colour = "orange",size = 2,linetype = "solid")) +
  annotate("text", x=-1.20, y=4,size = 5, label= paste("Correlation =",
            round(cor(Tether1819$TetherVolume,Tether1819$TetherMarket.Cap),2)))


ggplot(Bitcoin1819, aes(x=scale(BitcoinMarket.Cap), y=scale(BitcoinVolume))) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE,color = 'red') + 
  geom_rug()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = 'Scaled Bitcoin Market Capatalization ', y = 'Scaled Bitcoin Volume',
       title = 'Bitcoin Volume and Market Capatalization for 2018-19',
       subtitle = 'Scaled',caption = "Adi" ) +
  theme(panel.background = element_rect(colour = 'orange',size = 2, 
                                        linetype = "solid"))+
  annotate("text", x=-1, y=3.7,size = 5, label= paste("Correlation =",
        round(cor(Bitcoin1819$BitcoinVolume,Bitcoin1819$BitcoinMarket.Cap),2)))

# Plot closing prices Tether & Bitcoin

bitcoin_close = plot_ly(as.data.frame(Bitcoin1819),y = ~BitcoinClose, x = ~index(Bitcoin1819), 
        type = 'scatter',mode = 'lines',name = 'Bitcoin Closing Price') %>%
layout(xaxis = list(title = 'Years',tickangle = -45,tickfont = list(size = 20)),
       yaxis = list(title = 'Price Range',tickfont = list(size = 20)))

tether_close = plot_ly(as.data.frame(Tether1819),y = ~TetherClose, x = ~index(Tether1819), 
        type = 'scatter',mode = 'lines',name = 'Tether Closing Price') %>%
    layout(xaxis = list(title = 'Years',tickangle = 45,tickfont = list(size = 20)),
           yaxis = list(title = 'Price Range',tickfont = list(size = 20)))

subplot(bitcoin_close, tether_close) %>% layout(title="Closing Prices of Bitcoin and Tether",font = 20)

# Central Limit Theorem

# Sample of 10
xbar10 = c()

for (i in 1:5000) {
  xbar10[i] = mean(sample(x = Bitcoin1819$BitcoinVolume,size = 10,replace = TRUE))
}
xbar10 = as.numeric(unlist(xbar10))

# Plot sample 10

p1 = ggplot(mapping = aes(xbar10/100000000, fill = cut(xbar10,30))) + 
  geom_histogram(show.legend = FALSE,color = 'black',bins = 30,size = .001) + 
  scale_fill_discrete(h = c(240,10)) + 
  theme_minimal() +
  labs(x = 'Sample Bitcoin Volume', y = 'Frequency',title = 'Sample Bitcoin Volume for 2018-19',
       subtitle = 'in Hundred Millions/ Sample of 10') +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x=130, y=500, label= paste("Mean =",round(mean(xbar10/100000000),2))) +
  annotate("text", x=130, y=520, label= paste("SD =",round(sd(xbar10/100000000),2)))

# Sample of 40

xbar40 = c()

for (i in 1:5000) {
  xbar40[i] = mean(sample(x = Bitcoin1819$BitcoinVolume,size = 40,replace = TRUE))
}
xbar40 = as.numeric(unlist(xbar40))

# Plot sample 40

p2 = ggplot(mapping = aes(xbar40/100000000, fill = cut(xbar40,30))) + 
  geom_histogram(show.legend = FALSE,,bins = 30,color = 'black',size = .001) + 
  scale_fill_discrete(h = c(240,10)) + 
  theme_minimal() +
  labs(x = 'Sample Bitcoin Volume', y = 'Frequency',title = 'Sample Bitcoin Volume for 2018-19',
       subtitle = 'in Hundred Millions/ Sample of 40' ) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x=130, y=500, label= paste("Mean =",round(mean(xbar40/100000000),2))) +
  annotate("text", x=130, y=520, label= paste("SD =",round(sd(xbar40/100000000),2)))

# Sample of 100

xbar100 = c()

for (i in 1:5000) {
  xbar100[i] = mean(sample(x = Bitcoin1819$BitcoinVolume,size = 100,replace = TRUE))
}
xbar100 = as.numeric(unlist(xbar100))

# Plot sample 100

p3 = ggplot(mapping = aes(xbar100/100000000, fill = cut(xbar100,30))) + 
  geom_histogram(show.legend = FALSE,,bins = 30,color = 'black',size = .001) +
  scale_fill_discrete(h = c(240,10)) + 
  theme_minimal() +
  labs(x = 'Sample Bitcoin Volume', y = 'Frequency',title = 'Sample Bitcoin Volume for 2018-19',
       subtitle = 'in Hundred Millions/ Sample of 100' ) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x=128, y=500, label= paste("Mean =",round(mean(xbar100/100000000),2))) +
  annotate("text", x=130, y=520, label= paste("SD =",round(sd(xbar100/100000000),2)))

# Plot together

grid.arrange(p1, p2, p3, nrow = 1)

################################################################################

# Sampling Methods

# random sampling

set.seed(100)

Currency_list = c("Tezos","Binance_coin","Eos","Bitcoin","Tether","Xrp","Bitcoin_cash",
                  "Stellar","Litecoin","Ethereum","Cardano","Bitcoin_sv")

random_sampling_means = c()
count = 1

for (i in Currency_list){
  random_sampling_means[count] = mean(get(i)[(srswor(50,nrow(get(i)[,5])))!=0,])
  count = count + 1
}

sampling_DF = as.data.frame(random_sampling_means,Currency_list)

# Systematic Sampling

set.seed(100)

N = nrow(Bitcoin)
n = 50
k = floor(N / n)
r = sample(k, 1)
s = seq(r, by = k, length = n)

systematic_sampling_means = c()
count = 1

for (i in Currency_list){
  systematic_sampling_means[count] = mean(get(i)[s,][,5])
  count = count + 1
}
sampling_DF$systematic_sampling_means = systematic_sampling_means

# Stratified Sampling

# Pick 'year' Strata from each currency

freq = table(cd$Currency, cd$year)
st.sizes =as.vector(t(round((595 * freq / sum(freq))))) # 595 so as to get 50 samples for each currency
st.sizes = st.sizes[st.sizes != 0]

st.3 = strata(cd,stratanames = c('Currency','year'),
              size = st.sizes, method = 'srswor',
              description = FALSE)

stratified_sampling_means = getdata(cd, st.3)
stratified_sampling_means = tapply(stratified_sampling_means$Volume, stratified_sampling_means$Currency, mean)
stratified_sampling_means = as.data.frame(stratified_sampling_means)
sampling_DF$stratified_sampling_means = stratified_sampling_means[c(11,1,6,2,10,12,3,9,8,7,5,4),]

# Cluster

cluster_sampling_means = c()

# Assign random letters to create clusters
cd$category = sample(LETTERS[1:12],nrow(cd),replace = TRUE)
cluster_data = getdata(cd,(cluster(cd,c('category'),6,'srswor')))

data_Tezos = cluster_data[cluster_data$Currency == 'Tezos',]
sample_Tezos = srswor(50,nrow(data_Tezos))
cluster_sampling_means[1] = mean(data_Tezos[sample_Tezos != 0,][,7])

data_Binance_coin = cluster_data[cluster_data$Currency == 'Binance-coin',]
sample_Binance_coin = srswor(50,nrow(data_Binance_coin))
cluster_sampling_means[2] = mean(data_Binance_coin[sample_Binance_coin != 0,][,7])

data_Eos = cluster_data[cluster_data$Currency == 'Eos',]
sample_Eos = srswor(50,nrow(data_Eos))
cluster_sampling_means[3] = mean(data_Eos[sample_Eos != 0,][,7])

data_Bitcoin = cluster_data[cluster_data$Currency == 'Bitcoin',]
sample_Bitcoin = srswor(50,nrow(data_Bitcoin))
cluster_sampling_means[4] = mean(data_Bitcoin[sample_Bitcoin != 0,][,7])

data_Tether = cluster_data[cluster_data$Currency == 'Tether',]
sample_Tether = srswor(50,nrow(data_Tether))
cluster_sampling_means[5] = mean(data_Tether[sample_Tether != 0,][,7])

data_Xrp = cluster_data[cluster_data$Currency == 'Xrp',]
sample_Xrp = srswor(50,nrow(data_Xrp))
cluster_sampling_means[6] = mean(data_Xrp[sample_Xrp != 0,][,7])

data_Bitcoin_cash = cluster_data[cluster_data$Currency == 'Bitcoin-cash',]
sample_Bitcoin_cash  = srswor(50,nrow(data_Bitcoin_cash))
cluster_sampling_means[7] = mean(data_Bitcoin_cash[sample_Bitcoin_cash != 0,][,7])

data_Stellar = cluster_data[cluster_data$Currency == 'Stellar',]
sample_Stellar = srswor(50,nrow(data_Stellar))
cluster_sampling_means[8] = mean(data_Stellar[sample_Stellar != 0,][,7])

data_Litecoin = cluster_data[cluster_data$Currency == 'Litecoin',]
sample_Litecoin = srswor(50,nrow(data_Litecoin))
cluster_sampling_means[9] = mean(data_Litecoin[sample_Litecoin != 0,][,7])

data_Ethereum = cluster_data[cluster_data$Currency == 'Ethereum',]
sample_Ethereum = srswor(50,nrow(data_Ethereum))
cluster_sampling_means[10] = mean(data_Ethereum[sample_Ethereum != 0,][,7])

data_Cardano = cluster_data[cluster_data$Currency == 'Cardano',]
sample_Cardano = srswor(50,nrow(data_Cardano))
cluster_sampling_means[11] = mean(data_Cardano[sample_Cardano != 0,][,7])

data_Bitcoin_sv = cluster_data[cluster_data$Currency == 'Bitcoin-sv',]
sample_Bitcoin_sv = srswor(50,nrow(data_Bitcoin_sv))
cluster_sampling_means[12] = mean(data_Bitcoin_sv[sample_Bitcoin_sv != 0,][,7])

sampling_DF$cluster_sampling_means = cluster_sampling_means

# Population

population_means = c()
count = 1

for (i in Currency_list){
  population_means[count] = mean(get(i)[,5])
  count = count + 1
}

sampling_DF$population_means = population_means

# Plot

plot_ly(sampling_DF, x = Currency_list, y = ~random_sampling_means, type = 'bar', 
        name = 'Random Sampling Means',texttemplate = '%{y:.2s}', textposition = 'outside') %>% 
   add_trace(y = ~systematic_sampling_means, name = 'Systematic Means') %>%
   add_trace(y = ~stratified_sampling_means, name = 'Stratified Means') %>%
   add_trace(y = ~cluster_sampling_means, name = 'Cluster Means') %>%
   add_trace(y = ~population_means, name = 'Population Means') %>%
   layout(title = 'Means of Volume by Sampling Methods',xaxis = list(tickangle = 45,
                                                    tickfont = list(size = 20)),
         yaxis = list(title = 'Volume Means',tickfont = list(size = 20)))

# Calculate sampling error

sampling_DF$percentage_error_rs = round((abs(sampling_DF$population_means - 
         sampling_DF$random_sampling_means) / sampling_DF$random_sampling_means),4) * 100

sampling_DF$percentage_error_systematic = round((abs(sampling_DF$population_means - 
          sampling_DF$systematic_sampling_means) / sampling_DF$systematic_sampling_means),4) * 100

sampling_DF$percentage_error_stratified = round((abs(sampling_DF$population_means - 
          sampling_DF$stratified_sampling_means) / sampling_DF$stratified_sampling_means),4) * 100

sampling_DF$percentage_error_cluster = round((abs(sampling_DF$population_means - 
          sampling_DF$cluster_sampling_means) / sampling_DF$cluster_sampling_means),4) * 100

# Plot Sampling errors

plot_ly(sampling_DF, x = Currency_list, y = ~percentage_error_rs, type = 'bar', 
        name = 'Random Sampling Error %') %>% 
  add_trace(y = ~percentage_error_systematic, name = 'Systematic Means Error %') %>%
  add_trace(y = ~percentage_error_stratified, name = 'Stratified Means Error %') %>%
  add_trace(y = ~percentage_error_cluster, name = 'Cluster Means Error %') %>%
  layout(title = 'Error % of Sampling Methods',xaxis = list(tickangle = 45,tickfont = list(size = 20)),
         yaxis = list(title = 'Percentage',tickfont = list(size = 20)))