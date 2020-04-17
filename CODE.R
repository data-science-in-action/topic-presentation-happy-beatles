# 安装
library('remotes')
remotes::install_github("GuangchuangYu/nCov2019", dependencies = TRUE)

# 查询最新数据
library('nCov2019')
x <- get_nCov2019(lang = 'en')
x 
# 获取最新中国数据
head(summary(x))
# 获取中国的省级数据
head(x[])
# 要获取更详细的数据，只需指定省名。
head(x['Shanxi'])
# 通过使用x['global']，获取全球数据。
head(x['global'])

## 可视化
#1.累计确诊案例条形图
library('nCov2019')
x <- get_nCov2019(lang = 'en')
d <- summary(x)
library(ggplot2)
ggplot(d, 
       aes(as.Date(date, "%m.%d"), as.numeric(confirm))) +
  geom_col(fill = 'firebrick') + 
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) + 
  scale_x_date(date_labels = "%Y/%m/%d") +
  labs(caption = paste("accessed date:", time(x)))

#最新安徽省确诊诊断的条形图
library(ggplot2)
d = x['Anhui', ] # you can replace Anhui with any province
d = d[order(d$confirm), ]
ggplot(d, aes(name, as.numeric(confirm))) +
  geom_col(fill = 'firebrick') + 
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) +
  labs(caption = paste("accessed date:", time(x))) + 
  scale_x_discrete(limits = d$name) + coord_flip()

#访问详细的历史数据
#访问历史数据的方法与获取最新数据基本相同，但是输入功能为load_nCov2019()。
library('nCov2019')
y <- load_nCov2019(lang = 'en')
y  

#对比三个不同来源的数据库
library(nCov2019)
library(ggplot2)
nCov2019_set_country('China')
y = load_nCov2019(lang = 'en', source = 'github')
dxy = load_nCov2019(lang = 'en', source = 'dxy')
nhc = load_nCov2019(lang = 'en', source = 'cnnhc')
dxy_china <- aggregate(cum_confirm ~ + time, summary(dxy), sum)
y_china <- aggregate(cum_confirm ~ + time, summary(y), sum)
nhc_china <- aggregate(cum_confirm ~ + time, summary(nhc), sum)
dxy_china$source = 'DXY data'
y_china$source = 'GitHub data'
nhc_china$source = 'NHC data'
df = rbind(dxy_china, y_china, nhc_china)
ggplot(subset(df, time >= '2020-01-11'),
       aes(time,cum_confirm, color = source)) +
  geom_line() + scale_x_date(date_labels = "%Y-%m-%d") + 
  ylab('Confirmed Cases in China') + xlab('Time') + theme_bw() +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(legend.position = 'bottom')

#获取中国省级的历史数据：
head(summary(y))
#获取中国所有城市的历史数据
head(y[])
#指定省名来获取相应的历史数据
head(y['Anhui'])
#'global'参数获取全部历史数据
y <- load_nCov2019(lang = 'en', source='github')
d <- y['global']
tail(d)

#带有历史数据的可视化示例
#反映中国的死亡人数，确诊率和治愈率的折线图
library('tidyr')
library('ggrepel')
library('ggplot2')
y <- load_nCov2019(lang = 'en')
d <- subset(y['global'], country == 'China')
d <- gather(d, curve, count, -time, -country)
ggplot(d, aes(time, count, color = curve)) +
  geom_point() + geom_line() + xlab(NULL) + ylab(NULL) +
  theme_bw() + theme(legend.position = "none") +
  geom_text_repel(aes(label = curve), 
                  data = d[d$time == time(y), ], hjust = 1) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m-%d", 
               limits = c(as.Date("2020-01-15"), as.Date("2020-03-20"))) +
  labs(title="Number of deaths, confirms, and cures in China") 

#2.s山西省确诊人数曲线图
library('nCov2019')
library('ggrepel')
library('ggplot2')
y <- load_nCov2019(lang = 'en')
d <- y['Shanxi']
ggplot(d, aes(time, as.numeric(cum_confirm), 
              group = city, color = city)) +
  geom_point() + geom_line() + 
  geom_label_repel(aes(label = city), 
                   data = d[d$time == time(y), ], hjust = 1) +
  theme_minimal(base_size = 14) + theme(legend.position = 'none') + 
  scale_x_date(date_labels = "%Y-%m-%d") + xlab(NULL) + ylab(NULL) + 
  theme(axis.text.x = element_text(hjust = 1)) +
  labs(title = "Growth curve of confirms in Shanxi Province, China")

#3.世界十大国家的确诊人数曲线
library('nCov2019')
x = load_nCov2019()
d = x['global']
head(d)
library('dplyr')
n <- d %>% filter(time == time(x)) %>%
  top_n(10, cum_confirm) %>%
  arrange(desc(cum_confirm)) %>%
  pull(country)
n
library('ggplot2')
library('ggrepel')
ggplot(filter(d, country %in% n),
       aes(time, cum_confirm, color=country)) +
  geom_line() +
  geom_text_repel(aes(label=country),
                  function(d) d[d$time == time(x),]) +
  theme_minimal(base_size=14) +
  theme(legend.position = "none") +
  xlab(NULL) + ylab(NULL)





