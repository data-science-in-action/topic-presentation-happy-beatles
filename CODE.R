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

#访问详细的历史数据
#访问历史数据的方法与获取最新数据基本相同，但是输入功能为load_nCov2019()。
library('nCov2019')
y <- load_nCov2019(lang = 'en')
y  
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

#可视化
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





