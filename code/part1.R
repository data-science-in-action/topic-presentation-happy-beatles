# ��װ
library('remotes')
remotes::install_github("GuangchuangYu/nCov2019", dependencies = TRUE)

# ��ѯ��������
library('nCov2019')
x <- get_nCov2019(lang = 'en')
x 
# ��ȡ�����й�����
head(summary(x))
# ��ȡ�й���ʡ������
head(x[])
# Ҫ��ȡ����ϸ�����ݣ�ֻ��ָ��ʡ����
head(x['Shanxi'])
# ͨ��ʹ��x['global']����ȡȫ�����ݡ�
head(x['global'])

## ���ӻ�
#1.�ۼ�ȷ�ﰸ������ͼ
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

#���°���ʡȷ����ϵ�����ͼ
library(ggplot2)
d = x['Anhui', ] # you can replace Anhui with any province
d = d[order(d$confirm), ]
ggplot(d, aes(name, as.numeric(confirm))) +
  geom_col(fill = 'firebrick') + 
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) +
  labs(caption = paste("accessed date:", time(x))) + 
  scale_x_discrete(limits = d$name) + coord_flip()

#������ϸ����ʷ����
#������ʷ���ݵķ������ȡ�������ݻ�����ͬ���������빦��Ϊload_nCov2019()��
library('nCov2019')
y <- load_nCov2019(lang = 'en')
y  

#�Ա�������ͬ��Դ�����ݿ�
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

#��ȡ�й�ʡ������ʷ���ݣ�
head(summary(y))
#��ȡ�й����г��е���ʷ����
head(y[])
#ָ��ʡ������ȡ��Ӧ����ʷ����
head(y['Anhui'])
#'global'������ȡȫ����ʷ����
y <- load_nCov2019(lang = 'en', source='github')
d <- y['global']
tail(d)

#������ʷ���ݵĿ��ӻ�ʾ��
#��ӳ�й�������������ȷ���ʺ������ʵ�����ͼ
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

#2.sɽ��ʡȷ����������ͼ
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

#3.����ʮ����ҵ�ȷ����������
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




