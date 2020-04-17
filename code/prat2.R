## 全球疫情地图
- 数据来源于`nCov2019`包中更新数据
- 此数据报告2020年1月19日至当前数据
- confirm：2019-nCoV累计确诊病例数
- 调用疫情数据
library(remotes) 
remotes::install_github("GuangchuangYu/nCov2019", dependencies = TRUE) 
library(maps)
library(nCov2019) 
x = get_nCov2019()#此时的环境默认为中文



##   
- 绘制全球疫情地图
plot(x) 



## 
- 中国显示的疫情状况是否过于严重？
- 为了更细致的展示中国疫情状况，将中国疫情数据细分为各个省份
- 调用地图数据
remotes::install_github("GuangchuangYu/chinamap")
library(chinamap)
cn = get_map_china()



## 
- 绘制全球疫情地图（包含中国各省份数据）
plot(x, chinamap=cn)


## 
- 绘制外国疫情地图
x = get_nCov2019(lang = 'en')#此时应设置为英文
plot(x, region='Japan')




## 中国疫情地图
- 绘制中国疫情地图
plot(x, region='china', chinamap=cn)#默认数据为连续型数据



## 
- 展示离散型数据，添加`continuous_scale=FALSE`
- 渲染不同的颜色，添加` palette=''`
- 载入历史数据，添加`date=''`


## 
plot(x, region='china', chinamap=cn,
     continuous_scale=FALSE,palette='Greens',date='2020-02-15')



## 省市疫情地图 
- 调用市界地图数据（china-city-map.rar）
- https://github.com/moruizhe/Data
m = sf::st_read(".../市界.shp")

## 
- 绘制省市疫情地图
x = get_nCov2019()#此时的环境默认为中文
plot(x, region='湖北', chinamap=m)


## 
- 展示多个省市疫情地图
plot(x, region=c('广东','福建'),chinamap=m)



## magick
- 制作动图：`from`和`to`
- magick 包: 在R中进行高级图片处理
- 功能：
+ 格式转换：从一种格式转换成图像到另一个（例如 PNG 转 JPEG）
+ 变换：缩放，旋转，裁剪，翻转或修剪图像
+ 图像识别：描述的格式和图像性能
+ ...
- 调用函数
library(magick)
library(chinamap)



## 每日疫情变化图
- 绘制疫情变化图(全国)
cn = get_map_china()
y <- load_nCov2019()#载入nCov2019数据
d <- c(paste0("2020-01-", 24:31), 
       paste0("2020-02-", 1:10))#确定数据框
img <- image_graph(600, 450, res = 96)#打开图形设备
out <- lapply(d, function(date)
{
  p <- plot(y, region='china', chinamap=cn, date=date,
            label=FALSE)#绘制每一"帧"图像
  print(p)
})
dev.off()#关闭图形设备

animation <- image_animate(img, fps = 2)#fps控制放映速度
image_write(animation, "ncov2019.gif")
