## ȫ�������ͼ
- ������Դ��`nCov2019`���и�������
- �����ݱ���2020��1��19������ǰ����
- confirm��2019-nCoV�ۼ�ȷ�ﲡ����
- ������������
library(remotes) 
remotes::install_github("GuangchuangYu/nCov2019", dependencies = TRUE) 
library(maps)
library(nCov2019) 
x = get_nCov2019()#��ʱ�Ļ���Ĭ��Ϊ����



##   
- ����ȫ�������ͼ
plot(x) 



## 
- �й���ʾ������״���Ƿ�������أ�
- Ϊ�˸�ϸ�µ�չʾ�й�����״�������й���������ϸ��Ϊ����ʡ��
- ���õ�ͼ����
remotes::install_github("GuangchuangYu/chinamap")
library(chinamap)
cn = get_map_china()



## 
- ����ȫ�������ͼ�������й���ʡ�����ݣ�
plot(x, chinamap=cn)


## 
- ������������ͼ
x = get_nCov2019(lang = 'en')#��ʱӦ����ΪӢ��
plot(x, region='Japan')




## �й������ͼ
- �����й������ͼ
plot(x, region='china', chinamap=cn)#Ĭ������Ϊ����������



## 
- չʾ��ɢ�����ݣ�����`continuous_scale=FALSE`
- ��Ⱦ��ͬ����ɫ������` palette=''`
- ������ʷ���ݣ�����`date=''`


## 
plot(x, region='china', chinamap=cn,
     continuous_scale=FALSE,palette='Greens',date='2020-02-15')



## ʡ�������ͼ 
- �����н��ͼ���ݣ�china-city-map.rar��
- https://github.com/moruizhe/Data
m = sf::st_read(".../�н�.shp")

## 
- ����ʡ�������ͼ
x = get_nCov2019()#��ʱ�Ļ���Ĭ��Ϊ����
plot(x, region='����', chinamap=m)


## 
- չʾ���ʡ�������ͼ
plot(x, region=c('�㶫','����'),chinamap=m)



## magick
- ������ͼ��`from`��`to`
- magick ��: ��R�н��и߼�ͼƬ����
- ���ܣ�
+ ��ʽת������һ�ָ�ʽת����ͼ����һ�������� PNG ת JPEG��
+ �任�����ţ���ת���ü�����ת���޼�ͼ��
+ ͼ��ʶ�������ĸ�ʽ��ͼ������
+ ...
- ���ú���
library(magick)
library(chinamap)



## ÿ������仯ͼ
- ��������仯ͼ(ȫ��)
cn = get_map_china()
y <- load_nCov2019()#����nCov2019����
d <- c(paste0("2020-01-", 24:31), 
       paste0("2020-02-", 1:10))#ȷ�����ݿ�
img <- image_graph(600, 450, res = 96)#��ͼ���豸
out <- lapply(d, function(date)
{
  p <- plot(y, region='china', chinamap=cn, date=date,
            label=FALSE)#����ÿһ"֡"ͼ��
  print(p)
})
dev.off()#�ر�ͼ���豸

animation <- image_animate(img, fps = 2)#fps���Ʒ�ӳ�ٶ�
image_write(animation, "ncov2019.gif")