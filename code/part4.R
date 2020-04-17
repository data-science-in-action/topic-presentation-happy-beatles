install.packages("Synth")
install.packages("numDeriv")
install.packages("kernlab")
install.packages("optimx")

library("Synth")
library("numDeriv")
library("kernlab")
library("optimx")

data("basque")
basque[85:89, 1:4]

# 为合成控制法准备数据
dataprep.out <- dataprep(
  foo = basque,
  predictors = c("school.illit", "school.prim", "school.med",
                 "school.high", "school.post.high", "invest"),
  predictors.op = "mean",
  time.predictors.prior = 1964:1969,
  special.predictors = list(
    list("gdpcap", 1960:1969 , "mean"),
    list("sec.agriculture", seq(1961, 1969, 2), "mean"),
    list("sec.energy", seq(1961, 1969, 2), "mean"),
    list("sec.industry", seq(1961, 1969, 2), "mean"),
    list("sec.construction", seq(1961, 1969, 2), "mean"),
    list("sec.services.venta", seq(1961, 1969, 2), "mean"),
    list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
    list("popdens", 1969, "mean")),
  dependent = "gdpcap",
  unit.variable = "regionno",
  unit.names.variable = "regionname",
  time.variable = "year",
  treatment.identifier = 17,
  controls.identifier = c(2:16, 18),
  time.optimize.ssr = 1960:1969,
  time.plot = 1955:1997)

dataprep.out$X1
dataprep.out$Z1

# 整合最高和次高教育变量，将每个预测因素的总数用百分比份额代替
dataprep.out$X1["school.high",] <- dataprep.out$X1["school.high",] +
  dataprep.out$X1["school.post.high",]
dataprep.out$X1 <- as.matrix(dataprep.out$X1[
  -which(rownames(dataprep.out$X1) == "school.post.high"),])
dataprep.out$X0["school.high",] <- dataprep.out$X0["school.high",] +
  dataprep.out$X0["school.post.high",]
dataprep.out$X0 <- dataprep.out$X0[
  -which(rownames(dataprep.out$X0) == "school.post.high"),]

lowest <- which(rownames(dataprep.out$X0) == "school.illit")
highest <- which(rownames(dataprep.out$X0) == "school.high")

dataprep.out$X1[lowest:highest,] <-
  (100 * dataprep.out$X1[lowest:highest,]) /
  sum(dataprep.out$X1[lowest:highest,])
dataprep.out$X0[lowest:highest,] <-
  100 * scale(dataprep.out$X0[lowest:highest,], center = FALSE,
              scale = colSums(dataprep.out$X0[lowest:highest,]))

# 运行 Synth
synth.out <- synth(data.prep.obj = dataprep.out, 
                   method = "BFGS")

# 获取结果表格
synth.tables <- synth.tab(dataprep.res = dataprep.out, 
                          synth.res = synth.out)
synth.tables$tab.pred[1:5, ]
synth.tables$tab.w[8:14, ]

# 查看人均GDP差距
gaps <- dataprep.out$Y1plot - 
  (dataprep.out$Y0plot %*% synth.out$solution.w)
gaps[1:3, 1]

# path
path.plot(synth.res = synth.out, 
          dataprep.res = dataprep.out, 
          Ylab = "real per-capita GDP (1986 USD, thousand)", 
          Xlab = "year", Ylim = c(0, 12), 
          Legend = c("Basque country", 
                     "synthetic Basque country"), 
          Legend.position = "bottomright")

# gaps
gaps.plot(synth.res = synth.out, 
          dataprep.res = dataprep.out,
          Ylab = "gap in real per-capita GDP (1986 USD, thousand)", 
          Xlab = "year", Ylim = c(-1.5, 1.5), Main = NA)

# Placebo Tests 
data <- read.csv(file = "gaps.csv")
plot(data$x, data$y1, type = 'l', ylab = "gap in real per-capita GDP (1986 USD, thousand)", 
     xlab = "year", ylim = c(-1.5, 1.5), lwd = 2, main = NA)
abline(h = 0, lty = 2, lwd = 2)
abline(v = 1970, lty = 3,)
text(x = 1963, y = -1.25, labels = "Terrorism Onset →", cex = 0.65)
lines(data$x, data$y2, type = 'l', col = "gray")
lines(data$x, data$y3, type = 'l', col = "gray")
lines(data$x, data$y4, type = 'l', col = "gray")
lines(data$x, data$y6, type = 'l', col = "gray")
lines(data$x, data$y7, type = 'l', col = "gray")
lines(data$x, data$y8, type = 'l', col = "gray")
lines(data$x, data$y9, type = 'l', col = "gray")
lines(data$x, data$y10, type = 'l', col = "gray")
lines(data$x, data$y11, type = 'l', col = "gray")
lines(data$x, data$y13, type = 'l', col = "gray")
lines(data$x, data$y15, type = 'l', col = "gray")
lines(data$x, data$y16, type = 'l', col = "gray")
lines(data$x, data$y18, type = 'l', col = "gray")


data <- read.csv(file = "gaps.csv")
plot(data$x, data$y2, type = 'l', ylab = "gap in real per-capita GDP (1986 USD, thousand)", 
     xlab = "year", ylim = c(-1.5, 1.5), main = NA, col = "gray")
abline(h = 0, lty = 2, lwd = 2)
abline(v = 1970, lty = 3,)
text(x = 1963, y = -1.25, labels = "Terrorism Onset →", cex = 0.65)
lines(data$x, data$y2, type = 'l', col = "gray")
lines(data$x, data$y3, type = 'l', col = "gray")
lines(data$x, data$y4, type = 'l', col = "gray")
lines(data$x, data$y6, type = 'l', col = "gray")
lines(data$x, data$y7, type = 'l', col = "gray")
lines(data$x, data$y8, type = 'l', col = "gray")
lines(data$x, data$y9, type = 'l', col = "gray")
lines(data$x, data$y10, type = 'l', col = "gray")
lines(data$x, data$y11, type = 'l', col = "gray")
lines(data$x, data$y13, type = 'l', col = "gray")
lines(data$x, data$y15, type = 'l', col = "gray")
lines(data$x, data$y16, type = 'l', col = "gray")
lines(data$x, data$y1, type = 'l', lwd = 2)
