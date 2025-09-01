## ----include=FALSE, cache=FALSE--------------------------------
library(knitr)
#opts_chunk$set(cache=FALSE,tidy=FALSE,highlight=FALSE)
knitr::opts_chunk$set(fig.width=12, fig.height=8, out.width='100%', out.height='100%') 
opts_chunk$set(cache = FALSE, tidy = FALSE, fig.align = "center")
library(ALFAM2)
  options(width=65)

## ----eval=FALSE------------------------------------------------
#  install.packages('ALFAM2')

## --------------------------------------------------------------
library(ALFAM2)

## ----eval=FALSE------------------------------------------------
#  vignette("ALFAM2-start")

## --------------------------------------------------------------
packageVersion("ALFAM2")

## --------------------------------------------------------------
args(alfam2)

## ----eval=FALSE------------------------------------------------
#  ?alfam2

## --------------------------------------------------------------
packageVersion("ALFAM2")

## --------------------------------------------------------------
args(alfam2)

## --------------------------------------------------------------
alfam2pars03

## --------------------------------------------------------------
dat1 <- data.frame(ctime = 168, TAN.app = 50, man.dm = 8, 
                   air.temp = 20, wind.sqrt = 2, 
                   app.mthd = 'bc')
print(dat1)

## --------------------------------------------------------------
pred1 <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime')

## --------------------------------------------------------------
print(pred1)

## --------------------------------------------------------------
dat1b <- data.frame(ctime = 168, TAN.app = 50, man.dm = 8, 
                   air.temp = 20, wind.sqrt = 2, 
                   app.mthd = 'Broadcast')
print(dat1b)

## --------------------------------------------------------------
pred1b <- alfam2(dat1, app.name = 'TAN.app', time.name = 'ctime')

## --------------------------------------------------------------
all.equal(pred1, pred1b)

## --------------------------------------------------------------
dat2 <- dat1

## --------------------------------------------------------------
dat2$incorp <- 'deep'
dat2$t.incorp <- 0.5
print(dat2)

## --------------------------------------------------------------
pred2 <- alfam2(dat2, app.name = "TAN.app", time.name = "ctime", 
                   time.incorp = "t.incorp", warn = FALSE)
print(pred2)

## --------------------------------------------------------------
dat3 <- dat1
dat3$incorp <- 'shallow'
dat3$t.incorp <- 0.5
print(dat3)

## --------------------------------------------------------------
pred3 <- alfam2(dat3, app.name = "TAN.app", time.name = "ctime", 
                   time.incorp = "t.incorp", warn = FALSE)
print(pred3)

## --------------------------------------------------------------
datr <- data.frame(ctime = 168)
print(datr)

## --------------------------------------------------------------
predr <- alfam2(datr, app.name = 'TAN.app', time.name = 'ctime')
predr

## --------------------------------------------------------------
predr$j * predr$dt

## --------------------------------------------------------------
dat4 <- data.frame(scenario = 1:5, ctime = 168, TAN.app = 50, 
                   man.dm = 8, air.temp = 20, wind.sqrt = 2, 
                   app.mthd = 'bc',
                   incorp = 'deep',
                   t.incorp = c(0.1, 1, 6, 24, NA))
print(dat4)

## --------------------------------------------------------------
dat4 <- data.frame(scenario = 1:5, ctime = 168, TAN.app = 50, 
                   man.dm = 8, air.temp = 20, wind.sqrt = 2, 
                   app.mthd = 'bc',
                   incorp = c(rep('deep', 4), 'none'),
                   t.incorp = c(0.1, 1, 6, 24, NA))
print(dat4)

## --------------------------------------------------------------
pred4 <- alfam2(dat4, app.name = "TAN.app", time.name = "ctime", 
                   time.incorp = "t.incorp", group = "scenario", 
                   warn = FALSE)
print(pred4)

## --------------------------------------------------------------
barplot(pred4$er, names.arg = paste(dat4$t.incorp), xlab = 't.incorp', ylab = 'er')

## --------------------------------------------------------------
dat5 <- data.frame(scenario = 1:3, ctime = 168, TAN.app = 50, 
                   man.dm = 8, wind.sqrt = 2,
                   air.temp = c(15, 20, 25),
                   app.mthd = c('bc', 'bsth', 'os')
                   )
print(dat5)

## --------------------------------------------------------------
pred5 <- alfam2(dat5, app.name = "TAN.app", time.name = "ctime", 
                   group = "scenario", warn = FALSE)
print(pred5)

## --------------------------------------------------------------
dat5b <- data.frame(scenario = 1:3, ctime = 168, TAN.app = 50, 
                   man.dm = 8, wind.sqrt = 2,
                   air.temp = c(15, 20, 25),
                   app.mthd.bc = c(TRUE, FALSE, FALSE),
                   app.mthd.os = c(FALSE, FALSE, TRUE)
                   )
print(dat5b)

## --------------------------------------------------------------
pred5b <- alfam2(dat5b, app.name = "TAN.app", time.name = "ctime", 
                    group = "scenario", warn = FALSE)
print(pred5b)
all.equal(pred5b$e, pred5$e)

## --------------------------------------------------------------
alfam2pars03[grepl('^incorp', names(alfam2pars03))]

## --------------------------------------------------------------
dat6 <- data.frame(scenario = 1:6, ctime = 168, TAN.app = 100, 
                   man.dm = 5, man.ph = 7.2, air.temp = 10, 
                   wind.sqrt = 2, 
                   man.source = c(rep('Cattle', 2), rep('Pig', 4)),
                   app.mthd = rep(c('Broadcast', 'Trailing hose'), 
                                  each = 3),
                   incorp = rep(c('None', 'Shallow', 'Deep'), 2),
                   t.incorp = 4)
print(dat6)

## --------------------------------------------------------------
pred6 <- alfam2(dat6, app.name = "TAN.app", time.name = "ctime", 
                time.incorp = "t.incorp", group = "scenario", 
                warn = TRUE)
print(pred6)

## --------------------------------------------------------------
pred6b <- alfam2(dat6, app.name = "TAN.app", time.name = "ctime", 
                 time.incorp = "t.incorp", group = "scenario", 
                 warn = TRUE, add.incorp.rows = TRUE)
print(pred6b)

## --------------------------------------------------------------
set.seed(1201)
dat7 <- data.frame(ctime = 0:84*2, TAN.app = 100, man.dm = 8, 
                   air.temp = 7 + 7*sin(0:84*2 * 2*pi/24) + 
                              rnorm(85, 0, 2), 
                   wind.sqrt = sqrt(1.5 + 0.4*sin(0:84*2 * 2*pi/24)) + 
                              rnorm(85, 0, 0.12), 
                   app.mthd = 'ts')
plot(air.temp ~ ctime, data = dat7, type = 's', col = 'gray45')
plot(wind.sqrt^2 ~ ctime, data = dat7, type = 's', col = 'blue')

## --------------------------------------------------------------
pred7 <- alfam2(dat7, app.name = 'TAN.app', time.name = 'ctime',
                   warn = FALSE)

## --------------------------------------------------------------
plot(e ~ ctime, data = pred7, type = 'o', xlab = 'Time (h)', 
     ylab = 'Cumulative emission (kg/ha)')

plot(j ~ ctime, data = pred7, type = 'S', col = 'red', 
     xlab = 'Time (h)', ylab = 'Average flux (kg/ha-h)')

## --------------------------------------------------------------
plot(j ~ ctime, data = pred7, type = 'S', col = 'red', 
     xlab = 'Time (h)', ylab = 'Average flux (kg/ha-h)')
points(jinst ~ ctime, data = pred7, col = 'blue')

## --------------------------------------------------------------
dat8 <- dat7
dat8$incorp <- "deep"
dat8$t.incorp <- 6.5

## --------------------------------------------------------------
pred8 <- alfam2(dat8, app.name = 'TAN.app', time.name = 'ctime',
                time.incorp = 't.incorp', warn = FALSE, 
                add.incorp.rows = TRUE)

## --------------------------------------------------------------
plot(e ~ ctime, data = pred8, type = 'o', xlab = 'Time (h)', 
     ylab = 'Cumulative emission (kg/ha)')
abline(v = 6.5, col = 'blue', lty = 2)

plot(j ~ ctime, data = pred8, type = 'S', col = 'red', 
     xlab = 'Time (h)', ylab = 'Average flux (kg/ha-h)')
abline(v = 6.5, col = 'blue', lty = 2)

## ----eval=FALSE------------------------------------------------
#  set.seed(0812)
#  dat9 <- expand.grid(field = 1:1000, ct = 1:168,
#                      TAN.app = 100, man.dm = 8,
#                      app.rate.ni = 30, man.source = "pig",
#                      man.ph = 7, rain.rate = 0,
#                      app.mthd = "bsth")
#  
#  dat9$air.temp <- 7 + 7*sin(dat9$ct * 2 * pi / 24) +
#                   rnorm(1000, 0, 2)
#  dat9$wind.sqrt <- sqrt(1.5 + 0.4*sin(dat9$ct * 2 * 2 * pi / 24)) +
#                         rnorm(1000, 0, 0.1)
#  dat9 <- dat9[order(dat9$field, dat9$ct), ]

## ----eval=FALSE------------------------------------------------
#  head(dat9)
#  dim(dat9)

## ----eval=FALSE------------------------------------------------
#  system.time(
#    pred9 <- alfam2(dat9, app.name = 'TAN.app', time.name = 'ct',
#                    group = 'field', warn = FALSE)
#  )

## ----eval=FALSE------------------------------------------------
#  pred9sub <- subset(pred9, field %in% 1:100)
#  pred9sub <- pred9sub[order(pred9sub$field), ]
#  pred9sub[pred9sub$ct == 168, c('er', 'j')] <- NA

## ----eval=FALSE------------------------------------------------
#  plot(j ~ ct, data = pred9sub, type = 'S', col = 'red',
#       xlab = 'Time (h)', ylab = 'Average flux (kg/ha-h)')

## ----eval=FALSE------------------------------------------------
#  system.time(
#    alfam2(dat9, app.name = 'TAN.app', time.name = 'ct',
#           group = 'field', check = FALSE, warn = FALSE)
#  )

## ----eval=FALSE------------------------------------------------
#  dat9b <- dat9
#  dat9b$incorp <- 'shallow'
#  dat9b$t.incorp <- 4

## ----eval=FALSE------------------------------------------------
#  system.time(
#    pred9b <- alfam2(dat9b, app.name = 'TAN.app', time.name = 'ct',
#                     time.incorp = "t.incorp", group = 'field',
#                     warn = FALSE)
#  )

## ----eval=FALSE------------------------------------------------
#  dat9c <- alfam2(dat9b, app.name = 'TAN.app', time.name = 'ct',
#                  time.incorp = 't.incorp', group = 'field',
#                  warn = FALSE, value = 'incorp')
#  head(dat9c)

## ----eval=FALSE------------------------------------------------
#  system.time(
#    pred9c <- alfam2(dat9c, app.name = 'TAN.app', time.name = 'ct',
#                     time.incorp = "t.incorp", group = 'field',
#                     warn = FALSE, prep.dum = FALSE, prep.incorp = FALSE,
#                     check = FALSE)
#  )

## ----eval=FALSE------------------------------------------------
#  head(pred9b)
#  head(pred9c)
#  all.equal(pred9b$e, pred9c$e)

## --------------------------------------------------------------
set.seed(2609)
dat10 <- data.frame(ctime = 0:84*2, TAN.app = 100, man.dm = 8, 
                   air.temp = 7 + 7*sin(0:84*2 * 2*pi/24) + 
                              rnorm(85, 0, 2), 
                   wind.sqrt = sqrt(1.5 + 0.4*sin(0:84*2 * 2*pi/24)) + 
                              rnorm(85, 0, 0.12), 
                   app.mthd = 'bsth')


## --------------------------------------------------------------
pred10 <- alfam2(dat10, app.name = 'TAN.app', time.name = 'ctime',
                   warn = FALSE)
head(pred10)

## --------------------------------------------------------------
predci1 <- alfam2(dat10, app.name = 'TAN.app', time.name = 'ctime', 
                  warn = FALSE, conf.int = 0.90)
head(predci1)

## --------------------------------------------------------------
plot(er ~ ctime, data = predci1, type = 'l', 
     ylim = c(0, max(predci1$er.upr)))
lines(er.lwr ~ ctime, data = predci1, type = 'l', col = 'blue')
lines(er.upr ~ ctime, data = predci1, type = 'l', col = 'red')

## --------------------------------------------------------------
predci2 <- alfam2(dat10, app.name = 'TAN.app', time.name = 'ctime', 
                  warn = FALSE, conf.int = 0.90, var.ci = c('er', 'j', 'r1'))
head(predci2)

## --------------------------------------------------------------
dat11 <- data.frame(ctime = 168, TAN.app = 50, 
                    app.mthd = 'bc', 
                    man.dm = 1:10, air.temp = 20, wind.sqrt = 2) 

## --------------------------------------------------------------

predci3 <- alfam2(dat11, app.name = 'TAN.app', time.name = 'ctime', 
                  group = 'man.dm', conf.int = 0.90)
print(predci3)

## --------------------------------------------------------------
plot(dat11$man.dm, predci3$er, type = 'o', 
     ylim = c(0, max(predci3$er.upr)))
lines(dat11$man.dm, predci3$er.lwr, col = 'blue')
lines(dat11$man.dm, predci3$er.upr, col = 'blue')

## --------------------------------------------------------------
dim(alfam2pars03var)

## --------------------------------------------------------------
datuc1 <- data.frame(group = 1:100, ctime = 168, TAN.app = 50, 
                     app.mthd = 'bc', 
                     man.dm = rnorm(100, mean = 8, sd = 2), 
                     air.temp = 20, wind.sqrt = 2)
quantile(datuc1$man.dm)

## --------------------------------------------------------------
preduc1 <- alfam2(datuc1, app.name = 'TAN.app', time.name = 'ctime', 
                  group = 'group', conf.int = 'all')
head(preduc1)
dim(preduc1)

## --------------------------------------------------------------
quantile(preduc1$er, c(0.05, 0.95))

## --------------------------------------------------------------
datuc2 <- data.frame(group = 1, ctime = 168, TAN.app = 50, 
                     app.mthd = 'bc', 
                     man.dm = 8,
                     air.temp = 20, wind.sqrt = 2)

## --------------------------------------------------------------
preduc2 <- alfam2(datuc2, app.name = 'TAN.app', 
                  time.name = 'ctime', group = 'group',
                  conf.int = 0.9)
print(preduc2)

## ----eval=FALSE------------------------------------------------
#  write.csv(pred7, 'pred7.csv', row.names = FALSE)

## ----eval=FALSE------------------------------------------------
#  library(data.table)
#  library(ALFAM2)
#  dat1b <- data.table(ctime = 168, TAN.app = 50, man.dm = 8,
#                     air.temp = 20, wind.sqrt = 2,
#                     app.mthd = 'bc')
#  dat1b

## ----eval=FALSE------------------------------------------------
#  pred1b <- alfam2(dat1b, app.name = 'TAN.app', time.name = 'ctime')
#  pred1b
#  class(pred1b)
#  setDT(pred1b)
#  class(pred1b)

## ----eval=FALSE------------------------------------------------
#  library(tibble)
#  dat1c <- tibble(ctime = 168, TAN.app = 50, man.dm = 8,
#                     air.temp = 20, wind.sqrt = 2,
#                     app.mthd.bc = TRUE)
#  dat1c
#  class(dat1c)

## ----eval=FALSE------------------------------------------------
#  pred1c <- alfam2(dat1c, app.name = 'TAN.app', time.name = 'ctime')
#  class(pred1c)
#  pred1c <- as_tibble(pred1c)
#  class(pred1c)

