## ----setup, eval = FALSE------------------------------------------------------
#  install.packages("agricolaeplotr")

## ----message=FALSE, warning=FALSE---------------------------------------------
library("ggplot2")
library("agricolae")
library("agricolaeplotr")
library("raster")

## -----------------------------------------------------------------------------
library(agricolae) # origin of the needed design object
trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')

head(outdesign$book,10)

plot_design.factorial_crd(outdesign,ncols=7,nrows=3, width = 1, height = 1)


## ---- echo=TRUE, results='asis'-----------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 1, reverse_y = TRUE)

## ---- echo=TRUE, results='asis'-----------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 1, reverse_x = TRUE)

## ---- results='asis'----------------------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 1, reverse_x = TRUE,reverse_y = TRUE)

## ---- echo = TRUE, results='asis'---------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE)

## ---- echo = TRUE, results='asis'---------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE,space_width = 1,space_height = 1)

## ---- echo = TRUE, results='asis'---------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE,space_width = 0.7,space_height = 0.8)

## ---- echo = TRUE, results='asis'---------------------------------------------
plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE, factor_name = "B")

## ---- echo = TRUE, results='asis'---------------------------------------------
set.seed(129984)
trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')
plot_design.factorial_crd(outdesign,ncols=3,nrows=6, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE, factor_name = "B")

## ---- echo = TRUE, results='asis'---------------------------------------------
set.seed(129866478)
trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')
plot_design.factorial_crd(outdesign,ncols=3,nrows=6, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE, factor_name = "B") + theme_poster()

## ---- echo = TRUE, results='asis'---------------------------------------------
set.seed(12986)
trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')

plot_design.factorial_crd(outdesign,ncols=3,nrows=6, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE, factor_name = "B") + theme_pres()

## ---- echo = TRUE, results='asis'---------------------------------------------
trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')
plot_design.factorial_crd(outdesign,ncols=3,nrows=6, width = 1, height = 2.5 , reverse_x = FALSE,reverse_y = TRUE, factor_name = "B") + theme_pres() + scale_fill_viridis_d()

## -----------------------------------------------------------------------------
set.seed(23488833)
trt <-c(3,2)
outdesign<-design.ab(trt, serie=2, design="lsd",seed = 454555)
length_table <- dim(outdesign$book)[1] # length of the table

outdesign$book$yield <- sample(c(5:12,c(NA,NA,NA)), size = length_table, replace = TRUE)
plot_design.factorial_lsd(outdesign,factor_name = "yield") + scale_fill_viridis_c()


## -----------------------------------------------------------------------------
set.seed(23488833)
trt <-c(3,2)
outdesign<-design.ab(trt, serie=2, design="lsd",seed = 454555)
length_table <- dim(outdesign$book)[1] # length of the table

yield <- sample(c(5:20,c(NA,NA,NA)), size = length_table, replace = TRUE)
df <- cbind(plots=outdesign$book$plots,yield)
head(df,10)
outdesign$book <- merge(outdesign$book,df, by.x = "plots", by.y = "plots")
plot_design.factorial_lsd(outdesign,factor_name = "yield") + scale_fill_viridis_c()


## -----------------------------------------------------------------------------

set.seed(1298664)
plots <- as.factor(1:(8*6))
block <- as.factor((rep(1:6,each=8)))
A <- as.vector(replicate(8,sample(rep(1:2,times=3),6,replace=FALSE)))
outcome <- runif(48,20,100)
experiment <- cbind(plots,block,A,outcome)
experiment <- as.data.frame(experiment)
head(experiment)

experiment_design <- list()
experiment_design$parameters$design<- "factorial"
experiment_design$parameters$applied <-  "rcbd"

experiment_design$book <- experiment
head(experiment_design)
plot_design.factorial_rcbd(experiment_design,factor_name = "A")
plot_design.factorial_rcbd(experiment_design,factor_name = "outcome")


## -----------------------------------------------------------------------------
set.seed(1298664)
t1<-c('a','b','c','d','e',"f","g","h")
t2<-c("u",'v','w','x','y',"z")
outdesign2 <- design.split(trt1=t1, trt2=t2, r=r,serie = 2,
                           seed = 0, kinds = 'Super-Duper',
                           randomization=TRUE,first=TRUE,design = 'lsd')

plot_split_lsd(outdesign2,factor_name_1 = "t1",factor_name_2 = "t2",width = 2,height = 2, subplots = FALSE,labels = "plots")


plot_split_lsd(outdesign2,width = 2,height = 2, subplots = TRUE, labels = "splots", factor_name_1 = "t1", factor_name_2 = "t2")


## -----------------------------------------------------------------------------
set.seed(1298664)
t1<-c('a','b','c','d','e','f','g')
t2<-c('v','w','x','y','z')
r <- 4
outdesign2 <- design.split(trt1=t1, trt2=t2, r=r,
serie = 2, seed = 0, kinds = 'Super-Duper',
randomization=TRUE,first=TRUE,design = 'crd')
plot_split_crd(outdesign2,ncols = 5,nrows=6, subplots = FALSE,
               factor_name_1 = "t1",factor_name_2 = "t2")
plot_split_crd(outdesign2,ncols = 5,nrows=6, subplots = TRUE, labels="splots",factor_name_1 = "t1",factor_name_2 = "t2")


## -----------------------------------------------------------------------------
set.seed(1298664)
T1<-c('a','b','c','d','e',"f","g")
T2<-c("we",'v','w','x','y','z',"d")
r = 3
outdesign2 <- design.split(trt1=T1, trt2=T2, r=r,serie = 2,
 seed = 0, kinds = 'Super-Duper',randomization=TRUE,
 first=TRUE,design = 'rcbd')

plot_split_rcbd(outdesign2,width = 5,height = 5,subplots = FALSE, 
                factor_name_1 = "T1",factor_name_2 = "T2")

plot_split_rcbd(outdesign2,width = 5,height = 5,labels = "splots",
                factor_name_1 = "T1",factor_name_2 = "T2")



## ----message=FALSE, warning=FALSE---------------------------------------------

trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')
plt <- plot_design.factorial_crd(outdesign,ncols=3,nrows=6, width = 5, height = 7.5 , reverse_x = FALSE,reverse_y = TRUE, factor_name = "B") + theme_pres() + scale_fill_viridis_d()

spat_df <- make_polygons(plt,east = 3454206.89, 
                         north = 5939183.21 ,
                         projection_output = '+init=EPSG:4326')

plot(spat_df["fill"],col=spat_df$fill)

# this part does not work well in a vignette
library(leaflet)

spat_df <- sf:::as_Spatial(spat_df)

spat_df <- sp::elide(spat_df,rotate = -90)
                   
 leaflet(spat_df) %>% addPolygons(
   fillColor = spat_df$fill,
   opacity=1,
   color="black",
   fillOpacity = 1) %>% addProviderTiles(provider = "OpenStreetMap.DE")

## -----------------------------------------------------------------------------
varieties<-c('perricholi','yungay','maria bonita','tomasa')
outdesign <-design.youden(varieties,r=2,serie=2,seed=23)
p <- plot_youden(outdesign, labels = 'varieties', width=4, height=3)
stats <- DOE_obj(p)
r <- to_table(stats,part = "net_plot", digits = 2)
r
r <- to_table(stats,part = "gross_plot", digits = 2)
r
r <- to_table(stats,part = "field", digits = 2)
r
r <- to_table(stats,part = "experiment", digits = 2)
r
r <- to_table(stats,part = "all", digits = 2)
r

## -----------------------------------------------------------------------------

varieties<-c('perricholi','yungay','maria bonita','tomasa')
outdesign <-design.youden(varieties,r=2,serie=2,seed=23)
design <- outdesign$book
design

p <- full_control_positions(design,"col","row","varieties","plots",
                       width=3,height=4.5,
                       space_width=1,space_height=1,
                       shift_x=-0.5*3,shift_y=-0.5*4.5)
p

p <- full_control_positions(design,"col","row","varieties","plots",
                       width=3,height=4.5,
                       space_width=0.93,space_height=0.945,
                       start_origin = TRUE)
                       p

## -----------------------------------------------------------------------------
citation("agricolaeplotr")

