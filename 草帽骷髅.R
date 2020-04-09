library(tidyverse)
library(ggforce)
circleFun <- function(center = NULL,
                      r = NULL, 
                      start = NULL,
                      end = NULL,
                      n = 360){
    r <-  r
    tt <- seq(start,end,length.out = n)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}
hat <- circleFun(c(0,7),13,start = 0.0872664652777,end = pi-0.0872664652777,n = 360)
#geom_path will do open circles, geom_polygon will do filled circles
#红色带子
tape_data <- hat%>%slice(c(1:40,321:360))
red_tape <- rgb(221,65,43,maxColorValue = 255)
#黄色帽子
hat_data <- hat%>%slice(c(40:321))
yellow_hat <- rgb(242,232,184,maxColorValue = 255)

p1 <- ggplot()+geom_polygon(data=tape_data,aes(x,y),fill=red_tape)+
    coord_fixed()+geom_polygon(data=hat_data,aes(x,y),fill=yellow_hat)
###帽檐
p2 <- p1+geom_rect(aes(xmin=-17,xmax=17,ymin=7,ymax=8.2),fill=yellow_hat)+
    geom_circle(aes(x0=-17,y0=7.6,r=0.6),fill=yellow_hat,linetype = 0)+
    geom_circle(aes(x0=17,y0=7.6,r=0.6),fill=yellow_hat,linetype = 0)

#保存下草帽
hh <- p2+scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    #设定主题：主题主要用来控制图像的整体布局
    theme_bw() + #设定白色主题
    theme(panel.grid.major = element_blank(), #删除网格线
          panel.grid.minor = element_blank(), #删除网格线
          panel.border = element_blank(), #删除边框线
          axis.ticks = element_blank(), #删除刻度线
          axis.title = element_blank()) #去除x和y的标签名
#脸部下半轮廓
face_data <- circleFun(c(0,7),13,start = pi,end = 2*pi)
p3 <- p2+geom_path(data=face_data,aes(x,y),size=2)+
    #鼻子嘴巴
    geom_circle(aes(x0=0,y0=0,r=0.75),fill="black")+
    geom_circle(aes(x0=4.5,y0=4.5,r=1.75),fill="black")+
    geom_circle(aes(x0=-4.5,y0=4.5,r=1.75),fill="black")
#下巴
p4 <- p3+geom_arc(aes(x0=0,y0=-7,r=7,start=pi/2.7,end=2*pi-pi/2.7),size=2)+
    geom_arc(aes(x0=0,y0=1.5,r=10.5,start=pi/1.3,end=2*pi-pi/1.3),size=2)+
    geom_arc(aes(x0=0,y0=3,r=10.5,start=pi/1.29,end=2*pi-pi/1.29),size=2)
#三根竖线
three_line_data <- data.frame(group=c("one","one","two","two","three","three"),
                              x=c(0,0,-3,-4,3,4),y=c(-6,-9,-5.7,-8.3,-5.7,-8.3))

p5 <- p4+geom_line(data=three_line_data,aes(x=x,y=y,group=group),size=2)

####四根爪子==============================
#右上
paw <- function(p1,p3){
    p1 <- p1
    p2 <- c(p1[1]+3/5*3,p1[2]+3/5*4)
    aa <- circleFun(c(15.5,16.5),r=2,start = -0.3,end= 3.7)
    en <- data.frame(x=rev(aa$x),y=rev(aa$y))
    bb <- circleFun(c(16.7,14.3),r=2,start=-2.4,end=1)
    enbb <- data.frame(x=rev(bb$x),y=rev(bb$y))
    p3 <- p3
    return(rbind(p1,p2,en,enbb,p3))
}
a <- paw(p1=hat[44,],p3=hat[12,])
p6 <- p5+geom_path(data=a,aes(x,y),linetype=1,size=2)
########-----
b <- data.frame(x=(a$x)*-1,y=a$y)
p7 <- p6+geom_path(data=b,aes(x,y),linetype=1,size=2)

c <- data.frame(x=((a$x)*-1)+6.2,y=(a$y)*-1)
l <- data.frame(x=-5,y=-11.9)
c <- rbind(l,c)
p8 <- p7+geom_path(data=c,aes(x,y),linetype=1,size=2)
d <- data.frame(x=(c$x)*-1,y=c$y)
p9 <- p8+geom_path(data=d,aes(x,y),linetype=1,size=2)


#######
library(extrafont)
loadfonts(device = "win")

png(file="草帽骷髅.png",width = 10000,height = 4500,res = 1200)
p9+scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    #设定主题：主题主要用来控制图像的整体布局
    theme_bw() + #设定白色主题
    theme(panel.grid.major = element_blank(), #删除网格线
          panel.grid.minor = element_blank(), #删除网格线
          panel.border = element_blank(), #删除边框线
          axis.ticks = element_blank(), #删除刻度线
          axis.title = element_blank())+
    labs(caption="Author:Liripo")+theme(text=element_text(family="serif",size = 20,
                                                          face="bold"))
    
dev.off()


