cellsmake=function(zuotu){
  if (!require(readr)) install.packages("readr")
  if (!require(lubridate)) install.packages("lubridate")
  if (!require(magrittr)) install.packages("magrittr")
  Sys.setenv('VROOM_CONNECTION_SIZE'=99999999)
  zuotu[is.na(zuotu)]=''
  #读取模板设定参数----
  dis_col=data.frame(
    quhua=c("福田", "南山", "罗湖", "宝安", "龙岗",
            "盐田", "龙华",'坪山','光明','大鹏','深汕'),
    yanse=c('255,183,77','255,241,118','100,180,246','0,255,255','0,255,0',
            '27,94,31','186,104,200','242,8,160','161,105,56','229,155,155','255,255,255')
  )
  dis_col_font=data.frame(
    quhua=c("福田", "南山", "罗湖", "宝安", "龙岗",
            "盐田", "龙华",'坪山','光明','大鹏','深汕'),
    yanse=c('0,0,0','0,0,0','0,0,0','0,0,0','0,0,0',
            '255,255,255','255,255,255','255,255,255','255,255,255','0,0,0','0,0,0')
  )
  fway_col=data.frame(
    faxian=c('社会面','隔离管控','发现方式待核'),
    yanse=c('255,0,0','13,71,161','255,255,255')
  )
  #生成模板----
  y=cellmodels
  for (i in 1:nrow(zuotu)) {
    k1=paste('采样',sprintf("%02d",i),sep='')
    k2=paste('街道',sprintf("%02d",i),sep='')
    k3=paste('姓名',sprintf("%02d",i),sep='')
    k4=paste('报告',sprintf("%02d",i),sep='')
    k5=paste('职业',sprintf("%02d",i),sep='')
    col_sample0=paste(i*10,i*10,i*10,sep=',')
    col_sample1=fway_col$yanse[fway_col$faxian==zuotu$社会面[i]]
    col_qu0=paste(i*10+5,i*10+5,i*10+5,sep=',')
    col_qu1=dis_col$yanse[dis_col$quhua==zuotu$管辖区[i]]
    col_qu0_font=paste(i*10+8,i*10+8,i*10+8,sep=',')
    col_qu1_font=dis_col_font$yanse[dis_col_font$quhua==zuotu$管辖区[i]]
    y=gsub(k1,zuotu$采样[i],y)%>%
      gsub(k2,zuotu$街道[i],.)%>%
      gsub(k3,zuotu$姓名[i],.)%>%
      gsub(k4,zuotu$报告[i],.)%>%
      gsub(k5,zuotu$职业[i],.)%>%
      gsub(col_sample0,col_sample1,.)%>%
      gsub(col_qu0,col_qu1,.)%>%
      gsub(col_qu0_font,col_qu1_font,.)
  }
  fname=paste('待做图元素库',today()%>%format('%m%d'),'.pos',sep='')
  write_lines(y,fname)
}
