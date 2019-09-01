# Model code to accompany "Transmission dynamics
# Author: AJ Kucharski (2017-)

# Bar plot function

bar.plot <- function(xx,yy,xlim_d,ylim_d,x_label,y_label,lwd_d,col_def){
  
  plot(c(-1,-1),xlim = xlim_d , ylim = ylim_d ,xlab=x_label,ylab=y_label,xaxs="i",yaxs="i",bty="l")
  
  for(ii in 1:length(xx)){
    lines(c(xx[ii],xx[ii]),c(0,yy[ii]),col=col_def,lwd=lwd_d,lend='square')
  }
  
}


# - - - - - - - - - - - - - - - - - - - - - 
# Figure 1 chain plots

fig1_chain_plots <- function() {
  
  data_network <- read_csv("data/fig1_network.csv")
  
  par(mfrow=c(1,3),mgp=c(2,0.7,0),mar=c(3,3,1,1),las=1)
  
  # Case timeseries
  
  c_data <- table(floor(data_network$onset))
  c_data_x <- as.numeric(names(c_data))
  c_data_y <- as.numeric(c_data)
  
  bar.plot( xx=c_data_x,
            yy=c_data_y,
            xlim_d=c(-3,80),
            ylim_d=c(0,3),
            x_label="days since first case", y_label="cases", lwd_d=1.5, col_def="red" )
  
  # Serial interval
  
  serial_data <- table(round(data_network$delay))
  serial_data_x <- as.numeric(names(serial_data))
  serial_data_y <- as.numeric(serial_data)
  
  bar.plot( xx=serial_data_x,
            yy=serial_data_y,
            xlim_d=c(0,15),
            ylim_d=c(0,8),
            x_label="days", y_label="frequency", lwd_d=5, col_def="orange" )
  
  
  # Secondary cases
  
  second_data <- table(round(data_network$n_infected))
  second_data_x <- as.numeric(names(second_data))
  second_data_y <- as.numeric(second_data)
  
  bar.plot( xx=second_data_x,
            yy=second_data_y,
            xlim_d=c(-0.5,6),
            ylim_d=c(0,13),
            x_label="number", y_label="frequency", lwd_d=5, col_def="red" )
  
  
  dev.copy(pdf,paste0("plots/fig1_cases.pdf"),width=8,height=1.5)
  dev.off()
  
  
}


# - - - - - - - - - - - - - - - - - - - - - 
# Figure 2 reproduction number
fig2_statistics <- function(){
  
  data_input = read_csv("data/data_outbreaks.csv")

  data_type = unique(data_input$type); data_type = data_type[data_type!=0 & !is.na(data_type)] # Unique colours
  
  
  # Plot data
  par(mfrow=c(1,1),mgp=c(2,0.7,0),mar=c(4,4,1,1),las=1)

  # - - -
  # Plot R vs serial interval
  data_plot = data_input %>% filter(.,type!=0 & RR!=0 & serial_interval!=0)
  #data_type = unique(data_plot$type)
  match_col = match(data_plot$type,data_type)
  

  yticks = c(1/(3600*24),1/(60*24),1/24,1,28,365,365*100); ytick_lab = c("1 sec","1 min","1 hr","1 day","1 mth","1 yr","100 yr")
  xticks = c(10^seq(-4,4,1),50)
  
  plot(-1,-1,xlab="",xaxs="i",xaxt="n",yaxs="i",yaxt="n",ylab="",bty="l",col="white",log="xy",ylim=c(yticks[1],1.05*tail(yticks,1)),xlim=c(0.05,tail(xticks,1)))
  
  for(ii in 1:length(yticks)){
    lines(c(0.01,1e4),c(yticks[ii],yticks[ii]),col="light gray")
  }
  
  lines(c(1,1),c(1e-6,1e6),col="black",lty=2)
  points(data_plot$RR,data_plot$serial_interval,pch=19,col=col.list[match_col],cex=0.7)
  #points(data_plot$RR,data_plot$serial_interval,col="black")
  
  axis(2, at = yticks,labels = ytick_lab,col = "black") 
  axis(1, at = xticks,labels = xticks,col = "black") 
  title(ylab="serial interval", line=3, cex.lab=1)
  title(xlab="reproduction number", line=2, cex.lab=1)

  dev.copy(pdf,paste("plots/reproduction_serial.pdf",sep=""),width=6,height=4,useDingbats=F)
  dev.off()
  
  # - - -
  # Plot doubling time
  par(mfrow=c(1,1),mgp=c(2,0.7,0),mar=c(4,4,2,2),las=1)
  data_plot = data_input %>% filter(.,type!=0 & doubling_time!=0)
  match_col = match(data_plot$type,data_type)

  plot(data_plot$doubling_time,data_plot$doubling_time,xlab="",xaxs="i",xaxt="n",yaxs="i",yaxt="n",ylab="",bty="l",col="white",log="x",ylim=c(-1,1),xlim=c(1e-5,tail(yticks,1)))
  points(data_plot$doubling_time,0+0*data_plot$RR,pch=19,col=col.list[match_col])
  #points(data_plot$doubling_time,0+0*data_plot$RR,col="black")
  
  axis(1, at = yticks,labels = ytick_lab,col = "black") 
  title(xlab="doubling time", line=2, cex.lab=1)
  
  dev.copy(pdf,paste("plots/doubling.pdf",sep=""),width=6,height=1.5,useDingbats=F)
  dev.off()
  
  # - - -
  # Plot variance
  # data_plot = data_input %>% filter(.,type!=0 & variance!=0)
  # match_col = match(data_plot$type,data_type)
  # yticks = 10^seq(-4,5,1)
  # 
  # plot(0,xlab="",xaxs="i",xaxt="n",yaxs="i",yaxt="n",ylab="",bty="l",col="white",log="xy",xlim=c(0.05,tail(xticks,1)),ylim=c(1e-1,5e2))
  # 
  # points(data_plot$RR,data_plot$variance,pch=19,col=col.list[match_col])
  # axis(2, at = yticks,labels = yticks,col = "black") 
  # axis(1, at = xticks,labels = xticks,col = "black") 
  # title(ylab="variance", line=3, cex.lab=1)
  # title(xlab="reproduction number", line=2, cex.lab=1)
  # 
  # dev.copy(pdf,paste("plots/variance.pdf",sep=""),width=6,height=1.5)
  # dev.off()
  
  
  
}




# - - - - - - - - - - - - - - - - - - - - - 
# Figure 3

fig3_research <- function() {
  
  data_papers <- read_csv("data/social_papers.csv")
  data_papers_users <- read_csv("data/social_papers_users.csv")
  
  names_platforms <- tail(names(data_papers),-1)
  
  # Case timeseries
  
  par(mfrow=c(1,2),mgp=c(2,0.7,0),mar=c(3,3.5,1,1),las=1)
  
  # Case timeseries
  
  plot(c(1,1),xlim=c(2008,2020),ylim=c(0,200),xlab="",xaxs="i",yaxs="i",ylab="",bty="l",col="white")
  #axis(2, at = yticks,labels = ytick_lab,col = "black") 
  #axis(1, at = xticks,labels = xticks,col = "black") 
  title(ylab="WoS articles", line=2, cex.lab=1)
  title(xlab="year", line=2, cex.lab=1)
  
  for(ii in 1:length(names_platforms)){
    
    data_plot <- data_papers[,c(1,ii+1)] %>% as.matrix()
    lines(data_plot[,1],data_plot[,2]+1e-6,col=col.list[ii],lwd=2)
  #  text(x=2010,y=tail(data_plot[,2],1),adj=0,labels=names_platforms[ii],col=col.list[ii])
    
  }
  
  # Papers vs users
  
  plot(c(1,1),xlim=c(0,2500),ylim=c(0,200),xlab="",xaxs="i",yaxs="i",ylab="",bty="l",col="white")
  
  #axis(2, at = yticks,labels = ytick_lab,col = "black") 
  #axis(1, at = xticks,labels = xticks,col = "black") 
  title(ylab="WoS articles in 2018", line=2, cex.lab=1)
  title(xlab="users in 2018 (millions)", line=2, cex.lab=1)
  
  
  for(ii in 1:length(names_platforms)){
    
    data_plot <- data_papers_users %>% filter(Platform==names_platforms[ii]) %>% select(Users,Papers)
    points(data_plot$Users,data_plot$Papers,col=col.list[ii],pch=19)
  #  text(x=data_plot$Users+20,y=data_plot$Papers,adj=0,labels=names_platforms[ii],col=col.list[ii])

  }
  
  # data_model <- data_papers_users %>% filter(Platform!="Twitter" & Platform!="WhatsApp")
  # model <- lm(Papers ~Users ,data_model)
  # xx <- seq(0,2500)
  # lines(xx, model$coefficients[1] + model$coefficients[2]*xx)

  
  dev.copy(pdf,paste0("plots/fig3_papers.pdf"),width=10,height=4)
  dev.off()
  
  
}




