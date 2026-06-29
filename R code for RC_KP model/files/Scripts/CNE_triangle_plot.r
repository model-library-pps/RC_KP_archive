#################################################################
#Plotting in a CNE triangle, similar to a soil
#CNE_N values should be read parallel to green lines in triangle
#CNE_P values should be read parallel to black lines in triangle
#CNE_K values should be read parallel to red lines in triangle
#See example given for required input
#
#Tom Schut, December 2021
#
#################################################################
#
#EXAMPLE not run
# df <-data.frame(pCNE_N = c(10, 30, 60, 90 ),
#                 pCNE_P = c(10, 10, 20, 5 ),
#                 pCNE_K = c(80, 60, 20, 5 ),
#                 colors = c("red","green","blue","purple"),
#                 pch = c(21,22,23,24),
#                 leg_text = c("1980","1984","1988","1992"))
# plot_triangle(df)
# 
# plot_triangle(df, defined_symbols = TRUE, show_legend = TRUE)
# 
# df <-data.frame(pCNE_N = c(30, 20, 10, 70 ),
#                 pCNE_P = c(40, 50, 20, 20 ),
#                 pCNE_K = c(30, 30, 70, 10 ),
#                 colors = c("red","green","blue","purple"),
#                 pch = c(21,22,23,24),
#                 leg_text = c("1980","1984","1988","1992"))
# plot_triangle(df)
# plot_triangle(df,optimum_threshold = 28, xyoff = 10)



plot_triangle <- function(df, 
                          defined_symbols = FALSE, 
                          show_legend = FALSE, 
                          main = "",
                          optimum_threshold = 28, #boundary value for optimum CNE
                          xyoff = 10){            #determines ranges of CNE shown
  #Plot in triangle 
  #with CNE_P on x axis running from (0 + xyoff) - (100 - xyoff)
  #CNE_K on left y axis running from (100 - xyoff) to (0 + xyoff)
  #CNE_N on right y axis running from (0 + xyoff) to (100 - xyoff)
  #Draw triangle first

  xmin = xyoff
  xmax = 100- 2* xyoff
  xmed= (xmin+xmax)/2
  ymin = xyoff
  ymax = 100 - 2 * xyoff
  ymed= (ymin+ymax)/2
  
  plot(x=c(xmin, xmax),y=c(ymin, ymin),type="l",col="black", lwd=2,lty=1,
       xlim=c(xmin, xmax),ylim=c(ymin, ymax),
       xaxt="n",xlab="", las = 2, #mgp = c(3, 0.5, -0.5)
       yaxt="n",ylab="", bty="n", main=main)
  lines(x=c(xmed, xmax),y = c(ymax, ymin),col="red", lwd=2,lty=1) #CNE_K axis
  lines(x=c(xmin, xmed),y = c(ymin, ymax), col="green", lwd=2,lty=1) #CNE_N axis
  #Optimum CNE triangle, when CNE_N, CNE_P and CNE_K are > optimum_threshold  
  #y= ymin + (x-optimum_threshold)*2, with y>optimum_threshold: 
  #xmin = optimum_threshold + (optimum_threshold-ymin)/2
  min_x <- optimum_threshold + (optimum_threshold - ymin)/2
  max_y <- ymin + 2*(xmed - optimum_threshold)
  lines(x=c(min_x, xmed),y= c(optimum_threshold, max_y),col="black", lwd=2,lty=1)
  #slope of 2:
  #(max_y - min_y) = 2 * (max_x - xmed)
  #max_x = 0.5 * (max_y - min_y) + xmed
  max_x <- 0.5 * (max_y - optimum_threshold) + xmed
  lines(x=c(xmed, max_x),y= c(max_y, optimum_threshold),col="black", lwd=2,lty=1)
  lines(x=c(min_x, max_x),y= c(optimum_threshold, optimum_threshold), col="black", lwd=2,lty=1)
  

  for(x in seq(xmin + 5, xmax -5, 10)){
    #lines to read off the x-axis are running parallel to CNE_N axis: 
    slope = (ymax -ymin)/(xmax-xmed) 
    y <- ymax - (x -xmin)
    max_x <- xmed + (x - xmin)/slope #maximum x increases as y decreases
    lines(x=c(x, max_x),y=c(ymin, y), col="black", lwd=0.5,lty=3)
    text(max_x + 1.5, y + 1, y, srt = 0) #y_CNE_N-axis
    mtext(text = x, at = x, side =1, srt = 90, line = -0.5, cex=0.8) #x_CNE_P-axis
    #text(x, ymin - 1, x, srt = 0) #y_CNE_N-axis
    
    #lines to read off the CNE_N axis run parallel to the CNE_K axis: 
    slope = (ymax - ymin)/(xmed-xmax) #negative slope
    min_x <- x + (x - xmin)/slope
    y <- ymin + (x - xmin)
    lines(x=c(min_x, x),y=c(y, ymin), col="green", lwd=0.5,lty=3)
    text(min_x - 1, y + 3, ymin + ymax - y, srt = -45)       #y_CNE_K-axis
    
    #line to read off x_CNE_P
  }
  #Lines to read off y_CNE_K
  for(y in seq(ymin +5, ymax -5, 10)){
    min_x <- xmin + 0.5 * (y - ymin)
    max_x <- xmax - 0.5 * (y - ymin)
    lines(x=c(min_x, max_x), y=c(y, y), col="red", lwd=0.5, lty=3)
  }

  text(xmin + 0.4 *(xmed - xmin), ymin + 0.6 * (ymax- ymin), "CNE_N, %", srt = 48)
  text(xmax - 0.4 *(xmed - xmin), ymin + 0.6 * (ymax- ymin), "CNE_P, %", srt = -48)
  mtext("CNE_K, %", side=1, line= 0.5, outer=FALSE)

  
#plots points
  rs <- rowSums(subset(df, select=c("pCNE_N","pCNE_P","pCNE_K")))
  ii <- which(rs < 99.5 | rs > 100.5)
  if( length(ii) > 0){
    print(df[ii,])
    stop("error: sums of pCNE_N, pCNE_P,pCNE_K must be 100")
  }
  
  
  if(defined_symbols){
    cols <- df[,"colors"]
    pchs <- df[,"pch"]
  }else{
    cols <- rainbow(nrow(df))
    pchs <- rep(21,nrow(df))
  }
  for(i in 1:nrow(df)){
    #If the CNE_K increases, the x value for plotting also increases
    #Values have to shift with according to the axis lines.
    #These lines have a slope of (ymax-ymin)/(xmed-xmin) for the CNE_P axis 
    #and (ymax-ymin)/(xmed-xmax) for the CNE_N axis.
    #So when y increases with 10, x must increase with 10 * (xmed-xmin) /(ymax-ymin)
    #Take a triangle with values between 0-100 and a value 
    #of CNE_P = 60, CNE_K = 20. These must be plotted in the plotting device at:
    #y = 20, x = 60 + 20 * (50-0) /(100-0) = 70
    #when these points are plotted in a triangle with value of 10-80:
    #y = 20, x = 60 + (20- 10) * (45-10) /(80-10) = 65
    x <-  df[i, "pCNE_K"]
    y_CNE_P <- df[i, "pCNE_P"] #this gives y value in horizontal direction
    y_CNE_N <- df[i, "pCNE_N"]
    x= x + (y_CNE_P - ymin) * (xmed - xmin)/(ymax - ymin)  # x-axis 
    points(x,y_CNE_P,col="black", pch=pchs[i], bg=cols[i])

  }
  if(show_legend){
    uleg_txt  <- unique(df[,"leg_text"])
    ncol <- ceiling(length(uleg_txt)/8) #max 8 items in one column
    ix1 <- NULL
    ix2 <- NULL
    for(ult in uleg_txt){
      ii <- which(df[,"leg_text"] == ult)
      if(length(ix1) < 8){
        ix1 <- c(ix1, ii[1])
      }else{
        ix2 <- c(ix2, ii[1])
      }
      
    }
    legend("topleft", legend=df[ix1,"leg_text"],col="black", pt.bg=df[ix1,"colors"], pch = df[ix1,"pch"], bty="n" )
    if(ncol >1 ){
      legend("topright", legend=df[ix2,"leg_text"],col="black", pt.bg=df[ix2,"colors"], pch = df[ix2,"pch"], bty="n" )
    }
  }
}


df <-data.frame(pCNE_N = c(30, 20, 10, 70 ),
                pCNE_P = c(40, 50, 20, 20 ),
                pCNE_K = c(30, 30, 70, 10 ),
                colors = c("red","green","blue","purple"),
                pch = c(21,22,23,24),
                leg_text = c("1980","1984","1988","1992"))
plot_triangle(df,optimum_threshold = 28, xyoff = 10)