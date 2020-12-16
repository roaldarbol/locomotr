#' Data filtration
#'
#' Performs filtration, with options for both Butterworth, Loess and Spline.
#'
#' @param data Path to input folder
#' @param df.phase List of joints to be analyzed (begin with capital letter).
#' @param filt 'butter', 'loess' or 'spline'. Defaults to 'spline'.
#' @param fps For butterworth filter, frames per second the data was recorded at.
#' @param cons For loess or spline filter, a constant is used. Defaults to 0.1.
#' @export
#' @importFrom signal 'butter' 'filtfilt'
#' @importFrom stats 'loess' 'smooth.spline'
#' @importFrom zoo 'na.locf'
data_filter <- function(data, df.phase, filt='spline', fps = 120, cons = 0.1){

  # Filter data ----
  if(filt=='none'){
    df.filter <- data

  }else if (filt=='butter'){
    Fs <- fps
    Fn <- Fs/2
    Ws <- (1.4845+0.1532*sqrt(Fs))^2  # Yu, 1988
    Wc <- round(1000*Ws/Fn)/1000
    df.filter <- data.frame()

    pad <- 40
    extra <- data.frame(matrix(nrow=pad, ncol=ncol(data)))
    colnames(extra) <- colnames(data)
    data <- rbind(extra,data,extra)

    for (i in 1:pad){
      data$t[i] <- data$t[pad+1]-((pad+1-i)/Fs)
      times <- nrow(data)-pad
      data$t[times+i] <- data$t[times]+(i/Fs)
    }
    for (i in 2:ncol(data)){
      max <- max(which(!is.na(data[,i])))
      data[max:(max+pad),i] <- zoo::na.locf(data[max:(max+pad),i])
    }

    # THIS PART IS SUPPOSED TO BE ABLE TO CORRECTLY GET IMPACTS
    #
    # time <- sort(c(df.phase$touchdown, df.phase$takeoff, data$t[1]))
    #
    # for (k in 1:(length(time)-2)){
    #   df.temp <- data.table(data)
    #   df.temp <- df.temp[t %inrange% c(time[k], time[k+1])]
    #   df.temp <- data.frame(df.temp)
    #   df.pre <- -df.temp[seq(dim(df.temp)[1],1),]
    #   df.pre$t <- seq(to=df.temp$t[1], length.out = nrow(df.pre), by=1/Fs)
    #   for (j in 2:ncol(df.temp)){
    #     diff <- df.pre[nrow(df.pre),j]-df.temp[1,j]
    #     df.pre[,j] <- df.pre[,j]-diff
    #   }
    #   df.pre <- df.pre[-nrow(df.pre),]
    #
    #   df.post <- -df.temp[seq(dim(df.temp)[1],1),]
    #   df.post$t <- seq(from=df.temp$t[nrow(df.temp)], length.out = nrow(df.post), by=1/Fs)
    #   for (j in 2:ncol(df.temp)){
    #     diff <- df.post[1,j]-df.temp[nrow(df.temp),j]
    #     df.post[,j] <- df.post[,j]-diff
    #   }
    #   df.post <- df.post[-1,]
    #   df.temp <- rbind(df.pre, df.temp, df.post)
    #   df.temp <- df.temp[-1,]

    # for (i in 1:ncol(df.temp)){
    #   if (!all(is.na(df.temp[,i]))){
    #     min <- min(which(!is.na(df.temp[,i])))
    #     max <- max(which(!is.na(df.temp[,i])))
    #     df.temp[min:max,i] <- zoo::na.spline(df.temp[min:max,i], na.rm=TRUE)
    #   }
    # }


    # Butterworth filtering ----

    df.xy <- list()
    df.xy[['x']] <- data[,grep('x', names(data))]
    df.xy[['y']] <- data[,grep('y', names(data))]
    df.filter.list <- list()
    df.filter.list[['x']] <- as.data.frame(data[,'t'])
    df.filter.list[['y']] <- as.data.frame(data[,'t'])
    df.filter.list[['x']][1] <- NA
    df.filter.list[['y']][1] <- NA
    # colnames(df.filter.list[['x']])[1] <- colnames(data)[1]
    # colnames(df.filter.list[['y']])[1] <- colnames(data)[1]


      for (i in 1:length(df.xy)){
        if (names(df.xy[i]) == 'x'){
          b <- signal::butter(2, Wc, type = "low", plane='z')
        } else if(names(df.xy[i]) == 'y'){
          b <- signal::butter(2, Wc, type = "low", plane='z')
        }
        for (j in 1:ncol(df.xy[[i]])){
          if (!all(is.na(df.xy[[i]][,j]))){
            min <- min(which(!is.na(df.xy[[i]][,j]))+1)   #If a new filter is needed pr. segment, put code here
            max <- max(which(!is.na(df.xy[[i]][,j]))-1)
            x <- df.xy[[i]][min:max,j]
            suppressWarnings(df.filter.list[[i]][min:max,j] <- signal::filtfilt(b, x))
            # suppressWarnings(df.filter.list[[i]][min:max,j] <- signal::filter(b, x))
            # k <- rev(df.filter.list[[i]][min:max,j])
            # suppressWarnings(df.filter.list[[i]][min:max,j] <- rev(signal::filter(b, k)))
            colnames(df.filter.list[[i]])[j] <- colnames(df.xy[[i]])[j]
          } else {
            df.filter.list[[i]][j] <- NA
            colnames(df.filter.list[[i]])[j] <- colnames(df.xy[[i]])[j]
          }
        }
      }

      df.filter <- cbind(data$t, df.filter.list[['x']], df.filter.list[['y']])
      colnames(df.filter)[1] <- 't'

      # THIS PART IS SUPPOSED TO BE ABLE TO CORRECTLY GET IMPACTS
      #
      # df.temp.filter <- df.temp.filter[-c(1:nrow(df.pre)),]
      # df.temp.filter <- df.temp.filter[-c(nrow(df.temp.filter)-nrow(df.post)+1:nrow(df.temp.filter)),]
      # df.filter <- rbind(df.filter, df.temp.filter)

  } else if(filt=='loess'){

    df.xy <- list()
    df.xy[['x']] <- data[,grep('x', names(data))]
    df.xy[['y']] <- data[,grep('y', names(data))]
    df.filter.list <- list()
    df.filter.list[['x']] <- as.data.frame(data[,'t'])
    df.filter.list[['y']] <- as.data.frame(data[,'t'])
    colnames(df.filter.list[['x']])[1] <- colnames(data)[1]
    colnames(df.filter.list[['y']])[1] <- colnames(data)[1]

    for (i in 1:length(df.xy)){
      for (j in 1:ncol(df.xy[[i]])){
        min <- min(which(!is.na(df.xy[[i]][,j])))   #If a new filter is needed pr. segment, put code here
        max <- max(which(!is.na(df.xy[[i]][,j])))
        x <- loess(df.xy[[i]][min:max,j]~data$t[min:max], span=cons)
        suppressWarnings(df.filter.list[[i]][min:max,j] <- x$fitted)
        colnames(df.filter.list[[i]])[j] <- colnames(df.xy[[i]])[j]
      }
    }

    df.filter <- cbind(data$t, df.filter.list[['x']], df.filter.list[['y']])
    colnames(df.filter)[1] <- 't'

  }else if(filt=='spline'){

    df.xy <- list()
    df.xy[['x']] <- data[,grep('x', names(data))]
    df.xy[['y']] <- data[,grep('y', names(data))]
    df.filter.list <- list()
    df.filter.list[['x']] <- as.data.frame(data[,'t'])
    df.filter.list[['y']] <- as.data.frame(data[,'t'])
    colnames(df.filter.list[['x']])[1] <- colnames(data)[1]
    colnames(df.filter.list[['y']])[1] <- colnames(data)[1]

    for (i in 1:length(df.xy)){
      for (j in 1:ncol(df.xy[[i]])){
        min <- min(which(!is.na(df.xy[[i]][,j])))   #If a new filter is needed pr. segment, put code here
        max <- max(which(!is.na(df.xy[[i]][,j])))
        x <- smooth.spline(df.xy[[i]][min:max,j], spar=cons)
        suppressWarnings(df.filter.list[[i]][min:max,j] <- x$y)
        colnames(df.filter.list[[i]])[j] <- colnames(df.xy[[i]])[j]
      }
    }

    df.filter <- cbind(data$t, df.filter.list[['x']], df.filter.list[['y']])
    colnames(df.filter)[1] <- 't'
  }

  return(df.filter)
}
