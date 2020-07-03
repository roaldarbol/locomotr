#' Kinematics
#'
#' Computes velocities etc.
#'
#' This function outputs the desired results and graphs for a 2D analysis of triple jump
#'
#' @param path Path to input folder
#' @param joints List of joints to be analyzed (begin with capital letter).
#' @export
#' @import reshape2 dplyr
#' @importFrom yaml 'yaml.load_file'
#' @importFrom readxl 'read_excel'
#' @importFrom data.table 'as.data.table'
#' @importFrom zoo 'na.locf' 'na.spline'
#' @importFrom signal 'butter' 'filtfilt'
kinematics_2d <- function(filter='butter', input=NULL){
  #path = "/Users/roaldarbol/Library/Mobile Documents/com~apple~CloudDocs/Documents/SportsMechanics/Jannick/practice"
  #segments = c('Head', 'Shoulder', 'Ankle', 'Knee', 'Hip')
  #joints = c("Ankle", "Knee", "Hip")


  if (!length(input) == 3) {
    data <- data.frame(readxl::read_excel(Sys.glob('data/*.xlsx')))
    setup <- yaml::yaml.load_file(Sys.glob('data/setup*.yaml'))
    comments <- yaml::yaml.load_file(Sys.glob('data/comments*.yaml'))
  } else {
    df <- input[[1]]
    setup <- input[[2]]
    comments <- input[[3]]
  }


  for (i in 1:ncol(df)){
    if (!all(is.na(df[,i]))){
      min <- min(which(!is.na(df[,i])))
      max <- max(which(!is.na(df[,i])))
      df[min:max,i] <- zoo::na.spline(df[min:max,i], na.rm=TRUE)
    }
  }

  # Logical matrix & approximations ----
  matrixNA <- is.na(df)

  if(filter=='none'){
    df.filter <- df

  }else if (filter=='butter'){
    Fs <- 120
    Fn <- Fs/2
    Ws <- (1.4845+0.1532*sqrt(Fs))^2  # Yu, 1988
    Wc <- round(1000*Ws/Fn)/1000
    df.filter <- data.frame()

    # pad <- 40
    # extra <- data.frame(matrix(nrow=pad, ncol=ncol(df)))
    # colnames(extra) <- colnames(df)
    # df <- rbind(extra,df,extra)
    #
    # for (i in 1:pad){
    #   df$t[i] <- df$t[pad+1]-((pad+1-i)/Fs)
    #   times <- nrow(df)-pad
    #   df$t[times+i] <- df$t[times]+(i/Fs)
    # }
    # for (i in 2:ncol(df)){
    #   max <- max(which(!is.na(df[,i])))
    #   df[max:(max+pad),i] <- zoo::na.locf(df[max:(max+pad),i])
    # }

    time <- sort(c(setup$phase$touchdown, setup$phase$takeoff, df$t[1]))

    for (k in 1:(length(time)-2)){
      df.temp <- data.table(df)
      df.temp <- df.temp[t %inrange% c(time[k], time[k+1])]
      df.temp <- data.frame(df.temp)
      df.pre <- -df.temp[seq(dim(df.temp)[1],1),]
      df.pre$t <- seq(to=df.temp$t[1], length.out = nrow(df.pre), by=1/Fs)
      for (j in 2:ncol(df.temp)){
        diff <- df.pre[nrow(df.pre),j]-df.temp[1,j]
        df.pre[,j] <- df.pre[,j]-diff
      }
      df.pre <- df.pre[-nrow(df.pre),]

      df.post <- -df.temp[seq(dim(df.temp)[1],1),]
      df.post$t <- seq(from=df.temp$t[nrow(df.temp)], length.out = nrow(df.post), by=1/Fs)
      for (j in 2:ncol(df.temp)){
        diff <- df.post[1,j]-df.temp[nrow(df.temp),j]
        df.post[,j] <- df.post[,j]-diff
      }
      df.post <- df.post[-1,]
      df.temp <- rbind(df.pre, df.temp, df.post)
      df.temp <- df.temp[-1,]

      # for (i in 1:ncol(df.temp)){
      #   if (!all(is.na(df.temp[,i]))){
      #     min <- min(which(!is.na(df.temp[,i])))
      #     max <- max(which(!is.na(df.temp[,i])))
      #     df.temp[min:max,i] <- zoo::na.spline(df.temp[min:max,i], na.rm=TRUE)
      #   }
      # }

      # Butterworth filtering ----
      df.xy <- list()
      df.xy[['x']] <- df.temp[,grep('x', names(df.temp))]
      df.xy[['y']] <- df.temp[,grep('y', names(df.temp))]
      df.filter.list <- list()
      df.filter.list[['x']] <- as.data.frame(df.temp[,'t'])
      df.filter.list[['y']] <- as.data.frame(df.temp[,'t'])
      colnames(df.filter.list[['x']])[1] <- colnames(df.temp)[1]
      colnames(df.filter.list[['y']])[1] <- colnames(df.temp)[1]


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

      df.temp.filter <- cbind(df.temp$t, df.filter.list[['x']], df.filter.list[['y']])
      colnames(df.temp.filter)[1] <- 't'
      df.temp.filter <- df.temp.filter[-c(1:nrow(df.pre)),]
      df.temp.filter <- df.temp.filter[-c(nrow(df.temp.filter)-nrow(df.post)+1:nrow(df.temp.filter)),]
      df.filter <- rbind(df.filter, df.temp.filter)
    }

  } else if(filter=='loess'){

    df.xy <- list()
    df.xy[['x']] <- df[,grep('x', names(df))]
    df.xy[['y']] <- df[,grep('y', names(df))]
    df.filter.list <- list()
    df.filter.list[['x']] <- as.data.frame(df[,'t'])
    df.filter.list[['y']] <- as.data.frame(df[,'t'])
    colnames(df.filter.list[['x']])[1] <- colnames(df)[1]
    colnames(df.filter.list[['y']])[1] <- colnames(df)[1]

    cons=.06

    for (i in 1:length(df.xy)){
      for (j in 1:ncol(df.xy[[i]])){
        min <- min(which(!is.na(df.xy[[i]][,j])))   #If a new filter is needed pr. segment, put code here
        max <- max(which(!is.na(df.xy[[i]][,j])))
        x <- loess(df.xy[[i]][min:max,j]~df$t[min:max], span=cons)
        suppressWarnings(df.filter.list[[i]][min:max,j] <- x$fitted)
        colnames(df.filter.list[[i]])[j] <- colnames(df.xy[[i]])[j]
        }
    }

    df.filter <- cbind(df$t, df.filter.list[['x']], df.filter.list[['y']])
    colnames(df.filter)[1] <- 't'

  }else if(filter=='spline'){

    df.xy <- list()
    df.xy[['x']] <- df[,grep('x', names(df))]
    df.xy[['y']] <- df[,grep('y', names(df))]
    df.filter.list <- list()
    df.filter.list[['x']] <- as.data.frame(df[,'t'])
    df.filter.list[['y']] <- as.data.frame(df[,'t'])
    colnames(df.filter.list[['x']])[1] <- colnames(df)[1]
    colnames(df.filter.list[['y']])[1] <- colnames(df)[1]

    cons = 0.1

    for (i in 1:length(df.xy)){
      for (j in 1:ncol(df.xy[[i]])){
        min <- min(which(!is.na(df.xy[[i]][,j])))   #If a new filter is needed pr. segment, put code here
        max <- max(which(!is.na(df.xy[[i]][,j])))
        x <- smooth.spline(df.xy[[i]][min:max,j], spar=cons)
        suppressWarnings(df.filter.list[[i]][min:max,j] <- x$y)
        colnames(df.filter.list[[i]])[j] <- colnames(df.xy[[i]])[j]
      }
    }

    df.filter <- cbind(df$t, df.filter.list[['x']], df.filter.list[['y']])
    colnames(df.filter)[1] <- 't'
  }

  # Creating lists of joints ----
  segments <- c()
  segments.x <- c()
  segments.y <- c()
  joints.theta <- c()

  for (i in 1:length(setup$skeleton$ref.points)){
    segments[i] <- setup$skeleton$ref.points[i]
    segments.x[i] <- paste('x', setup$skeleton$ref.points[i], sep = '.')
    segments.y[i] <- paste('y', setup$skeleton$ref.points[i], sep = '.')
  }

  for (i in 1:length(setup$skeleton$joints)){
    joints.theta[i] <- paste('theta', names(setup$skeleton$joints[i]), sep = '.')
  }

  joints.list <- list(segments.x, segments.y, joints.theta)

  # Compute angles ----
  df.angles <- as.data.frame(df.filter$t)
  colnames(df.angles) <- 't'
  for (i in 1:length(setup$skeleton$joints)){
    joint <- paste('theta', names(setup$skeleton$joints[i]), sep='.')
    x.vertex <- paste('x', setup$skeleton$joints[[i]][1], sep='.')
    x.arm <- paste('x', setup$skeleton$joints[[i]][2], sep='.')
    x.base <- paste('x', setup$skeleton$joints[[i]][3], sep='.')
    y.vertex <- paste('y', setup$skeleton$joints[[i]][1], sep='.')
    y.arm <- paste('y', setup$skeleton$joints[[i]][2], sep='.')
    y.base <- paste('y', setup$skeleton$joints[[i]][3], sep='.')

    min.vertex <- min(which(!is.na(df.filter[,x.vertex])))
    min.arm <- min(which(!is.na(df.filter[,x.arm])))
    min.base <- min(which(!is.na(df.filter[,x.base])))
    min <- max(min.vertex, min.arm, min.base)

    max.vertex <- max(which(!is.na(df.filter[,x.vertex])))
    max.arm <- max(which(!is.na(df.filter[,x.arm])))
    max.base <- max(which(!is.na(df.filter[,x.base])))
    max <- min(max.vertex, max.arm, max.base)

      for (j in min:max){
        angle.rad  <- atan2(df.filter[[j,y.arm]] - df.filter[[j,y.vertex]], df.filter[[j,x.arm]] - df.filter[[j,x.vertex]]) -
                      atan2(df.filter[[j,y.base]] - df.filter[[j,y.vertex]], df.filter[[j,x.base]] - df.filter[[j,x.vertex]])
        angle.deg <- rad2deg(angle.rad)
        if (angle.deg<1){
          df.angles[j,joint]  <- angle.deg+360
        # # } else if (angle.rad>180){
        # #   df.filter[j,joint]  <- angle.rad-90
         } else {
          df.angles[j,joint]  <- angle.deg
        }
      }
    }

  # Compute linear velocities and accelerations ----
  nrows <- nrow(df.filter)
  for (i in 2:ncol(df.filter)){
    colnames(df.filter)[i] <- paste('pos', colnames(df.filter)[i], sep='.')
  }
  com <- centerofmass(df.filter, setup)
  df.filter <- cbind(df.filter, com)

  # Velocity
  pos.names <- grep('pos', names(df.filter), value=TRUE)
  new.names <- sub('pos.', '', pos.names)
  nrows <- nrow(df.filter)
  for (i in 1:length(pos.names)){
    varname <- paste('vel', new.names[i], sep='.')
    diff.var <- diff(df.filter[,pos.names[i]])
    diff.t <- diff(df.filter[,'t'])
    df.filter[2:nrows,varname] <- diff.var/diff.t
  }

  # Acceleration
  vel.names <- grep('vel', names(df.filter), value=TRUE)
  new.names <- sub('vel.', '', vel.names)
  nrows <- nrow(df.filter)
  for (i in 1:length(vel.names)){
    varname <- paste('acc', new.names[i], sep='.')
    diff.var <- diff(df.filter[,vel.names[i]])
    diff.t <- diff(df.filter[,'t'])
    df.filter[2:nrows,varname] <- diff.var/diff.t
  }

  # Compute angular speeds and accelerations ----
  nrows <- nrow(df.angles)
  for (i in 2:ncol(df.angles)){
    colnames(df.angles)[i] <- paste('pos', colnames(df.angles)[i], sep='.')
  }

  # Velocity
  pos.names <- grep("pos", names(df.angles), value=TRUE)
  for (i in 1:length(pos.names)){
    new.names <- sub('pos.', '', pos.names)
    varname <- paste('vel', new.names[i], sep='.')
    diff.angle <- diff(df.angles[,pos.names[i]])
    diff.t <- diff(df.angles[,'t'])
    df.angles[2:nrows,varname] <- diff.angle/diff.t
  }

  # Acceleration
  vel.names <- grep("vel", names(df.angles), value=TRUE)
  for (i in 1:length(vel.names)){
    new.names <- sub('vel.', '', pos.names)
    varname <- paste('acc', new.names[i], sep='.')
    diff.angle <- diff(df.angles[,vel.names[i]])
    diff.t <- diff(df.angles[,'t'])
    df.angles[2:nrows,varname] <- diff.angle/diff.t
  }


  # Compute touchdown and take-off ----
  df.filter <- df.filter %>%
    mutate(phase = "flight") %>%
    select(t, phase, everything())
  df.filter <- cbind(df.filter, df.angles[2:ncol(df.angles)])

  for(j in 1:length(setup$phase$touchdown)){
    for(k in 1:nrow(df.filter)){
      if(df.filter$t[k] >= setup$phase$touchdown[j] &&
         df.filter$t[k] <= setup$phase$takeoff[j]){
        df.filter$phase[k] = "support"
      }
    }
  }

  if (filter=='butter'){
    df.filter$phase[nrow(df.filter)] <- "support"
  }


  df.filter <- df.filter %>%
    mutate(td = if_else(phase != lag(phase) & phase=="support", TRUE, FALSE),
           to = if_else(phase != lag(phase) & phase=="flight", TRUE, FALSE))

  # # Round off - NEEDS UPDATING! ----
  # df <- na.exclude(df)
  # df.1 <- df  %>%
  #   select_if(stringr::str_detect(names(.), "t\\b")) %>%
  #   mutate_if(is.numeric, round, 3)
  # df.2 <- df  %>%
  #   select_if(stringr::str_detect(names(.), "t\\b", negate = TRUE)) %>%
  #   mutate_if(is.numeric, round, 2)
  # df <- cbind(df.1, df.2)
  #
  #
  # # Round off
  # df.filter <- na.exclude(df.filter)
  # df.filt1 <- df.filter  %>%
  #   select_if(stringr::str_detect(names(.), "t\\b")) %>%
  #   mutate_if(is.numeric, round, 3)
  # df.filt2 <- df.filter  %>%
  #   select_if(stringr::str_detect(names(.), "t\\b", negate = TRUE)) %>%
  #   mutate_if(is.numeric, round, 2)
  # df.filter <- cbind(df.filt1, df.filt2)


  # Melting data ----
  df.melt <- reshape2::melt(df.filter, id=c('t', 'phase', 'td', 'to'), na.rm = FALSE)
  df.melt <- na.exclude(df.melt)
  df.melt <- df.melt %>%
    mutate(motion = as.factor(if_else(grepl('theta', variable), 'ang', 'lin')),
           vector = as.factor(case_when(grepl('pos', variable) == TRUE ~ 'Position',
                                        grepl('vel', variable) == TRUE ~ 'Velocity',
                                        grepl('acc', variable) == TRUE ~ 'Acceleration')),
           direction = as.factor(case_when(grepl('x', variable) == TRUE ~ 'Horizontal',
                                           grepl('y', variable) == TRUE ~ 'Vertical')))

  df.melt$segment <- NA
  for (i in 1:length(segments)){
    df.melt$segment <- case_when(grepl(segments[i], df.melt$variable) == TRUE ~ segments[i],
                                 TRUE ~ as.character(df.melt$segment))
  }
  for (i in 1:length(joints.theta)){
    df.melt$segment <- case_when(grepl(joints.theta[i], df.melt$variable) == TRUE ~ sub('theta.', '', joints.theta[i]),
                                 TRUE ~ as.character(df.melt$segment))
  }
  df.melt$segment <- as.factor(df.melt$segment)

  data.list <- list(df.filter, df.melt, setup, matrixNA)
  return(data.list)
}
