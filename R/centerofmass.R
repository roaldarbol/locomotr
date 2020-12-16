#' Calculate center of mass
#'
#' Computes center of mass using a data set and a .yaml meta file.
#'
#' @param data Data file
#' @param setup A setup file.
#' @importFrom stats 'complete.cases'
#' @export

centerofmass <- function(data, setup = setup){
  segment.com <- data.frame(tibble(.rows = nrow(data)))
                            for (i in 1:length(setup$skeleton$segments)){
                              name.x <- paste0('pos.x.', names(setup$skeleton$segments[i]))
                              name.y <- paste0('pos.y.', names(setup$skeleton$segments[i]))
                              if (length(setup$skeleton$segments[[i]][['endpoints']]) == 1){
                                origin.x <- paste0('pos.x.', setup$skeleton$segments[[i]][['endpoints']][[1]])
                                origin.y <- paste0('pos.y.', setup$skeleton$segments[[i]][['endpoints']][[1]])
                                segment.com[,name.x] <- data[,origin.x]
                                segment.com[,name.y] <- data[,origin.y]
                              } else if (length(setup$skeleton$segments[[i]][['endpoints']]) == 2){
                                origin.x <- paste0('pos.x.', setup$skeleton$segments[[i]][['endpoints']][[1]])
                                origin.y <- paste0('pos.y.', setup$skeleton$segments[[i]][['endpoints']][[1]])
                                origin.com <- setup$skeleton$segments[[i]][['com']][[1]]
                                end.x <- paste0('pos.x.', setup$skeleton$segments[[i]][['endpoints']][[2]])
                                end.y <- paste0('pos.y.', setup$skeleton$segments[[i]][['endpoints']][[2]])
                                end.com <- setup$skeleton$segments[[i]][['com']][[2]]
                                segment.com[,name.x] <- data[,origin.x]*origin.com + data[,end.x]*end.com
                                segment.com[,name.y] <- data[,origin.y]*origin.com + data[,end.y]*end.com
                              }
                            }

                            # Calculate wholebody CoM x and y seperately
                            segment.com[!stats::complete.cases(segment.com), ] <- NA
                            x <- which(grepl('x', names(segment.com)))
                            y <- which(grepl('y', names(segment.com)))
                            segment.com.x <- segment.com[,x]
                            segment.com.y <- segment.com[,y]
                            bodymass <- setup$profile$bodymass
                            body.com <- data.frame(matrix(nrow = nrow(segment.com)))
                            body.com <- body.com[-1]

                            # Denominator
                            denominator <- 0
                            for (i in 1:length(setup$skeleton$segments)){
                              denominator <- denominator + bodymass*setup$skeleton$segments[[i]][['mass']]
                            }

                            # Numerator and final calculation
                            for (i in 1:nrow(segment.com.x)){
                              numerator.x <- 0
                              numerator.y <- 0
                              for (j in 1:ncol(segment.com.x)){
                                numerator.x <- numerator.x + segment.com.x[[i,j]]*(bodymass*setup$skeleton$segments[[j]][['mass']])
                                numerator.y <- numerator.y + segment.com.y[[i,j]]*(bodymass*setup$skeleton$segments[[j]][['mass']])
                              }
                              body.com[i,'pos.x.com'] <- numerator.x / denominator
                              body.com[i,'pos.y.com'] <- numerator.y / denominator
                            }

                            return(body.com)
}
