#' @name plot.niche.model
#'
#' @title Plot the predicted model output with data points on top.
#'
#' @description This is just a pre-built function for easy visualization. Other options are possible.
#' Only works with the default gaussian model (integrated and niche model).
#'
#' @param pars Parameters of the model obtained with fit.niche.prob function (alpha0, alpha 1, beta 0 and beta 1)
#' @param Tlevel1 Vector of trait values of the first interaction partner.
#' @param Tlevel2 Vector of trait values of the second interaction partner.
#' @param xlab label for x axes
#' @param ylab label for y axes
#' @param legend Logical value indicating if the legend should be plotted. Default = TRUE
#' @param pch Default = 19. Use "." when lots of data available.
#' @param ... other parameters to be passed to plot.
#'
#' @return A plot
#'
#' @note
#' See more here: https://github.com/ibartomeus/trait_match
#'
#' @author
#' Dominique Gravel and Ignasi Bartomeus
#'
#'
#' @export
plot_pred <- function(pars,Tlevel1, Tlevel2,
                      xlab = "Trait level 2", ylab = "Trait level 1",
                      legend = TRUE,
                      pch = 19, ...){
  incrementX <- (max(Tlevel2)-min(Tlevel2))/100
  incrementY <- (max(Tlevel1)-min(Tlevel1))/100
  seqX = seq(min(Tlevel2),max(Tlevel2),incrementX)
  seqY = seq(min(Tlevel1),max(Tlevel1),incrementY)

  XY = expand.grid(seqX,seqY)

  # Optimum and range
  o = pars[1] + pars[2]*XY[,1]
  r = pars[3] + pars[4]*XY[,1]

  # Compute the conditional
  pLM = exp(-(o-XY[,2])^2/2/r^2)

  Z = matrix(pLM, nrow = length(seqX), ncol = length(seqY))

  par(mar = c(5.5,4.1,4.1,5.5)) #tune up
  par(xpd=TRUE)
  image(seqX,seqY,Z,xlab = xlab ,ylab = ylab,
        col=heat.colors(100),cex.axis = 1.25, cex.lab = 1.5, las = 1)
  points(Tlevel2,Tlevel1,pch = pch, cex = 0.5)
  if(legend){
    SDMTools::legend.gradient(pnts = cbind(x =c(max(seqX)+(incrementX*2),
                                      max(seqX)+(incrementX*10),
                                      max(seqX)+(incrementX*10),
                                      max(seqX)+(incrementX*2)),
                                 y =c(max(seqY),max(seqY),min(seqY),min(seqY))),
                    cols = heat.colors(100), limits = c(0, 1),
                    title = "Probability", cex = 0.8)
  }
  par(mar = c(5,1.1,4.1,2.1))
  par(xpd=FALSE)
}

