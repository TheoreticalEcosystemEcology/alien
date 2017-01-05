// double extinter(double inter, double prob0, double mn, double mx, double shape){
// 	double tmp, val;
//   val = 0;
//   tmp = 1/(mx-mn);
//   val = 1/(tmp + (1/(prob0-mn)-tmp)*exp(shape*inter));
// 	val += mn;
// 	return val;
// }
//
// #' Generate a function that simulate n dependent parameters to get correlated
// #' parameters values of binomial.
// #'
// #' @param val
// #' @param metaweb
// #' @param t1
// #' @param t2
// #' @param t3
// #' @param alpha
//
//
// probenvi <- function(val, metaweb, t1, t2 = NULL, t3 = NULL, alpha = 1){
//   if (is.null(t2)) t2 <- rep(1, length(t1))
//   if (is.null(t3)) t3 <- rep(1, length(t1))
//   nsp <- length(t1)
//   out <- double(nsp)
//   ##
//   for (i in 1:nsp){
//     dif <- val-t1[i]
//     out[i] <- t3[i]*exp(-(dif*dif)/t2[i])
//   }
//   ##
//   pre <- matrix(out, nrow=1)
//   score <-  pre %*% t(metaweb) - pre %*% metaweb
//   ##
//   for (i in 1:length(score)){
//     out[i] <- extinter(score[i], pre[i], 0, 1, alpha)
//   }
//
//   return(out)
// }
