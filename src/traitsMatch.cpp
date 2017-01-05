// NumericMatrix traitsMatch(int nbsp, double alpha, NumericVector tr1, NumericVector tr2){
//   // if no interaction => feed upon the pp
//   NumericMatrix metaweb(nbsp, nbsp);
// 	double dif;
// 	int i,j ;
//   //
// 	for(i = 0; i < nbsp; i++) {
// 		for(j = 0; j < nbsp; j++) {
//       dif = abs(tr1[i]-tr2[j]);
//       metaweb(i, j) = exp(-abs(alpha)*dif);
// 		}
//     metaweb(i, i) = 0;
// 	}
//   return metaweb;
// }
//
