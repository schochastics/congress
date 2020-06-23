// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;

//count function
// [[Rcpp::export]]
int lazarus_count_cpp2D(IntegerMatrix A,IntegerVector perm){
  int countzero = 0;
  int m = A.ncol();
  int k = A.nrow();
  
  for(int c=0; c<m; ++c){
    int first = -1;
    int last = -1;
    //find first and last 1
    for(int r=0;r<k;++r){
      if (A(perm[r],c)!=1){
        continue;
      }
      if(first==-1){
        first = r;
      }
      last = r;
    }
    
    if((first==-1) | (first==last)){
      continue;
    }
    for(int r=first;r<=last;++r){
      if(A(perm[r],c)==0){
        countzero+=1;
      }
    }
  }
  return countzero;
}

// [[Rcpp::export]]
Rcpp::List simann2D(IntegerMatrix C,IntegerVector perm,int max_iter,double min_temp) {
  double temp = 100;
  int stuck = 0;
  int n = C.rows();
  
  IntegerVector nodes(n);
  IntegerVector perm_sol(n);
  IntegerVector perm_best(n);
  for(int i=0;i<n;++i){
    nodes[i]=i;
    perm_sol[i] = perm[i];
    perm_best[i] = perm[i]+1;
  }
  
  int lcount_min = lazarus_count_cpp2D(C,perm_sol);
  int lcount_best = lcount_min;
  
  while(temp>min_temp){
    for(int q=0;q<max_iter;++q){  
      
      IntegerVector swap = RcppArmadillo::sample(nodes, 2, false);
      int a = perm_sol[swap[0]];
      int b = perm_sol[swap[1]];
      
      perm_sol[swap[0]] = b;
      perm_sol[swap[1]] = a;
      
      int lcount=lazarus_count_cpp2D(C,perm_sol);
      
      if(lcount<lcount_min){
        lcount_min = lcount;
        if(lcount_min<lcount_best){
          for(int i=0;i<n;++i){
            perm_best[i] =  perm_sol[i] + 1;
          }
          lcount_best = lcount_min;
          if(lcount_best == 0){
            return Rcpp::List::create(Rcpp::Named("lazarus") = lcount_best,
                                      Rcpp::Named("perm") = perm_best);
          }
        }
        stuck = 0;
      } else{
        double p = exp(-(lcount-lcount_min)/temp);
        if(R::runif(0,1)<=p){
          lcount_min = lcount;
          stuck = 0;
        } else {
          perm_sol[swap[1]] = b;
          perm_sol[swap[0]] = a;
          stuck+=1;
        }
      }
      
    }
    temp = 0.99*temp;
    
    if(stuck >= 2*max_iter){
      return Rcpp::List::create(Rcpp::Named("lazarus") = lcount_best,
                                Rcpp::Named("perm") = perm_best);
    }
  }
  return Rcpp::List::create(Rcpp::Named("lazarus") = lcount_best,
                            Rcpp::Named("perm") = perm_best);
  
}

// [[Rcpp::export]]
IntegerMatrix Jneighborhood(IntegerMatrix A,IntegerVector perm){
  int n = A.nrow();
  IntegerMatrix D(n,n);
  for(int i = 0; i<n; ++i){
    D(i,i) = 1;
  }
  for(int h = 0; h<(n-1); ++h){
    for(int k = h+1;k<n;++k){
      for(int i=0; i<=h; ++i){
        if(A(perm[i],perm[k])==1){
          D(h,k) = 1;
          break;
        }
      }
    }
  }
  return D;
}

// [[Rcpp::export]]
IntegerMatrix cvmatrix(IntegerMatrix A,IntegerVector perm, IntegerMatrix C, IntegerVector Jh){
  int n  = C.nrow();
  int m  = C.ncol();
  int nl = Jh.length();
  int k;
  int r;
  
  IntegerMatrix Ch(n,m);
  std::fill( Ch.begin(), Ch.end(), IntegerVector::get_na() ) ;
  
  for(int j=0; j<n; ++j){
   for(int i=0; i<nl; ++i){
      k = Jh[i];
     for(int l=0; l<nl ; ++l){
       r = Jh[l];
       if((C(j,perm[r])==1) & (A(perm[k],perm[r])==0)){
         Ch(j,k) = C(j,perm[k]);
       }
     }
   }  
  }
  return Ch;
}

// [[Rcpp::export]]
IntegerVector get_jvec(IntegerVector x) {
  int s = sum(x);
  IntegerVector y(s);
  int k = 0;
  for(int i=0; i<x.length(); ++i){
    if(x[i]==1){
      y[k]=i;
      k+=1;
    }
  }
  return y;
}

// wrapper around R's RNG such that we get a uniform distribution over
// [0,n) as required by the STL algorithm
inline int randWrapper(const int n) { return floor(unif_rand()*n); }

// [[Rcpp::export]]
Rcpp::IntegerVector randomShuffle(Rcpp::IntegerVector a) {
  
  // clone a into b to leave a alone
  Rcpp::IntegerVector b = Rcpp::clone(a);
  
  std::random_shuffle(b.begin(), b.end(), randWrapper);
  
  return b;
}

// [[Rcpp::export]]
List boxicity2(IntegerMatrix A, IntegerMatrix C){
  
  IntegerVector jrow; 
  IntegerVector jvec; 
  IntegerMatrix cvmat;
  List Jstar;
  IntegerMatrix J;
  int lcount;
    
  int n = A.nrow();
  int m = C.nrow();
  
  Environment env = Environment::global_env();
  Function dom = env["jstar_dom"];
  IntegerVector iperm(m);
  IntegerVector cperm(m);
    
  for(int j=0; j<m;++j){
    iperm[j] = j;
    cperm[j] = j;
  }
  
  
  // sim-ann prep
  double temp = 100;
  double min_temp = 0.01;
  int max_iter = 500;
  int stuck = 0;

  IntegerVector nodes(n);
  IntegerVector perm_sol(n);
  IntegerVector perm_best(n);
  for(int i=0;i<n;++i){
    nodes[i] = i;
    perm_sol[i]  = i;
    perm_best[i] = i;
  }
  int lcount_min = 99999;
  int lcount_best = lcount_min;
  
  // repeat from here
  while(temp>min_temp){
    
    for(int q=0;q<max_iter;++q){
      // Rcout << q << ":" << lcount_best <<std::endl;
      
      IntegerVector swap = RcppArmadillo::sample(nodes, 2, false);
      int a = perm_sol[swap[0]];
      int b = perm_sol[swap[1]];
      
      perm_sol[swap[0]] = b;
      perm_sol[swap[1]] = a;
      // A = permuteMat(A,perm_sol);
      
      // check val
      J = Jneighborhood(A,perm_sol);
      Jstar = dom(J);
      IntegerMatrix cvmat_all(m,n * Jstar.length());
      // Rcout << cvmat_all.ncol() << std::endl;
      for(int i=0; i< Jstar.length(); ++i){
        jrow  = J(i,_);
        jvec  = get_jvec(jrow);
        cvmat = cvmatrix(A,perm_sol,C,jvec);
        for(int r=0;r<m;++r){
          for(int c=0;c<n;++c){
            cvmat_all(r,i*n+c) = cvmat(r,c);    
          }
        }
        
      }
      List res = simann2D(cvmat_all,iperm,500,0.01);
      int tmp = res["lazarus"];
      lcount = tmp;
      // Rcout<< temp<< "-" << q << " : " << lcount << "(" << lcount_best << ")" <<std::endl;
      if(lcount<lcount_min){
        lcount_min = lcount;
        if(lcount_min<lcount_best){
          for(int i=0;i<n;++i){
            perm_best[i] =  perm_sol[i] + 1;
          }
          lcount_best = lcount_min;
          cperm = res["perm"];
          if(lcount_best == 0){
            return Rcpp::List::create(Rcpp::Named("lazarus") = lcount_best,
                                      Rcpp::Named("Aperm") = perm_best,
                                      Rcpp::Named("Cperm") = cperm,
                                      Rcpp::Named("mat") = cvmat_all,
                                      Rcpp::Named("J") = J);
          }
        }
        stuck = 0;
      } else{
        double p = exp(-(lcount-lcount_min)/temp);
        if(R::runif(0,1)<=p){
          lcount_min = lcount;
          stuck = 0;
        } else {
          perm_sol[swap[1]] = b;
          perm_sol[swap[0]] = a;
          // A = permuteMat(A,perm_sol);
          stuck+=1;
        }
      }
      
      //jump back or randomize
      if(R::runif(0,1)<=0.01){
        for(int i=0;i<n;++i){
          perm_sol[i] = perm_best[i] - 1;
        }
      } else{
        if(R::runif(0,1)<=0.005){
          perm_sol = randomShuffle(perm_sol);
        }
      } 
    }
    temp = 0.99 * temp;
  }
  
  return Rcpp::List::create(Rcpp::Named("lazarus") = lcount_best,
                            Rcpp::Named("Aperm") = perm_best,
                            Rcpp::Named("Cperm") = cperm);
}

// // [[Rcpp::export]]
// IntegerMatrix permuteMat(const IntegerMatrix& M, const IntegerVector& ind) {
//   
//   int col = M.ncol();
//   IntegerMatrix M2(col, col);
//   
//   for (int j = 0; j < col; j++)
//     for (int i = 0; i < col; i++)
//       M2(i, j) = M(ind[i], ind[j]);
//   
//   return M2;   
// }