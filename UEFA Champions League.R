##                       _oo0oo_
##                      o8888888o
##                      88" . "88
##                      (| -_- |)
##                      0\  =  /0
##                    ___/`---'\___
##                  .' \\|     |// '.
##                 / \\|||  :  |||// \
##                / _||||| -:- |||||- \
##               |   | \\\  -  /// |   |
##               | \_|  ''\---/''  |_/ |
##               \  .-\__  '-'  ___/-. /
##             ___'. .'  /--.--\  `. .'___
##          ."" '<  `.___\_<|>_/___.' >' "".
##         | | :  `- \`.;`\ _ /`;.`/ - ` : | |
##         \  \ `_.   \_ __\ /__ _/   .-` /  /
##     =====`-.____`.___ \_____/___.-`___.-'=====
##                       `=---='
##
##     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##               佛祖保佑         永无BUG

#################################
library(lubridate)
library(foreach)
library(doParallel)

set.seed(1234)
options(max.print=100000)
sink(file="C:/Users/Ding/Desktop/UEFA Champions League.txt",append=T)

#################################
#detectCores(logical = FALSE) # 2
cores <- detectCores(logical = FALSE)
cl <- makeCluster(cores)
registerDoParallel(cl, cores=cores)
clusterEvalQ(cl, library(lubridate))

n_1 <- 100000 # 100000 times Monte Carlos Simulations each experiment
n_2 <- 1000 # 1000 experiments

FUN_1 <- function(a,b) {
  result[a,1] <<- set_1[b]
  result[a,2] <<- set_2_2[[set_1[b]]]
  set_1 <<- setdiff(set_1,result[a,1])
  for(j in 1:8) {
    set_1_1[[j]] <<- setdiff(set_1_1[[j]],result[a,1])
  }
  set_2 <<- setdiff(set_2,result[a,2])
  for(j in 1:8) {
    set_2_2[[j]] <<- setdiff(set_2_2[[j]],result[a,2])
  } 
}

FUN_2 <- function(a,b) {
  result[a,1] <<- set_1_1[[set_2[b]]]
  result[a,2] <<- set_2[b]
  set_1 <<- setdiff(set_1,result[a,1])
  for(j in 1:8) {
    set_1_1[[j]] <<- setdiff(set_1_1[[j]],result[a,1])
  }
  set_2 <<- setdiff(set_2,result[a,2])
  for(j in 1:8) {
    set_2_2[[j]] <<- setdiff(set_2_2[[j]],result[a,2])
  } 
}

now()
result_all <- array(NA,dim=c(8,8,n_2))
foreach(x=1:n_2, .combine = rbind) %dopar% {
  result_cum <- matrix(0,nrow=8,ncol=8)
  
  for (k in 1:n_1) { 
    result <- matrix(NA,nrow=8,ncol=2)
    set_1 <- 1:8
    set_2 <- 1:8
    set_1_1 <- list(setdiff(1:8, 1),setdiff(1:8, 2),setdiff(1:8, c(1,3,5,6,8)),setdiff(1:8, c(3,4)),
                    setdiff(1:8, c(4,5)),setdiff(1:8,6),setdiff(1:8,7),setdiff(1:8, c(4,8)))
    set_2_2 <- list(setdiff(1:8, c(1,3)),setdiff(1:8, 2),setdiff(1:8, c(3,4)),setdiff(1:8, c(4,5,8)),
                    setdiff(1:8, c(3,5)),setdiff(1:8, c(3,6)),setdiff(1:8, 7),setdiff(1:8, c(3,8)))
    for (i in 1:2) {
      result[i,1] <- sample(set_1,1)
      result[i,2] <- sample(set_2_2[[result[i,1]]],1)
      set_1 <- setdiff(set_1,result[i,1])
      for(j in 1:8) {
        set_1_1[[j]] <- setdiff(set_1_1[[j]],result[i,1])
      }
      set_2 <- setdiff(set_2,result[i,2])
      for(j in 1:8) {
        set_2_2[[j]] <- setdiff(set_2_2[[j]],result[i,2])
      }
    }
    
    if (length(set_2_2[[set_1[1]]])==1) {
      FUN_1(3,1)
    } else if (length(set_2_2[[set_1[2]]])==1) {
      FUN_1(3,2)
    } else if (length(set_2_2[[set_1[3]]])==1) {
      FUN_1(3,3)
    } else if (length(set_2_2[[set_1[4]]])==1) {
      FUN_1(3,4)
    } else if (length(set_2_2[[set_1[5]]])==1) {
      FUN_1(3,5)
    } else if (length(set_2_2[[set_1[6]]])==1) {
      FUN_1(3,6)
    } else if (length(set_1_1[[set_2[1]]])==1) {
      FUN_2(3,1)
    } else if (length(set_1_1[[set_2[2]]])==1) {
      FUN_2(3,2)
    } else if (length(set_1_1[[set_2[3]]])==1) {
      FUN_2(3,3)
    } else if (length(set_1_1[[set_2[4]]])==1) {
      FUN_2(3,4)
    } else if (length(set_1_1[[set_2[5]]])==1) {
      FUN_2(3,5)
    } else if (length(set_1_1[[set_2[6]]])==1) {
      FUN_2(3,6)
    } else {
      result[3,1] <- sample(set_1,1)
      result[3,2] <- sample(set_2_2[[result[3,1]]],1)
      set_1 <- setdiff(set_1,result[3,1])
      for(j in 1:8) {
        set_1_1[[j]] <- setdiff(set_1_1[[j]],result[3,1])
      }
      set_2 <- setdiff(set_2,result[3,2])
      for(j in 1:8) {
        set_2_2[[j]] <- setdiff(set_2_2[[j]],result[3,2])
      }
    }
    
    if (length(set_2_2[[set_1[1]]])==1) {
      FUN_1(4,1)
    } else if (length(set_2_2[[set_1[2]]])==1) {
      FUN_1(4,2)
    } else if (length(set_2_2[[set_1[3]]])==1) {
      FUN_1(4,3)
    } else if (length(set_2_2[[set_1[4]]])==1) {
      FUN_1(4,4)
    } else if (length(set_2_2[[set_1[5]]])==1) {
      FUN_1(4,5)
    } else if (length(set_1_1[[set_2[1]]])==1) {
      FUN_2(4,1)
    } else if (length(set_1_1[[set_2[2]]])==1) {
      FUN_2(4,2)
    } else if (length(set_1_1[[set_2[3]]])==1) {
      FUN_2(4,3)
    } else if (length(set_1_1[[set_2[4]]])==1) {
      FUN_2(4,4)
    } else if (length(set_1_1[[set_2[5]]])==1) {
      FUN_2(4,5)
    } else {
      result[4,1] <- sample(set_1,1)
      result[4,2] <- sample(set_2_2[[result[4,1]]],1)
      set_1 <- setdiff(set_1,result[4,1])
      for(j in 1:8) {
        set_1_1[[j]] <- setdiff(set_1_1[[j]],result[4,1])
      }
      set_2 <- setdiff(set_2,result[4,2])
      for(j in 1:8) {
        set_2_2[[j]] <- setdiff(set_2_2[[j]],result[4,2])
      }
    }
    
    if (length(set_2_2[[set_1[1]]])==1) {
      FUN_1(5,1)
    } else if (length(set_2_2[[set_1[2]]])==1) {
      FUN_1(5,2)
    } else if (length(set_2_2[[set_1[3]]])==1) {
      FUN_1(5,3)
    } else if (length(set_2_2[[set_1[4]]])==1) {
      FUN_1(5,4)
    } else if (length(set_1_1[[set_2[1]]])==1) {
      FUN_2(5,1)
    } else if (length(set_1_1[[set_2[2]]])==1) {
      FUN_2(5,2)
    } else if (length(set_1_1[[set_2[3]]])==1) {
      FUN_2(5,3)
    } else if (length(set_1_1[[set_2[4]]])==1) {
      FUN_2(5,4)
    } else {
      result[5,1] <- sample(set_1,1)
      result[5,2] <- sample(set_2_2[[result[5,1]]],1)
      set_1 <- setdiff(set_1,result[5,1])
      for(j in 1:8) {
        set_1_1[[j]] <- setdiff(set_1_1[[j]],result[5,1])
      }
      set_2 <- setdiff(set_2,result[5,2])
      for(j in 1:8) {
        set_2_2[[j]] <- setdiff(set_2_2[[j]],result[5,2])
      }
    }
    
    if (length(set_2_2[[set_1[1]]])==1) {
      FUN_1(6,1)
    } else if (length(set_2_2[[set_1[2]]])==1) {
      FUN_1(6,2)
    } else if (length(set_2_2[[set_1[3]]])==1) {
      FUN_1(6,3)
    } else if (length(set_1_1[[set_2[1]]])==1) {
      FUN_2(6,1)
    } else if (length(set_1_1[[set_2[2]]])==1) {
      FUN_2(6,2)
    } else if (length(set_1_1[[set_2[3]]])==1) {
      FUN_2(6,3)
    } else {
      result[6,1] <- sample(set_1,1)
      result[6,2] <- sample(set_2_2[[result[6,1]]],1)
      set_1 <- setdiff(set_1,result[6,1])
      for(j in 1:8) {
        set_1_1[[j]] <- setdiff(set_1_1[[j]],result[6,1])
      }
      set_2 <- setdiff(set_2,result[6,2])
      for(j in 1:8) {
        set_2_2[[j]] <- setdiff(set_2_2[[j]],result[6,2])
      }
    }
    
    if (length(set_2_2[[set_1[1]]])==1) {
      FUN_1(7,1)
    } else if (length(set_2_2[[set_1[2]]])==1) {
      FUN_1(7,2)
    } else if (length(set_2_2[[set_1[3]]])==1) {
      FUN_1(7,3)
    } else if (length(set_1_1[[set_2[1]]])==1) {
      FUN_2(7,1)
    } else if (length(set_1_1[[set_2[2]]])==1) {
      FUN_2(7,2)
    } else if (length(set_1_1[[set_2[3]]])==1) {
      FUN_2(7,3)
    } else {
      result[7,1] <- sample(set_1,1)
      result[7,2] <- sample(set_2_2[[result[7,1]]],1)
      set_1 <- setdiff(set_1,result[7,1])
      for(j in 1:8) {
        set_1_1[[j]] <- setdiff(set_1_1[[j]],result[7,1])
      }
      set_2 <- setdiff(set_2,result[7,2])
      for(j in 1:8) {
        set_2_2[[j]] <- setdiff(set_2_2[[j]],result[7,2])
      }
    }
    
    result[8,1] <- setdiff(1:8,result[1:7,1])
    result[8,2] <- setdiff(1:8,result[1:7,2])
    for (i in 1:8) {
      result_cum[result[i,1],result[i,2]] <- result_cum[result[i,1],result[i,2]]+1
    }
    #print(k)
  }
  #print(x)
  #print(now())
  result_all[,,x] <- result_cum/n_1
}

now()
stopCluster(cl)

#################################
#Note: Firstly, you need to delete useless information in "UEFA Champions League.txt" by hand.
result_matrix <- as.matrix(read.table(file="F:/我的论文/欧冠淘汰赛抽签概率/UEFA Champions League.txt"))

n_1 <- 100000 # 100000 times Monte Carlos Simulations each experiment
n_2 <- 1000 # 1000 experiments
result_all <- array(NA,dim=c(8,8,n_2))
for (i in 1:n_2) {
  result_all[,,i] <- result_matrix[((i-1)*8+1):(i*8),]
}

result_mean <- matrix(NA,nrow=8,ncol=8)
for (i in 1:8) {
  for (j in 1:8) {
    result_mean[i,j] <- mean(result_all[i,j,])
  }
}

result_sd <- matrix(NA,nrow=8,ncol=8)
for (i in 1:8) {
  for (j in 1:8) {
    result_sd[i,j] <- sd(result_all[i,j,])
  }
}    

result_all[,,1]

#################################
#### Confidence Interval

ci_95 <- data.frame(cbind(rep(NA,8),rep(NA,8),rep(NA,8),rep(NA,8),
                          rep(NA,8),rep(NA,8),rep(NA,8),rep(NA,8)))
#ci denotes confidence interval

colnames(ci_95) <- as.character(1:8)

# 95% confidence interval
for (i in 1:8) {
  for (j in 1:8) {
    ci_95[i,j] <- paste0("(",round((result_mean+qnorm(.025)*result_sd/sqrt(n_2))[i,j]*100,4),",",
                         round((result_mean+qnorm(.975)*result_sd/sqrt(n_2))[i,j]*100,4),")") 
  }
}
ci_95 # in percentage

####
ci_99 <- data.frame(cbind(rep(NA,8),rep(NA,8),rep(NA,8),rep(NA,8),
                          rep(NA,8),rep(NA,8),rep(NA,8),rep(NA,8)))
colnames(ci_99) <- as.character(1:8)

# 99% confidence interval
for (i in 1:8) {
  for (j in 1:8) {
    ci_99[i,j] <- paste0("(",round((result_mean+qnorm(.005)*result_sd/sqrt(n_2))[i,j]*100,4),",",
                         round((result_mean+qnorm(.995)*result_sd/sqrt(n_2))[i,j]*100,4),")") 
  }
}
ci_99 # in percentage

#################################
#### Quantile

# From .025th quantile to .975th quantile
quantile_1 <- data.frame(cbind(rep(NA,8),rep(NA,8),rep(NA,8),rep(NA,8),
                               rep(NA,8),rep(NA,8),rep(NA,8),rep(NA,8)))

colnames(quantile_1) <- as.character(1:8)

for (i in 1:8) {
  for (j in 1:8) {
    quantile_1[i,j] <- paste0("(",round(quantile(result_all[i,j,],.025)*100,4),",",
                              round(quantile(result_all[i,j,],.975)*100,4),")") 
  }
}
quantile_1 # in percentage

#### From .005th quantile to .995th quantile
quantile_2 <- data.frame(cbind(rep(NA,8),rep(NA,8),rep(NA,8),rep(NA,8),
                               rep(NA,8),rep(NA,8),rep(NA,8),rep(NA,8)))

colnames(quantile_2) <- as.character(1:8)

for (i in 1:8) {
  for (j in 1:8) {
    quantile_2[i,j] <- paste0("(",round(quantile(result_all[i,j,],.005)*100,4),",",
                              round(quantile(result_all[i,j,],.995)*100,4),")") 
  }
}
quantile_2 # in percentage

#################################
sink()
rm(list=ls())

1/4+1/5.5+1/6+1/8.5+1/9+1/15+1/15+1/17+1/28+1/34

#################################
## It takes 22.56139 hours to run the code above.
## Lasciate ogni speranza, voi ch'entrate! 


