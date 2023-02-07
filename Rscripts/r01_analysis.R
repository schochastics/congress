#_______________________________ ----
#------------------------------------------------------------------------------#
# R script to reproduce the results of 
# Legislators' roll-call voting behavior increasingly corresponds to intervals in 
#  the political spectrum
#
# David Schoch and Ulrik Brandes
# please contact david.schoch@gesis.org for help
#
# https://github.com/schochastics/congress
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# A) initialisation ----
#------------------------------------------------------------------------------#
# 1) empty environment ----
rm(list=ls())

#------------------------------------------------------------------------------#
# 2) libraries ----
cat("\ninstalling and loading libraries\n")
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages(pacman)
}

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages(remotes)
}

pacman::p_load(tidyverse,
               vroom,
               wnominate,
               Rcpp,
               igraph,
               ggraph,
               backbone,
               ggthemes,
               ggforce,
               graphlayouts,
               netrankr,
               patchwork,
               extrafont,
               remotes,
               update = FALSE)

# add github packages (specific commit is given as ref argument)
if (!requireNamespace("levelnet", quietly = TRUE)) {
  remotes::install_github("schochastics/levelnet",ref = "d6cf3d1") 
}
# add github packages (specific commit is given as ref argument)
if (!requireNamespace("igraphUtils", quietly = TRUE)) {
  remotes::install_github("schochastics/igraphUtils",ref = "1b601a3") 
}
suppressMessages(library(levelnet))
suppressMessages(library(igraphUtils))

#load fonts
suppressMessages(loadfonts())
base_font <- "LM Roman 10" #change if not available

cat("\nsourcing helper functions\n")
# source helper R functions
source("Rscripts/r00_helper.R")

# source helper Rcpp functions
Rcpp::sourceCpp("Rscripts/_boxicity2.cpp")
#------------------------------------------------------------------------------#
# 3) set script parameters ----
# these parameters should be set appropriately before the script is sourced.
# for a complete reproduction set all to TRUE

dir_create <- TRUE

do_parallel <- TRUE # some analyses can be parallelised with the parallel pkg

# data wrangling
data_download <- TRUE
data_clean    <- TRUE

# create and visualize original networks
networks_create    <- TRUE
networks_visualize <- TRUE

# compute and visualize the distance interval graphs 
lazarus_calculate <- TRUE
lazarus_visualize <- TRUE

intervals_visualize <- TRUE

# analyses and visualizations with the Fiedler vector(s)
fiedler_calculate   <- TRUE
fiedler_visualize   <- TRUE

# analyses around boxicity 2 networks
box2_calculate <- FALSE
box2_visualize <- FALSE

# compute and visualize approximated super interval-box5 graphs (NOT IN PAPER)
superbox1_compute <- FALSE
superbox2_compute <- FALSE
superbox3_compute <- FALSE
superbox4_compute <- FALSE
superbox5_compute <- FALSE


# compare with "traditional" methods (NOT IN PAPER)
compute_wnom       <- FALSE
compare_dim        <- FALSE
compare_clustering <- FALSE

#MSE analysis
mse_compute   <- TRUE
mse_visualize <- TRUE


c_seq <- 81:116 #senates to run the script with
n <- length(c_seq)

#------------------------------------------------------------------------------#
# 4) directory structure ----
# |project_dir
# |-- Rscripts -> r01_analysis.R, r00_helper.R, _boxicity2.cpp
# |--data
#   |-- raw (scraped data)
#   |-- processed (manipulated data)
#   |-- figures (R script figure output)

# create directories (project_dir and Rscripts should be present!)
if(dir_create){
  dir.create("data")
  dir.create("data/raw")
  dir.create("data/processed")
  dir.create("data/processed/boxicity2")
  dir.create("data/processed/networks")
  dir.create("data/processed/superbox1")
  dir.create("data/processed/superbox2")
  dir.create("data/processed/superbox3")
  dir.create("data/processed/superbox4")
  dir.create("data/processed/superbox5")
  dir.create("data/processed/votes")
  dir.create("data/processed/wnominate")
  dir.create("data/figures")
}
#_______________________________ ----
#------------------------------------------------------------------------------#
# B) data preparation ----
# downloads and cleans all rollcalls and MetaData for the 1st-current congress
# source: https://voteview.com/data
#------------------------------------------------------------------------------#
# 1) download data ----
if(data_download){
  cat("\ndownloading raw data from voteview.com\n")
  download.file("https://voteview.com/static/data/out/members/Sall_members.csv",
                destfile = "data/raw/Sall_members.csv",quite = TRUE)
  download.file("https://voteview.com/static/data/out/votes/Sall_votes.csv",
                destfile = "data/raw/Sall_votes.csv",quite = TRUE)
  download.file("https://voteview.com/static/data/out/parties/Sall_parties.csv",
                destfile = "data/raw/Sall_parties.csv",quite = TRUE)
  download.file("https://voteview.com/static/data/out/rollcalls/Sall_rollcalls.csv",
                destfile = "data/raw/Sall_rollcalls.csv",quite = TRUE)
}
#------------------------------------------------------------------------------#
# 2) clean data ----
if(data_clean){
  cat("\ncleaning raw data\n")
  clean_data(end_date = "2020-05-19")
}

#------------------------------------------------------------------------------#
# 3) create networks ----
# networks are stored as rds files in the folder data/processed/networks/

#------------------------------------------------------------------------------#
# Vote with minority of 2.5% are excluded. This is in line with Poole/Rosenthal 1985:
#For the model used here, we have found
# a cutoff level of 2.5 percent, used throughout the next section, to be a good
# tradeoff between the quality of legislator coordinates and the quality of roll call coordinates
#------------------------------------------------------------------------------#
# Senators who voted on less than 10% of bills are excluded. DW Nominate excludes
# Senators who voted on less than 20 bills. (ROBUSTNESS CHECK)
#------------------------------------------------------------------------------#
if(networks_create){
  cat("\ncreating senate networks from roll call votes\n")
  
  for(c in c_seq){
    out_file_rds <- paste0("data/processed/networks/senate_",str_pad(c,3,"left","0"),".rds")
    cat("Current Senate:", str_pad(c,3,"left","0"),"\r")
    g <- create_network(c,model="scobit",filter_senator = 0.5,filter_bills = 0.025,alpha = 0.05)
    saveRDS(g,out_file_rds)
  }
    
}

#_______________________________ ----
#------------------------------------------------------------------------------#
# C) analysis ----
#------------------------------------------------------------------------------#
# 1) compute Lazarus count ----
# also calculates some additional network statistics

if(lazarus_calculate){
  cat("\ncompute Lazarus counts using Fiedler Vector\n")
  
  fl <- list.files("data/processed/networks",pattern = "senate",full.names = T)
  fl <- fl[readr::parse_number(fl)%in%c_seq]
  fl <- fl[order(readr::parse_number(fl))]
  
  tbl <- map_dfr(fl,function(x) {
    l <- readRDS(x)
    l <- delete_vertices(l,which(degree(l)==0))
    MSE <- igraphUtils::structural_equivalence(l)
    l1 <- delete.vertices(l,which(duplicated(MSE)))
    comps <- components(l)
    if(comps$no==1){
      lz <- tryCatch(lazarus_count(l1,mode="mcl"),error=function(e) NA)
      congress <- str_extract(x,"[0-9]+") %>% as.integer()
      tbl <- tibble(congress=congress,
                    lz_fiedler = lz,
                    density = graph.density(l),
                    legislators=vcount(l),
                    edges=ecount(l),
                    mcl=count_max_cliques(l1))
    } else{
      lz <- 0
      for(i in 1:comps$no){
        l1 <- induced_subgraph(l,which(comps$membership==i))
        MSE <- igraphUtils::structural_equivalence(l1)
        l1 <- delete.vertices(l1,which(duplicated(MSE)))
        if(vcount(l1)>1){
          lz <- lz + tryCatch(lazarus_count(l1,mode="mcl"),error=function(e) 0)
        }
      }
      congress <- str_extract(x,"[0-9]+") %>% as.integer()
      tbl <- tibble(congress=congress,
                    lz_fiedler = lz,
                    density = graph.density(l),
                    legislators=vcount(l),
                    edges=ecount(l),
                    mcl=count_max_cliques(l))
    }
    return(tbl)
  })
  
  # simulated annealing is only performed for the networks that we know are not interval
  # graphs (from results above) to save time.
  
  id_interval <- tbl$congress[tbl$lz_fiedler==0]
  fl <- fl[!str_detect(fl,paste0(str_pad(id_interval,3,"left","0"),collapse = "|"))]
  
  cat("\ncompute Lazarus counts using Simmulated Annealing\n")
  k <- 0
  lz <- rep(-1,length(fl))
  set.seed(433)
  for(f in fl){
    cat("processing",f,"\r")
    k <- k + 1
    l <- readRDS(f)
    l <- delete.vertices(l,which(degree(l)==0))
    MSE <- igraphUtils::structural_equivalence(l)
    l1 <- delete.vertices(l,which(duplicated(MSE)))
    M <- clique_vertex_mat(l1)

    #start from fiedler solution
    if(is.connected(l1)){
      perm <- order(fiedler_order(l1, mode = "mcl"))
    } else{
      perm <- sample(1:nrow(M))
    }
    
    res_sa <- optim(par = perm, fn = lazarus_cliques, A = M,gr = genperm_lz,method = "SANN",
                    control = list(maxit = 100000, temp = 100, tmax = 500, trace = FALSE,
                                   REPORT = 5))
    
    
    res_sa <- optim(par = c(res_sa$par), fn = lazarus_cliques, A = M,gr = genperm_lz,method = "SANN",
                    control = list(maxit = 50000, temp = 5, tmax = 500, trace = FALSE,
                                   REPORT = 5))
    lz[k] <- res_sa$value
  }
  res <- tibble(congress=parse_number(fl),lz_sa=lz)
  res <- bind_rows(res,tibble(congress=id_interval,lz_sa=0)) %>%
    arrange(congress)
  tbl <- left_join(tbl,res,by="congress")
  write_csv(tbl,"data/processed/network_stats.csv")
}
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# 2) compute fiedler vectors ----
if(fiedler_calculate){
  cat("\nCalculating Fiedler vectors\n")
  fl <- list.files("data/processed/networks/",pattern="senate",full.names = T)
  fl <- fl[readr::parse_number(fl)%in%c_seq]
  tbl <- map_dfr(fl,function(x) {
    l <- readRDS(x)
    tbl <- fiedler_scores(l,normalize = T)
    tbl$congress <- str_extract(x,"[0-9]+") %>% as.integer()
    tbl$connected <- is.connected(l)
    return(select(tbl,congress,name,party,state,fiedler,y,connected))
  }
  )
  
  write_csv(arrange(tbl,congress),"data/processed/fiedler_scores.csv")
}

#------------------------------------------------------------------------------#
# 3) compute boxicity 2 ----

if(box2_calculate){
  cat("\n Find Boxicity 2 networks\n")
  net_stats <- read_csv("data/processed/network_stats.csv",
                        col_types = cols(
                          congress = col_double(),
                          lz_fiedler = col_double(),
                          density = col_double(),
                          legislators = col_double(),
                          edges = col_double(),
                          mcl = col_double(),
                          lz_sa = col_double()))
  
  candidates <- net_stats$congress[net_stats$lz_sa>0]
  done <- parse_number(list.files("data/processed/boxicity2/",recursive = F,pattern = "\\.csv"))
  candidates <- candidates[!candidates%in%done]
  fl <- paste0("data/processed/networks/senate_",str_pad(candidates,3,"left","0"),".rds")
  if(do_parallel){
    library(parallel)
    xy_tbl_list <- mclapply(fl, function(f) {
      l <- readRDS(f)
      l <- permute.vertices(l,sample(1:vcount(l)))
      xy <- tryCatch(get_2dintervals(l),error=function(e) matrix(0,1,4))
      if(nrow(xy)==1){
        return(xy)
      }
      colnames(xy) <- c("x1","x2","y1","y2")
      xy_tbl <- as_tibble(xy) %>% 
        mutate(party=V(l)$party,name=V(l)$display_name)
      write_csv(xy_tbl,str_replace(f,"networks","boxicity2") %>% str_replace("rds","csv"))
      return(xy_tbl)
    }, mc.cores = 4)
    
  } else{
    for(f in fl){
      cat(f,"\r")
      l <- readRDS(f)
      l <- permute.vertices(l,sample(1:vcount(l)))
      xy <- tryCatch(get_2dintervals(l),error=function(e) matrix(0,vcount(l),4))
      colnames(xy) <- c("x1","x2","y1","y2")
      xy_tbl <- as_tibble(xy) %>% 
        mutate(party=V(l)$party,name=V(l)$display_name)
      
      write_csv(xy_tbl,str_replace(f,"networks","boxicity2") %>% str_replace("rds","csv"))
    }  
  }
  
}

#------------------------------------------------------------------------------#
# 4) compute super box 1 ----

if(superbox1_compute){
  cat("\ncompute super interval graph\n")
  net_stats <- read_csv("data/processed/network_stats.csv",
                        col_types = cols(
                          congress = col_double(),
                          lz_fiedler = col_double(),
                          density = col_double(),
                          legislators = col_double(),
                          edges = col_double(),
                          mcl = col_double(),
                          lz_sa = col_double()))
  
  candidates <- net_stats$congress[net_stats$lz_sa!=0]
  fl <- paste0("data/processed/networks/senate_",str_pad(candidates,3,"left","0"),".rds")
  if(do_parallel){
    library(parallel)
    glist <- mclapply(fl, function(f) {
      l <- readRDS(f)
      g <- create_superbox1(l)
      saveRDS(g,str_replace(f,"networks","superbox1"))
      g
    }, mc.cores = 7)
    
  } else{
    for(f in fl){
      l <- readRDS(f)
      g <- create_superbox1(l)
      saveRDS(g,str_replace(f,"networks","superbox1"))
    }  
  }
}

#------------------------------------------------------------------------------#
# 5) compute super box 2 ----

if(superbox2_compute){
  cat("\ncompute super box2 graphs\n")
  net_stats <- read_csv("data/processed/network_stats.csv",
                        col_types = cols(
                          congress = col_double(),
                          lz_fiedler = col_double(),
                          density = col_double(),
                          legislators = col_double(),
                          edges = col_double(),
                          mcl = col_double(),
                          lz_sa = col_double()))
  
  candidates <- net_stats$congress[net_stats$lz_sa!=0] #delete interval graphs
  candidates <- candidates[!candidates%in%parse_number(list.files("data/processed/boxicity2/",pattern = "csv"))]
  fl <- paste0("data/processed/networks/senate_",str_pad(candidates,3,"left","0"),".rds")
  # fl <- fl[c(22:25)]
  if(do_parallel){
    library(parallel)
    glist <- mclapply(fl, function(f) {
      l <- readRDS(f)
      g <- create_superbox2(l)
      saveRDS(g,str_replace(f,"networks","superbox2"))
      g
    }, mc.cores = 7)
    
  } else{
    for(f in fl){
      l <- readRDS(f)
      g <- create_superbox2(l)
      saveRDS(g,str_replace(f,"networks","superbox2"))
    }  
  }
}

#------------------------------------------------------------------------------#
# 6) compute super box 3 ----

if(superbox3_compute){
  cat("\ncompute super box3 graphs\n")
  net_stats <- read_csv("data/processed/network_stats.csv",
                        col_types = cols(
                          congress = col_double(),
                          lz_fiedler = col_double(),
                          density = col_double(),
                          legislators = col_double(),
                          edges = col_double(),
                          mcl = col_double(),
                          lz_sa = col_double()))
  
  candidates <- net_stats$congress[net_stats$lz_sa!=0] #delete interval graphs
  candidates <- candidates[!candidates%in%parse_number(list.files("data/processed/boxicity2/",pattern = "csv"))]
  fl <- paste0("data/processed/networks/senate_",str_pad(candidates,3,"left","0"),".rds")
  if(do_parallel){
    library(parallel)
    glist <- mclapply(fl, function(f) {
      l <- readRDS(f)
      g <- create_superbox3(l)
      saveRDS(g,str_replace(f,"networks","superbox3"))
      g
    }, mc.cores = 7)
    
  } else{
    for(f in fl){
      l <- readRDS(f)
      g <- create_superbox3(l)
      saveRDS(g,str_replace(f,"networks","superbox3"))
    }  
  }
}

#------------------------------------------------------------------------------#
# 7) compute super box 4 ----
if(superbox4_compute){
  cat("\ncompute super box4 graphs\n")
  net_stats <- read_csv("data/processed/network_stats.csv",
                        col_types = cols(
                          congress = col_double(),
                          lz_fiedler = col_double(),
                          density = col_double(),
                          legislators = col_double(),
                          edges = col_double(),
                          mcl = col_double(),
                          lz_sa = col_double()))
  
  candidates <- net_stats$congress[net_stats$lz_sa!=0] #delete interval graphs
  candidates <- candidates[!candidates%in%parse_number(list.files("data/processed/boxicity2/",pattern = "csv"))]
  fl <- paste0("data/processed/networks/senate_",str_pad(candidates,3,"left","0"),".rds")
  if(do_parallel){
    library(parallel)
    glist <- mclapply(fl, function(f) {
      l <- readRDS(f)
      g <- create_superbox4(l)
      saveRDS(g,str_replace(f,"networks","superbox4"))
      g
    }, mc.cores = 7)
    
  } else{
    for(f in fl){
      l <- readRDS(f)
      g <- create_superbox4(l)
      saveRDS(g,str_replace(f,"networks","superbox4"))
    }  
  }
}

#------------------------------------------------------------------------------#
# 8) compute super box 5 ----
if(superbox5_compute){
  cat("\ncompute super box5 graphs\n")
  net_stats <- read_csv("data/processed/network_stats.csv",
                        col_types = cols(
                          congress = col_double(),
                          lz_fiedler = col_double(),
                          density = col_double(),
                          legislators = col_double(),
                          edges = col_double(),
                          mcl = col_double(),
                          lz_sa = col_double()))
  
  candidates <- net_stats$congress[net_stats$lz_sa!=0] #delete interval graphs
  candidates <- candidates[!candidates%in%parse_number(list.files("data/processed/boxicity2/",pattern = "csv"))]
  fl <- paste0("data/processed/networks/senate_",str_pad(candidates,3,"left","0"),".rds")
  if(do_parallel){
    library(parallel)
    for(run in 1:5){
      glist <- mclapply(fl, function(f) {
        l <- readRDS(f)
        g <- create_superbox5(l)
        saveRDS(g,str_replace(f,"networks",paste0("superbox5/run",run)))
        g
      }, mc.cores = 7)
    }
    
  } else{
    for(f in fl){
      l <- readRDS(f)
      g <- create_superbox5(l)
      saveRDS(g,str_replace(f,"networks","superbox5"))
    }  
  }
}

#------------------------------------------------------------------------------#
#9) compute MSE ----

if(mse_compute){
  fl <- list.files("data/processed/networks",full.names = T)
  fl <- fl[readr::parse_number(fl)%in%c_seq]
  fl <- fl[order(readr::parse_number(fl))]
  res <- tibble()
  for(f in 1:length(fl)){
    l <- readRDS(fl[f])
    MSE <- igraphUtils::structural_equivalence(l)
    tmp <- tibble(id=1:length(MSE),class=MSE,party=V(l)$party) %>% 
      count(class,party) %>% 
      group_by(class) %>% 
      mutate(size=sum(n)) %>% 
      ungroup() %>% 
      mutate(congress=c_seq[f],class=as.character(class))
    
    res <- bind_rows(res,tmp)
  }
  saveRDS(res,"data/processed/mse_senators.rds")
}
#_______________________________ ----
#------------------------------------------------------------------------------#
# D) visualizations ----
#------------------------------------------------------------------------------#
# 1) visualize networks ----

if(networks_visualize){
  cat("\nvisualize senate networks\n")
  fl <- list.files("data/processed/networks",pattern="senate",full.names = T)
  fl <- fl[readr::parse_number(fl)%in%c_seq]
  fl <- fl[order(readr::parse_number(fl))]
  
  pList_senate <- list()
  # senates <- paste0(c_seq,c("th","st","nd","rd",rep("th",6))[(c_seq%%10)+1]," Session")
  # senate_years <- seq(1789,2019,2)[c_seq]
  for(f in 1:length(fl)){
    l <- readRDS(fl[f])
    l <- delete.vertices(l,which(degree(l)==0))
    pList_senate[[f]] <- plot_network(l)
  }
  
  p <- wrap_plots(pList_senate) + plot_layout(ncol=9)
  
  ggsave("data/figures/networks_senate.pdf",p,width=18,height=8,device="pdf") #height=11
}

#------------------------------------------------------------------------------#
# 2) visualize Lazarus count ----
# The Lazarus count visualization in the paper uses the minimum count found by either
# of the two methods. More explanations are given in the Supplementary Information

if(lazarus_visualize){
  cat("\n visualize lazarus count over time\n")
  if(!file.exists("data/processed/network_stats.csv")){
    stop("need to calculate lazarus counts first.\nset lazarus_calculate <- TRUE")
  }
  net_stats <- read_csv("data/processed/network_stats.csv",
                        col_types = cols(
                          congress = col_double(),
                          lz_fiedler = col_double(),
                          density = col_double(),
                          legislators = col_double(),
                          edges = col_double(),
                          mcl = col_double(),
                          lz_sa = col_double()))
  
  net_stats$dim <- 0
  net_stats$dim[net_stats$lz_sa==0] <- 1
  net_stats$dim[which(net_stats$congress%in%parse_number(list.files("data/processed/boxicity2/")))] <- 2
  
  p <- net_stats %>%
    mutate(dim=factor(dim,levels=c(1,2,0))) %>% 
    ggplot(aes(x=congress,y=lz_sa/legislators))+
    geom_line()+
    geom_point(aes(col=dim,shape=dim),size=4)+
    scale_color_manual(values=c("#EE7600","#7600EE","#424242"),
                       name="boxicity",labels=c("1","2","2+"))+
    scale_shape_manual(values = c(15,16,17),name="boxicity",labels=c("1","2","2+"))+
    scale_y_continuous(breaks = seq(0,60,10))+
    scale_x_continuous(breaks = c(seq(81,115,5),116),
                       labels = c(paste0(seq(1949,2009,10),"-",
                                         seq(1951,2011,10),"\n","(",seq(81,111,5),")"),
                                  "2019-2021\n(116)"))+
    theme_tufte()+
    theme(legend.position = "bottom",
          text=element_text(family=base_font,size=16),
          panel.background =  element_rect(colour="black",fill=NA),
          panel.grid.major.y = element_line(colour="grey",size=0.2))+
    labs(x="Session",y="Normalized Lazarus count")

  
  p1 <- net_stats %>%
    dplyr::filter(congress>= 103) %>% 
    mutate(dim=factor(dim,levels=c(1,2,0))) %>% 
    ggplot(aes(x=congress,y=lz_sa/legislators))+
    geom_line()+
    geom_point(aes(col=dim,shape=dim),size=3)+
    scale_color_manual(values=c("#EE7600","#7600EE","#424242"),
                       name="boxicity",labels=c("1","2","2+"))+
    scale_shape_manual(values = c(15,16,17),name="boxicity",labels=c("1","2","2+"))+
    scale_y_continuous(breaks = seq(0,1,0.1))+
    scale_x_continuous(breaks = 0,labels="")+
    theme_tufte()+
    theme(legend.position = "none",
          text=element_text(family=base_font,size=12),
          panel.background =  element_rect(colour="black",fill="white"),
          panel.grid.major.y = element_line(colour="grey",size=0.2))+
    labs(x="",y="")
  
  
  p1 <- ggplotGrob(p1)
  
  p <- p + annotation_custom(p1,  xmin = 100,  xmax = 118,  ymin = 10,  ymax = 40 )
  ggsave("data/figures/lazarus_senate.pdf",p,width=8,height=4)#h6
}

#------------------------------------------------------------------------------#
# 3) visualize intervals ----

if(intervals_visualize){
  cat("\ncalculate and visualize interval representations\n")
  if(!file.exists("data/processed/network_stats.csv")){
    stop("need to calculate lazarus counts first.\nset lazarus_calculate <- TRUE")
  }
  
  net_stats <- read_csv("data/processed/network_stats.csv",
                        col_types = cols(
                          congress = col_double(),
                          lz_fiedler = col_double(),
                          density = col_double(),
                          legislators = col_double(),
                          edges = col_double(),
                          mcl = col_double(),
                          lz_sa = col_double()))
  
  interval_senats <- net_stats %>% 
    dplyr::filter(lz_sa==0) %>% 
    pull(congress)
  
  for(i in seq_along(interval_senats)){
    s <- interval_senats[i]
    l <- readRDS(paste0("data/processed/networks/senate_",
                        str_pad(s,3,"left","0"),".rds"))
    
    # if(components(l)$no>1){ #unconnected networks are skipped ......{FIX}
    #   next()
    # }
    p <- plot_intervals(l)
    ggsave(paste0("data/figures/intervals_senate_",str_pad(s,3,"left",0),".pdf"),
           p,width=26.8,height=13,dpi=300)  
  }
  # plot_collins() #{FIX}
}

#------------------------------------------------------------------------------#
#4)  visualize fiedler vectors ----

if(fiedler_visualize){
  if(!file.exists("data/processed/network_stats.csv")){
    stop("need to calculate network stats first.\nset lazarus_calculate <- TRUE")
  }
  if(!file.exists("data/processed/fiedler_scores.csv")){
    stop("need to calculate fiedler vectors first.\nset fiedler_calculate <- TRUE")
  }
  senate_scores <- read_csv("data/processed/fiedler_scores.csv")
  senate_net_stats <- read_csv("data/processed/network_stats.csv")
  plot_fiedler(senate_scores,senate_net_stats,font=base_font,text_size = 14,scale=F)
  
  # ggsave("figures/fiedler_senate.pdf",width=12.8,height=5,dpi=400) #7x5
  ggsave("data/figures/fiedler_senate.pdf",width=12,height=6) #5x7
}


#------------------------------------------------------------------------------#
#5)  visualize rectangles ----

if(box2_visualize){
  fl <- list.files("data/processed/boxicity2",full.names = TRUE,pattern="\\.csv")
  for(f in fl){
    s <- str_extract(f,"[0-9]{3,3}")
    p <- plot_box2d(f,font = base_font,text_size = 12)
    ggsave(paste0("data/figures/box2d_senate_",s,".pdf"),
           p,width=7,height=7,dpi=300)  
  }
}

#------------------------------------------------------------------------------#
# 6) visualize super interval ----
if(superint_visualize){
  cat("\nvisualize super interval networks\n")
  net_stats <- read_csv("data/processed/network_stats.csv",
                        col_types = cols(
                          congress = col_double(),
                          lz_fiedler = col_double(),
                          density = col_double(),
                          legislators = col_double(),
                          edges = col_double(),
                          mcl = col_double(),
                          lz_sa = col_double()))
  fl_super <- list.files("data/processed/superbox1",full.names = T)
  fl_orig  <- list.files("data/processed/networks",full.names = T)[net_stats$lz_sa==0]
  
  fl <- c(fl_super,fl_orig)
  fl <- fl[readr::parse_number(fl)%in%c_seq]
  fl <- fl[order(readr::parse_number(fl))]
  
  pList_senate <- list()
  senates <- paste0(c_seq,c("th","st","nd","rd",rep("th",6))[(c_seq%%10)+1]," Session")
  senate_years <- seq(1789,2019,2)[c_seq]
  ged <- c()
  for(f in 1:length(fl)){
    if(str_detect(fl[f],"superbox1")){
      l <- readRDS(fl[f])
      g <- readRDS(str_replace(fl[f],"superbox1","networks"))
      V(l)$display_name <- V(g)$display_name
      V(l)$party <- V(g)$party
      V(l)$state <- V(g)$state
      ged <- c(ged,sum(as_adj(l)-as_adj(g))/2)
    } else{
      l <- readRDS(fl[f])  
      ged <- c(ged,0)
    }
    l <- delete.vertices(l,which(degree(l)==0))
    pList_senate[[f]] <- plot_network(l)+
      labs(title = senates[f],subtitle=paste0("(",senate_years[f]," - ",senate_years[f]+2,")"))+
      theme(plot.title = element_text(family=base_font,size = 16,hjust = 0.5),
            plot.subtitle = element_text(family=base_font,size = 12,hjust = 0.5))
  }
  
  p <- wrap_plots(pList_senate) + plot_layout(ncol=6)
  
  ggsave("data/figures/superbox1_senate.pdf",p,width=18,height=14,device="pdf")
  
}

#------------------------------------------------------------------------------#
# 7) visualize MSE ----

if(mse_visualize){
  circ_scale <- 16
  res <- readRDS("data/processed/mse_senators.rds")
  
  p <- res %>% 
    mutate(is_rep=(party!="Democrat" & party!="Independent")) %>% 
    mutate(num_rep=if_else(is_rep,1,-1)) %>% 
    group_by(congress,is_rep) %>% 
    arrange(congress,is_rep,-size,class) %>% 
    mutate(y = ifelse(lag(class)!=class,sqrt(lag(size)+1)+sqrt(size+1),0)) %>% 
    mutate(y = coalesce(y,0)) %>% 
    mutate(y = cumsum(y)) %>% 
    mutate(y = num_rep*y+num_rep*12) %>% 
    ggplot()+
    geom_arc_bar(aes(y0=y,fill=party,amount=n,r=sqrt(size+1),x0=circ_scale*congress,r0=0),
                 stat="pie",col=NA,size=0.05,n=50)+
    geom_hline(yintercept = 0,size=0.2,colour="grey")+
    scale_fill_manual(values=party_cols,guide=F)+
    scale_x_continuous(breaks=c(81,86,91,96,101,106,111,116)*circ_scale,
                       labels = c(paste0(seq(1949,2009,10),"-",
                                         seq(1951,2011,10),"\n","(",seq(81,111,5),")"),
                                  "2019-2021\n(116)"))+
    theme_tufte(ticks=T)+
    theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
    labs(x="",y="")
  
  p
  ggsave("data/figures/mse_senate.pdf",p,width = 8,height = 4)
}
#_______________________________ ----
#------------------------------------------------------------------------------#
# E) comparative analysis ----
#------------------------------------------------------------------------------#
# 1) compute wnominate ----
if(compute_wnom){
  fl <- list.files("data/processed/votes",full.names = T)
  fl <- fl[parse_number(fl)%in%c_seq]
  for(f in fl){
    res <- wnominate1(f)
    saveRDS(res,str_replace(f,"votes","wnominate") %>% str_replace("csv","rds"))
  }
}

#------------------------------------------------------------------------------#
# 2) dimensionality via MDS ----

if(compare_dim){
  fl <- list.files("data/processed/votes",full.names = T)
  fl <- fl[parse_number(fl)%in%c_seq]
  tbl_pca <- map_dfr(fl,pca_dim)
  
  
 p2 <-  tbl_pca %>% 
    gather("dim","var",dim1:dim3) %>%
   dplyr::filter(dim!="dim3") %>% 
    group_by(congress) %>% 
    mutate(explained=cumsum(var)) %>% 
    ungroup() %>% 
    ggplot(aes(x=congress,y=explained,col=dim))+
    geom_point()+
    geom_smooth(aes(group=dim),se=F,method = "loess",formula = y~x)+
    scale_x_continuous(breaks = c(seq(81,115,5),116))+
    scale_y_continuous(limits=c(0.3,1))+
    scale_colour_manual(values=c("#0A0A0A", "#8A8A8A"), name=" ",
                        labels=c("1st Dimension", "1st+2nd Dimension"))+
    theme_tufte()+
    theme(legend.position = "bottom",
          text=element_text(family=base_font,size=18),
          panel.background =  element_rect(colour="black",fill=NA),
          panel.grid.major.y = element_line(colour="grey",size=0.2))+
    labs(x="Session",y="Variance explained")
  
  fl <- list.files("data/processed/wnominate",full.names = T)
  fl <- fl[parse_number(fl)%in%c_seq]
  tbl_nom <- suppressMessages(map_dfr(fl,wnom_dim))
  
  p1 <- tbl_nom %>% 
    ggplot(aes(x=congress))+
    geom_point(aes(y=dim1),col="#0A0A0A")+
    geom_point(aes(y=dim2),col="#8A8A8A")+
    geom_smooth(aes(y=dim1),se=F,method = "loess",formula = y~x,col="#0A0A0A")+
    geom_smooth(aes(y=dim2),se=F,method = "loess",formula = y~x,col="#8A8A8A")+
    scale_x_continuous(breaks = c(seq(81,115,5),116))+
    scale_y_continuous(limits=c(0.3,1))+
    theme_tufte()+
    theme(legend.position = "none",
          text=element_text(family=base_font,size=18),
          panel.background =  element_rect(colour="black",fill=NA),
          panel.grid.major.y = element_line(colour="grey",size=0.2))+
    labs(x="Session",y="APRE")
  
  p <- p1 + p2 + plot_layout(guides = "collect") + 
    plot_annotation(tag_levels = "a",tag_suffix = ")") & theme(legend.position = 'bottom')
    
  ggsave("data/figures/pca_vs_wnom_var.pdf",p,width = 12,height = 6)
}

#------------------------------------------------------------------------------#
# 3) polarisation via Clustering ----

if(compare_clustering){
  fl <- list.files("data/processed/networks",full.names = T)
  fl <- fl[parse_number(fl)%in%c_seq]
  tbl_clu <- map_dfr(fl,mod_polarise)
  
  ggplot(tbl_clu,aes(x=congress,y=modularity))+
    geom_line()+
    geom_point()+
    scale_y_continuous(breaks = seq(0,0.5,0.1),limits=c(0,0.5))+
    scale_x_continuous(breaks = c(seq(81,115,5),115),
                       labels = c(paste0(seq(1949,2009,10),"\n","(",seq(81,111,5),")"),"2017\n(115)"))+
    ggthemes::theme_tufte()+
    theme(legend.position = "none",
          text=element_text(family=base_font,size=14),
          panel.background =  element_rect(colour="black",fill=NA),
          panel.grid.major.y = element_line(colour="grey",size=0.2))+
    labs(x="Session",y="Modularity")
  
  ggplot(tbl_clu,aes(x=congress,y=adjrand))+
    geom_line()+
    geom_point()+
    scale_y_continuous(breaks = seq(0,1,0.2),limits=c(0,1))+
    scale_x_continuous(breaks = c(seq(81,115,5),115),
                       labels = c(paste0(seq(1949,2009,10),"\n","(",seq(81,111,5),")"),"2017\n(115)"))+
    ggthemes::theme_tufte()+
    theme(legend.position = "none",
          text=element_text(family=base_font,size=14),
          panel.background =  element_rect(colour="black",fill=NA),
          panel.grid.major.y = element_line(colour="grey",size=0.2))+
    labs(x="Session",y="Adjusted Rand Index")
  
}

#_______________________________ ----
#------------------------------------------------------------------------------#
# E) Misc Figures ----

#------------------------------------------------------------------------------#
# 1) PCA/Wnom Example ----

nom_tbl <- readRDS("data/processed/wnominate/senate_115.rds") %>% 
  .[["legislators"]] %>% 
  mutate(id=1:nrow(.)) %>% 
  select(id,party,coord1D,coord2D) %>% 
  dplyr::filter(!is.na(coord1D))

votes <- vroom(paste0("data/processed/votes/senate_115.csv"),
               col_types = cols(
                 .default=col_double(),
                 date = col_date(format = ""),
                 bioname = col_character(),
                 state_abbrev = col_character(),
                 party_name = col_character()),progress=FALSE)

votes[["cast_code"]] <- case_when(votes[["cast_code"]] <= 3~"yea",
                                  votes[["cast_code"]] <= 6~"nay",
                                  votes[["cast_code"]] <= 8~"present",
                                  TRUE~"not voting")

peeps <- votes %>% select(icpsr,party_name) %>% distinct()
# filter bills
votes <- votes %>% 
  mutate(frac = yea_count/(yea_count+nay_count)) %>% 
  mutate(frac = if_else(frac<=0.5,frac,1-frac)) %>% 
  dplyr::filter(frac>=0.025) %>% 
  select(-frac)

#create two-mode network
el <- votes %>% 
  dplyr::filter(cast_code%in%c("yea","nay")) %>% 
  mutate(vote_ident = paste0(rollnumber,"-",cast_code))

g <- bipartite_from_data_frame(el,"vote_ident","icpsr")
B <- get.incidence(g)
P <- B%*%t(B)
P <- P/diag(P)
diag(P) <- 0
# R = P*0 + rowMeans(P)  
# C = t(P*0 + colMeans(P))
# P = P - R - C + mean(P)

pca <- tibble(icpsr=as.numeric(rownames(P)),coord1D=prcomp(P)$x[,1],coord2D=prcomp(P)$x[,2]) %>% 
  mutate(coord1D=normalise(coord1D,to=c(-1,1)),coord2D=normalise(coord2D,to=c(-1,1))) %>% 
  mutate(coord1D=-coord1D) %>% 
  # mutate(r=sqrt(coord1D^2+coord2D^2),ang=atan2(coord2D,coord1D)) %>% 
  # mutate(coord1D=coord1D/(max(r)),coord2D=coord2D/(max(r))) %>% 
  left_join(peeps,by="icpsr")

p1 <- ggplot()+
  # ggforce::geom_circle(aes(x0=0,y0=0,r=1),col="black",size=0.1)+
  geom_point(data=nom_tbl,aes(coord1D,coord2D,col=party))+
  scale_color_manual(values=party_cols)+
  theme_tufte()+
  theme(legend.position = "none",
        text=element_text(family=base_font,size=18),
        panel.background =  element_rect(colour="black",fill=NA))+
  labs(x="1st Dimension",y="2nd Dimension")

p2 <- ggplot()+
  # ggforce::geom_circle(aes(x0=0,y0=0,r=1),col="black",size=0.1)+
  geom_point(data=pca,aes(coord1D,coord2D,col=party_name))+
  scale_color_manual(values=party_cols)+
  theme_tufte()+
  theme(legend.position = "none",
        text=element_text(family=base_font,size=18),
        panel.background =  element_rect(colour="black",fill=NA))+
  labs(x="1st Dimension",y="")

p <- p1+p2+plot_annotation(tag_levels = "a",tag_suffix = ")")
p
ggsave("data/figures/pca_vs_wnom.pdf",p,width = 12,height = 6)


#------------------------------------------------------------------------------#
# 2) polarization ----
senate_scores <- read_csv("data/processed/fiedler_scores.csv", 
                          col_types = cols(
                            congress = col_double(),
                            name = col_character(),
                            party = col_character(),
                            state = col_character(),
                            fiedler = col_double(),
                            y = col_double(),
                            connected = col_logical()
                          ))
senate_scores <- senate_scores %>% 
  left_join(states,by=c("state"="state_abbrev"))

p1 <- senate_scores %>% 
  group_by(congress) %>% 
  # mutate(fiedler=ideo_scale(fiedler)) %>%
  ungroup() %>%
  mutate(party = case_when(party=="Democrat" & south ~ "Democrat (South)",
                           party=="Democrat" & !south ~ "Democrat (North)",
                           TRUE ~ party)) %>% 
  bind_rows(dplyr::filter(senate_scores,party=="Democrat")) %>% 
  # mutate(fiedler=ideo_scale(fiedler)) %>%
  group_by(congress,party) %>% 
  dplyr::summarise(cmean = mean(fiedler,na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::filter(str_detect(party,"Democrat") | party=="Republican") %>% 
  rowwise() %>% 
  mutate(cmean=case_when(str_detect(party,"Democrat") & cmean>0~-1*cmean,
                         party=="Republican" & cmean<0~-1*cmean,
                         TRUE~cmean)) %>% 
  ggplot(aes(congress,cmean,col=party,shape=party))+
  geom_smooth(method="loess",se=F,formula=y~x)+
  geom_point(size=3,alpha=0.75)+
  scale_color_manual(values=party_cols,name="",labels=c("Democrats","Northern Democrats","Southern Democrats","Republicans"))+
  scale_shape_manual(values = c(15,16,17,18),name="",labels=c("Democrats","Northern Democrats","Southern Democrats","Republicans")) +
  scale_y_continuous(breaks = seq(-1,1,0.5))+
  scale_x_continuous(breaks = c(seq(81,115,5),116),
                     labels = c(paste0(seq(1949,2009,10),"-",
                                       seq(1951,2011,10),"\n","(",seq(81,111,5),")"),
                                "2019-2021\n(116)"))+
  theme_tufte()+
  theme(legend.position = "bottom",
        text=element_text(size=16,family=base_font),
        panel.background =  element_rect(colour="black",fill=NA),
        panel.grid.major.y = element_line(colour="grey",size=0.2))+
  labs(x="Session",y="Party mean of Fiedler vector")


ggsave("data/figures/polarization_senate.pdf",p1,width=8,height=4)


#------------------------------------------------------------------------------#
# 3) Interval Senate 115 ----

l <- readRDS("data/processed/networks/senate_115.rds")

ints <- get_intervals(l)
ints$party <- V(l)$party
initials <- str_squish(V(l)$display_name) %>% 
  str_remove_all("\\s*\\(.*\\)") %>% 
  str_remove_all(", Jr\\.") %>% 
  word(2,sep=", ") %>% 
  str_extract_all("[A-Z]") %>% 
  map(paste,collapse="") %>% 
  unlist()
last_name <- str_squish(V(l)$display_name) %>% 
  word(1,sep=",") 
ints$name <- paste0(initials," ",last_name," (",V(l)$state,")")

ints %>% 
  mutate(x=fscale(min(ints$X1),max(ints$X2),X1),xend=fscale(min(ints$X1),max(ints$X2),X2)) %>%
  rowwise() %>% 
  mutate(middle=mean(c(x,xend))) %>% 
  ungroup() %>% 
  mutate(grp=ifelse(party=="Republican",2,1)) %>% 
  group_by(grp) %>% 
  mutate(sign=case_when(#(congress==112 & grp==2~1),
    (mean(x)>0 & grp==2)~1,
    (mean(x)>0 & grp==1)~-1,
    (mean(x)<0 & grp==2)~-1,
    (mean(x)<0 & grp==1)~1)) %>% 
  arrange(ifelse(grp==2,1,-1)*sign*middle,sign*xend-sign*x) %>% 
  mutate(rk=row_number()) -> dat

dat$name <- ifelse(dat$name%in%c("SM Collins (ME)","MK Heitkamp (ND)","J Manchin (WV)"),dat$name,"")
dat$name[dat$name=="SM Collins (ME)"] <- "1"
dat$name[dat$name=="MK Heitkamp (ND)"] <- "2"
dat$name[dat$name=="J Manchin (WV)"] <- "3"
dat$xannot <- 0
dat$xannot[dat$name == "1"] <- 1.025
dat$xannot[dat$name == "2"] <- -1.05
dat$xannot[dat$name == "3"] <- -1.025
  
# ggplot(dat)+
#   geom_segment(aes(y=rk,yend=rk),x=-1,xend=1,col="grey",size=0.1)+
#   annotate("segment",xend=-1.047,x=-1,y=2,yend=2,col="grey",size=0.2) +
#   annotate("segment",xend=-1.023,x=-1,y=1,yend=1,col="grey",size=0.2) +
#   annotate("segment",xend=1.023,x=1,y=1,yend=1,col="grey",size=0.2) +
#   geom_segment(aes(x=sign*x,
#                    xend=sign*xend,
#                    y=rk+(grp==2)*0.1+(grp==1)*-0.1,
#                    yend=rk+(grp==2)*0.1+(grp==1)*-0.1,
#                    col=party),size=1.2,alpha=1)+
#   geom_text(aes(y=rk,label=name),x=dat$xannot,
#   size=4.5, #2.5
#   hjust=ifelse(dat$grp==2,0,1))+
#   annotate("segment", y= 0,yend=0,x=-1,xend=1)+
#   coord_cartesian(clip = "off")+
#   ggthemes::theme_tufte()+
#   theme(legend.position = "none",
#         axis.text.x = element_text(size=14),
#         axis.title.x = element_text(size=14))+
#   scale_color_manual(values=c("Republican"="#CD3333", 
#                               "Democrat"="#104E8B",
#                               "Independent"="#EEB422"))+
#   scale_x_continuous(limits=c(-1.1,1.1),breaks=c(-1,0,1))+
#   # scale_x_continuous(breaks=c(-1,0,1))+
#   scale_y_continuous(breaks=NULL)+
#   labs(y="",x="")

dat1 <- dat %>% 
  group_by(x,xend) %>% 
  dplyr::summarise(size=n(),rk1=min(rk),party=list(unique(party)),sign=sign[1]) %>% 
  ungroup() %>%
  mutate(pident=map_chr(party,function(x) paste0(str_sub(x,1,1),collapse=""))) %>% 
  mutate(class=paste0(pident,dense_rank(rk1))) %>% 
  unnest(party)

ggplot(dat1)+
  geom_rect(aes(xmin=sign*x,xmax=xend*sign,ymin=rk1+0.5,ymax=rk1+size-0.5,fill=party),alpha=0.75)+
  geom_rect(aes(xmin=sign*x,xmax=xend*sign,ymin=rk1+0.5,ymax=rk1+size-0.5,col=party),fill=NA)+
  geom_text(aes(x=ifelse(size==1,ifelse(party=="Republican",sign*xend,sign*x),(sign*x+sign*xend)/2),
                y=rk1+size/2,label=class),
            hjust=ifelse(dat1$size==1,ifelse(dat1$party=="Republican",0,1),0.5))+
  scale_fill_manual(values=c("Republican"="#CD3333", 
                             "Democrat"="#104E8B",
                             "Independent"="#EEB422"))+
  scale_colour_manual(values=c("Republican"="#CD3333", 
                               "Democrat"="#104E8B",
                               "Independent"="#EEB422"))+
  scale_x_continuous(limits=c(-1.1,1.1),breaks=c(-1,0,1))+
  # scale_x_continuous(breaks=c(-1,0,1))+
  scale_y_continuous(breaks=NULL)+
  coord_cartesian(clip = "off")+
  ggthemes::theme_tufte()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=14),
        axis.title.x = element_text(size=14))+
  labs(y="",x="")


ggsave("data/figures/intervals_senate_115_noname.pdf",width=14,height=5)#5  

