# constants ----
party_cols <- 
c(Democrat = "#104E8B", Republican = "#CD3333", `Democrat-Republican` = "#9A32CD", 
  Independent = "#EEB422", Federalist = "black", Whig = "#F0DC82", 
  Jackson = "#9A32CD", `Farmer-Labor` = "#228B22", Progressive = "#B63D34", 
  Populist = "#B7B7B7", `Pro-Administration` = "#104E8B", Adams = "#A4A4A4", 
  American = "#C8C8C8", Silver = "#E6E6E6", `Anti-Jackson` = "#ABABAB", 
  Nullifier = "#7D7D7D", Conservative = "#5C5C5C", Unionist = "#CDCDCD", 
  `Free Soil` = "#DCDCDC", `Anti-Administration` = "#CD3333", Readjuster = "#686868", 
  `Unconditional Unionist` = "#B1B1B1", `Liberal Republican` = "#C3C3C3", 
  `Silver Republican` = "#9D9D9D", `Jackson Republican` = "#D2D2D2", 
  `Crawford Republican` = "#8E8E8E", `Ind. Democrat` = "#737373", 
  `Ind. Republican` = "#E1E1E1", `Law and Order` = "#969696", Liberty = "#4D4D4D", 
  Opposition = "#868686", `Adams-Clay Federalist` = "#BDBDBD", 
  `Adams-Clay Republican` = "#D7D7D7",`Democrat (North)` = "#c5d4e3", `Democrat (South)` = "#7600EE")


states <- tibble(state_icpsr = c(1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 21, 22, 23, 24, 25, 31,
                                 32, 33, 34, 35, 36, 37, 40, 41, 42, 43, 44, 45, 46, 47, 48,
                                 49, 51, 52, 53, 54, 56, 61, 62, 63, 64, 65, 66, 67, 68, 71,
                                 72, 73, 81, 82, 99),
                 state_abbrev = c("CT", "ME", "MA", "NH", "RI", "VT", "DE", "NJ", "NY", "PA",
                                  "IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE",
                                  "ND", "SD", "VA", "AL", "AR", "FL", "GA", "LA", "MS", "NC",
                                  "SC", "TX", "KY", "MD", "OK", "TN", "WV", "AZ", "CO", "ID",
                                  "MT", "NV", "NM", "UT", "WY", "CA", "OR", "WA", "AK", "HI",
                                  "USA")
)
states$south <- FALSE 
states$south[states$state_icpsr%in%c(40:49,51,53)] <- TRUE


#------------------------------------------------------------------------------#
# data wrangling ----
clean_data <- function(){
  if(length(list.files("data/processed/votes/"))>0){
    stop("data/processed/votes is not empty. delete content to rerun the cleaning process or set force=TRUE")
  }
  Sall_membs <- vroom("data/raw/Sall_members.csv",
                      col_select = c(congress,chamber,icpsr,state_abbrev,party_code,bioname,
                                     nominate_dim1,nominate_dim2),
                      col_types = cols(congress = col_double(),
                                       chamber = col_character(),
                                       state_abbrev = col_character(),
                                       bioname = col_character(),
                                       congress = col_double(),
                                       icpsr = col_double(),
                                       party_code = col_double(),
                                       nominate_dim1 = col_double(),
                                       nominate_dim2 = col_double())) %>% 
    dplyr::filter(chamber=="Senate") %>% 
    select(-chamber)
  
  Sall_membs[["bioname"]] <- str_to_title(Sall_membs[["bioname"]])
  
  Sall_votes <- vroom("data/raw/Sall_votes.csv", progress = FALSE,
                      col_select = c(congress,chamber,rollnumber,icpsr,cast_code),
                      col_types = cols(congress = col_double(),
                                       chamber = col_character(),
                                       rollnumber = col_double(),
                                       icpsr = col_double(),
                                       cast_code = col_double())) %>% 
    dplyr::filter(chamber=="Senate") %>% 
    select(-chamber)
  
  Sall_party <- vroom("data/raw/Sall_parties.csv", 
                      col_select = c(congress,chamber,party_code,party_name,n_members),
                      col_types = cols(
                        congress = col_double(),
                        chamber = col_character(),
                        party_code = col_double(),
                        party_name = col_character(),
                        n_members = col_double()))
  
  Sall_party <- Sall_party %>% 
    select(party_code,party_name) %>% 
    distinct()
  
  
  Sall_rcall <- vroom("data/raw/Sall_rollcalls.csv",
                      col_select = c(congress,rollnumber,date,yea_count,nay_count),
                      col_types = cols(congress = col_double(),
                                       rollnumber = col_double(),
                                       date = col_date(format = ""),
                                       yea_count = col_double(),
                                       nay_count = col_double()))
  
  Sall <- left_join(Sall_votes,Sall_membs,by=c("congress","icpsr"))
  Sall <- left_join(Sall,Sall_party,by=c("party_code"))
  Sall <- left_join(Sall,Sall_rcall,by=c("congress","rollnumber"))
  
  Sall <- select(Sall,congress,rollnumber,date,yea_count,nay_count,bioname,icpsr,cast_code,
         state_abbrev,party_code,party_name,nominate_dim1,nominate_dim2)
  
  Sall <- Sall %>% dplyr::filter(!is.na(bioname))
  
  Sall_lst <- Sall %>% 
    group_split(congress)
  
  n <- length(Sall_lst)
  dir.create("data/processed/votes")
  tmp <- map(1:n,function(x) write_csv(Sall_lst[[x]],
                                path = paste0("data/processed/votes/senate_",str_pad(x,3,"left","0"),".csv")))
  
  cat("created files: data/processed/votes/senate_xxx.csv","for senates: 1 -",n,"\n")
}
#------------------------------------------------------------------------------#
# vote_stats <- function(c){
#   votes <- vroom(paste0("data/processed/votes/senate_",str_pad(c,3,"left","0"),".csv"),
#                  col_types = cols(
#                    .default=col_double(),
#                    date = col_date(format = ""),
#                    bioname = col_character(),
#                    state_abbrev = col_character(),
#                    party_name = col_character()))
#   
#   no_votes <- length(unique(votes$rollnumber)) 
#   unani
# }
#------------------------------------------------------------------------------#
# network formation ----
create_network <- function(c,model="logit",filter_senator = 0.5,filter_bills = 0.025,alpha = 0.05,write = TRUE){
  
  votes <- vroom(paste0("data/processed/votes/senate_",str_pad(c,3,"left","0"),".csv"),
                 col_types = cols(
                   .default=col_double(),
                   date = col_date(format = ""),
                   bioname = col_character(),
                   state_abbrev = col_character(),
                   party_name = col_character()),progress=FALSE)
  
  # clean casted votes: 1-3=yea,4-6=nay,7-8=present,9=not voting (https://voteview.com/articles/data_help_votes)
  votes[["cast_code"]] <- case_when(votes[["cast_code"]] <= 3~"yea",
                                    votes[["cast_code"]] <= 6~"nay",
                                    votes[["cast_code"]] <= 8~"present",
                                    TRUE~"not voting")
  
  # filter senator
  bills <- length(unique(votes$rollnumber))
  votes1 <- votes %>%
    group_by(icpsr) %>% 
    mutate(frac=sum(cast_code=="yea" | cast_code=="nay")/bills) %>% 
    ungroup() %>% 
    dplyr::filter(frac>=filter_senator) %>% 
    select(-frac)
  
  # filter bills
  votes2 <- votes1 %>% 
    mutate(frac = yea_count/(yea_count+nay_count)) %>% 
    mutate(frac = if_else(frac<=0.5,frac,1-frac)) %>% 
    dplyr::filter(frac>=filter_bills) %>% 
    select(-frac)

  #deleted bills/senators
  del_bills <- length(unique(votes$rollnumber))-length(unique(votes2$rollnumber))
  del_senat <- length(unique(votes$icpsr))-length(unique(votes1$icpsr))
    
  #create two-mode network
  el <- votes2 %>% 
    dplyr::filter(cast_code%in%c("yea","nay")) %>% 
    mutate(vote_ident = paste0(rollnumber,"-",cast_code))
  
  g <- bipartite_from_data_frame(el,"vote_ident","icpsr")
  
  B <- suppressMessages(backbone::sdsm(g,model=model))
  l <- backbone.extract(B,signed = FALSE,alpha = alpha,class = "igraph")
  
  nodes <- votes2 %>% 
    select(icpsr,bioname,state_abbrev,party_code,party_name) %>% 
    distinct()
  
  V(l)$display_name <- nodes$bioname[match(V(l)$name,nodes$icpsr)]
  V(l)$state <- nodes$state_abbrev[match(V(l)$name,nodes$icpsr)]
  V(l)$party <- nodes$party_name[match(V(l)$name,nodes$icpsr)]
  V(l)$party_code <- nodes$party_code[match(V(l)$name,nodes$icpsr)]
  
  l
}

create_superbox1 <- function(l){
  B <- as_adj(l,sparse = FALSE)
  MSE <- igraphUtils::structural_equivalence(l)
  l1 <- delete.vertices(l,which(duplicated(MSE)))
  adj <- neighborhood(l1)
  adj <- lapply(adj,function(x) as.integer(x))
  A <- as_adj(l1,sparse=FALSE)
  perm <- sample(1:vcount(l1))
  res_sa <- optim(par = perm, fn = ged1, A = A,adj = adj,gr = genperm_sb1,method = "SANN",
               control = list(maxit = 500000, temp = 100, tmax = 500, trace = FALSE,
                              REPORT = 5))
  
  res_sa <- optim(par = res_sa$par, fn = ged1, A = A,adj = adj,gr = genperm_sb1,method = "SANN",
                  control = list(maxit = 50000, temp = 10, tmax = 500, trace = FALSE,
                                 REPORT = 5))
  
  Ared <- perm2int(adj,res_sa$par)
  
  Afull <- Ared[MSE,MSE]
  for(grp in unique(MSE)){
    Afull[MSE==grp,MSE==grp] <- B[MSE==grp,MSE==grp]
  }
  
  res <- list(perm1 = res_sa$par,ged=res_sa$value,A_MSE = Ared,A=Afull,MSE = MSE)
  res
}

create_superbox2 <- function(l){
  B <- as_adj(l,sparse = FALSE)
  MSE <- igraphUtils::structural_equivalence(l)
  l1 <- delete.vertices(l,which(duplicated(MSE)))
  adj <- neighborhood(l1)
  adj <- lapply(adj,function(x) as.integer(x))
  A <- as_adj(l1,sparse=FALSE)
  perm <- c(sample(1:vcount(l1)),sample(1:vcount(l1)))
  res_sa <- optim(par = perm, fn = ged2, A = A,adj = adj,gr = genperm_sb2,method = "SANN",
                  control = list(maxit = 500000, temp = 100, tmax = 500, trace = FALSE,
                                 REPORT = 5))
  perm1 <- res_sa$par[1:vcount(l1)]
  perm2 <- res_sa$par[(vcount(l1)+1):length(res_sa$par)]
  
  res_sa <- optim(par = c(perm1,perm2), fn = ged2, A = A,adj = adj,gr = genperm_sb2,method = "SANN",
                  control = list(maxit = 50000, temp = 10, tmax = 500, trace = FALSE,
                                 REPORT = 5))
  
  perm1 <- res_sa$par[1:vcount(l1)]
  perm2 <- res_sa$par[(vcount(l1)+1):length(res_sa$par)]
  
  Ared <- perm2box(adj,perm1,perm2)
  
  Afull <- Ared[MSE,MSE]
  for(grp in unique(MSE)){
    Afull[MSE==grp,MSE==grp] <- B[MSE==grp,MSE==grp]
  }
  
  res <- list(perm1 = perm1,perm2 = perm2,ged=res_sa$value,A_MSE = Ared,A=Afull,MSE = MSE)
  res
}

create_superbox3 <- function(l){
  B <- as_adj(l,sparse = FALSE)
  MSE <- igraphUtils::structural_equivalence(l)
  l1 <- delete.vertices(l,which(duplicated(MSE)))
  adj <- neighborhood(l1)
  adj <- lapply(adj,function(x) as.integer(x))
  A <- as_adj(l1,sparse=FALSE)
  perm <- c(sample(1:vcount(l1)),sample(1:vcount(l1)),sample(1:vcount(l1)))
  res_sa <- optim(par = perm, fn = ged3, A = A,adj = adj,gr = genperm_sb3,method = "SANN",
                  control = list(maxit = 500000, temp = 100, tmax = 500, trace = FALSE,
                                 REPORT = 5))
  
  perm1 <- res_sa$par[1:vcount(l1)]
  perm2 <- res_sa$par[(vcount(l1)+1):(2*vcount(l1))]
  perm3 <- res_sa$par[(2*vcount(l1)+1):(3*vcount(l1))]
  
  res_sa <- optim(par = c(perm1,perm2,perm3), fn = ged3, A = A,adj = adj,gr = genperm_sb3,method = "SANN",
                  control = list(maxit = 50000, temp = 10, tmax = 500, trace = FALSE, REPORT = 5))
  
  perm1 <- res_sa$par[1:vcount(l1)]
  perm2 <- res_sa$par[(vcount(l1)+1):(2*vcount(l1))]
  perm3 <- res_sa$par[(2*vcount(l1)+1):(3*vcount(l1))]
  
  Ared <- perm2cube(adj,perm1,perm2,perm3)
  
  Afull <- Ared[MSE,MSE]
  for(grp in unique(MSE)){
    Afull[MSE==grp,MSE==grp] <- B[MSE==grp,MSE==grp]
  }
  
  res <- list(perm1 = perm1,perm2 = perm2,perm3=perm3,ged=res_sa$value,A_MSE = Ared,A=Afull,MSE = MSE)
  res
}

create_superbox4 <- function(l){
  B <- as_adj(l,sparse = FALSE)
  MSE <- igraphUtils::structural_equivalence(l)
  l1 <- delete.vertices(l,which(duplicated(MSE)))
  adj <- neighborhood(l1)
  adj <- lapply(adj,function(x) as.integer(x))
  A <- as_adj(l1,sparse=FALSE)
  perm <- c(sample(1:vcount(l1)),sample(1:vcount(l1)),sample(1:vcount(l1)),sample(1:vcount(l1)))
  res_sa <- optim(par = perm, fn = ged4, A = A,adj = adj,gr = genperm_sb4,method = "SANN",
                  control = list(maxit = 600000, temp = 100, tmax = 500, trace = FALSE,
                                 REPORT = 5))
  
  perm1 <- res_sa$par[1:vcount(l1)]
  perm2 <- res_sa$par[(vcount(l1)+1):(2*vcount(l1))]
  perm3 <- res_sa$par[(2*vcount(l1)+1):(3*vcount(l1))]
  perm4 <- res_sa$par[(3*vcount(l1)+1):(4*vcount(l1))]
  
  res_sa <- optim(par = c(perm1,perm2,perm3,perm4), fn = ged4, A = A,adj = adj,gr = genperm_sb4,method = "SANN",
                  control = list(maxit = 50000, temp = 10, tmax = 500, trace = FALSE, REPORT = 5))
  
  perm1 <- res_sa$par[1:vcount(l1)]
  perm2 <- res_sa$par[(vcount(l1)+1):(2*vcount(l1))]
  perm3 <- res_sa$par[(2*vcount(l1)+1):(3*vcount(l1))]
  perm4 <- res_sa$par[(3*vcount(l1)+1):(4*vcount(l1))]
  
  Ared <- perm2hyper(adj,perm1,perm2,perm3,perm4)
  
  Afull <- Ared[MSE,MSE]
  for(grp in unique(MSE)){
    Afull[MSE==grp,MSE==grp] <- B[MSE==grp,MSE==grp]
  }
  
  res <- list(perm1 = perm1,perm2 = perm2,perm3=perm3,perm4=perm4,ged=res_sa$value,A_MSE = Ared,A=Afull,MSE = MSE)
  res
}

create_superbox5 <- function(l){
  B <- as_adj(l,sparse = FALSE)
  MSE <- igraphUtils::structural_equivalence(l)
  l1 <- delete.vertices(l,which(duplicated(MSE)))
  adj <- neighborhood(l1)
  adj <- lapply(adj,function(x) as.integer(x))
  A <- as_adj(l1,sparse=FALSE)
  perm <- c(sample(1:vcount(l1)),sample(1:vcount(l1)),
            sample(1:vcount(l1)),sample(1:vcount(l1)),sample(1:vcount(l1)))
  res_sa <- optim(par = perm, fn = ged5, A = A,adj = adj,gr = genperm_sb5,method = "SANN",
                  control = list(maxit = 600000, temp = 100, tmax = 500, trace = FALSE,
                                 REPORT = 5))
  
  perm1 <- res_sa$par[1:vcount(l1)]
  perm2 <- res_sa$par[(vcount(l1)+1):(2*vcount(l1))]
  perm3 <- res_sa$par[(2*vcount(l1)+1):(3*vcount(l1))]
  perm4 <- res_sa$par[(3*vcount(l1)+1):(4*vcount(l1))]
  perm5 <- res_sa$par[(4*vcount(l1)+1):(5*vcount(l1))]
  
  res_sa <- optim(par = c(perm1,perm2,perm3,perm4,perm5), fn = ged5, A = A,adj = adj,gr = genperm_sb5,method = "SANN",
                  control = list(maxit = 50000, temp = 10, tmax = 500, trace = FALSE, REPORT = 5))
  
  perm1 <- res_sa$par[1:vcount(l1)]
  perm2 <- res_sa$par[(vcount(l1)+1):(2*vcount(l1))]
  perm3 <- res_sa$par[(2*vcount(l1)+1):(3*vcount(l1))]
  perm4 <- res_sa$par[(3*vcount(l1)+1):(4*vcount(l1))]
  perm5 <- res_sa$par[(4*vcount(l1)+1):(5*vcount(l1))]
  
  Ared <- perm2pente(adj,perm1,perm2,perm3,perm4,perm5)
  
  Afull <- Ared[MSE,MSE]
  for(grp in unique(MSE)){
    Afull[MSE==grp,MSE==grp] <- B[MSE==grp,MSE==grp]
  }
  
  res <- list(perm1 = perm1,perm2 = perm2,perm3=perm3,perm4=perm4,perm5=perm5,
              ged=res_sa$value,A_MSE = Ared,A=Afull,MSE = MSE)
  res
}


#------------------------------------------------------------------------------#
# visualization ----

plot_network <- function(l, cols=party_cols){
  xy <- layout_with_stress(l)
  xy <- adjust_layout(l,xy)
  e1 <- V(l)$party[ends(l, es=E(l), names=F)[,1]]
  e2 <- V(l)$party[ends(l, es=E(l), names=F)[,2]]
  E(l)$color <- "XX"
  E(l)$color[e1==e2 & e1=="Republican"] <- "RR"
  E(l)$color[e1==e2 & e1=="Democrat"] <- "DD"
  ggraph(l,layout="manual",x=xy[,1],y=xy[,2])+
    geom_edge_link0(aes(edge_colour=color),alpha=0.25)+
    geom_node_point(aes(fill=party),shape=21,size=2,stroke=0.1)+
    scale_fill_manual(values=cols)+
    scale_edge_color_manual(values=c("RR"="#CD3333", "DD"="#104E8B","XX"="gray55"))+
    theme_graph()+
    theme(legend.position = "none")
}


plot_intervals <- function(l){
  comps <- components(l)
  if(comps$no==1){
    check <- FALSE
  } else{
    check <- TRUE
  }
  
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
  
  if(check){
    ints$X1[ints$party!="Republican"] <- ints$X1[ints$party!="Republican"]-7
    ints$X2[ints$party!="Republican"] <- ints$X2[ints$party!="Republican"]-7
    ints$X1[ints$party=="Republican"] <- ints$X1[ints$party=="Republican"]+7
    ints$X2[ints$party=="Republican"] <- ints$X2[ints$party=="Republican"]+7
  }
  
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
  
  ggplot(dat)+
    geom_segment(aes(y=rk,yend=rk),x=-1,xend=1,col="grey",size=0.1)+
    geom_segment(aes(x=sign*x,
                     xend=sign*xend,
                     y=rk+(grp==2)*0.1+(grp==1)*-0.1,
                     yend=rk+(grp==2)*0.1+(grp==1)*-0.1,
                     col=party),size=1.8,alpha=1)+
    geom_text(aes(y=rk,label=name),x=case_when((dat$grp==2 & dat$rk%%2==0)~1,
                                               (dat$grp==2 & dat$rk%%2!=0)~1.45,
                                               (dat$grp!=2 & dat$rk%%2==0)~-1,
                                               (dat$grp!=2 & dat$rk%%2!=0)~-1.53,
    ), 
    size=9,
    hjust=ifelse(dat$grp==2,0,1))+
    annotate("segment", y= 0,yend=0,x=-1,xend=1)+
    coord_cartesian(clip = "off")+
    ggthemes::theme_tufte()+
    theme(legend.position = "none",
          axis.text.x = element_text(size=30),
          axis.title.x = element_text(size=30))+
    scale_color_manual(values=c("Republican"="#CD3333", 
                                "Democrat"="#104E8B",
                                "Independent"="#EEB422"))+
    # scale_x_continuous(limits=c(-1.2,1.2),breaks=c(-1,0,1))+
    scale_x_continuous(limits=c(-1.8,1.8),breaks=c(-1,0,1))+
    scale_y_continuous(breaks=NULL)+
    labs(y="",x="")
}

plot_box2d <- function(boxfile,font,text_size = 12){
  box_tbl <- read_csv(boxfile,
                      col_types = cols(
                        x1 = col_double(),
                        x2 = col_double(),
                        y1 = col_double(),
                        y2 = col_double(),
                        party = col_character(),
                        name = col_character()))
  
  sign_tbl <- box_tbl %>% group_by(party) %>% dplyr::summarize(xmean=mean(x1))
  dimxy <- c(min(box_tbl$x1),max(box_tbl$x2),min(box_tbl$y1),max(box_tbl$y2))
  
  p <- ggplot(box_tbl)+
    geom_rect(aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,fill=party),
              alpha=0.25,col="black",size=0.2)+
    scale_fill_manual(values=party_cols)+
    ggthemes::theme_tufte(ticks = FALSE)+
    theme(legend.position = "none",
          text = element_text(size=text_size,family=font),
          panel.background = element_blank(),
          axis.text = element_blank())+
    labs(x="1st Dimension",y="2nd Dimension")
  
  if(sign_tbl$xmean[sign_tbl$party=="Republican"]<sign_tbl$xmean[sign_tbl$party=="Democrat"]){
    p <- p+scale_x_reverse(limits = c(dimxy[2:1]))
  } else{
    p <- p+scale_x_continuous(limits = c(dimxy[1:2]))
  }
  box_tbl_sum <- box_tbl %>% 
    group_by(x1,x2,y1,y2,party) %>% 
    count() %>% 
    ungroup() %>% 
    arrange(x1) %>% 
    group_by(party) %>% 
    mutate(id=row_number())
  
  px <- box_tbl_sum %>% 
    ggplot(aes(x=x1,xend=x2,y=id,yend=id))+geom_segment(aes(col=party,size=n))+
    scale_color_manual(values=party_cols)+
    scale_size_continuous(range = c(0.1,1))+
    ggthemes::theme_tufte(ticks = FALSE)+
    theme(legend.position = "none",
          text = element_text(size=text_size,family=font),
          panel.background = element_blank(),
          axis.text = element_blank())+
    labs(x="",y="")
  
  if(sign_tbl$xmean[sign_tbl$party=="Republican"]<sign_tbl$xmean[sign_tbl$party=="Democrat"]){
    px <- px + scale_x_reverse(limits = c(dimxy[2:1]))
  } else{
    px <- px+scale_x_continuous(limits = c(dimxy[1:2]))
  }
  
  pyD <- box_tbl_sum %>% 
    dplyr::filter(party!="Republican") %>% 
    ggplot(aes(y=y1,yend=y2,x=id,xend=id))+geom_segment(aes(col=party,size=n))+
    scale_color_manual(values=party_cols)+
    scale_size_continuous(range = c(0.1,1))+
    scale_y_continuous(limits=dimxy[3:4])+
    ggthemes::theme_tufte(ticks = FALSE)+
    theme(legend.position = "none",
          text = element_text(size=text_size,family=font),
          panel.background = element_blank(),
          axis.text = element_blank())+
    labs(x="",y="")
  
  pyR <- box_tbl_sum %>% 
    dplyr::filter(party=="Republican") %>% 
    ggplot(aes(y=y1,yend=y2,x=id,xend=id))+geom_segment(aes(col=party,size=n))+
    scale_color_manual(values=party_cols)+
    scale_size_continuous(range = c(0.1,1))+
    scale_y_continuous(limits=dimxy[3:4])+
    ggthemes::theme_tufte(ticks = FALSE)+
    theme(legend.position = "none",
          text = element_text(size=text_size,family=font),
          panel.background = element_blank(),
          axis.text = element_blank())+
    labs(x="",y="")
  
  
  pyD + p + pyR + plot_spacer() + px + plot_spacer() + 
    plot_layout(heights = c(0.9,0.1),nrow=2,ncol=3,widths = c(0.05,0.9,0.05))
}

plot_fiedler <- function(scores,net_stats,text_size=12,font="LM Roman 10",scale=F){
  scores %>% 
    dplyr::filter(party%in%c("Democrat","Republican","Independent")) %>% 
    group_by(congress,party) %>% 
    dplyr::summarise(centre=mean(fiedler)) %>% 
    mutate(sign=case_when((party=="Democrat" & centre<0) ~ 1,
                          (party=="Republican" & centre>0) ~ 1,
                          (party=="Democrat" & centre>0) ~ -1,
                          (party=="Republican" & centre<0) ~ -1)) %>% 
    select(congress,party,sign) -> congress_sign
  
  #correct independent
  id <- which(is.na(congress_sign$sign))
  congress_sign$sign[id] <- congress_sign$sign[id-1] 

  #scale
  if(scale){
    scores <- scores %>% 
      group_by(congress) %>% 
      mutate(fiedler=ideo_scale(fiedler)) %>% 
      ungroup()
  }
  senates <- paste0(c_seq,c("th","st","nd","rd",rep("th",6))[(81:116%%10)+1]," Session")
  years <- paste0(seq(1949,2019,2),"-",seq(1951,2021,2))
  years <- paste0(years,"\n","(",senates,")")
  years <- factor(years,levels = years)
  scores %>%
    left_join(congress_sign,by = c("congress", "party")) %>% 
    dplyr::filter(party%in%c("Republican","Democrat","Independent")) %>% 
    left_join(net_stats,by="congress") %>% 
    dplyr::filter(congress>80) %>%
    mutate(title=years[congress-80]) %>% 
    ggplot(aes(col=party))+
    geom_hline(yintercept = 0,col="grey")+
    geom_point(aes(x=sign*fiedler,y=0),alpha=0.5,pch=20,size=2)+
    scale_x_continuous(breaks=NULL)+
    scale_y_continuous(breaks=NULL,limits=c(-0.1,0.1))+
    scale_color_manual(values=c("Republican"="#CD3333",
                                "Democrat"="#104E8B",
                                "Independent"="#EEB422"))+
    facet_wrap(~title,ncol = 6)+
    ggthemes::theme_tufte()+
    theme(legend.position = "none",
          text=element_text(size=text_size,family=font),
          panel.background = element_blank())+
    labs(x="",y="")
  
}

#------------------------------------------------------------------------------#
# comparison functions -----
#------------------------------------------------------------------------------#
#wnominate
wnominate1 <- function(f){
  votes <- suppressMessages(read_csv(f))
  Vmat <- votes %>% 
    select(icpsr,rollnumber,cast_code) %>% 
    pivot_wider(names_from = rollnumber,names_prefix = "V",values_from = cast_code)
  
  Vmat <- as.data.frame(Vmat)
  rownames(Vmat) <- Vmat$icpsr
  Vmat <- Vmat[,-1] %>% as.matrix()
  Vmat[is.na(Vmat)] <- 9
  
  Ldat <- votes %>% select(icpsr,state_abbrev,party_name,party_code) %>% distinct() %>% as.data.frame()
  rownames(Ldat) <- Ldat[["icpsr"]]
  Ldat <- Ldat %>% select(-icpsr) %>% rename(state=state_abbrev,party=party_name,party.1=party_code)
  
  df <- list()
  df[["votes"]] <- Vmat
  df[["codes"]][["yea"]] <- 1:3
  df[["codes"]][["nay"]] <- 4:6
  df[["codes"]][["notInLegis"]] <- 0
  df[["codes"]][["missing"]] <- 7:9
  df[["n"]] <- length(unique(votes$icpsr))
  df[["m"]] <- length(unique(votes$rollnumber))
  df[["legis.data"]] <- Ldat
  class(df) <- "rollcall"
  result <- wnominate(df,polarity = c(which(Ldat$party=="Republican")[1],which(Ldat$party=="Democrat")[1]))
  result
}

# variance explained by pca
pca_dim <- function(f){
  votes <- suppressMessages(read_csv(f))
  # votes[["cast_code"]] <- case_when(votes[["cast_code"]] <= 3~1,
  #                                   votes[["cast_code"]] <= 6~-1,
  #                                   TRUE~0)
  # votes %>% 
  #   mutate(frac = yea_count/(yea_count+nay_count)) %>% 
  #   mutate(frac = if_else(frac<=0.5,frac,1-frac)) %>% 
  #   dplyr::filter(frac>=0.025) %>% 
  #   select(-frac) %>% 
  #   select(icpsr,rollnumber,cast_code) %>% 
  #   pivot_wider(names_from = rollnumber,values_from = cast_code,
  #               names_prefix = "vote_",values_fill = list(cast_code=0)) %>% 
  #   select(-icpsr) %>% 
  #   as.matrix() -> A
  # 
  # pca <- prcomp(A)
  # eigs <- pca$sdev^2
  # tibble(congress = parse_number(f),dim1 = eigs[1] / sum(eigs),dim2 = eigs[2]/sum(eigs))
  
  votes[["cast_code"]] <- case_when(votes[["cast_code"]] <= 3~"yea",
                                    votes[["cast_code"]] <= 6~"nay",
                                    votes[["cast_code"]] <= 8~"present",
                                    TRUE~"not voting")
  
  peeps <- votes %>% select(icpsr,party_name) %>% distinct()
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
  pca <- prcomp(P)
  eigs <- pca$sdev^2
  tibble(congress = parse_number(f),dim1 = eigs[1] / sum(eigs),dim2 = eigs[2]/sum(eigs),dim3=eigs[3]/sum(eigs))
}

#apre of wnominate
wnom_dim <- function(f){
  res <- readRDS(f)
  tibble(congress=parse_number(f),dim1=res$fits[["apre1D"]],dim2=res$fits[["apre2D"]])
}

# modularity clustering for polarization
mod_polarise <- function(f){
  g <- readRDS(f)  
  clu <- cluster_louvain(g)
  tibble(congress = parse_number(f), modularity = modularity(clu), 
         adjrand = mclust::adjustedRandIndex(membership(clu),as.integer(factor(V(g)$party))))
}


#------------------------------------------------------------------------------#
# helper -----
#------------------------------------------------------------------------------#

#layouts should put democrats to the left and republicans to the right 
adjust_layout <- function(l,xy){
  if(!any(V(l)$party%in%c("Democrat","Republican"))){
    return(xy)
  } else{
    mxy_tbl <- tibble(x=xy[,1],y=xy[,2],party=V(l)$party) %>% 
      dplyr::filter(party%in%c("Democrat","Republican")) %>% 
      group_by(party) %>% 
      dplyr::summarise(mx=mean(x),my=mean(y))
    
    rot <- -atan2(mxy_tbl$my[mxy_tbl$party=="Democrat"],
                  mxy_tbl$mx[mxy_tbl$party=="Democrat"])/pi*180
    xy <- layout_rotate(xy,rot)
    xy <- layout_mirror(xy,"vertical")
    return(xy)
  }
}

# calculate intervals
get_intervals <- function(l){
  
  #structural equivalent nodes (cumbersome)
  P <- neighborhood_inclusion(l)
  MSE <- which((P + t(P)) == 2, arr.ind = T)
  MSE <- t(apply(MSE, 1, sort))
  MSE <- MSE[!duplicated(MSE), ]
  g <- graph.empty()
  g <- add.vertices(g, nrow(P))
  g <- add.edges(g, c(t(MSE)))
  g <- as.undirected(g)
  MSE <- clusters(g)$membership
  equi <- which(duplicated(MSE))
  l1 <- delete_vertices(l,equi)
  
  perm <- order(fiedler_order(l1,mode = "mcl"))
  M <- clique_vertex_mat(l1)
  findI <- t(M[perm,])
  ints <- do.call("rbind",lapply(apply(findI,1,function(x) which(x==1)),function(x) c(min(x),max(x))))
  ints <- as_tibble(ints)
  names(ints) <- c("X1","X2")
  
  ints <- ints %>%  mutate(X1=X1-0.1,X2=X2+0.1)
  ints <- ints[MSE,]
  ints
}

fiedler_scores <- function(g,normalize=F){
  g <- delete.vertices(g,which(degree(g)==0))
  comps <- components(g)
  if(comps$no>1){
    map_dfr(1:comps$no,function(x){
      g1 <- induced_subgraph(g,which(comps$membership==x))
      if(graph.density(g1)==1){
        sL <- matrix(1,nrow = vcount(g1),ncol = 2)
      } else{
        sL <- abs(eigen(graph.laplacian(g1,normalized = TRUE))$vectors[,(vcount(g1)-1):(vcount(g1)-2)])
        sL[,1] <- normalise(sL[,1],to=c(0.85,1))
        sL[,2] <- normalise(sL[,2],to=c(0.85,1))
      }
      tibble(name=V(g1)$display_name,party=V(g1)$party,state=V(g1)$state,fiedler=sL[,1],y=sL[,2])
    })
  } else{
    sL <- eigen(graph.laplacian(g,normalized = normalize))$vectors[,(vcount(g)-1):(vcount(g)-2)]
    sL[,1] <- normalise(sL[,1],to=c(-1,1))
    sL[,2] <- normalise(sL[,2],to=c(-1,1))
    tibble(name=V(g)$display_name,party=V(g)$party,state=V(g)$state,fiedler=sL[,1],y=sL[,2])  
  }
}

ideo_scale <- function(x){
  a <- min(x)
  b <- max(x)
  alpha <- -2/(a-b)
  beta <- 1 + 2*b/(a-b)
  return(alpha*x+beta)
}

# calculate rectangles
get_2dintervals <- function(l){
  
  #structural equivalent nodes (cumbersome)
  P <- neighborhood_inclusion(l)
  MSE <- which((P + t(P)) == 2, arr.ind = T)
  MSE <- t(apply(MSE, 1, sort))
  MSE <- MSE[!duplicated(MSE), ]
  g <- graph.empty()
  g <- add.vertices(g, nrow(P))
  g <- add.edges(g, c(t(MSE)))
  g <- as.undirected(g)
  MSE <- clusters(g)$membership
  equi <- which(duplicated(MSE))
  l1 <- delete_vertices(l,equi)
  
  #calculate boxes
  # perm <- order(fiedler_order(l1, mode = "mcl"))
  A <- as_adj(l1,sparse=F)
  C <- clique_vertex_mat(l1)
  perm <- sample(1:nrow(C))
  C <- C[perm,]
  res <- boxicity2(A,C)
  if(res$lazarus>0){
    cat("Lazarus count is too high:",res$lazarus)
  }
  xy <- get_boxes(A,C,res$Aperm,res$Cperm)
  err <- sum(abs(A[res$Aperm,res$Aperm]-box2net(xy)+diag(1,nrow(A))))
  if(err>0){
    stop("doesn't produce the same network")
  }
  # un-permute and return
  xy <- xy[order(res$Aperm),]
  xy <- xy[MSE,]
  xy
}

# function to find inclusion maximal Js for box2
jstar_dom <- function(A){
  fct <- function(x, y) all(x <= y) + 0
  vecfct <- Vectorize(fct)
  r.rows <- split(A, row(A))
  D <- outer(r.rows, r.rows, vecfct)
  diag(D) <- 0
  unname(which(rowSums(netrankr::positional_dominance(A,"two-mode"))==0))
}

# extract 2D boxes from A,C and permutations
get_boxes <- function(A,C,Aperm,Cperm){
  A <- A[Aperm,Aperm]
  C <- C[Cperm,Aperm]
  
  J <- Jneighborhood(A,0:(nrow(A)-1))
  n <- nrow(A)
  xy <- matrix(0,n,4)
  for(k in 1:n){
    vkJh <- which(J[,k]==1)
    vkCj <- which(C[,k]==1)
    el <- expand.grid(vkCj,n-vkJh)
    xy[k,1] <- min(el[,1])-0.1
    xy[k,2] <- max(el[,1])+0.1
    xy[k,3] <- min(el[,2])-0.1
    xy[k,4] <- max(el[,2])+0.1
  }
  xy
}

# boxes to network
box2net <- function(xy){
  n <- nrow(xy)
  
  x <- xy[,1:2]
  y <- xy[,3:4]
  B <- matrix(0,n,n)
  for(i in 1:n){
    for(j in 1:n){
      XA1 <- x[i,1]
      XA2 <- x[i,2]
      YA1 <- y[i,1]
      YA2 <- y[i,2]
      XB1 <- x[j,1]
      XB2 <- x[j,2]
      YB1 <- y[j,1]
      YB2 <- y[j,2]
      SI <-  max(0, min(XA2, XB2) - max(XA1, XB1)) * max(0, min(YA2, YB2) - max(YA1, YB1))
      if(SI>0){
        B[i,j] <- 1
      }
    }
  }
  B
}

cppFunction("
  IntegerMatrix getA_cpp(NumericVector x,NumericVector y){
    int n = x.length();
    IntegerMatrix A(n,n);
    for(int i=0;i<n;++i){
      for(int j=0;j<n;++j){
        if( ((x[i] >= x[j]) & (x[i]<=y[j])) | ((x[i] <= x[j]) & (y[i]>=x[j])) ){
          A(i,j) = 1;
        }
      }
    }
    return A;
  }")

# create interval graph from permutation
perm2int <- function(N,perm){
  Nperm <- lapply(N,function(x) perm[x])
  
  xy <- cbind(sapply(Nperm,function(x) c(min(x))),perm)
  a <- xy[,1]
  b <- xy[,2]
  # A <- ((outer(a,a,">=") & outer(a,b,"<=")) | (outer(a,b,"<=") & outer(b,b,">="))) + 0
  # A <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A <- getA_cpp(a,b)
  diag(A) <- 0
  A
}

perm2box <- function(N,perm1,perm2){
  Nperm1 <- lapply(N,function(x) perm1[x])
  Nperm2 <- lapply(N,function(x) perm2[x])
  
  xy1 <- cbind(sapply(Nperm1,function(x) c(min(x))),perm1)
  xy2 <- cbind(sapply(Nperm2,function(x) c(min(x))),perm2)  
  
  a <- xy1[,1]
  b <- xy1[,2]
  # A1 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A1 <- getA_cpp(a,b)
  
  a <- xy2[,1]
  b <- xy2[,2]
  # A2 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A2 <- getA_cpp(a,b)
  
  diag(A1) <- diag(A2) <- 0
  # A1 <- (A1==1 | t(A1)==1)+0
  # A2 <- (A2==1 | t(A2)==1)+0
  
  (A1==1 & A1==A2)+0
}

perm2cube <- function(N,perm1,perm2,perm3){
  Nperm1 <- lapply(N,function(x) perm1[x])
  Nperm2 <- lapply(N,function(x) perm2[x])
  Nperm3 <- lapply(N,function(x) perm3[x])
  
  xy1 <- cbind(sapply(Nperm1,function(x) c(min(x))),perm1)
  xy2 <- cbind(sapply(Nperm2,function(x) c(min(x))),perm2)  
  xy3 <- cbind(sapply(Nperm3,function(x) c(min(x))),perm3)  
  
  a <- xy1[,1]
  b <- xy1[,2]
  # A1 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A1 <- getA_cpp(a,b)
  
  a <- xy2[,1]
  b <- xy2[,2]
  # A2 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A2 <- getA_cpp(a,b)
  
  a <- xy3[,1]
  b <- xy3[,2]
  # A3 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A3 <- getA_cpp(a,b)
  
  diag(A1) <- diag(A2) <- diag(A3) <- 0
  # A1 <- (A1==1 | t(A1)==1)+0
  # A2 <- (A2==1 | t(A2)==1)+0
  # A3 <- (A3==1 | t(A3)==1)+0
  
  (A1==1 & A1==A2 & A2==A3)+0
}

perm2hyper <- function(N,perm1,perm2,perm3,perm4){
  Nperm1 <- lapply(N,function(x) perm1[x])
  Nperm2 <- lapply(N,function(x) perm2[x])
  Nperm3 <- lapply(N,function(x) perm3[x])
  Nperm4 <- lapply(N,function(x) perm4[x])
  
  xy1 <- cbind(sapply(Nperm1,function(x) c(min(x))),perm1)
  xy2 <- cbind(sapply(Nperm2,function(x) c(min(x))),perm2)  
  xy3 <- cbind(sapply(Nperm3,function(x) c(min(x))),perm3)  
  xy4 <- cbind(sapply(Nperm4,function(x) c(min(x))),perm4)  
  
  a <- xy1[,1]
  b <- xy1[,2]
  # A1 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A1 <- getA_cpp(a,b)
  
  a <- xy2[,1]
  b <- xy2[,2]
  # A2 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A2 <- getA_cpp(a,b)
  
  a <- xy3[,1]
  b <- xy3[,2]
  # A3 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A3 <- getA_cpp(a,b)
  
  a <- xy4[,1]
  b <- xy4[,2]
  # A4 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A4 <- getA_cpp(a,b)
  
  diag(A1) <- diag(A2) <- diag(A3) <- diag(A4) <- 0
  # A1 <- (A1==1 | t(A1)==1)+0
  # A2 <- (A2==1 | t(A2)==1)+0
  # A3 <- (A3==1 | t(A3)==1)+0
  # A4 <- (A4==1 | t(A4)==1)+0
  
  (A1==1 & A1==A2 & A2==A3 & A3==A4)+0
}

perm2pente <- function(N,perm1,perm2,perm3,perm4,perm5){
  Nperm1 <- lapply(N,function(x) perm1[x])
  Nperm2 <- lapply(N,function(x) perm2[x])
  Nperm3 <- lapply(N,function(x) perm3[x])
  Nperm4 <- lapply(N,function(x) perm4[x])
  Nperm5 <- lapply(N,function(x) perm5[x])
  
  xy1 <- cbind(sapply(Nperm1,function(x) c(min(x))),perm1)
  xy2 <- cbind(sapply(Nperm2,function(x) c(min(x))),perm2)  
  xy3 <- cbind(sapply(Nperm3,function(x) c(min(x))),perm3)  
  xy4 <- cbind(sapply(Nperm4,function(x) c(min(x))),perm4)  
  xy5 <- cbind(sapply(Nperm5,function(x) c(min(x))),perm5)  
  
  a <- xy1[,1]
  b <- xy1[,2]
  # A1 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A1 <- getA_cpp(a,b)
  
  a <- xy2[,1]
  b <- xy2[,2]
  # A2 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A2 <- getA_cpp(a,b)
  
  a <- xy3[,1]
  b <- xy3[,2]
  # A3 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A3 <- getA_cpp(a,b)
  
  a <- xy4[,1]
  b <- xy4[,2]
  # A4 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A4 <- getA_cpp(a,b)

  a <- xy5[,1]
  b <- xy5[,2]
  # A5 <- ((outer(a,a,">=") & outer(a,b,"<=")) | outer(a,a,"<=") & outer(b,a,">=") )+0
  A5 <- getA_cpp(a,b)
  
    
  diag(A1) <- diag(A2) <- diag(A3) <- diag(A4) <- diag(A5) <- 0

  
  (A1==1 & A1==A2 & A2==A3 & A3==A4 & A4==A5)+0
}


# get the interval representation of a permutation
perm2rep <- function(N,perm){
  Nperm <- lapply(N,function(x) perm[x])
  xy <- cbind(sapply(Nperm,function(x) c(min(x))),perm)
  colnames(xy) <- NULL
  xy
}

#generate new permutation for lazarus count problem
genperm_lz <- function(sq,A) { 
  changepoints <- sample(sq,2)
  tmp <- sq[changepoints[1]]
  sq[changepoints[1]] <- sq[changepoints[2]]
  sq[changepoints[2]] <- tmp
  sq
}


#generate new permutation for super interval problem
genperm_sb1 <- function(sq,A,adj) { 
  changepoints <- sample(sq,2)
  tmp <- sq[changepoints[1]]
  sq[changepoints[1]] <- sq[changepoints[2]]
  sq[changepoints[2]] <- tmp
  sq
}

#generate new permutation for super box2 problem
genperm_sb2 <- function(sq,A,adj) {
  n <- length(sq)/2
  perm1 <- sq[1:n]
  perm2 <- sq[(n+1):length(sq)]
    
  changepoints <- sample(perm1,2)
  tmp <- perm1[changepoints[1]]
  perm1[changepoints[1]] <- perm1[changepoints[2]]
  perm1[changepoints[2]] <- tmp
  
  changepoints <- sample(perm2,2)
  tmp <- perm2[changepoints[1]]
  perm2[changepoints[1]] <- perm2[changepoints[2]]
  perm2[changepoints[2]] <- tmp
  sq <- c(perm1,perm2)
  sq
}

#generate new permutation for super box3 problem
genperm_sb3 <- function(sq,A,adj) { 
  n <- length(sq)/3
  perm1 <- sq[1:n]
  perm2 <- sq[(n+1):(2*n)]
  perm3 <- sq[(2*n+1):length(sq)]
  
  changepoints <- sample(perm1,2)
  tmp <- perm1[changepoints[1]]
  perm1[changepoints[1]] <- perm1[changepoints[2]]
  perm1[changepoints[2]] <- tmp
  
  changepoints <- sample(perm2,2)
  tmp <- perm2[changepoints[1]]
  perm2[changepoints[1]] <- perm2[changepoints[2]]
  perm2[changepoints[2]] <- tmp
  
  changepoints <- sample(perm3,2)
  tmp <- perm3[changepoints[1]]
  perm3[changepoints[1]] <- perm3[changepoints[2]]
  perm3[changepoints[2]] <- tmp
  
  sq <- c(perm1,perm2,perm3)
  sq
}

#generate new permutation for super box4 problem
genperm_sb4 <- function(sq,A,adj) { 
  n <- length(sq)/4
  perm1 <- sq[1:n]
  perm2 <- sq[(n+1):(2*n)]
  perm3 <- sq[(2*n+1):(3*n)]
  perm4 <- sq[(3*n+1):(4*n)]
  
  changepoints <- sample(perm1,2)
  tmp <- perm1[changepoints[1]]
  perm1[changepoints[1]] <- perm1[changepoints[2]]
  perm1[changepoints[2]] <- tmp
  
  changepoints <- sample(perm2,2)
  tmp <- perm2[changepoints[1]]
  perm2[changepoints[1]] <- perm2[changepoints[2]]
  perm2[changepoints[2]] <- tmp
  
  changepoints <- sample(perm3,2)
  tmp <- perm3[changepoints[1]]
  perm3[changepoints[1]] <- perm3[changepoints[2]]
  perm3[changepoints[2]] <- tmp
  
  changepoints <- sample(perm4,2)
  tmp <- perm4[changepoints[1]]
  perm4[changepoints[1]] <- perm4[changepoints[2]]
  perm4[changepoints[2]] <- tmp
  
  sq <- c(perm1,perm2,perm3,perm4)
  sq
}

#generate new permutation for super box5 problem
genperm_sb5 <- function(sq,A,adj) { 
  n <- length(sq)/5
  perm1 <- sq[1:n]
  perm2 <- sq[(n+1):(2*n)]
  perm3 <- sq[(2*n+1):(3*n)]
  perm4 <- sq[(3*n+1):(4*n)]
  perm5 <- sq[(4*n+1):(5*n)]
  
  changepoints <- sample(perm1,2)
  tmp <- perm1[changepoints[1]]
  perm1[changepoints[1]] <- perm1[changepoints[2]]
  perm1[changepoints[2]] <- tmp
  
  changepoints <- sample(perm2,2)
  tmp <- perm2[changepoints[1]]
  perm2[changepoints[1]] <- perm2[changepoints[2]]
  perm2[changepoints[2]] <- tmp
  
  changepoints <- sample(perm3,2)
  tmp <- perm3[changepoints[1]]
  perm3[changepoints[1]] <- perm3[changepoints[2]]
  perm3[changepoints[2]] <- tmp
  
  changepoints <- sample(perm4,2)
  tmp <- perm4[changepoints[1]]
  perm4[changepoints[1]] <- perm4[changepoints[2]]
  perm4[changepoints[2]] <- tmp
  
  changepoints <- sample(perm5,2)
  tmp <- perm5[changepoints[1]]
  perm5[changepoints[1]] <- perm5[changepoints[2]]
  perm5[changepoints[2]] <- tmp
  
  sq <- c(perm1,perm2,perm3,perm4,perm5)
  sq
}


#graph edit distance
ged1 <- function(sq,A,adj){
  Anew <- perm2int(adj,sq)
  sum(Anew-A)/2
}

ged2 <- function(sq,A,adj){
  n <- length(sq)/2
  perm1 <- sq[1:n]
  perm2 <- sq[(n+1):(2*n)]
  Anew <- perm2box(adj,perm1,perm2)
  sum(Anew-A)/2
}

ged3 <- function(sq,A,adj){
  n <- length(sq)/3
  perm1 <- sq[1:n]
  perm2 <- sq[(n+1):(2*n)]
  perm3 <- sq[(2*n+1):length(sq)]
  Anew <- perm2cube(adj,perm1,perm2,perm3)
  sum(Anew-A)/2
}

ged4 <- function(sq,A,adj){
  n <- length(sq)/4
  perm1 <- sq[1:n]
  perm2 <- sq[(n+1):(2*n)]
  perm3 <- sq[(2*n+1):(3*n)]
  perm4 <- sq[(3*n+1):(4*n)]
  Anew <- perm2hyper(adj,perm1,perm2,perm3,perm4)
  sum(Anew-A)/2
}

ged5 <- function(sq,A,adj){
  n <- length(sq)/5
  perm1 <- sq[1:n]
  perm2 <- sq[(n+1):(2*n)]
  perm3 <- sq[(2*n+1):(3*n)]
  perm4 <- sq[(3*n+1):(4*n)]
  perm5 <- sq[(4*n+1):(5*n)]
  Anew <- perm2pente(adj,perm1,perm2,perm3,perm4,perm5)
  sum(Anew-A)/2
}

lazarus_cliques <- function(A,perm){
  lazarus_count_cpp2D(A,perm-1)
}


normalise <- function (x, from = range(x), to = c(0, 1)) 
{
  x <- (x - from[1])/(from[2] - from[1])
  if (!identical(to, c(0, 1))) {
    x <- x * (to[2] - to[1]) + to[1]
  }
  x
}

fscale <- function(a,b,x){
  alpha <- 2/(b-a)
  beta <- -1-2*a/(b-a)
  alpha*x+beta
}

