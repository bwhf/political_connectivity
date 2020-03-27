## function wrapping algorithm for custom layout - origin to non-origin country network plot ##

lay_breed2visit <- function(routes_igraph, which_lay = c("EEZ", "RFMO"), plotit=FALSE){
  
  library(igraph)
  
  ### original script ###
  ## trying a custom layout (high seas alone, origins in middle, others below) ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  HS <- which(V(routes_igraph)$name == "High seas")
  halfV <- max(vcount(routes_igraph))/2
  vn <- vcount(routes_igraph)              # total N
  
  ## setting coords. for origins and others
  l <- cbind(1:vn, 1:vn)
  
  # on <- sum(V(routes_igraph)$is_origin==T) # N origins
  on <- sum(V(routes_igraph)$breed_node==T) # bn = breed node
  
  if(which_lay == "EEZ"){
    nn <- sum(V(routes_igraph)$breed_node==F) - 1 # N non-origins (-1 for high seas) # vn = visiting node
  } else if(which_lay == "RFMO"){
    nn <- sum(V(routes_igraph)$breed_node==F) 
  }

  # vnro <- plyr::round_any(vcount(routes_igraph), on, ceiling) # total nodes, rounded up to nearest multiple of N origins
  
  # origin positions
  # ospots <- seq(1, vnro, vnro/on) # evenly spaced spots for origins #** original way **
  # ol <- (1:vn)[sapply(ospots, function(x) which.min(abs(x-1:vn)))] # desired x axis positions of origins (fancy stuff to make sure spots exist)
  ol <- seq(1, vn, vn/on) # evenly spaced spots for origins # *** trying to simplify ***
  oi <- which(V(routes_igraph)$breed_node==T) # indexes for origin countries
  
  l[oi, ] <- cbind(ol, rep(halfV, on))
  
  # non-origin positions
  # nl <- seq(1, vn, vn/nn) #OLD
  nl <- seq((vn/nn)/2, vn, vn/nn)  # evenly spaced spots for non-origins

  if(which_lay == "EEZ"){
    ni <- which(V(routes_igraph)$breed_node==F)[-1] # indexes for non-origin countries [-1] omits high seas
    
    l[ni, ] <- cbind(nl, rep(1, nn))
    
    l[which(V(routes_igraph)$label == "High seas"), ] <- c(halfV, halfV*2) # add high seas back
    
  } else if(which_lay == "RFMO"){
    ni <- which(V(routes_igraph)$breed_node==F) # indexes for RFMOs
    
    l[ni, ] <- cbind(nl, rep(1, nn))
    
  }

  if(plotit==T){
    print(plot(routes_igraph, edge.arrow.size = 0.2, edge.width = E(routes_igraph)$weight * 50, layout = l))
  }
  return(l)
}
