
## IPF functions written by Josh Goldstein.

## These functions are designed to be run on a single ACS sample.

## I made an attempt to iteratively run IPF and had to make some finicky changes to these functions, and ultimately could
## not get accurate results. I may give it another go some other time.

library(tigris)
library(dplyr)
library(sp)
library(reshape2)
library(mipfp)

# run iterative proportional fitting using specified marginal constaints
# return a data frame of person counts for the joint distirbution of each block group
run_ipf <- function(acs_margins,
                    inputs){
  ipf_counts <- list() # for each blockgrup, store an array of counts from IPF
  for(i in 1:nrow(acs_margins)){
    # create a list of marginal constraints
    margin_constraints <- list()
    ind_margins <- list()
    for(j in 1:length(inputs)){
      margin_constraints[[j]] <- as.numeric( acs_margins[i,inputs[[j]]$acs_names])
      ind_margins[[j]] <- j
    }
    dim <- sapply(inputs,function(x){length(x$acs_names)})
    seed <- array(1,dim)
    # call the Ipfp function
    ipf_out <- Ipfp(seed=seed, target.list=ind_margins, target.data=margin_constraints)
    ipf_counts[[i]] <- round(ipf_out$x.hat,0) # save the array of counts
  }
  names(ipf_counts) <- acs_margins$GEOID
  return(ipf_counts)
}

# -------------------------------------------------------------------

# resample microdata according to IPF counts
# return a synthetic population data frame
resample_ipf <- function(ipf_counts,
                         inputs,
                         microdata,
                         microdata_category,
                         micro_cols=c("PUMA","PWGTP")){
  input_names <- sapply(inputs,function(x){x$micro_name})
  microdata <- microdata[,c(micro_cols, input_names)]
  ipf_index <- list() # index of microdata in each block group
  for(ind in 1:length(ipf_counts)){ # loop over block groups
    ipf_index[[ind]] <- numeric(0)
    counts <- melt(ipf_counts[[ind]]) # melt counts to loop over each category
    ndim <- ncol(counts)-1
    for(i in 1:nrow(counts)){
      nsamp <- counts$value[i]
      samp_ind <- which(microdata_category[,1]==counts[i,1])
      if(ndim > 1){
        for(d in 2:ndim){
          samp_ind <- intersect(samp_ind,which(microdata_category[,d]==counts[i,d]))
        }
      }
      samp_prob <- microdata$PWGTP[samp_ind]
      ipf_index[[ind]] <- c(ipf_index[[ind]],
                            sample(x=samp_ind, size=nsamp, replace=TRUE, prob=samp_prob))
    }
  }
  names(ipf_index) <- names(ipf_counts)
  # convert ipf_index to a dataframe, and attach microdata variables to it
  ipf_df <- stack(ipf_index)
  names(ipf_df) <- c("ind","GEOID")
  synthetic_pop <- cbind(GEOID=ipf_df$GEOID, microdata[ipf_df$ind,])
  synthetic_pop
}

# -------------------------------------------------------------------

# attach a latitude and longitude to each person in the synthetic population
# for this we need the blockgroup shapefile; call from tigris by default using specified geometries
#   a) attach lats+longs uniformly over the blockgroup (default)
#   b) using housing locations (sample uniformly from a dataframe of lat/longs)
#   c) draw using weights according to housing properties (e.g. CoreLogic)
attach_latlong <- function(synth_pop,
                           method="uniform",
                           state_names,
                           county_names,
                           year){
  if(method=="uniform"){
    bgs_shape <- block_groups(state = state_names, county = county_names, year = year)
    # number of people in each block group
    bgdat <- synth_pop %>% group_by(GEOID) %>%
      dplyr::summarize(n=n()) %>% group_by()
    # for each block group, uniformly draw latitudes and longitudes over the region
    latlong <- list()
    for(i in 1:nrow(bgdat)){
      ids <- which( bgs_shape@data$GEOID == bgdat$GEOID[i] )
      bg_sub <- bgs_shape[ids,]
      samp <- spsample(x=bg_sub,n=bgdat$n[i],type="random")
      latlong[[i]] <- samp@coords
    }
    latlong <- as.data.frame( do.call(rbind,latlong) )
    names(latlong) <- c("long","lat")
    
    synth_pop$long <- latlong$long
    synth_pop$lat <- latlong$lat
    return(synth_pop)
  } else if(method=="locations"){
    # not yet implemented; draw over specified lat/longs by block group
  } else if(method=="housing"){
    # not yet implemented; draw over houses weighted by tax assessment data
  }
}

# and attach a category from each input variable in the microdata
# by keeping SERIALNO and SPORDER, we can add any ACS variables we want back from pums_orig
create_micro_categories <- function(microdata,
                                    micro_cols=c("PUMA","PWGTP"),
                                    inputs){
  input_names <- sapply(inputs,function(x){x$micro_name})
  microdata <- microdata[,c(micro_cols, input_names), drop=FALSE]
  microdata_category <- as.data.frame( matrix(ncol=length(inputs), nrow=nrow(microdata)) )
  names(microdata_category) <- input_names
  # loop over each input; create category for continuous or categorical inputs
  for(i in 1:length(inputs)){
    if(inputs[[i]]$micro_type == "continuous"){
      microdata_category[,i] <- as.numeric( cut(microdata[,eval(input_names[i])], breaks=inputs[[i]]$micro_breaks))
    } else if(inputs[[i]]$micro_type == "categorical"){
      for(j in 1:length(inputs[[i]]$micro_ids)){
        microdata_category[,i][(microdata[,eval(input_names[i])] %in% inputs[[i]]$micro_ids[[j]])] <- j
      }
    }
  }
  microdata_category
}

