get_stereotype <- function(ds, dist = dist) {
    # print(str(ev[[1]]))
    # stop()
    if (!any(ds$event_type)){
        ster <- amatch(ds$EVTYPE, dict, maxDist = dist)
        if (!any(is.na(ster))){
            # browser()
            ds$EVTYPE <- dict[ster]
            ds$event_type <- T
        }
    }
    return(ds)
}

find_matches <- function(dataset, dist, max_dist = 4){
    
    library(stringdist)
    x <- by(dataset,
            dataset$EVTYPE, get_stereotype, dist = dist)

    browser()
    return(x)    
    # for (i in 1:length(storm_data$EVTYPE)){
    #   if (!storm_data[i, 'event_type']) {
    #     type_mach <- get_stereotype(storm_data[i, 'EVTYPE'], dist)
    #     if (!is.na(type_mach)){
    #       storm_data[i, 'EVTYPE'] <- type_mach
    #       storm_data[i, 'event_type'] <- TRUE
    #     }
    #   }
    # }
    # if (dist < max_dist) {
    #   find_matches(storm_data, dist = dist + 1)
    # }
}

find_matches2 <- function(dataset, max_dist = 4){
    library(stringdist)
    dataset$event_type <- rep(NA, length(dataset$EVTYPE))
    ev_lvls <- levels(dataset$EVTYPE)
    for (i in 1:length(ev_lvls)){
        new_flag <- T
        dist = 1
        while (new_flag & (dist <= max_dist)){
            if (length(grep("[()]", ev_lvls[i])) > 0){
                break
            }
            new_lvl <- get_stereotype2(ev_lvls[i], dist)
            if (!is.na(new_lvl)){
                dataset[dataset$EVTYPE == ev_lvls[i],]['event_type'] <- new_lvl
                new_flag <- F
            } else {
                dist <- dist + 1
            }
        }
        if (new_flag) {
            dataset[dataset$EVTYPE == ev_lvls[i],]['event_type'] <- "unclassified"
        }
    }
    return(dataset)
}

get_stereotype2 <- function(old_lvl, dist){
    a <- grep(old_lvl, dict)
    if (length(a) > 0){
        return(dict[a[1]])
        break
    }
    ster <- amatch(old_lvl, dict, maxDist = dist)
    if (!is.na(ster)){
        return(dict[ster])
    } else {
        return(NA)
    }
}
