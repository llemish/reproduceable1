get_stereotype <- function(ev, dist = dist) {
    if (!ev[2]){
        ster <- amatch(ev, dict, maxDist = dist)
        if (!any(is.na(ster))){
            ev[1] <- dict[ster]
            ev[2] <- T
        }
    }
    return(ev)
}

find_matches <- function(dataset, dist, max_dist = 4){
    
    list(storm_data$EVTYPE, storm_data$event_type) <- by(list(storm_data$EVTYPE, storm_data$event_type),
                                                         storm_data$EVTYPE, get_stereotype, dist = 1)
    
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
