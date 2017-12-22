
BorderLineSMOTE1 <-function (data, target, K = 5, C = 5, dupSize = 0)
  {
    obeservationsNumber = ncol(data) # le nombre d'observations
    targetTable = table(target) # la variable à prédire
    
    minorityName = names(which.min(targetTable))
    minorityData = subset(data, target == minorityName)[sample(min(targetTable)), ] # classe minoritaire
    minorityRowsNumber = nrow(minorityData)
    
    majorityData = subset(data, target != minorityName) #classe majoritaire
    majorityRowsNumber = nrow(majorityData)
    
    allData = rbind(minorityData, majorityData)
    
    knear_D = knearest(allData, minorityData, C)
    knct = kncount(knear_D, minorityRowsNumber)
    
    # dinstinger la zone de danger et la zone safe
    zonesData = knct[, 1] / C
    dangerIndexes = which(zonesData <= 0.5 & zonesData > 0)
    safeIndexes = which(zonesData > 0.5)
    
    dangerMinorityData = minorityData[dangerIndexes,]
    dangerMinorityDataRowsNumber = nrow(dangerMinorityData)
    safeMinorityData = minorityData[safeIndexes,]
    # on prend pas les données dans la zone = 0
    
    if (K >= dangerMinorityDataRowsNumber) {
      stop("Prenez une valeur iférieure pour la paramètre K")
    }
    
    dupSum = n_dup_max(
      minorityRowsNumber + majorityRowsNumber,
      dangerMinorityDataRowsNumber,
      majorityRowsNumber,
      dupSize
    )
    syntheticData = NULL
    
    knear_P = knearest(dangerMinorityData, dangerMinorityData, K)
    for (i in 1:dangerMinorityDataRowsNumber) {
      pair_idx = knear_P[i, ceiling(runif(dupSum) * K)]
      g = mapply(gap,
                 sl_p = rep(1, dupSum),
                 sl_n = rep(1, dupSum))
      P_i = matrix(unlist(dangerMinorityData[i,]),
                   dupSum,
                   obeservationsNumber,
                   byrow = TRUE)
      Q_i = as.matrix(dangerMinorityData[pair_idx,])
      syn_i = P_i + g * (Q_i - P_i)
      syntheticData = rbind(syntheticData, syn_i)
    }
    
    minorityData[, obeservationsNumber + 1] = rep(minorityName, minorityRowsNumber)
    colnames(minorityData) = c(colnames(data), "class")
    
    majorityData[, obeservationsNumber + 1] = target[target != minorityName]
    colnames(majorityData) = c(colnames(data), "class")
    
    rownames(syntheticData) = NULL
    syntheticData = data.frame(syntheticData)
    syntheticData[, obeservationsNumber + 1] = rep(minorityName, nrow(syntheticData))
    colnames(syntheticData) = c(colnames(data), "class")
    
    # Les données finaux (minority + synthetic + majority)
    newData = rbind(minorityData, syntheticData, majorityData)
    rownames(newData) = NULL
    
    return(newData)
  }

