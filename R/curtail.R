Curtail <- function(dataset.test, Xstar, highest = NULL, lowest = NULL, 
                    plot = TRUE) {

  ## Define dataset etc.:
  nitems <- dim(dataset.test)[2]
  nobs <- dim(dataset.test)[1]
  if (is.null(highest)) highest <- max(dataset.test, na.rm = TRUE)
  if (is.null(lowest)) lowest <- min(dataset.test, na.rm = TRUE)
  
  ## Compute boundaries:
  risk.boundaries <- norisk.boundaries <- rep(NA, times = nitems)
  for (j in 1:nitems) {
    risk.boundaries[j] <- Xstar - (nitems-j) * lowest
    norisk.boundaries[j] <- (Xstar-1) - (nitems-j) * highest
  }

  ## Calculate cumulative testscores:
  dataset.test[paste0("test", 1)] <- dataset.test[ , 1] # testscore after first item equals itemscore of first item
  for (j in 2:nitems) {
    dataset.test[ , paste0("test", j)] <- rowSums(dataset.test[ , 1:j]) 
  }

  ## Perform the curtailment:
  dataset.test$currit <- dataset.test$currts <- NA
  dataset.test$Crisk <- dataset.test$Cnorisk <- NA
  for (i in 1:nobs) {
    for (j in 1:nitems) {
      if (is.na(dataset.test$Cnorisk[i]) && is.na(dataset.test$Crisk[i])) {
        if (dataset.test[i, paste0("test", j)] >= risk.boundaries[j]) {
          dataset.test$currit[i] <- j
          dataset.test$currts[i] <- dataset.test[i, paste0("test", j)]
          dataset.test$Crisk[i] <- TRUE
        } else if (dataset.test[i, paste0("test", j)] <= norisk.boundaries[j]) {
          dataset.test$currit[i] <- j
          dataset.test$currts[i] <- dataset.test[i, paste0("test", j)] 
          dataset.test$Cnorisk[i] <- TRUE
        }
      }
    }
  }
  dataset.test$Crisk[is.na(dataset.test$Crisk)] <- FALSE
  dataset.test$Cnorisk[is.na(dataset.test$Cnorisk)] <- FALSE

  ## Return results:
  accuracy = list(
    risk = table(dataset.test$Crisk, dataset.test[ , paste0("test", nitems)] >= Xstar, 
                 useNA = "ifany", 
                 dnn = c("curtailed: flagged 'risk'","full length: 'at risk'")),
    no.risk = table(dataset.test$Crisk, dataset.test[ , paste0("test", nitems)] < Xstar, 
                    useNA = "ifany", 
                    dnn = c("curtailed: flagged 'no risk'","full length: 'not at risk'")))
  
  out <- list(
    item.scores = dataset.test[ , 1:nitems],
    cumulative.scores = dataset.test[ , (nitems+1):(nitems+nitems)],
    current.item = dataset.test$currit,
    current.score = dataset.test$currts,
    curtailed.test.length.distribution = list(
      mean = mean(dataset.test$currit),
      standard.deviation = sd(dataset.test$currit),
      median = median(dataset.test$currit),
      proportion.curtailed = sum(dataset.test[,(nitems*2)+1] < nitems) / nobs),
    accuracy = accuracy
  )

  print(accuracy)
  if (plot) {
    hist(dataset.test$currit, main = "Test lengths", 
         xlab = "Number of items administered")
  }
  invisible(out)
}


stochCurtail <- function(dataset.train, dataset.test = NULL, Xstar, 
                         gamma0 = .95, gamma1 = .95, plot = TRUE) {

  ## Define dataset etc:
  if (is.null(dataset.test)) dataset.test <- dataset.train
  nitems <- dim(dataset.test)[2]
  nobs <- dim(dataset.test)[1]

  ## Calculate restscores in training data:
  dataset.train[, paste0("rest", 1:(nitems-1))] <- NA
  for (i in 2:(nitems-1)) {
    dataset.train[ , (nitems-1) + i] <- rowSums(dataset.train[ , i:nitems]) 
  }
  dataset.train[, paste0("rest", nitems - 1)] <- dataset.train[ , nitems] # restscore after second last item equals itemscore of last item
  
  ## Calculate cumulative testscores in test data:
  dataset.test[paste0("test", 1)] <- dataset.test[ , 1] # testscore after first item equals itemscore of first item
  for (i in 2:nitems) {
    dataset.test[ , paste0("test", i)] <- rowSums(dataset.test[ , 1:i]) 
  }
  
  # Calculate total score for every participant in training data:
  dataset.train[ , paste0("test", nitems)] <- rowSums(dataset.train[ , 1:nitems])

  # split dataset into plus (above cutoff) and min (below cutoff dataset)
  T_plus <- dataset.train[dataset.train[paste0("test", nitems)] >= Xstar, ]
  T_min <- dataset.train[dataset.train[paste0("test", nitems)] < Xstar, ]

  ## Calculate Pkplus and Pkmin (prop. of restscores in T_plus resp T_min 
  ## yielding a total score >= Xstar when added to person j's testscore on 
  ## item i):
  for (i in 1:(nitems-1)) {
    for (j in 1:(nobs)) {
      dataset.test[j, paste0("Pkplus", i)] <-
        mean(T_plus[ , paste0("rest", i)] + dataset.test[j, paste0("test", i)] >= Xstar)
      dataset.test[j, paste0("Pkmin", i)] <-
        mean(T_min[ , paste0("rest", i)] + dataset.test[j, paste0("test", i)] >= Xstar)
    }
  }

  ## Check for every observations whether pkplus and pkmin are >= gamma1 and <= 1-gamma0:
  for (i in 1:nobs) {
    #dataset.test$plusflag1[i] <- which(dataset.test[i, paste0("Pkplus", 1:(nitems-1))] >= gamma1)[1]
    ## plusflag1 corresponds to the first item where the proportion of at risk decisions in the at risk dataset
    ## equals or exceeds gamma1 
    #dataset.test$plusflag0[i] <- which(dataset.test[i, paste0("Pkplus", 1:(nitems-1))] <= 1-gamma0)[1]
    ## plusflag0 corresponds to the first item where the proportion of at risk decisions in the at risk dataset
    ## equals or is lower than 1 minus gamma0
    #dataset.test$minflag1[i] <- which(dataset.test[i, paste0("Pkmin", 1:(nitems-1))] >= gamma1)[1]
    ## minflag1 corresponds to the first item where the proportion of at risk decisions in the non-at-risk dataset
    ## equals or exceeds gamma1
    #dataset.test$minflag0[i] <- which(dataset.test[i, paste0("Pkmin", 1:(nitems-1))] <= 1-gamma0)[1]
    ## minflag0 corresponds to the first item where the proportion of at risk decisions in the non-at-risk dataset
    ## equals or is lower than 1 minus gamma0
    dataset.test$riskflag[i] <- which(dataset.test[i, paste0("Pkplus", 1:(nitems-1))] >= gamma1 &
                                         dataset.test[i, paste0("Pkmin", 1:(nitems-1))] >= gamma1)[1]
    dataset.test$noriskflag0[i] <- which(dataset.test[i, paste0("Pkplus", 1:(nitems-1))] <= 1-gamma0 &
                                           dataset.test[i, paste0("Pkmin", 1:(nitems-1))] <= 1-gamma0)[1]
  }
  
  #dataset.test$SCrisk <- !is.na(dataset.test$minflag1) & !is.na(dataset.test$plusflag1)
  #dataset.test$SCnorisk <- !is.na(dataset.test$plusflag0) & !is.na(dataset.test$minflag0)
  #dataset.test$SCrisk[dataset.test[paste0("test", nitems)] >= Xstar & 
  #                      is.na(dataset.test$minflag1) & 
  #                      is.na(dataset.test$plusflag0)] <- TRUE # in case no curtailment took place
  #dataset.test$SCnorisk[dataset.test[paste0("test", nitems)] < Xstar & 
  #                        is.na(dataset.test$plusflag0) & 
  #                        is.na(dataset.test$minflag1)] <- TRUE # in case no curtailment took place
  dataset.test$currit <- dataset.test$currts <- dataset.test$SCrisk <- dataset.test$SCnorisk <- NA
  for(j in 1:nobs) {
    if (is.na(dataset.test$riskflag[j]) && is.na(dataset.test$noriskflag[j])) {
      ## Then no curtailment was performed. Get decision based on full test score and set currit <- nitems
      dataset.test$SCrisk[j] <- dataset.test[j, paste0("test", nitems)] >= Xstar
      dataset.test$SCnorisk[j] <- dataset.test[j, paste0("test", nitems)] < Xstar
      dataset.test$currit[j] <- nitems
      dataset.test$currts[j] <- dataset.test[j, paste0("test", nitems)]
    } else if (is.na(dataset.test$riskflag[j])) {
      ## Then take noriskflag
      dataset.test$SCrisk[j] <- FALSE
      dataset.test$SCnorisk[j] <- TRUE
      dataset.test$currit[j] <- dataset.test$noriskflag[j]
      dataset.test$currts[j] <- dataset.test[j, paste0("item", dataset.test$noriskflag[j])]
    } else if (is.na(dataset.test$noriskflag[j])) {
      ## Then take riskflag
      dataset.test$SCrisk[j] <- TRUE
      dataset.test$SCnorisk[j] <- FALSE
      dataset.test$currit[j] <- dataset.test$riskflag[j]
      dataset.test$currts[j] <- dataset.test[j, paste0("item", dataset.test$riskflag[j])]
    } else {
      ## Them both riskflag and noriskflag. Take whichever value is lowest
      risk <- dataset.test$risk[j] <= dataset.test$norisk[j] 
      dataset.test$SCrisk[j] <- risk
      dataset.test$SCnorisk[j] <- !risk
      dataset.test$currit[j] <- ifelse(risk, dataset.test$riskflag[j], dataset.test$riskflag[j])
      dataset.test$currts[j] <- ifelse(risk,
                                       dataset.test[j, paste0("item", dataset.test$riskflag[j])],
                                       dataset.test[j, paste0("item", dataset.test$noriskflag[j])])
    }
  }
    
  #dataset.test$currit <- NA  # tracking the current item number
  #dataset.test$currit[!is.na(dataset.test$minflag1)] <- dataset.test$minflag1[!is.na(dataset.test$minflag1)]
  #dataset.test$currit[!is.na(dataset.test$plusflag0)] <- dataset.test$plusflag0[!is.na(dataset.test$plusflag0)]
  #dataset.test$currit[is.na(dataset.test$currit)] <- nitems # in case no curtailment took place

  #dataset.test$currts <- NA  # for tracking the current test score
  #for (i in 1:nobs) {
  #  dataset.test[i, "currts"] <- dataset.test[i, paste0("test", dataset.test[i, "currit"])]
  #}

  accuracy <- list(
    risk = table(dataset.test[,"SCrisk"], dataset.test[,(nitems*2)] >= Xstar, 
                 useNA="ifany", 
                 dnn = c("curtailed: flagged 'risk'","full length: 'at risk'")),
    no.risk = table(dataset.test[,"SCnorisk"], dataset.test[,(nitems*2)] < Xstar, 
                    useNA="ifany", 
                    dnn = c("curtailed: flagged 'no risk'","full length: 'not at risk'")))
  
  out <- data.frame(
    decision.full = ifelse(dataset.test[ , paste0("test", nitems)] >= Xstar, "at risk", "not at risk"),
    SCrisk = dataset.test$SCrisk,
    SCnorisk = dataset.test$SCnorisk,
    current.item = dataset.test$currit,
    current.score = dataset.test$currts
  )
  
  print(accuracy)
  if (plot) {
    hist(dataset.test$currit, main = "Test lengths", 
         xlab = "Number of items administered")
  }
  invisible(out)

}









stochCurtailXval <- function(dataset, Xstar, gamma0 = .95, gamma1 = .95, plot = TRUE, verbose = FALSE) {

  ## Define dataset etc:
  nitems <- dim(dataset)[2]
  nobs <- dim(dataset)[1]

  ## Calculate restscores:
  dataset[ , paste0("rest", 1:(nitems-1))] <- NA
  for (i in 2:(nitems-1)) {
    dataset[ , (nitems-1)+i] <- rowSums(dataset[ , i:nitems]) 
  }
  dataset[ , paste0("rest", nitems-1)] <- dataset[ , nitems] # restscore after second last item equals itemscore of last item

  ## Calculate cumulative scores:
  dataset[paste0("test", 1:nitems)] <- NA
  dataset[paste0("test", 1)] <- dataset[1] # testscore after first item equals itemscore of first item
  for (i in 2:nitems) {
    dataset[paste0("test", i)] <- rowSums(dataset[ , 1:i]) 
  }

  # Get Pkplus and Pkmin:
  dataset[, paste0("Pkplus", 1:(nitems-1))] <- NA
  dataset[, paste0("Pkmin", 1:(nitems-1))] <- NA
  #for (i in 1:(nitems-1)) {
    for (j in 1:nobs) {
      if (verbose) print(paste("observation", j))
      traindata <- dataset[-j, ] # exclude person j from dataset
      T_plus <- traindata[traindata[paste0("test", nitems)] >= Xstar, ] # select above-cutoff rows
      #dataset[j, paste0("Pkplus", i)] <-
      #  mean(T_plus[ , paste0("rest", i)] + dataset[j, paste0("test", i)] >= Xstar)
      dataset[j, paste0("Pkplus", 1:(nitems-1))] <- rowMeans(sapply(apply(
        T_plus[ , paste0("rest", 1:(nitems-1))], 1, `+`, 
        dataset[j, paste0("test", 1:(nitems-1))]), 
        `>=`, Xstar))
      T_min <- traindata[traindata[paste0("test", nitems)] < Xstar, ]  # select below-cutoff rows
      #dataset[j, paste0("Pkmin", i)] <-
      #  mean(T_min[ , paste0("rest", i)] + dataset[j, paste0("test", i)] >= Xstar)
      dataset[j, paste0("Pkmin", 1:(nitems-1))] <- rowMeans(sapply(apply(
        T_min[ , paste0("rest", 1:(nitems-1))], 1, `+`, 
        dataset[j, paste0("test", 1:(nitems-1))]), 
        `>=`, Xstar))
    }
  #}
  
  # for every row, pkplus and pkmin should be checked for being >=gamma1 and <=1-gamma0
  for (i in 1:nobs) {
    dataset$plusflag1[i] <- which(dataset[i, paste0("Pkplus", 1:(nitems-1))] >= gamma1)[1]
    dataset$plusflag0[i] <- which(dataset[i, paste0("Pkplus", 1:(nitems-1))] <= 1-gamma0)[1]
    dataset$minflag1[i] <- which(dataset[i, paste0("Pkmin",1:(nitems-1))] >= gamma1)[1]
    dataset$minflag0[i] <- which(dataset[i, paste0("Pkmin",1:(nitems-1))] <= 1-gamma0)[1]
  }

  dataset$SCrisk <- !is.na(dataset$minflag1) & !is.na(dataset$plusflag1)
  dataset$SCrisk[dataset[paste("test",nitems,sep="")] >= Xstar & is.na(dataset$minflag1) & is.na(dataset$plusflag0)] <- TRUE # in case no curtailment took place
  dataset$SCnorisk <- !is.na(dataset$plusflag0)  & !is.na(dataset$minflag0)
  dataset$SCnorisk[dataset[paste("test",nitems,sep="")] < Xstar & is.na(dataset$plusflag0) & is.na(dataset$minflag1)] <- TRUE # in case no curtailment took place

  dataset$currit <- NA  # for tracking the current item number
  dataset$currit[!is.na(dataset$minflag1)] <- dataset$minflag1[!is.na(dataset$minflag1)]
  dataset$currit[!is.na(dataset$plusflag0)] <- dataset$plusflag0[!is.na(dataset$plusflag0)]
  dataset$currit[is.na(dataset$currit)] <- nitems # in case no curtailment took place

  dataset$currts <- NA  # for tracking the current test score
  for (i in 1:nobs) {
    dataset[i,"currts"] <- dataset[i,paste("test", dataset[i,"currit"], sep="")]
  }

  ## return results:
  if (plot) {
    hist(dataset$currit,  main = "Test lengths", 
         xlab = "Number of items administered")
  }
  print(table(dataset$SCrisk, dataset[,paste0("test", nitems)] >= Xstar,
              dnn = c("curtailed: flagged 'risk'", "full length: 'at risk'")))
  print(table(dataset$SCnorisk, dataset[,paste0("test", nitems)] < Xstar,
              dnn = c("curtailed: flagged 'no risk'", "full length: 'no risk'")))
  excl <- which(names(dataset) %in% c(
    paste0("rest", 1:(nitems-1)), paste0("test", 1:nitems), 
    paste0("Pkplus", 1:nitems), paste0("Pkmin", 1:nitems),
    "plusflag1", "plusflag0", "minflag1", "minflag0"))
  invisible(dataset[, -excl])
}









Table <- function(dataset.train = NULL, Xstar, nitems = NULL, highest = NULL, 
                  lowest = NULL) {

  # Define dataset etc:
  if (is.null(highest)) highest <- max(dataset.train, na.rm = TRUE)
  if (is.null(lowest)) lowest <- min(dataset.train, na.rm = TRUE)
  if (is.null(nitems)) nitems <- dim(dataset.train)[2]

  # Create lookup table:
  boundaries <- matrix(NA, nrow = 2, ncol = nitems)
  boundaries <- data.frame(boundaries, row.names = c("no.risk", "risk"))
  colnames(boundaries) <- paste0("item", 1:nitems)

  # Compute boundaries
  for (i in 1:nitems) {
    boundaries["risk", i] <- Xstar - (nitems-i)*lowest
    if (boundaries["risk", i] > i*highest) boundaries["risk", i] <- NA
    boundaries["no.risk", i] <- (Xstar-1) - (nitems-i)*highest
    if (boundaries["no.risk", i] < i*lowest) boundaries["no.risk", i] <- NA
  }

  return(boundaries)
}




stochTable <- function(dataset.train, Xstar, gamma0=.95, gamma1=.95) {

  # Define dataset etc:
  nitems <- dim(dataset.train)[2]
  nobs <- dim(dataset.train)[1]
  maxitemscore <- max(as.vector(dataset.train))
  minitemscore <- min(as.vector(dataset.train))

  # Calculate restscores:
  dataset.train[ , paste0("rest", 1:(nitems-1))] <- NA
  for (i in 2:(nitems-1)) {
    dataset.train[ , (nitems-1)+i] <- rowSums( dataset.train[ , i:nitems]) 
  }
  dataset.train[ , paste0("rest", nitems-1)] <-  dataset.train[ , nitems] # restscore after second last item equals itemscore of last item

  # Calculate total scores:
  dataset.train[ , paste0("test", nitems)] <- rowSums( dataset.train[ , 1:nitems])

  # Split dataset into plus (above cutoff) and min (below cutoff observations):
  Tplus <-  dataset.train[dataset.train[paste0("test", nitems)] >= Xstar, ]
  Tmin <-  dataset.train[dataset.train[paste0("test", nitems)] < Xstar, ]
  Nplus <- dim(Tplus)[1]
  Nmin <- dim(Tmin)[1]

  # create vector for collecting critical "halt testing and flag"-values
  critvalvec <- matrix(rep(NA, times = 4*nitems), nrow = 4, ncol = nitems)
  critvalvec <- data.frame(critvalvec, row.names=c("critval.plus.g0","critval.plus.g1",
                                                   "critval.min.g0","critval.min.g1"))
  colnames(critvalvec) <- paste0("item", 1:nitems)

  # find critical values for gamm0 ("not at risk") and gamma 1 ("at risk") in Plus and Min datasets
  for (i in 1:(nitems-1)) {
    # Flagging in Pkplus
    critvalvec["critval.plus.g0",i] <- Xstar - min(as.numeric(names(which(cumsum(table(Tplus[paste("rest",i,sep="")])/Nplus) >= gamma0))))
    # probability of at least .95 that restscore will be equal to, or smaller than, this value
    # needed for flagging "not at risk" and gamma0
    # score should be < this value for early stopping and "not at risk" flagging
    critvalvec["critval.plus.g1",i] <- Xstar - min(as.numeric(names(which(cumsum(table(Tplus[paste("rest",i,sep="")])/Nplus) >= 1-gamma1))))
    # probability of at least .95 that restscore will be equal to, or greater than, this value
    # needed for flagging "not at risk" and gamma1
    # score should be >= to this value for early stopping and "at risk" flagging
    # Flagging in Pkmin
    critvalvec["critval.min.g0",i] <- Xstar - min(as.numeric(names(which(cumsum(table(Tmin[paste("rest",i,sep="")])/Nmin) >= gamma0))))
    # probability of at least .95 that restscore will be equal to, or smaller than, this value
    # needed for flagging "not at risk" and gamma0
    # score should be < this value for early stopping and "not at risk" flagging
    critvalvec["critval.min.g1",i] <- Xstar - min(as.numeric(names(which(cumsum(table(Tmin[paste("rest",i,sep="")])/Nmin) >= 1-gamma1))))
    # probability of at least .95 that restscore will be equal to, or greater than, this value
    # needed for flagging "not at risk" and gamma1
    # score should be >= to this value for early stopping and "at risk" flagging
  }

  # Set critical values for last item equal to original Xstar:
  critvalvec[, nitems] <- Xstar

  ## Create final lookup table:
  boundaries <- matrix(NA, nrow = 2, ncol = nitems)
  boundaries <- data.frame(boundaries, row.names = c("no.risk", "risk"))
  colnames(boundaries) <- paste0("item", 1:nitems)
  for (i in 1:dim(boundaries)[2]) {
    if ({critvalvec["critval.plus.g0", i] <= critvalvec["critval.min.g0", i]} & 
        {!is.na(critvalvec["critval.plus.g0",i])} & {!is.na(critvalvec["critval.min.g0",i])}) {
      boundaries["no.risk", i] <- critvalvec["critval.plus.g0", i]-1
    } else {
      boundaries["no.risk",i] <- NA
    }
  }
  for (i in 1:dim(boundaries)[2]) {
    if ({critvalvec["critval.plus.g1",i] <= critvalvec["critval.min.g1",i]} & 
        {!is.na(critvalvec["critval.plus.g1",i])} & {!is.na(critvalvec["critval.min.g1",i])}) {
      boundaries["risk",i] <- critvalvec["critval.min.g1",i]
    } else {
      boundaries["risk",i] <- NA
    }
  }

  # Set impossible critical values to NA:
  for (i in 1:nitems) {
    boundaries[boundaries[ , i] < minitemscore*i | boundaries[ , i] > maxitemscore*i, i] <- NA
  }

  return(boundaries)
}
