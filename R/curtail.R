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
  #dataset.test$Crisk <- NA
  dataset.test$Crisk <- NA
  for (i in 1:nobs) {
    for (j in 1:nitems) {
      #if (is.na(dataset.test$Cnorisk[i]) && is.na(dataset.test$Crisk[i])) {
      if (is.na(dataset.test$Crisk[i])) {
        if (dataset.test[i, paste0("test", j)] >= risk.boundaries[j]) {
          dataset.test$currit[i] <- j
          dataset.test$currts[i] <- dataset.test[i, paste0("test", j)]
          dataset.test$Crisk[i] <- "at risk"
        } else if (dataset.test[i, paste0("test", j)] <= norisk.boundaries[j]) {
          dataset.test$currit[i] <- j
          dataset.test$currts[i] <- dataset.test[i, paste0("test", j)] 
          #dataset.test$Cnorisk[i] <- TRUE
          dataset.test$Crisk[i] <- "not at risk"
        }
      }
    }
  }

  ## Return results:
  fulllength <- ifelse(dataset.test[ , paste0("test", nitems)] >= Xstar, "at risk", "not at risk")
  accuracy <- table(dataset.test$Crisk, fulllength, useNA = "ifany", 
                    dnn = c("curtailed","full length"))

  out <- list(
    test.results= data.frame(
      fulllength.decision = fulllength,
      curtailed.decision = dataset.test$Crisk,
      current.item = dataset.test$currit,
      current.score = dataset.test$currts),
    curtailed.test.length.distribution = list(
      mean = mean(dataset.test$currit),
      standard.deviation = sd(dataset.test$currit),
      median = median(dataset.test$currit),
      proportion.curtailed = sum(dataset.test$currit < nitems) / nobs),
    accuracy = accuracy)

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
    dataset.test$riskflag[i] <- which(dataset.test[i, paste0("Pkplus", 1:(nitems-1))] >= gamma1 &
                                         dataset.test[i, paste0("Pkmin", 1:(nitems-1))] >= gamma1)[1]
    dataset.test$noriskflag[i] <- which(dataset.test[i, paste0("Pkplus", 1:(nitems-1))] <= 1-gamma0 &
                                           dataset.test[i, paste0("Pkmin", 1:(nitems-1))] <= 1-gamma0)[1]
  }
  

  dataset.test$currit <- dataset.test$currts <- dataset.test$SCrisk <- NA
  for(j in 1:nobs) {
    if (is.na(dataset.test$riskflag[j]) && is.na(dataset.test$noriskflag[j])) {
      ## Then no curtailment was performed. Get decision based on full test score and set currit <- nitems
      dataset.test$SCrisk[j] <- ifelse(dataset.test[j, paste0("test", nitems)] >= Xstar, 
                                       "at risk", "not at risk")
      dataset.test$currit[j] <- nitems
      dataset.test$currts[j] <- dataset.test[j, paste0("test", nitems)]
    } else if (is.na(dataset.test$riskflag[j])) {
      ## Then take noriskflag
      dataset.test$SCrisk[j] <- "not at risk"
      dataset.test$currit[j] <- dataset.test$noriskflag[j]
      dataset.test$currts[j] <- dataset.test[j, paste0("test", dataset.test$noriskflag[j])]
    } else if (is.na(dataset.test$noriskflag[j])) {
      ## Then take riskflag
      dataset.test$SCrisk[j] <- "at risk"
      dataset.test$currit[j] <- dataset.test$riskflag[j]
      dataset.test$currts[j] <- dataset.test[j, paste0("test", dataset.test$riskflag[j])]
    } else {
      ## Them both riskflag and noriskflag. Take whichever value is lowest.
      risk <- dataset.test$risk[j] <= dataset.test$norisk[j] 
      dataset.test$SCrisk[j] <- ifelse(risk, "at risk", "not at risk")
      dataset.test$currit[j] <- ifelse(risk, dataset.test$riskflag[j], dataset.test$noriskflag[j])
      dataset.test$currts[j] <- ifelse(risk,
                                       dataset.test[j, paste0("test", dataset.test$riskflag[j])],
                                       dataset.test[j, paste0("test", dataset.test$noriskflag[j])])
    }
  }
  
  ## Return results:
  fulllength <- ifelse(dataset.test[ , paste0("test", nitems)] >= Xstar, "at risk", "not at risk")
  accuracy <- table(dataset.test$SCrisk, fulllength, useNA = "ifany", 
                    dnn = c("curtailed","full length"))
  
  out <- list(
    test.results= data.frame(
      fulllength.decision = fulllength,
      curtailed.decision = dataset.test$SCrisk,
      current.item = dataset.test$currit,
      current.score = dataset.test$currts),
    curtailed.test.length.distribution = list(
      mean = mean(dataset.test$currit),
      standard.deviation = sd(dataset.test$currit),
      median = median(dataset.test$currit),
      proportion.curtailed = sum(dataset.test$currit < nitems) / nobs),
    accuracy = accuracy)
  
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

  ## Get Pkplus and Pkmin:
  dataset[, paste0("Pkplus", 1:(nitems-1))] <- NA
  dataset[, paste0("Pkmin", 1:(nitems-1))] <- NA
  for (j in 1:nobs) {
    if (verbose) print(paste("observation", j))
    traindata <- dataset[-j, ] # exclude person j from dataset
    T_plus <- traindata[traindata[paste0("test", nitems)] >= Xstar, 
                        paste0("rest", 1:(nitems-1))] # select above-cutoff rows
    dataset[j, paste0("Pkplus", 1:(nitems-1))] <- rowMeans(sapply(apply(
      T_plus, 1, `+`, dataset[j, paste0("test", 1:(nitems-1))]), 
      `>=`, Xstar))
    T_min <- traindata[traindata[paste0("test", nitems)] < Xstar, 
                                 paste0("rest", 1:(nitems-1))]  # select below-cutoff rows
    dataset[j, paste0("Pkmin", 1:(nitems-1))] <- rowMeans(sapply(apply(
      T_min, 1, `+`, dataset[j, paste0("test", 1:(nitems-1))]), 
      `>=`, Xstar))
  }
  
  ## Check for every observations whether pkplus and pkmin are >= gamma1 and <= 1-gamma0:
  for (i in 1:nobs) {
    dataset$riskflag[i] <- which(dataset[i, paste0("Pkplus", 1:(nitems-1))] >= gamma1 &
                                   dataset[i, paste0("Pkmin", 1:(nitems-1))] >= gamma1)[1]
    dataset$noriskflag[i] <- which(dataset[i, paste0("Pkplus", 1:(nitems-1))] <= 1-gamma0 &
                                      dataset[i, paste0("Pkmin", 1:(nitems-1))] <= 1-gamma0)[1]
  }
  
  dataset$currit <- dataset$currts <- dataset$SCrisk <- NA
  for(j in 1:nobs) {
    if (is.na(dataset$riskflag[j]) && is.na(dataset$noriskflag[j])) {
      ## Then no curtailment was performed. Get decision based on full test score and set currit <- nitems
      dataset$SCrisk[j] <- ifelse(dataset[j, paste0("test", nitems)] >= Xstar, 
                                  "at risk", "not at risk")
      dataset$currit[j] <- nitems
      dataset$currts[j] <- dataset[j, paste0("test", nitems)]
    } else if (is.na(dataset$riskflag[j])) {
      ## Then take noriskflag
      dataset$SCrisk[j] <- "not at risk"
      dataset$currit[j] <- dataset$noriskflag[j]
      dataset$currts[j] <- dataset[j, paste0("test", dataset$noriskflag[j])]
    } else if (is.na(dataset$noriskflag[j])) {
      ## Then take riskflag
      dataset$SCrisk[j] <- "at risk"
      dataset$currit[j] <- dataset$riskflag[j]
      dataset$currts[j] <- dataset[j, paste0("test", dataset$riskflag[j])]
    } else {
      ## Them both riskflag and noriskflag. Take whichever value is lowest.
      risk <- dataset$risk[j] <= dataset$norisk[j] 
      dataset$SCrisk[j] <- ifelse(risk, "at risk", "not at risk")
      dataset$currit[j] <- ifelse(risk, dataset$riskflag[j], dataset$noriskflag[j])
      dataset$currts[j] <- ifelse(risk,
                                       dataset[j, paste0("test", dataset$riskflag[j])],
                                       dataset[j, paste0("test", dataset$noriskflag[j])])
    }
  }

  ## return results:
  fulllength <- ifelse(dataset[ , paste0("test", nitems)] >= Xstar, "at risk", "not at risk")
  accuracy <- table(dataset$SCrisk, fulllength, useNA = "ifany", 
                    dnn = c("curtailed","full length"))
  
  if (plot) {
    hist(dataset$currit,  main = "Test lengths", 
         xlab = "Number of items administered")
  }
  
  out <- list(
    test.results= data.frame(
      fulllength.decision = fulllength,
      curtailed.decision = dataset$SCrisk,
      current.item = dataset$currit,
      current.score = dataset$currts),
    curtailed.test.length.distribution = list(
      mean = mean(dataset$currit),
      standard.deviation = sd(dataset$currit),
      median = median(dataset$currit),
      proportion.curtailed = sum(dataset$currit < nitems) / nobs),
    accuracy = accuracy)
  
  print(accuracy)
  
  invisible(out)
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
