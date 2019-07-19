RANKING_FILE <- "/var/tmp/petanque-ranking.rds"

#' Update ranking based on the result of the game
#' @param players Player names (character vector of length 2)
#' @param winner Winning player's number 1 or 2 or 0 (tie)
#' @param score Score
#' @param file Path to the RDS file with saved ranking
updateRanking <- function(players, winner, score, file = RANKING_FILE) {
  if (file.exists(file)) {
      rankingDB <- readRDS(file)
  } else {
      rankingDB <- data.frame('rank' = 'new player' ,'player'= players, 'rating' = rep(750,2),  'gamesPlayed' = rep(0,2), 'nWins' = rep(0,2), 'nLosses' = rep(0,2),stringsAsFactors=FALSE) 
      saveRDS(rankingDB, file = file) 
  }
  if (length(missingPlayers <- setdiff(players, rankingDB$player))) { # add player(s) that don't have ranking
    rankingDB <- rbind(rankingDB, 
        data.frame('player' = missingPlayers, 'rank' = 'new player', 'rating' = 750,  'gamesPlayed' = 0, 'nWins' = 0, 'nLosses' = 0, type = 'unranked', stringsAsFactors = FALSE)
    )
  }
  
  # step 1) get previous ranking / initialize ranking
  oldRanks <- lapply(players, function(p) getRanking(player = p, rankingDB = rankingDB))
  oldRanks <- do.call('rbind',oldRanks)
  
  # step 2) effective number of games played
  effectiveNrGames <- sapply(1:nrow(oldRanks), function(iRow) getEffectiveNrGames(oldRanking = oldRanks[iRow,'rating'],gamesPlayed = oldRanks[iRow,'gamesPlayed'])) 
  oldRanks$effectiveGames <- effectiveNrGames 
  
  scoreFinal <- c(0.5,0.5)
  addedValue <- switch(score,
          '1' = 0.0,
          '2'= 0.3,
          '3' = 0.6)

  if(winner==1){
      scoreFinal <- c(1 + addedValue,0)
  }else{
      scoreFinal <- c(0,1 + addedValue)
  }
  
  # step 3) for unrated players only: temparory estimates of ratings
  initialRanking <- list('player1' = 0, 'player2' = 0)
  
  for(iplayer in 1:nrow(oldRanks)){
#      if(oldRanks[iplayer,'rank']=='new player'){
#          initR <- specialRanking(infoPlayer1 = oldRanks[iplayer,], ratingPlayer2 = oldRanks[2-(iplayer-1),'rating'], score = scoreFinal[iplayer])
#      }
#      else{
          initR <- oldRanks[iplayer,'rating']
#      }
#      
#      if(initR<100){
#           
#          initR = 100
#      }
      
      initialRanking[[iplayer]] <- initR
  }
 
 
  # step 4) calculate an intermediate rating : special ranking for less than 8 games or all wins or all losses / standard ranking otherwise
    
    intermediateRanking <- list('player1' = 0, 'player2' = 0) 
    
    for(iplayer in 1:nrow(oldRanks)){
#        
#        if(oldRanks[iplayer,'gamesPlayed'] < 4  | oldRanks[iplayer,'gamesPlayed'] == oldRanks[iplayer,'nWins'] | oldRanks[iplayer,'gamesPlayed'] == oldRanks[iplayer,'nLosses']){
#            intermediateR <- specialRanking(infoPlayer1 = oldRanks[iplayer,], ratingPlayer2 =  initialRanking[[2-(iplayer-1)]], score = scoreFinal[iplayer])
#        }
#        else{
#            
#          addedValue <- switch(score,
#          '1' = 0.0,
#          '2'= 0.3,
#          '3' = 0.6)
#            
#            if(winner==1){
#                scoreFinal <- c(1 + addedValue,0)
#            }else{
#                scoreFinal <- c(0,1 + addedValue)
#            }
#            intermediateR <- standardRanking(infoPlayer1 = oldRanks[iplayer,], ratingPlayer2 =  initialRanking[[2-(iplayer-1)]], score = scoreFinal[iplayer])
#        }
#        
##        if(intermediateR <100){
##            
##            intermediateR  = 100
##        }
#        
#        intermediateRanking[[iplayer]] <-  intermediateR 
        intermediateRanking[[iplayer]] <-  initialRanking[[iplayer]]
    }

  # step 5) calculate a final rating
    
    finalRanking <- list('player1' = 0, 'player2' = 0) 
    
    for(iplayer in 1:nrow(oldRanks)){
#        if(oldRanks[iplayer,'gamesPlayed'] < 4  | oldRanks[iplayer,'gamesPlayed'] == oldRanks[iplayer,'nWins'] | oldRanks[iplayer,'gamesPlayed'] == oldRanks[iplayer,'nLosses']){
#            finalR <- specialRanking(infoPlayer1 = oldRanks[iplayer,], ratingPlayer2 =  intermediateRanking[[2-(iplayer-1)]], score = scoreFinal[iplayer])
#        }
#        else{
            finalR <- standardRanking(infoPlayer1 = oldRanks[iplayer,], ratingPlayer2 =  intermediateRanking[[2-(iplayer-1)]], score = scoreFinal[iplayer])
#        }
        
#        if(finalR<100){
#            
#            finalR = 100
#        }
        
        finalRanking[[iplayer]] <- round(finalR)
    }
    
    
    #replace in database

    newRanks <- oldRanks
    newRanks$rating <- unlist(finalRanking)
    newRanks$gamesPlayed <- newRanks$gamesPlayed + 1
    newRanks$nWins <- newRanks$nWins + floor(scoreFinal)
    newRanks$nLosses <- newRanks$nLosses + floor(abs(scoreFinal-1))
    #newRanks$type <- 'ranked'
    
    rankingDB[which(rankingDB$player%in%players),] <- newRanks[,-c(which(colnames(newRanks)=='effectiveGames'))]
    rankingDB <-  rankingDB[order(rankingDB$rating,decreasing = TRUE),]  
    rankingDB$rank <-rank(-rankingDB$rating, ties.method = "min") 
    saveRDS(rankingDB, file = RANKING_FILE) 

   #TODO The maximum rating change in a match is 50 points;  
  
    #TODO
  # ELO ranking : based on logistic curve --> probability of winning
  # based on difference in ranking: bigger difference == higher probability of winning for best ranked player
  # scoring: 16 for win, -16 for loss, +8 for tie
  # ! if much higher ranked player wins: added score is not much (expected to win) <-> if much lower player wins: gains a lot (~ based on difference and oldRankings)
  #  
    
}

#' Get the current ranking of a player or initialize a player's ranking
#' @param player Player's name
#' @param rankingDB The current ranking database
#' @inheritParams updateRanking
getRanking <- function(player, rankingDB = NULL, file = RANKING_FILE) {

    if (is.null(rankingDB) && file.exists(file))
      rankingDB <- readRDS(file)
  
    if(player %in% rankingDB$player){
            oldRankingPlayer <- rankingDB[which(rankingDB$player==player),]           
    }
    else{
        oldRankingPlayer <- data.frame('rank' = 'new player', 'player'= player, 'rating' = 750,  'gamesPlayed' = 0, 'nWins' = 0, 'nLosses' = 0,stringsAsFactors=FALSE)   
        rankingDB <- rbind(rankingDB,oldRankingPlayer)
        saveRDS(rankingDB, file = file)
    }   
    
    return(oldRankingPlayer)
}

#' Get the number of effective games played by a player
#' @param oldRanking Current ranking of a player
#' @param gamesPlayed Number of games played
getEffectiveNrGames <-function(oldRanking,gamesPlayed){
    
    if(oldRanking < 2355){
        N_star <- round(50/sqrt(0.662 + 0.00000739*(2569 - oldRanking)^2))
    }else{
        N_star <- 50
    }
   
    N_eff <- min(gamesPlayed, N_star)
    
}

#' Special rating algorithm for unrated players, 
#' players with less than 8 games and player with all wins or all losses
#' @param rankingPlayer Numeric with ranking player 1.
#' @param rankingPlayeri Numeric with ranking player 2.
computePWe <- function(rankingPlayer, rankingPlayeri){
    
    if(rankingPlayer <= (rankingPlayeri - 400)){
        
        PWe <- 0
        
    }
    else if((rankingPlayeri - 400) < rankingPlayer & rankingPlayer < (rankingPlayeri + 400)){
        
        PWe <- 0.5 + (rankingPlayer - rankingPlayeri)/800
    }
    else if(rankingPlayer >= (rankingPlayeri + 400)){
        
        PWe <- 1
    }
    
    return(PWe)
}

#' Objective function to minimize
#' @param rank Current rank
#' @param effectiveGames Number of effective games
#' @param R_0_prime Result of R_0_prime
#' @param ratingPlayer2 The rating of player 2
#' @param S_prime Result of the S_prime parameter
objFunction <- function(rank, effectiveGames, R_0_prime, ratingPlayer2, S_prime){
    newR <- effectiveGames * computePWe(rank, R_0_prime) + computePWe(rank, ratingPlayer2) - S_prime
    return(newR)
}


#' Special rating algorithm for unrated players, players with less than 8 games or all successive wins/losses
#' @param infoPlayer1 information of player 1
#' @param ratingPlayer2 The rating of player 2
#' @param score Scored obtained by the player
specialRanking <- function(infoPlayer1, ratingPlayer2, score){
     
   if(infoPlayer1$gamesPlayed == infoPlayer1$nWins){ #all wins
       R_0_prime <- infoPlayer1$rating  - 400
       S_prime <- score + infoPlayer1$effectiveGames
   }
   else if(infoPlayer1$gamesPlayed == infoPlayer1$nLosses){ #all losses
       R_0_prime <- infoPlayer1$rating  + 400
       S_prime <- score      
   }
   else{ #less than 8 games or unrated
       R_0_prime <- infoPlayer1$rating 
       S_prime <- score + infoPlayer1$effectiveGames/2
   }
   
   Sz <- c(R_0_prime - 400, R_0_prime + 400, ratingPlayer2 - 400, ratingPlayer2 + 400)
   
   M <- ((infoPlayer1$effectiveGames*R_0_prime) + ratingPlayer2 + 400*(2*score-1))/(infoPlayer1$effectiveGames + 1)
   
   exit = FALSE
   tol <- 10e-7
   
   while(!exit){
       
       newR <- objFunction(rank = M, effectiveGames =infoPlayer1$effectiveGames, R_0_prime = R_0_prime, ratingPlayer2 = ratingPlayer2 , S_prime = S_prime)
      
       if(newR>tol){
           
           z_a <- max(Sz[which(M>Sz)])
           
           z_a_objFunction <- objFunction(rank = z_a, effectiveGames =infoPlayer1$effectiveGames, R_0_prime =R_0_prime, ratingPlayer2 = ratingPlayer2 , S_prime = S_prime)
           
           if(abs(newR -  z_a_objFunction) < tol){
               M <- z_a
           }else{
               M_star <- M - newR * ((M-z_a)/(newR - z_a_objFunction))
               
               if(M_star < z_a){
                   M <- z_a
               }else if(z_a <= M_star & M_star < M){
                   M <- M_star
               }
           }
       }
       else if(newR < (-tol)){
           z_b <- min(Sz[which(M<Sz)])
           
           z_b_objFunction <- objFunction(rank = z_b, effectiveGames =infoPlayer1$effectiveGames, R_0_prime =R_0_prime, ratingPlayer2 = ratingPlayer2 , S_prime = S_prime)
           
           if(abs(newR -  z_b_objFunction) < tol){
               M <- z_b
           }
           else{
               M_star <- M - newR * ((z_b - M)/(z_b_objFunction - newR))
               
               if(M_star > z_b){
                   M <- z_b
               }
               else if(M <= M_star & M_star < z_b){
                   M <- M_star
               }
           }
           
           
       }
       else if(abs(newR) <= tol){
           
           p <- 0
           if(abs(M-ratingPlayer2)<=400){
               p <- 1
           }
           
           if(abs(M-R_0_prime)<=400){
               p <- p+1
           }
           
           if(p >0){
               exit = TRUE
           }else if(p==0){
               z_a <- max(Sz[which(M>Sz)])
               z_b <- min(Sz[which(M<Sz)])
               
               if( z_a <= infoPlayer1$rating & infoPlayer1$rating <= z_b){
                   M <- infoPlayer1$rating
               }
               else if(infoPlayer1$rating < z_a){
                   M <- z_a
               }
               else if(infoPlayer1$rating > z_b){
                   M <- z_b
               }
               
           }
       }
       
       if(M > 2700){
           M <- 2700
           exit = TRUE
       }
       
   }
   
   return(M)
   
   
   
}


#' Standard rating algorithm for players with more than 8 games
#' @param infoPlayer1 information of player 1
#' @param ratingPlayer2 The rating of player 2
#' @param score Scored obtained by the player
standardRanking <- function(infoPlayer1, ratingPlayer2, score){
    
    ratingPlayer1 <- infoPlayer1$rating
    
    We <- 1/(1+10^(-(ratingPlayer1-ratingPlayer2)/400))
    
    #K <- 800/(infoPlayer1$effectiveGames + 1)
    
    if(ratingPlayer1 < 2100){
        K <- 32
    }else if(ratingPlayer1 > 2100 && ratingPlayer1 < 2400){
        K  <- 24
    }else{
        K <- 16
    }
    
    R_s <- ratingPlayer1 + K*(score - We )
    
    return(R_s)
    
}

#' Extract ranking info from the file.
#' @inheritParams updateRanking
#' @return Data.frame with ranking info.
printRankings <- function(file = RANKING_FILE) {
  if (!file.exists(file)) {
    return(NULL)
  } else { 
    rankingDB <- readRDS(file)
  }
  
  out <- rankingDB[order(rankingDB$rating, decreasing = TRUE), ]
  #out <- cbind(rank = rank(-out$rating, ties.method = "min"), out)  # note hack using negative rating

  out
  
}

# for dev purposes
# note that file is saved as backup
#' Reset ranking.
#' @inheritParams updateRanking
#' @return No return value, the \code{file} is saved and removed..
resetRanking <- function(file = RANKING_FILE) {
  inc <- 0
  while (file.exists(file.path(dirname(file), paste0(basename(file), inc))))
    inc <- inc+1

  backupFile <- file.path(dirname(file), paste0(basename(file), inc))
  
  file.copy(file, to = backupFile)
  file.remove(file)
}
