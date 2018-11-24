# load 2016 pl data:
pl2016 <- read.csv('pl2016data.csv',header=T)

Teams <- as.character(unique(pl2016$HomeTeam))

on.target.f <- rep(0, 20)
on.target.a <- rep(0, 20)
goals.f <- rep(0, 20)
goals.a <- rep(0, 20)

n.matches <- dim(pl2016)[1]

for (i in 1:n.matches){
  game <- pl2016[i,]
  
  # get team indices:
  home.team <- which(Teams==game$HomeTeam)
  away.team <- which(Teams==game$AwayTeam)
  # cat('home',home.team,'\n')
  # cat('away',away.team,'\n')
  # cat('on.target.f',on.target.f,'\n')
  # cat('on.target.a',on.target.a,'\n')
  # cat('goals.f',goals.f,'\n')
  # cat('goals.a',goals.a,'\n')
  
  # Home team stats:
  on.target.f[home.team] <- on.target.f[home.team] + game$HST
  on.target.a[home.team] <- on.target.a[home.team] + game$AST
  goals.f[home.team] <- goals.f[home.team] + game$FTHG
  goals.a[home.team] <- goals.a[home.team] + game$FTAG
  
  # Away team stats:
  on.target.f[away.team] <- on.target.f[away.team] + game$AST
  on.target.a[away.team] <- on.target.a[away.team] + game$HST
  goals.f[away.team] <- goals.f[away.team] + game$FTAG
  goals.a[away.team] <- goals.a[away.team] + game$FTHG
  
  # cat('on.target.f',on.target.f,'\n')
  # cat('on.target.a',on.target.a,'\n')
  # cat('goals.f',goals.f,'\n')
  # cat('goals.a',goals.a,'\n')
}

dat <- data.frame(teams = Teams,
                  GoalsFor = goals.f,
                  GoalsAgainst = goals.a,
                  ShotsOnTarget = on.target.f,
                  ShotsOnTargetAgainst = on.target.a)

shots.per.game <- on.target.f/20
block.percentage <- (on.target.a-goals.a)/(on.target.a)

team.valuation <- data.frame(teams=Teams,
                             shot.lambda=shots.per.game,
                             block.p=block.percentage)

simulateGame <- function(Valuations, T1.index, T2.index,L=1){
  T1.lambda <- Valuations$shot.lambda[T1.index]
  T1.block <- Valuations$block.p[T1.index]
  T2.lambda <- Valuations$shot.lambda[T2.index]
  T2.block <- Valuations$block.p[T2.index]
  
  T1.score <- rpois(1,L*T1.lambda*(1-T2.block))
  T2.score <- rpois(1,L*T2.lambda*(1-T1.block))
  
  output <- c(T1.score,T2.score)
  names(output) <- c('home','away')
  return(output)
}

simulateSeason <- function(team.valuation, L=1){
  n <- dim(team.valuation)[1]
  points <- rep(0, n)
  
  for (away in 1:n){   # each team plays every team
    for (home in 1:n){ # for a home and away game, 
      if (home!=away){ # but can't play themselves
        result <- simulateGame(team.valuation, home, away, L)
        if(result[1]>result[2]) points[home] <- points[home] + 3
        else if (result[1]==result[2]){
          points[home] <- points[home] + 1
          points[away] <- points[away] + 1
        }
        else{points[away] <- points[away] + 3}
      }
    }
  }
  
  return(data.frame(teams=team.valuation$teams,
                    points=points)[order(points, decreasing=TRUE),])
}
