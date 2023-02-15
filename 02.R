## Part 1 - RPS tournament

setname <- function(x, name) { names(x) <- name; x}

rps <- c("rock","paper","scissors")

code1 <- rps |> setname(c("A","B","C"))
code2 <- rps |> setname(c("X","Y","Z"))

score1 <- c(rock=1, paper=2, scissors=3)
score2 <- c(loss=0, draw=3, win=6)

rules <- function(x, y){
    c("draw","win","loss")[(score1[x]-score1[y]) %% 3 + 1]
}

#input <- read.table("test.02.txt") |> setname(c("abc","xyz"))
input <- read.table("input.02.txt") |> setname(c("abc","xyz"))

foe  <- code1[input$abc]
self <- code2[input$xyz]

sum(score1[self] + score2[rules(self, foe)]) |> print()

## Part 2

code3 <- c(X="loss", Y="draw", Z="win")

result <- outer(score1, score1, rules)

outcome <- code3[input$xyz]

bout <- cbind(foe=rep(rps, 3), self=rep(rps, each=3))
game <- cbind(bout, result=apply(bout, 2, rules)[,2]) |> as.data.frame()

play <- game$self[match(paste(foe, outcome),
                  paste(game$foe, game$result))] |> na.omit()

sum(score2[outcome] + score1[play]) |> print()

