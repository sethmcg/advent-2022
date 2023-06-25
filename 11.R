
## to simplify pipelines
elide <- function(x, pat){gsub(pat,'',x)}

## example input
# Monkey 0:
#   Starting items: 79, 98
#   Operation: new = old * 19
#   Test: divisible by 23
#     If true: throw to monkey 2
#     If false: throw to monkey 3

monkify <- function(x){
    m <- list()

    m$items <- grep("Starting items", x, val=TRUE) |>
        elide('.*:') |> strsplit(',') |> unlist() |> as.numeric()
    
    m$op <- paste("function(old){",
                  grep("Operation", x, val=TRUE) |> elide(".*="),
                  "}") |> str2expression() |> eval()
    m$test <- grep("Test", x, val=TRUE) |> elide(".*by") |> as.numeric()
    m$pass <- grep("true", x, val=TRUE) |> elide(".*monkey ")
    m$fail <- grep("false", x, val=TRUE) |> elide(".*monkey ")
    m$count <- 0
    return(m)
}

#lines <- readLines("test.11.txt")
lines <- readLines("input.11.txt")
monkeys <- split(lines, cumsum(lines=="")) |> lapply(monkify)

for(round in 1:20){
    for(id in names(monkeys)){
        m <- monkeys[[id]]
        for(i in m$items){
            m$count <- m$count + 1
            j <- m$op(i)  # inspection
            k <- floor(j / 3) # relief
            
            if(k %% m$test == 0 ){
                monkeys[[m$pass]]$items <- c(monkeys[[m$pass]]$items, k)
            } else {
                monkeys[[m$fail]]$items <- c(monkeys[[m$fail]]$items, k)
            }        
        }
        m$items <- c()
        monkeys[[id]] <- m
    }
}

print(sapply(monkeys, `[[`, "count") |> sort() |> tail(2) |> prod())


## Part 2

## relief no longer divides worry by 3
## 10,000 rounds
## need to keep worry manageable
## monkey test values are the first N primes
## worry mod product of primes, yes?


#lines <- readLines("test.11.txt")
lines <- readLines("input.11.txt")
monkeys <- split(lines, cumsum(lines=="")) |> lapply(monkify)

wmax <- sapply(monkeys, `[[`, "test") |> prod()

for(round in 1:10000){
    for(id in names(monkeys)){
        m <- monkeys[[id]]
        for(i in m$items){
            m$count <- m$count + 1
            j <- m$op(i)  # inspection
            #            k <- floor(j / 3) # relief
            k <- j %% wmax
            
            if(k %% m$test == 0 ){
                monkeys[[m$pass]]$items <- c(monkeys[[m$pass]]$items, k)
            } else {
                monkeys[[m$fail]]$items <- c(monkeys[[m$fail]]$items, k)
            }        
        }
        m$items <- c()
        monkeys[[id]] <- m
    }
}

print(sapply(monkeys, `[[`, "count") |> sort() |> tail(2) |> prod())
