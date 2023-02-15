## Part 1: maximum total per bag

infile <- "input.01.txt"
input <- readLines(infile) |> as.numeric()
elves <- split(input, cumsum(is.na(input)))
total <- sapply(elves, sum, na.rm=TRUE)
total[which.max(total)] |> print()

## Part 2: sum of top 3

sort(total) |> tail(3) |> sum() |> print()

