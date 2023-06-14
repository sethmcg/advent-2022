
#prog <- data.frame(op=c("noop","addx","addx"),arg=c(NA,3,-5))
#prog <- read.table("test.10.txt", fill=TRUE, col.names=c("op","arg"))
prog <- read.table("input.10.txt", fill=TRUE, col.names=c("op","arg"))

prog$arg[is.na(prog$arg)] <- 0

reg <- c(1)

cycle <- 1
for(i in 1:nrow(prog)){
    op <- prog$op[i]
    if(op == "noop") {
        cycle <- cycle + 1
        reg[cycle] <- reg[cycle-1]
    }
    if(op == "addx"){
        cycle <- cycle + 1
        reg[cycle] <- reg[cycle-1]
        cycle <- cycle + 1
        reg[cycle] <- reg[cycle-1] + prog$arg[i]
    }
}


inds <- 40*1:6 - 20

print(sum(reg[inds]*inds))

### Part 2

reg <- reg[1:240]
crt <- array(".",dim=c(40,6))

x <- rep(1:40, 6) - 1

for(i in 1:240){
    if(any(reg[i] + c(-1,0,1) == x[i]))
        crt[i] <- '#'
}

cat(rbind(crt,"\n"), sep='')
