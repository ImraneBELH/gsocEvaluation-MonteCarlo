library("volesti")

# Exercice 1: Generating a random H-polytope. Compute its volume
H=GenRandHpoly(5,25)
vol=volume(H)

cat("volume of the polytope is: ", vol, "\n")

# Exercice 2 : integral of f(x) = exp^{-a||x||^2} with curbature: over a [-1,1]^n cube

library("cubature")

# lets use 4 values for a
f <- function(x,a) {exp(-a * sum(x^2)) }

f1 <- function(x) { f(x,.1)}
f2 <- function(x) { f(x,1)}
f3 <- function(x) { f(x,5)}
f4 <- function(x) { f(x,10)}

# for a = .1(f1), we are able to complete 10 iterations,
# 11th is too long (more than 10 minutes)
for (n in 1:10){
    cat("n =", n)
    # cubintegrate and hcubature give approximately same results but cubintegrate is slower.
    #print(cubintegrate(f1, rep(-1,n), rep(1,n), 1, "hcubature")[1])
    print(hcubature(f1, rep(-1,n), rep(1,n),tol=1e-5)[1]) 
}

# # for a = 1(f2), we are able to complete¢ 7 iterations,
# # 8th is too long (more than 10 minutes)
for (n in 1:10){
    cat("n =", n)
    print(hcubature(f2, rep(-1,n), rep(1,n),tol=1e-5)[1]) 
}

# # for a = 1(f2), we are able to complete 6 iterations,
# # 7th is too long (more than 10 minutes)
for (n in 1:6){
    cat("n =", n)
    print(hcubature(f3, rep(-1,n), rep(1,n),tol=1e-5)[1])
}

# # for a = 1(f2), we are able to complete 4 iterations,
# # 5th is too long (more than 10 minutes)
for (n in 1:5){
    cat("n =", n)
    print(hcubature(f4, rep(-1,n), rep(1,n),tol=1e-5)[1])
}



## Exercie 3 Compute approximation if the integrales with volesti

cube = GenCube(3,'H')
vol = volume(cube)

mat50points = sample_points(cube,
                        N=50,
                        WalkType = 'BW',
                        distribution = 'uniform')

mat500points = sample_points(cube,
                        N=500,
                        WalkType = 'BW',
                        distribution = 'uniform')

mat5000points = sample_points(cube,
                        N=5000,
                        WalkType ='BW',
                        distribution ='uniform')

MCintegral <- function(func,sampledPoints,N) { 
    total<-0
    for(columns in 1:ncol(sampledPoints)){ 
        sum<-0
        for(rows in 1:nrow(sampledPoints)){
            sum=sum+(sampledPoints[rows,columns])^2
        }
    }
    total=total+func(sum)
    return(vol*total/N) 
}

# Example en use with 5000 samples points
print(hcubature(f4, rep(-1,3), rep(1,3),tol=1e-5)[1])
print(MCintegral(f4, mat5000points, 5000))
## error : 5%

## To change dimention of the cube, new sampled points need to be computed.
