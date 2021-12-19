using Primes

"""
digits collect to number
to_number(d)
"""
function to_number(d, direction=false, base=10)
    if direction
        sum(d.*base .^((length(d)-1):-1:0))
    else
        sum(d.*base .^(0:(length(d)-1)))
    end
end

"""
all divisors of a number.
divisors(n), return a vector of divisors
"""
function divisors(n::Integer)
    0 < n || throw(ArgumentError("number to be factored must be ≥ 0, got $n"))
    1 < n || return Int64[]
    !isprime(n) || return Int64[one(Int64), n]
    f = factor(n)
    d = Int64[one(Int64)]
    for (k, v) in f
        c = Int64[k^i for i in 0:v]
        d = d*c'
        d = reshape(d, length(d))
    end
    sort!(d)
    return d
end

"""
count all divisors of a number.
"""
function count_divisors(n::Int64)
    reduce(*, map(x -> x + 1, values(factor(Dict, n))))
end

function floyd_cycle(f, x0)
    local tort = f(x0)
    local hare = f(tort)
    while tort != hare
        tort = f(tort)
        hare = f(f(hare))
        if isnothing(hare)
            return [0, 0]
        end
    end
 
    local μ = 0
    tort = x0
    while tort != hare
        tort = f(tort)
        hare = f(hare)
        μ += 1
    end
 
    λ = 1
    hare = f(tort)
    while tort != hare
        hare = f(hare)
        λ += 1
    end
 
    return λ, μ
end