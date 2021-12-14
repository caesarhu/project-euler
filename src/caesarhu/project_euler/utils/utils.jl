using Primes

"""
digits collect to number
to_number(d)
"""
function to_number(d, base=10)
    sum(d.*base .^(0:(length(d)-1)))
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