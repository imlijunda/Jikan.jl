import Base: start, next, done, eltype, length, first, last
import Base: size, ndims, isempty, endof, getindex, setindex!

mutable struct TSD{T<:TimeType, V}

    nametag::String
    timetag::Vector{T}

    f::Vector{Symbol}
    v::Matrix{V}

    function TSD{T, V}(
        nt::String,
        tt::AbstractVector{T},
        f::AbstractVector{Symbol},
        v::AbstractMatrix{V}
        ) where {T<:TimeType, V}

        p = sortperm(tt)

        return new(nt, tt[p], copy(f), v[p, :])
    end
end
const tsd = TSD
TSD(nt::String, tt::AbstractVector{T}, f::AbstractVector{Symbol}, v::AbstractMatrix{V}) where {T<:TimeType, V} = TSD{T, V}(nt, tt, f, v)
TSD(nt::String, tt::T, f::Symbol, v::V) where {T<:TimeType, V} = TSD{T, V}(nt, [tt], [f], [v])
TSD(nt::String, tt::AbstractVector{T}, v::AbstractMatrix{V}) where {T<:TimeType, V} = TSD{T, V}(nt, tt, gen_f(size(v, 2)), v)
TSD(nt::String, tt::T, v::V) where {T<:TimeType, V} = TSD{T, V}(nt, [tt], gen_f(1), [v])
gen_f(n::T) where {T<:Integer} = Symbol.(["Column$i" for i = 1:n])

function TSD(x::T; cols = 1:size(x, 2)) where {T<:ATS}

    nc = collect(cols)
    nt = x.nametag
    tt = x.timetag
    f = fieldnames(x)[nc .+ 2]
    v = x[:, nc]

    return TSD(nt, tt, f, v)
end

#Iteration
start(x::TSD) = start(x.timetag)
next(x::TSD, i::Int) = next(x.timetag, i)
done(x::TSD, i::Int) = done(x.timetag, i)
eltype(x::TSD) = eltype(x.timetag)
length(x::TSD) = length(x.timetag)
first(x::TSD) = first(x.timetag)
last(x::TSD) = last(x.timetag)

#Indexing
size(x::TSD) = size(x.v)
size(x::TSD, dim::Int) = size(x.v, dim)
ndims(x::TSD) = 2
isempty(x::TSD) = isempty(x.v)
endof(x::TSD) = endof(x.v)

#generic getindex returns arrays for simplier data manipulation
getindex(x::TSD, val...) = getindex(x.v, val...)

#Special getindex returns a new TSD object
#fieldname as column
getindex(x::TSD, c::Symbol) = TSD(x.nt, x.tt, c, x.v[:, batch_locate(x.f, c)])
getindex(x::TSD, c::AbstractVector{Symbol}) = TSD(x.nt, x.tt, c, x.v[:, batch_locate(x.f, c)])
#time as row
function getindex(x::TSD, r::T) where {T<:TimeType}
    tt = batch_locate(x.timetag, r)
    return TSD(x.nt, tt, x.f, x.v[tt, :])
end
function getindex(x::TSD, r::AbstractVector{<:TimeType})
    tt = batch_locate(x.timetag, r)
    return TSD(x.nt, tt, x.f, x.v[tt, :])
end
function getindex(x::TSD, r::StepRange{<:TimeType})
    tt = range_locate(x.timetag, r)
    return TSD(x.nt, tt, x.f, x.v[tt, :])
end
function getindex(x::TSD, from::T, len::Int) where {T<:TimeType}
    idx1 = findfirst(t -> t >= from, x.timetag)
    idxn = idx1 + len - 1
    return TSD(x.nt, x.tt[idx1:idxn], x.f, x.v[idx1:idxn, :])
end

#generic setindex!
setindex!(x::TSD, v, I...) = setindex!(x.v, v, I...)
#fieldname as column
function setindex!(x::TSD, v, c::Symbol)
    cid = batch_locate(x.f, c)
    for i = 1:length(x)
        x[i, cid] = v[i]
    end

    return x
end
function setindex!(x::TSD, v, c::AbstractVector{Symbol})
    cid = batch_locate(x.f, c)
    nrow = length(x)
    ncol = length(cid)
    for i = 1:ncol
        for j = 1:nrow
            x.v[j, cid[i]] = v[j, i]
        end
    end

    return x
end
#time as row
function setindex!(x::TSD, v, r::T) where {T<:TimeType}
    rid = batch_locate(x.timetag, r)
    for i = 1:size(x, 2)
        x[rid, i] = v[i]
    end

    return x
end
function setindex!(x::TSD, v, r::AbstractVector{<:TimeType})
    rid = batch_locate(x.timetag, r)
    nrow = length(rid)
    ncol = size(x, 2)
    for i = 1:ncol
        for j = 1:nrow
            x.v[rid[j], i] = v[j, i]
        end
    end

    return x
end
function setindex!(x::TSD, v, r::StepRange{<:TimeType})
    rid = range_locate(x.timetag, r)
    nrow = length(rid)
    ncol = size(x, 2)
    for i = 1:ncol
        for j = 1:nrow
            x.v[rid[j], i] = v[j, i]
        end
    end

    return x
end
