batch_locate(coll::AbstractVector{T}, item::T) where{T} = findfirst(coll, item)
function batch_locate(coll::AbstractVector{T}, items::AbstractVector{T}) where {T}

    n = length(items)
    found = Vector{Int}(n)
    for i = 1:n
        found[i] = findfirst(coll, items[i])
    end

    return found
end

function range_locate(coll::AbstractVector{T}, items::AbstractVector{T}) where {T}
    #@assert issorted(coll) "coll must be sorted."
    #@assert issorted(items) "items must be sorted."
    return findfirst(t -> t >= first(items), coll):findlast(t -> t <= last(items), coll)
end

function sort_timetag!(x::ats)

    p = sortperm(x.timetag)
    n = length(fieldnames(x))
    for i = 2:n
        ptr = getfield(x, i)
        sorted = ptr[p]
        for j = 1:length(ptr)
            ptr[j] = sorted[j]
        end
    end

    return x
end
