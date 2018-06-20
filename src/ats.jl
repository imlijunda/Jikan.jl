import Base: size, length, isempty, ndims, start, next, done, first, last, eltype
import Base: getindex

abstract type AbstractTimeStruct end
const ATS = AbstractTimeStruct
const ats = AbstractTimeStruct

#implements getindex interface

#when nothing is passed, instead of returning first element, returns ref of x itself
getindex(x::ATS) = x
#use single colon to get all timetag as vector
getindex(x::ATS, ::Colon) = x.timetag[:]
#use double colon to get all data as matrix
getindex(x::ATS, ::Colon, ::Colon) = [x[i, j] for i = 1:size(x, 1), j = 1:size(x, 2)]

#use single index to get column data as vector
getindex(x::ATS, f) = getfield(x, f)
#if integer index is passed, skip nametag and timetag
getindex(x::ATS, f::T) where {T<:Integer} = getfield(x, f + 2)

#ATS[i, :], ATS[:, j], ATS[i, j]
getindex(x::ATS, r::T, ::Colon) where {T<:Integer} = [x[i][r] for i = 1:size(x, 2)]
getindex(x::ATS, ::Colon, c::T) where {T<:Integer} = x[c]
getindex(x::ATS, r::T, c::T) where {T<:Integer} = x[c][r]

#ATS[n, i:j], ATS[i:j, n]
getindex(x::ATS, r::T, c::V) where {T<:Integer, V<:AbstractVector} = [x[r, i] for i = c]
getindex(x::ATS, r::V, c::T) where {T<:Integer, V<:AbstractVector} = [x[i, c] for i = r]

#ATS[i:j, :], ATS[:, i:j], ATS[i:j, i2:j2]
getindex(x::ATS, r::T, ::Colon) where {T<:AbstractVector} = [x[i, j] for i = r, j = 1:size(x, 2)]
getindex(x::ATS, ::Colon, c::T) where {T<:AbstractVector} = [x[i, j] for i = 1:size(x, 1), j = c]
getindex(x::ATS, r::T, c::T) where {T<:AbstractVector} = [x[i, j] for i = r, j = c]

#getindex by time
getindex(x::ATS, t::T) where {T<:Union{TimeType, AbstractVector{<:TimeType}}} = x[batch_locate(x.timetag, t), :]
getindex(x::ATS, t::T, ::Colon) where {T<:Union{TimeType, AbstractVector{<:TimeType}}} = x[t]
getindex(x::ATS, t::T, c) where {T<:Union{TimeType, AbstractVector{<:TimeType}}} = x[batch_locate(x.timetag, t), c]

getindex(x::ATS, t::T) where {T<:StepRange{<:TimeType}} = x[range_locate(x.timetag, t), :]
getindex(x::ATS, t::T, ::Colon) where {T<:StepRange{<:TimeType}} = x[t]
getindex(x::ATS, t::T, c) where {T<:StepRange{<:TimeType}} = x[range_locate(x.timetag, t), c]

#Iteration
start(x::ATS) = start(x.timetag)
next(x::ATS, i::Int) = next(x.timetag, i)
done(x::ATS, i::Int) = done(x.timetag, i)
eltype(x::ATS) = eltype(x.timetag)
length(x::ATS) = length(x.timetag)
first(x::ATS) = first(x.timetag)
last(x::ATS) = last(x.timetag)

size(x::ATS) = (length(x.timetag), length(fieldnames(x)) - 2)
size(x::ATS, dim::Int) = size(x)[dim]
isempty(x::ATS) = isempty(x.timetag)
ndims(x::ATS) = 2

#endof is ill-defined for ATS data structure, do not implement
