module MLIRGen
using JSON3

struct GenerationContext{J<:JSON3.Object}
    dialect::String
    json::J

    function GenerationContext(dialectname, json::J) where J<:JSON3.Object
        dialectname = string(dialectname)
        found_dialect = false
        for d in instanceof(json, :Dialect)
            if json[d].name == dialectname
                found_dialect = true
                dialectname = d
                break
            end
        end
        found_dialect || throw(ArgumentError("Dialect $dialectname not found in provided JSON"))
        new{J}(dialectname, json)
    end
end
const Ctx = GenerationContext

Base.show(io::IO, ctx::Ctx) = print(io, "GenerationContext{", ctx.dialect, "}(...)")
instanceof(json::JSON3.Object) = json.var"!instanceof"
instanceof(json::JSON3.Object, name::Symbol) = json.var"!instanceof"[name]
instanceof(ctx::Ctx, args...) = instanceof(ctx.json, args...)

function operations(ctx::Ctx)
    filter(instanceof(ctx, :Op)) do op
        ctx.json[op].opDialect.def == ctx.dialect
    end
end

isoperand(arg) = "TypeConstraint" ∈ arg.var"!superclasses" || "Arg" ∈ arg.var"!superclasses"
isattribute(arg) = "AttrConstraint" ∈ arg.var"!superclasses"
isvariadic(arg) = "Variadic" ∈ arg.var"!superclasses"
isoptional(arg) = "Optional" ∈ arg.var"!superclasses"
abstract type Def end
for f in (:isvariadic, :isoperand, :isattribute, :isoptional)
    @eval $f(ctx::Ctx, arg::String) = $f(ctx.json[arg])
    @eval $f(ctx::Ctx, def::Def) = $f(ctx.json[def.def])
end

Def(name, def) = isnothing(name) ? UnnamedDef(def) : NamedDef(name, def)
struct UnnamedDef <: Def
    def::String
end
struct NamedDef <: Def
    name::String
    def::String
end

struct Operation
    def::String

    name::String
    dialect_def::String
    description::Union{Nothing, String}
    summary::Union{Nothing, String}
    operands::Vector{Def}
    attributes::Vector{Def}
    results::Vector{Def}
    regions::Vector{Def}
    successors::Vector{Def}

    function Operation(ctx::Ctx, def::String)
        json = ctx.json[def]

        name = json.opName
        dialect_def = json.opDialect.def
        description = json.description
        isempty(description) && (description = nothing)

        summary = json.summary
        isempty(summary) && (summary = nothing)

        results = [Def(res[2], res[1].def) for res in json.results.args]
        regions = [Def(reg[2], reg[1].def) for reg in json.regions.args]
        successors = [Def(suc[2], suc[1].def) for suc in json.successors.args]

        operands = Def[]
        attributes = Def[]
        for arg in json.arguments.args
            argname = arg[2]
            argdef = arg[1].def

            if isoperand(ctx, argdef)
                push!(operands, Def(argname, argdef))
            elseif isattribute(ctx, argdef)
                push!(attributes, Def(argname, argdef))
            else
                error("Unknown argument type")
            end
        end

        return new(def, name, dialect_def, description, summary, operands, attributes, results, regions, successors)
    end
end

dialectname(ctx::Ctx, op::Operation) = ctx.json[op.dialect_def].name

function Base.show(io::IO, op::Operation)
    operands = join([arg isa UnnamedDef ? "?" : arg.name for arg in op.operands], ", ")
    results = join([res isa UnnamedDef ? "?" : res.name for res in op.results], ", ")
    println(io, "Operation: $(op.name)($(operands)) -> ($(results))")
end

# some placeholder functions and types to use in the generated code for now:
create_op_placeholder(args...; kwargs...) = nothing
struct Value end
struct MType end
struct Region end
struct Attribute end
struct Block end

argname(def::Def) = Symbol.(def isa UnnamedDef ? "?" : def.name)

function argtype(ctx::Ctx, basetype, def::Def)
    T = basetype
    if isvariadic(ctx, def)
        T = Vector{T}
    end
    if isoptional(ctx, def)
        T = Union{T, Nothing}
    end
    return T
end

arg_expr(ctx::Ctx, def, basetype) = isoptional(ctx, def) ?
                arg_expr(argname(def), argtype(ctx, basetype, def), nothing) :
                arg_expr(argname(def), argtype(ctx, basetype, def))
arg_expr(name, type, default) = Expr(:kw, arg_expr(name, type), :($default))
arg_expr(name, type) = Expr(:(::), name, type)

"""
Return the expression of a list containing the names of all non-optional defs.
"""
function nonoptional_expr(ctx, defs, basetype)
    return :($basetype[$([argname(def) for def in defs if !isoptional(ctx, def)]...)])
end

function optional_push_exprs(ctx::Ctx, defs, pushdest::Symbol)
    exprs = []
    for def in defs
        !isoptional(ctx, def) && continue
        
        defname = argname(def)
        expr = :(!isnothing($defname) && push!($pushdest, $(defname)))
        push!(exprs, expr)
    end
    return exprs
end

function generate_op(ctx::Ctx, op::Operation)
    # TODO: 
    # * [x] optional operands should default to nothing
    # * [ ] result inference...
    
    name = Symbol(op.name)

    location = gensym(:location)
    
    operands = gensym(:operands)
    results = gensym(:results)
    regions = gensym(:regions)
    attributes = gensym(:attributes)
    successors = gensym(:successors)

    return quote
        function $name(
                    $(arg_expr.(Ref(ctx), op.operands, Ref(Value))...);
                    $(arg_expr.(Ref(ctx), op.results, Ref(MType))...),
                    $(arg_expr.(Ref(ctx), op.attributes, Ref(Attribute))...),
                    $(arg_expr.(Ref(ctx), op.regions, Ref(Region))...),
                 )

            $(operands) = $(nonoptional_expr(ctx, op.operands, Value))
            $(results) = $(nonoptional_expr(ctx, op.results, MType))
            $(regions) = $(nonoptional_expr(ctx, op.regions, Region))
            $(attributes) = $(nonoptional_expr(ctx, op.attributes, Attribute))
            $(successors) = $(nonoptional_expr(ctx, op.successors, Attribute))

            $(optional_push_exprs(ctx, op.operands, operands)...)
            $(optional_push_exprs(ctx, op.results, results)...)
            $(optional_push_exprs(ctx, op.regions, regions)...)
            $(optional_push_exprs(ctx, op.attributes, attributes)...)
            $(optional_push_exprs(ctx, op.successors, successors)...)
            
            return $(create_op_placeholder)(
                $("$(dialectname(ctx, op)).$(op.name)"),
                location=$location;
                operands=$operands,
                results=$results,
                owned_regions=$regions,
                successors=$successors,
                attributes=$attributes,
                result_inference,
            )
        end
    end
end

end