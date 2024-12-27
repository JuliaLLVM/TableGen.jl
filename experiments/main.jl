using LLVM_full_jll, Tablegen, JSON3, MacroTools
import Tablegen.MLIRGen: generate_op, Ctx, operations, Operation

includepath = joinpath(LLVM_full_jll.artifact_dir, "include")
dialectpath = joinpath(includepath, "mlir", "Dialect", "GPU", "IR", "GPUOps.td")

result = Tablegen.tblgen_to_json(dialectpath; includes=[includepath])
# JSON3.write("./GPUOps.json", result)

ctx = Ctx("gpu", result)

# Create an operation helper object from its def-string and the context:
Operation(ctx, "GPU_Create2To4SpMatOp")

function f(ctx)
    ops = []
    for op in operations(ctx)
        push!(ops, Operation(ctx, op))
    end
    return ops
end

ops = f(ctx)

generate_op(ctx, ops[2])

prettify(generate_op(ctx, ops[2]))

function g()
    finaltraits = Set()
    traitdefs = map(result.GPU_BlockDimOp.traits) do x
        x.def
    end
    # traitdefs = ["InferIntRangeInterface"]
    while !isempty(traitdefs)
        def = pop!(traitdefs)
        trait = result[def]
        push!(finaltraits, trait)
        print(def)
        if haskey(trait, "trait")
            println("\t($(trait.trait))")
        else
            println()
        end
        if haskey(trait, "traits")
            for newtrait in trait.traits
                push!(traitdefs, newtrait.def)
            end
        end
    end
    return finaltraits
end

test = g()

collect(test)


eval(generate_op(ctx, ops[2]))

