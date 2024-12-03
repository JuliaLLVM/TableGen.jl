module Tablegen

using LLVM_full_jll
using JSON3

llvm_tblgen_path = joinpath(LLVM_full_jll.PATH[], "llvm-tblgen")

function tblgen_to_json(filepath; includes=[])
    output = IOBuffer()
    run(`$llvm_tblgen_path --dump-json $filepath $("-I" .* includes)`, stdin, output)
    JSON3.read(String(take!(output)))
end

include("MLIRGen.jl")

end # module Tablegen
