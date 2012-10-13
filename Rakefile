# -*- ruby -*-

# TODO
task "default" => "compile"

task "compile" do
  `ghc Main.hs -o bin/macra 1>&2`
end

task "test" do
  `runhaskell spec/ParserSpec.hs 1>&2`
  `runhaskell spec/CompilerSpec.hs 1>&2`
end
