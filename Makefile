SOURCES = util.ml primop.ml sexpr.ml JIf.ml CVar.ml X86Int.ml JasmInt.ml Chapter2.ml 
LIBS = unix str
RESULT = driver

include OCamlMakefile
