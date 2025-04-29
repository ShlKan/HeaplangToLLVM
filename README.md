# HeaplangToLLVM
The project provides toolchains to translate Heaplang to LLVM IR. 

1. Heaplang is an imperative language in IRIS. You can prove the correctness of your Heaplang code in IRIS.
2. LLVM IR is an independent IR widely used in the industry.

A translation from Heaplang to LLVMIR could facilitate us to generate certified code. 
However, currently, the implementation of the translation is not formally verified. 
This project is exploratory. We will develop a certified version in the future, before we 
accumulate enough experience for the translation.
