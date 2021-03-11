# CGL PARSER

Our CGL Paser is implemented using F# and the FsLexYacc library.

## How to run

To run simply you can simply run the project on Visual Studio.

It's also possible to run in the cmd:

```bash
msbuild Modelling.sln /p:Configuration=Release
cd .\Modelling\
cd .\bin\
cd .\Release\
cd .\netcoreapp3.1\
start Modelling.exe
```

## Usage

After executing, you can enter your CGL program in the prompt. 

The program will then build and print the AST for the input, or
return an error, giving information of what failed.

It will also evaluate the program, and display the memory state 
after execution.



