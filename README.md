# lacogen1
LaCoGen - Lazarus Compiler Generator - Version 1.2

#### Synopsis
LaCoGen is a command line driven lexical analyser and [LALR(1)](https://en.wikipedia.org/wiki/LALR_parser) parser table generator in the style of the LEX and YACC programs used in the world of C development. It takes a set of grammar rules in a .lac file and creates the following optional items:

* .lacobj file which contains the DFA and LALR(1) tables. These can be used to control your parser and lexer
* .txt dump file which contains detailed descriptions of the DFA and LALR(1) items and how they were arrived at
* .xml dump file containing the DFA and LALR(1) tables

The .lacobj file can be read by the included lexer/parser code to form a complete working module. The included code is written in Lazarus (Free Pascal Compiler) however there is no reason it cannot be ported to C, C++, Java, Python, etc.

#### Development Status
This is very much experimental and was developed by the author as a learning tool for how compiler generation tools worked in general.
Please don't use this for anything serious that you would object to losing. Whilst having been extensively tested, and coming with 
working examples, there is no guarantee that it will work correctly with all grammars.

#### Development Requirements
To compile this software, you will need Lazarus 2.2.0 or later. It has been tested on Windows. As it is
only a simple text and file based application, it should be relatively easy to recompile on other hosts which are
supported by the Lazarus ecosystem.

#### Documentation
The docs/ folder contains a user guide and a description of the file format for the .lacobj files.

#### Folder Structure
Folders are organised as follows:

* root/ - The Lazarus project files, licence and .gitignore
  * binaries/ - The main executable file
  * docs/ The documentation folder
  * lac/core/ LaCoGen core files (LaCoGen is built with LaCoGen!)
    * deployment_parser_module.pas - A utility parser and lexer that can load .lacobj files from file, resource or stream. Include this code with your application that uses LaCoGen
    * deployment_parser_types.pas - Some types defined for you to reference in your own application
    * lacogen10.lac - The grammar file to describe LaCoGen version 1.0
    * lacogen10.lacobj - The compiled grammar file containing the DFA and LALR items created from lacogen10.lac
    * lacogen10_setliteral.lac - The grammar file to define set literals - there is a parser within a parser for efficiency
    * lacogen10_setliteral.lacobj - The compiled grammar for set literals
    * security_save/ folder containing known good copies of the above. If you mess up with any of the files above, you won't be able to recompile Lazarus
  * test/ - Random stuff used to test the software, can be used to get to learn how it works
    * 6502/ - Grammar for a 6502 macro assembler which I wrote
    * ansi_c/ - Grammar for ANSI C - this was taken from the C grammar for the Gold Parser at http://www.goldparser.org/grammars/index.htm and converted into LaCoGen
    * errors/ - A bunch of bad grammars which deliberately induce faults such as Shift/Reduce and Reduce/Reduce errors
    * expr/ - A complete Lazarus application for an expression evaluator - use this to learn how to create your own application
    * literal/ - Test grammar for literals and escape sequences
    * misc/ - Miscellaneous grammars used for testing purposes
    * terminals/ - Test grammars for terminals including the MIXED() function for handling mixed case
    * unused_rule/ - Grammar which deliberately creates unused rules to test if the software flags this up
    * utf8/ - Grammer to check if UTF8 characters are correctly handled
  * units/ - The PASCAL units which make up the core of the software. Most give a description in the header

#### Known Issues 
* None known

#### Author
Duncan Munro
