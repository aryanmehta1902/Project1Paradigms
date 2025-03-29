CS4337 Project 1 – Prefix Notation Calculator 
Aryan Mehta (AAM210028)

Description:
This project implements a prefix-notation expression calculator using Racket. The calculator supports basic arithmetic operations including addition, multiplication, integer division (with division-by-zero handling), and negation. It also maintains a history of evaluated expressions, allowing users to reference previous results using the '$' operator. The program can run in interactive mode (default) or in batch mode (using the -b or --batch flag).

Files:
- project1.rkt: Main source file containing calculator implementation.
- devlog.md: Development log documenting your progress and session notes.
- README.txt: This file, which explains project and how to run it.

How to Run:
1. Ensure you have Racket installed on your machine.
2. Open a terminal and navigate to project directory.
3. For interactive mode, run:
   racket project1.rkt
4. For batch mode, run:
   racket project1.rkt -b
   or
   racket project1.rkt --batch

Additional Notes:
-  commit history and devlog (devlog.md) included to showcase development process and invisible work.
- project has been designed to work on cs1 and cs2 machines and doesn't depend on any IDE-specific features or external libraries.
- Please refer to devlog.md for detailed insights into development sessions and challenges encountered.