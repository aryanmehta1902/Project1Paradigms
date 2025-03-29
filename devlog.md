Devlog

2025-03-26 09:00:00

Today I set up the project repository and reviewed the project requirements.  

-> Today
  - Created the Git repo.
  - Added initial project file `project1.rkt` with basic utility functions (`interactive?`, `display-prompt`, and `handle-error`).

-> Future work
  - Established project structure and made first commit.
  - started to implement expression evaluator for handling whitespace and addition.

---

2025-03-26 14:00:00

Implemented basic expression evaluation that skips whitespace and processes addition operator.  
-> Today
  - Developed `evaluate-expression` function to handle whitespace.
  - Added support for addition operator.
-> Future work
  - Tested addition functionality.
  - Began working on expanding support for other operators (multiplication, division, negation).

---

2025-03-27 10:00:00

Extended evaluator to support more arithmetic operations.  
-> Today
  - Added multiplication and division operators (with division-by-zero handling).
  - Implemented unary negation operator.
-> Future work
  - Test these new operators thoroughly.
  - Start integrating history lookup feature.

---

2025-03-27 17:30:00

Integrated history lookup and refined numeric literal parsing.  
-> Today
  - Implemented history lookup operator using the `$` symbol.
  - Created helper function `get-number` for parsing numeric values.
-> Future work
  - Validate history functionality with various expressions.
  - Finalize remaining evaluator logic and prepare for interactive loop.

---

2025-03-28 23:27:00

Finalized interactive evaluation loop and main routine.  
-> Today 
  - Added `evaluation-loop` function to handle user input continuously.
  - Ensured program runs in both interactive and batch modes.
  - Updated README file.
-> Reflection
  - The project met all functional requirements.
  - History lookup and error handling were challenging but now robust.
-> Future work
  - Prepare the final submission by cleaning up the repository and verifying that all files (including this devlog) are up-to-date.
