Nerd Quiz
=========

**Nerd Quiz** is a single-page HTML application that lets you test
your nerdiness through short, focused quiz sets.  Each question is
handcrafted from real moments of curiosity and learning.

Whenever I encounter something curious or fascinating, I often turn it
into a quiz question to share it with you. Each question includes a
footer that shows when I wrote the question as well as the penalty
score if you answer it incorrectly.

New quizzes are published whenever I have five fresh questions ready.
There is no fixed schedule.


Try Nerd Quiz
-------------

The current stable version of Nerd Quiz is available at the following
links:

- https://susam.net/nq.html
- https://susam.github.io/nq.html
- https://susam.codeberg.page/nq.html


## How It Works

The hash value in the URL determines the quiz presented to you.

- If it is an integer like [#1][nq1], then the corresponding quiz with
  a set of five questions is loaded.
- If the hash value starts with the prefix `q`, like [`#q1`][nqq1],
  then a specific question is loaded.
- You can also mix and match questions to form a custom quiz.  For
  example [`#q1,3,5`][nqq135] loads questions 1, 3 and 5 together.

[nq1]: https://susam.net/nq.html#1
[nqq1]: https://susam.net/nq.html#q1
[nqq135]: https://susam.net/nq.html#q1,3,5


License
-------

This is free and open source software.  You can use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of it,
under the terms of the MIT License.  See [LICENSE.md][L] for details.

This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
express or implied. See [LICENSE.md][L] for details.

[L]: LICENSE.md


Contact
-------

To report bugs or ask questions, [create issues][ISSUES].

[ISSUES]: https://github.com/susam/nq/issues
