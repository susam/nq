Changelog
=========

2.0.0 (UNRELEASED)
------------------

### Added

- Quiz #2 with questions #6 to #10.
- Add quiz number or question number to the title.  If the quiz is
  composed of an arbitrary list of questions, mention 'Assorted' in
  the title.
- Show 'Correct' vs 'Incorrect' result explicitly while marking
  answers.
- Clicking the question card footer now opens a dialog displaying
  details about the question.
- Add 'Reveal Answers' link when a quiz loads.  This replaces the
  'Submit Answers' button that appeared earlier after loading a quiz.
  As soon as one question is answered, the 'Submit Answers' button
  appears while the 'Reveal Answers' link disappears.
- Add a text prompt to remind the user to select at least one answer
  to begin the quiz.  As soon as one question is answered, this prompt
  is hidden.

### Changed

- Replace 'None of the above' option for Question #5 (Phosphor) with
  another incorrect answer.
- Change nerd level phrase from 'Your nerd level today' to 'You have
  been deemed'.
- Rename nerd level 'Rookie' to 'Neophyte'.
- Prevent wrapping of multi-word nerd levels like 'Pro Nerd' to ensure
  the full name stays on one line.
- Use a faded dark colour scheme for disabled button in dark mode.
- URL `#0` now shows an error instead of loading the latest quiz.

### Fixed

- Hide 'About' page by default to prevent initial flash of text.
- Do not show duplicate questions for custom quizzes like `#q1,1,2,2'`.
- Display error when a non-existent QID is used in custom quiz URL.
- Render LaTeX content when a new quiz is loaded.


1.0.0 (2025-10-16)
------------------

### Added

- Quiz #1 with questions #1 to #5.
- Light and dark colour themes.
- Load the latest quiz by default.
- Load individual questions using location hash like `#q1`, `#q2`, etc.
- Load custom quiz using location hash like `#q1,3`, `#q5,1,3`, etc.
- Invalid location hashes like `#0`, `#foo`, etc. load the latest quiz.
