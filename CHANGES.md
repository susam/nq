Changelog
=========

2.0.0 (UNRELEASED)
------------------

### Added

- Quiz #2 with questions #6 to #10.

### Changed

- Replace 'None of the above' option for Question #5 (Phosphor) with
  another incorrect answer.
- Change nerd level phrase from 'Your nerd level today' to 'You have
  been deemed'.
- Rename nerd level 'Rookie' to 'Neophyte'.
- Prevent wrapping of multi-word nerd levels like 'Pro Nerd' to ensure
  the full name stays on one line.
- Use a faded dark colour scheme for disabled button in dark mode.
- Change the initial submit button from 'Submit Answers' to 'Reveal
  Answers'.  As soon as one question is answered, the submit button
  changes to 'Submit Answers'.
- URL `#0` now shows an error instead of loading the latest quiz.

### Fixed

- Hide 'About' page by default to prevent initial flash of text.
- Do not show duplicate questions for custom quizzes like `#q1,1,2,2'`.
- Display error when a non-existent QID is used in custom quiz URL.


1.0.0 (2025-10-16)
------------------

### Added

- Quiz #1 with questions #1 to #5.
- Light and dark colour themes.
- Load the latest quiz by default.
- Load individual questions using location hash like `#q1`, `#q2`, etc.
- Load custom quiz using location hash like `#q1,3`, `#q5,1,3`, etc.
- Invalid location hashes like `#0`, `#foo`, etc. load the latest quiz.
