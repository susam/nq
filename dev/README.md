Developer Notes
===============

Release Checklist
-----------------

 1. Update copyright in [LICENSE.md][].
 2. Update [CHANGES.md][].
 3. Run the following commands:

    ```sh
    make checks
    git status
    git add -p

    M=
    VER=$M.0.0
    git commit -m "Set version to $VER"
    git tag $VER -m "Nerd Quiz #$VER"
    git push origin main $VER

    git remote add cb https://codeberg.org/susam/nq.git
    git push cb --all
    git push cb --tags
    ```

  4. Publish to website.

[LICENSE.md]: LICENSE.md
[CHANGES.md]: CHANGES.md
