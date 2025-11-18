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

  4. Write release notes:

     ```sh
     cd ~/git/susam.net/
     git checkout main
     em content/tree/code/news/nq/$VER.post.html
     ```

  5. Commit to website:

     ```
     cd ~/git/susam.net/
     git checkout main
     cp ~/git/nq/nq.html content/tree/nq.html
     git status
     git add -p
     git add content/tree/code/news/nq/$VER.post.html
     git commit -m "Add Nerd Quiz version $VER"
     git push
     ```

  6. Publish to website:

     ```
     git checkout cu
     git rebase main
     make pub
     ```

  7. Publish to website.

  8. Make a new release on GitHub.

  9. Make a new release on Codeberg.

 10. Share on Mastodon.

 11. Share on newsletter.

 12. Share on IRC.

[LICENSE.md]: LICENSE.md
[CHANGES.md]: CHANGES.md
