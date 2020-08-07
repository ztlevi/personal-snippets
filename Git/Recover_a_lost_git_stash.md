Fortunately, I managed to recover lost stashes. I’m not a Git expert, but here’s what worked for me after going through
various readings (including [some](https://stackoverflow.com/questions/89332/how-to-recover-a-dropped-stash-in-git)
[Stack Overflow](https://stackoverflow.com/questions/32517870/how-to-undo-git-stash-clear)
[answers](https://stackoverflow.com/questions/20537223/when-should-i-use-git-stash)).

Here’s the two-steps recovery procedure.

## [1. List lost stashes](https://dev.to/meduzen/recover-a-lost-git-stash-in-two-steps-569#1-list-lost-stashes)

Let’s run this command for a project where all stashes were trashed:

```
git fsck --unreachable | grep commit | cut -d ' ' -f3 | xargs git log --merges --no-walk
```

It returns a list of lost stashes, ordered by date.  
[![Ho, 3 lost stashes!](https://res.cloudinary.com/practicaldev/image/fetch/s--4JIMymbF--/c_limit%2Cf_auto%2Cfl_progressive%2Cq_auto%2Cw_880/https://thepracticaldev.s3.amazonaws.com/i/e696wbixz6qvz2r3bc4w.png)](https://res.cloudinary.com/practicaldev/image/fetch/s--4JIMymbF--/c_limit%2Cf_auto%2Cfl_progressive%2Cq_auto%2Cw_880/https://thepracticaldev.s3.amazonaws.com/i/e696wbixz6qvz2r3bc4w.png)

## 2. Send a lost stash back where it comes from

Let’s use the commit hash of the second stash:

```
git update-ref refs/stash 4b3fc45c94caadcc87d783064624585c194f4be8 -m "My recovered stash"
```

And that’s it! You’ll find your stash as usual, using `git stash list` or by having a look in your
[favorite Git client](http://gitup.co/).

## Gotchas

### 1. I still can’t see my recovered stash

Retry using the `--create-reflog` parameter (thanks [studoggithub](https://dev.to/studoggithub/comment/d54a)):

```
git update-ref refs/stash 4b3fc45c94caadcc87d783064624585c194f4be8 --create-reflog -m "My recovered stash"
```

### 2. My Git isn’t in English

If your Git isn’t in English, you’ll have to run `alias git='LANG=en_GB git'` each time you want to recover a set of
stashes (thanks [mathieuschopfer](https://dev.to/mathieuschopfer/comment/egd0)).

## Some advices

### Commit messages are healthy

**Always use a commit message** using `git stash save -m "My commit message"`: without message, the only informations
helping to identify a stash are its timestamp and the branch it was saved from, which may not be enough compared to a
strong explicit name.

Commit messages also help Git clients:

- GitUp, the Git client I use, completely fails at showing unnamed stashes. That’s probably why you can’t create a stash
  in GitUp without giving it a name, which is great!
- The well-known [SourceTree](https://www.sourcetreeapp.com/) succeeds at showing unnamed stashes, but as you can guess,
  the list isn’t friendly to browse:
  ![Unnamed stashes in SourceTree](https://res.cloudinary.com/practicaldev/image/fetch/s--APfyYmz0--/c_limit%2Cf_auto%2Cfl_progressive%2Cq_auto%2Cw_880/https://thepracticaldev.s3.amazonaws.com/i/lgval3bsy2g9x7h96avl.png)

### Yes, `git stash apply` > `git stash pop`

Unlike `git stash pop`, **`git stash apply`** does not remove the stash from the list of stashes, which can avoid some
loss.

### Branches > stashes

Finally, I’d recommend to avoid `git stash`. Instead, try to [use a branch](https://git-scm.com/docs/git-checkout). This
seems obvious but it only comes to me as I was finding a way to recover a stash: maybe I should use temporary branches
instead of stashes. Using the [Git Flow](http://nvie.com/posts/a-successful-git-branching-model/) method at work, this
could have come to my mind before encountering a painful experience.

If you have any _stash_ hint or experience that you want to share, comments are welcome.
