# dotfiles

## Highlights

- [xmonad](http://xmonad.org/) with a series of [dzen](http://robm.github.io/dzen/) status bars and menus
- `git` aware prompt
- optimized for the Dvorak keyboard layout

![Screenshot](./screenshot.png)

## Cloning

The simplest procedure for cloning into home, is to first clone into a temporary directory, move the `.git` folder into home, and from there pull and overwrite local files:

```sh
mkdir tmp
cd tmp
git clone git@github.com:Thhethssmuz/dotfiles.git
mv .git ~/.git
cd ..
rm -rf tmp

cd
git reset --hard
git pull
```

From this point on you should be able to proceed as normal.

## XMonad

Build status-bar icons:

```sh
.xmonad/scripts/mkicons.sh
```
