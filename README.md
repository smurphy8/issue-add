# issue-add
Issue-add started out life as a way to add labels for issues to an existing repo.  But is evolving into a batch system for repo handling. 

If a command is expecting authorization strings it will be suffixed with-auth that means provide auth-user and pass to it when ran

## Installation
The best way to install is to copy the relevant binaries into your PATH
## Usage
+ add-labels-with-auth
  - give a user and a repo and it will add a default set of descriptive labels

+ add-issues-with-auth
  - give a user and a repo, plus a .org file  It will parse it with the folloing rules:

``` org-mode

* <Issue>
+ <Label>
+ <Label>
** Description
```

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

