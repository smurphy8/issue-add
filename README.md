# issue-add
Issue-add started out life as a way to add labels for issues to an existing repo.  But is evolving into a batch system for repo handling. 

## Installation
The best way to install is to copy the relevant binaries into your PATH
## Usage
+ add-labels
  - give a user and a repo and it will add a default set of descriptive labels

+ issues-from-org
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

