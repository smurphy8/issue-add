# issue-add
Issue-add started out life as a way to add labels for issues to an existing repo.  But is evolving into a batch system for repo handling. 

If a command is expecting authorization strings it will be suffixed with-auth that means provide auth-user and pass to it when ran

## Installation
The best way to install is to copy the relevant binaries into your PATH
## Usage
+ add-labels-with-auth
  - **give a user and a repo and it will add a default set of descriptive labels**
  - "Bug"                    
  - "Test"                   
  - "Enhancement"            
  - "Documentation"          
  - "Question"               
  - "User Interface"         
  - "Performance"            
  - "Monitoring/Logging"     
  - "Network Issue"          
  - "Deployment"             
  - "Regression"             
  - "Dependency Management"  
  - "Customer Request"       
  - "Field Service Request"  
  - "Need Discussion"        
  - "Need Test Case"         
  - "Need Info"              
  - "Duplicate"              
  - "Wontfix"                
  - "Ready"                  
  - "In Progress"            
  - "Clean"                  


+ add-issues-with-auth
  - give a user and a repo, plus a .org file  It will parse it with the following rules:

``` org-mode

* TODO <issue-title> :<label>:
<issue-body>
'@'<assignee>
```
## Add with
```
> add-issues-with-auth --auth-user="user" --pass="pass" --org-file="file" --user="repo-owner" --repo="repo"
```

## Notes:
* The labels you can use are defined above.
* the <text> represents the issue title
* the <issue-body> represents the issue form
* only 1 assignee per user

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

