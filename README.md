# slack-conversations

![](https://github.com/matsubara0507/haskell-slack-conversations/workflows/verify/badge.svg?branch=master)

[Slack Conversations API](https://api.slack.com/docs/conversations-api) Haskell Client library.


```Haskell
gchi> import Lens.Micro
gchi> import Data.Extensible
ghci> import Web.Slack.Conversations as Conversations
ghci> client = newClient "XXX" -- Slack API Token
ghci> Right ch <- run $ Conversations.info client "YYY" vacancy
ghci> ch ^. #channel ^. #name
Just "general"
```

## Usage

### Stack

write to stack.yaml:

```yaml
extra-deps:
  github: matsubara0507/haskell-slack-conversations
  commit: XXXXXXX
```

### Cabal

write to cabal.project

```cabal
source-repository-package
  type: git
  location: https://github.com/matsubara0507/haskell-slack-conversations
  tag: XXXXXXX
```
