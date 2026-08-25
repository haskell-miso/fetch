# :ramen: ⚡ miso-fetch

<a href="https://fetch.haskell-miso.org/">
  <img width="798" height="532" alt="image" src="https://github.com/user-attachments/assets/05af6001-94bf-4886-8a37-0b16ba57e3f7" />
</a>

### Usage

```haskell
----------------------------------------------------------------------------
data Action
  = FetchGitHub
  | SetGitHub (Response GitHub)
  | ErrorHandler (Response MisoString)
----------------------------------------------------------------------------
app :: App Model Action
app = component emptyModel updateModel viewModel
----------------------------------------------------------------------------
updateModel :: Action -> Effect ROOT Model Action
updateModel = \case
  FetchGitHub ->
    getJSON "https://api.github.com" [] SetGitHub ErrorHandler
  SetGitHub Response {..} ->
    info ?= body
  ErrorHandler Response {..} ->
    io_ (consoleError body)
----------------------------------------------------------------------------
```

## Build and run

Install [Nix Flakes](https://nixos.wiki/wiki/Flakes), then:

```
nix develop .#wasm
make
make serve
```
