# theinventory/server

### Setup

* Have a working `cabal` and somewhat modern `ghc`
* Have `~/.cabal/bin` in your `$PATH`
* `cabal update`
* `cabal install`
* `cabal install dbm`
* `dbm migrate development`
* `cabal run theinventory-server`

### Generating docs

* `cabal run theinventory-docs`

### Testing

You can use `curl` or `httpie` for testing. We use `httpie` here.

#### Examples

See [the API docs](API.md) for auto-generated documentation. Here are some
example endpoints worth trying:

* `http POST localhost:8081/tags`
* `http POST localhost:8081/tags/create name=communication parent_tag:=null`
* `http POST localhost:8081/tags/create name=phone parent_tag:=1`
* `http POST localhost:8081/tags/create name=landline parent_tag:=2`
* `http POST localhost:8081/tags/create name=cellphone parent_tag:=2`
* ... etc.
