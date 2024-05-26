# Auth Application

- Simple Haskell frontend/backend.
- Backend:
  - Servant GET/POST routes to view/modify a TVar.
  - POST requires authentication.
- Frontend:
  - Miso application, interfacing with the backend using servant-jsaddle.

## Running

```
# Build and run backend
nix build .#"hello-spa:exe:hello-backend" -o result-backend
./result-backend/bin/hello-backend

# Test it out
curl http://localhost:8081/counter
curl -d "1" -H "Content-Type: application/json" -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyLCJlbWFpbCI6ImZvb0BiYXIuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWV9.pV7R4m7Jo0hvWKVRJsTYrggTuYNZ1H0HP6kTpwagzEE" http://localhost:8081/counter
curl http://localhost:8081/counter

# Build frontend
nix build .#hello-frontend-js
# Serve frontend
nix-shell -p miniserve --command "miniserve --spa --index index.html --port 8000 ./result/bin"

javascript-unknown-ghcjs-cabal build exe:hello-frontend
```

## Notes

Be careful of packages you add to the "library". They must be able to be built by the Javascript backend of GHC.

For example, don't add `servant-server` to the library, it doesn't make sense to compile that the Javascript backend, and it won't work.

You may opt to split up the packages differently, with one package for the frontend, one for the backend, and one for shared code, that may lead to a more sensible module structure.

As it is, library code is placed in the `exe:hello-backend` component, simply to avoid putting it in the shared library.
