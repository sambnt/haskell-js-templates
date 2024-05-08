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
curl -d "1" -H "Content-Type: application/json" http://localhost:8081/counter
curl http://localhost:8081/counter

# Build frontend
nix build .#hello-frontend-js
# Serve frontend
python3 -m http.server --dir ./result/bin
```
